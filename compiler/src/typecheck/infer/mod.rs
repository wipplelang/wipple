pub mod errors;
pub mod expression;
pub mod item;
pub mod pattern;
pub mod r#trait;
pub mod r#type;
pub mod types;

use crate::{
    lower::Path,
    syntax::Location,
    typecheck::{
        Driver,
        infer::{
            errors::{QueuedError, report_queued_errors},
            expression::{
                finalize_expression, infer_expression, resolve_expression,
                substitute_defaults_in_expression,
            },
            r#type::{infer_instance, infer_type},
            types::{
                Instance, Type, TypeVariable, context::TypeContext, unify::try_unify_expression,
            },
        },
    },
    util::WithInfo,
};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct Expression {
    pub r#type: Type,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Unknown(Option<Box<ExpressionKind>>),
    Variable(String, Path),
    UnresolvedConstant(Path),
    UnresolvedTrait(Path),
    ResolvedConstant {
        path: Path,
        substitutions: HashMap<Path, Type>,
        bounds: Vec<WithInfo<Result<Path, Instance>>>,
    },
    ResolvedTrait {
        trait_path: Path,
        substitutions: HashMap<Path, Type>,
        instance: WithInfo<Result<Path, Instance>>,
    },
    Number(String),
    Text(String),
    Block {
        statements: Vec<WithInfo<Expression>>,
        captures: Vec<Path>,
        top_level: bool,
    },
    Do(WithInfo<Box<Expression>>),
    Function {
        inputs: Vec<WithInfo<crate::typecheck::Pattern>>,
        body: WithInfo<Box<Expression>>,
        captures: Vec<Path>,
    },
    Call {
        function: WithInfo<Box<Expression>>,
        inputs: Vec<WithInfo<Expression>>,
    },
    When {
        input: WithInfo<Box<Expression>>,
        arms: Vec<WithInfo<Arm>>,
    },
    Intrinsic {
        name: String,
        inputs: Vec<WithInfo<Expression>>,
    },
    Initialize {
        pattern: WithInfo<crate::typecheck::Pattern>,
        value: WithInfo<Box<Expression>>,
    },
    Mutate {
        name: String,
        path: WithInfo<Path>,
        value: WithInfo<Box<Expression>>,
    },
    Marker(Path),
    UnresolvedStructure(Vec<WithInfo<StructureFieldValue>>),
    ResolvedStructure {
        structure: Path,
        fields: Vec<WithInfo<StructureFieldValue>>,
    },
    Variant {
        variant: WithInfo<Path>,
        values: Vec<WithInfo<Expression>>,
    },
    Wrapper(WithInfo<Box<Expression>>),
    Tuple(Vec<WithInfo<Expression>>),
    Format {
        segments: Vec<FormatSegment<WithInfo<Expression>>>,
        trailing: String,
    },
}

impl ExpressionKind {
    pub fn is_reference(&self) -> bool {
        matches!(
            self,
            ExpressionKind::UnresolvedConstant(_)
                | ExpressionKind::UnresolvedTrait(_)
                | ExpressionKind::ResolvedConstant { .. }
                | ExpressionKind::ResolvedTrait { .. }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormattedText<T> {
    pub segments: Vec<FormatSegment<T>>,
    pub trailing: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormatSegment<T> {
    pub text: String,
    pub value: T,
}

#[derive(Debug, Clone)]
pub struct StructureFieldValue {
    pub name: String,
    pub value: WithInfo<Expression>,
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub pattern: WithInfo<crate::typecheck::Pattern>,
    pub body: WithInfo<Expression>,
}

pub struct InferContext<'a> {
    pub driver: &'a dyn Driver,
    pub type_context: &'a mut TypeContext,
    pub error_queue: &'a mut Vec<WithInfo<QueuedError>>,
    pub errors: &'a mut Vec<WithInfo<crate::typecheck::Diagnostic>>,
    pub variables: &'a mut HashMap<Path, Type>,
}

pub struct ResolveContext<'a> {
    pub driver: &'a dyn Driver,
    pub type_context: &'a mut TypeContext,
    pub error_queue: &'a mut Vec<WithInfo<QueuedError>>,
    pub errors: &'a mut Vec<WithInfo<crate::typecheck::Diagnostic>>,
    pub variables: &'a mut HashMap<Path, Type>,
    pub recursion_stack: &'a mut Vec<Location>,
    pub bound_instances: Vec<Vec<WithInfo<Instance>>>,
}

pub struct FinalizeContext<'a> {
    pub driver: &'a dyn Driver,
    pub type_context: &'a mut TypeContext,
    pub bound_instances: Vec<Vec<WithInfo<Instance>>>,
    pub error_queue: Option<&'a mut Vec<WithInfo<QueuedError>>>,
    pub errors: Option<&'a mut Vec<WithInfo<crate::typecheck::Diagnostic>>>,
    pub unresolved_variables: Option<&'a mut HashSet<TypeVariable>>,
    pub contains_unknown: bool,
    pub subexpression_types: Option<&'a mut Vec<crate::typecheck::Type>>, // in the order of traversal
}

impl<'a, 'b: 'a> InferContext<'a> {
    pub fn from_resolve_context(context: &'a mut ResolveContext<'b>) -> Self {
        InferContext {
            driver: context.driver,
            type_context: context.type_context,
            error_queue: context.error_queue,
            errors: context.errors,
            variables: context.variables,
        }
    }
}

pub fn resolve(
    driver: &dyn Driver,
    item_declaration: impl crate::typecheck::IntoItemDeclaration,
) -> crate::typecheck::Result {
    struct Queued {
        use_info: Location,
        type_context: TypeContext,
        bounds: Vec<Vec<WithInfo<Instance>>>,
        body: WithInfo<Expression>,
    }

    let mut recursion_stack = Vec::new();
    let recursion_limit = driver.recursion_limit();

    let mut error_queue = Vec::new();
    let mut errors = Vec::new();

    let mut type_context = TypeContext::default();

    let mut variables = HashMap::new();
    let mut infer_context = InferContext {
        driver,
        type_context: &mut type_context,
        error_queue: &mut error_queue,
        errors: &mut errors,
        variables: &mut variables,
    };

    let item_declaration = item_declaration.into_item_declaration(driver, infer_context.errors);

    let item_declaration = match item_declaration.item {
        Some(declaration) => WithInfo {
            info: item_declaration.info,
            item: declaration.0,
        },
        None => {
            return crate::typecheck::Result {
                item: None,
                captures: Vec::new(),
                diagnostics: errors,
            };
        }
    };

    let declared_type = infer_type(
        driver,
        item_declaration.item.r#type.as_ref(),
        infer_context.type_context,
        infer_context.error_queue,
        infer_context.errors,
    );

    let body = infer_context.with_tracked_expression(None, |infer_context, expression_id| {
        infer_expression(item_declaration.item.body, expression_id, infer_context)
    });

    let bounds = vec![
        item_declaration
            .item
            .bounds
            .into_iter()
            .map(|bound| {
                infer_instance(
                    driver,
                    bound,
                    infer_context.type_context,
                    infer_context.error_queue,
                    infer_context.errors,
                )
            })
            .collect(),
    ];

    let mut queued = Queued {
        use_info: item_declaration.info,
        type_context,
        bounds,
        body,
    };

    let mut prev_subexpression_types = Vec::new();
    let item = loop {
        if recursion_stack.len() as u32 > recursion_limit {
            errors.push(WithInfo {
                info: queued.body.info.clone(),
                item: crate::typecheck::Diagnostic::RecursionLimit,
            });

            let mut finalize_context = FinalizeContext {
                driver,
                type_context: &mut queued.type_context,
                bound_instances: queued.bounds.clone(),
                error_queue: None,
                errors: None,
                unresolved_variables: None,
                contains_unknown: false,
                subexpression_types: None,
            };

            break finalize_expression(queued.body, false, &mut finalize_context);
        }

        let mut resolve_context = ResolveContext {
            driver,
            type_context: &mut queued.type_context,
            error_queue: &mut error_queue,
            errors: &mut errors,
            variables: &mut variables,
            recursion_stack: &mut recursion_stack,
            bound_instances: queued.bounds.clone(),
        };

        try_unify_expression(
            driver,
            queued.body.as_mut(),
            &declared_type,
            resolve_context.type_context,
            resolve_context.error_queue,
        );

        queued.body = resolve_expression(queued.body, &mut resolve_context);

        let subexpression_types = {
            let mut subexpression_types = Vec::new();

            let mut finalize_context = FinalizeContext {
                driver,
                type_context: resolve_context.type_context,
                bound_instances: queued.bounds.clone(),
                error_queue: None,
                errors: None,
                unresolved_variables: None,
                contains_unknown: false,
                subexpression_types: Some(&mut subexpression_types),
            };

            let item = finalize_expression(queued.body.clone(), false, &mut finalize_context);

            if finalize_context.contains_unknown {
                break item;
            }

            subexpression_types
        };

        let made_progress = subexpression_types != prev_subexpression_types;

        if !made_progress {
            let substituted_defaults = substitute_defaults_in_expression(
                driver,
                queued.body.as_mut(),
                &mut resolve_context,
            );

            if !substituted_defaults {
                let mut unresolved_variables = HashSet::new();

                let mut finalize_context = FinalizeContext {
                    driver,
                    type_context: &mut queued.type_context,
                    bound_instances: queued.bounds.clone(),
                    error_queue: Some(&mut error_queue),
                    errors: Some(&mut errors),
                    unresolved_variables: Some(&mut unresolved_variables),
                    contains_unknown: false,
                    subexpression_types: None,
                };

                break finalize_expression(queued.body, true, &mut finalize_context);
            }
        }

        prev_subexpression_types = subexpression_types;
        recursion_stack.push(queued.use_info.clone());
    };

    report_queued_errors(driver, &mut queued.type_context, error_queue, &mut errors);

    // Remove `UnresolvedInstance` errors where all the types are unknown in
    // favor of `UnknownType` errors

    let is_unknown_type_error =
        |error: &WithInfo<_>| matches!(error.item, crate::typecheck::Diagnostic::UnknownType(_));

    let not_unresolved_instance_all_unknown_error = |error: &WithInfo<_>| {
        !matches!(
            &error.item,
            crate::typecheck::Diagnostic::UnresolvedInstance { instance, ..}
                if instance.parameters.iter().all(|r#type| matches!(r#type.item, crate::typecheck::Type::Unknown))
        )
    };

    if errors.iter().any(is_unknown_type_error)
        && errors.iter().any(not_unresolved_instance_all_unknown_error)
    {
        errors.retain(not_unresolved_instance_all_unknown_error);
    }

    // Remove `UnknownType` errors in favor of other errors

    let not_unknown_type_error =
        |error: &WithInfo<_>| !matches!(error.item, crate::typecheck::Diagnostic::UnknownType(_));

    if errors.iter().any(not_unknown_type_error) {
        errors.retain(not_unknown_type_error);
    }

    crate::typecheck::Result {
        item: Some(item),
        captures: item_declaration.item.captures,
        diagnostics: errors,
    }
}
