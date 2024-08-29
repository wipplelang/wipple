pub mod errors;
pub mod expression;
pub mod item;
pub mod pattern;
pub mod r#trait;
pub mod r#type;
pub mod types;

use crate::{
    infer::{
        errors::{report_queued_errors, QueuedError},
        expression::{
            finalize_expression, infer_expression, resolve_expression,
            substitute_defaults_in_expression,
        },
        r#type::{infer_instance, infer_type},
        types::{context::TypeContext, unify::try_unify_expression, Instance, Type, TypeVariable},
    },
    Driver,
};
use derivative::Derivative;
use std::collections::{HashMap, HashSet};
use wipple_util::WithInfo;

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
pub struct Expression<D: Driver> {
    pub r#type: Type<D>,
    pub kind: ExpressionKind<D>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
pub enum ExpressionKind<D: Driver> {
    Unknown(Option<Box<ExpressionKind<D>>>),
    Variable(String, D::Path),
    UnresolvedConstant(D::Path),
    UnresolvedTrait(D::Path),
    ResolvedConstant {
        path: D::Path,
        parameters: Vec<Type<D>>,
        bounds: Vec<WithInfo<D::Info, Result<D::Path, Instance<D>>>>,
    },
    ResolvedTrait {
        trait_path: D::Path,
        parameters: Vec<Type<D>>,
        instance: WithInfo<D::Info, Result<D::Path, Instance<D>>>,
    },
    Number(String),
    Text(String),
    Block {
        statements: Vec<WithInfo<D::Info, Expression<D>>>,
        captures: Vec<D::Path>,
        top_level: bool,
    },
    Do(WithInfo<D::Info, Box<Expression<D>>>),
    Function {
        inputs: Vec<WithInfo<D::Info, crate::Pattern<D>>>,
        body: WithInfo<D::Info, Box<Expression<D>>>,
        captures: Vec<D::Path>,
    },
    Call {
        function: WithInfo<D::Info, Box<Expression<D>>>,
        inputs: Vec<WithInfo<D::Info, Expression<D>>>,
    },
    When {
        input: WithInfo<D::Info, Box<Expression<D>>>,
        arms: Vec<WithInfo<D::Info, Arm<D>>>,
    },
    Intrinsic {
        name: String,
        inputs: Vec<WithInfo<D::Info, Expression<D>>>,
    },
    Initialize {
        pattern: WithInfo<D::Info, crate::Pattern<D>>,
        value: WithInfo<D::Info, Box<Expression<D>>>,
    },
    Mutate {
        name: String,
        path: WithInfo<D::Info, D::Path>,
        value: WithInfo<D::Info, Box<Expression<D>>>,
    },
    Marker(D::Path),
    UnresolvedStructure(Vec<WithInfo<D::Info, StructureFieldValue<D>>>),
    ResolvedStructure {
        structure: D::Path,
        fields: Vec<WithInfo<D::Info, StructureFieldValue<D>>>,
    },
    Variant {
        variant: WithInfo<D::Info, D::Path>,
        values: Vec<WithInfo<D::Info, Expression<D>>>,
    },
    Wrapper(WithInfo<D::Info, Box<Expression<D>>>),
    Tuple(Vec<WithInfo<D::Info, Expression<D>>>),
    Format {
        segments: Vec<FormatSegment<WithInfo<D::Info, Expression<D>>>>,
        trailing: String,
    },
}

impl<D: Driver> ExpressionKind<D> {
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

#[derive(Debug, Clone, PartialEq)]
pub struct FormattedText<T> {
    pub segments: Vec<FormatSegment<T>>,
    pub trailing: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FormatSegment<T> {
    pub text: String,
    pub value: T,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
pub struct StructureFieldValue<D: Driver> {
    pub name: String,
    pub value: WithInfo<D::Info, Expression<D>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
pub struct Arm<D: Driver> {
    pub pattern: WithInfo<D::Info, crate::Pattern<D>>,
    pub body: WithInfo<D::Info, Expression<D>>,
}

pub struct InferContext<'a, D: Driver> {
    pub driver: &'a D,
    pub type_context: &'a mut TypeContext<D>,
    pub error_queue: &'a mut Vec<WithInfo<D::Info, QueuedError<D>>>,
    pub errors: &'a mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
    pub variables: &'a mut HashMap<D::Path, Type<D>>,
}

pub struct ResolveContext<'a, D: Driver> {
    pub driver: &'a D,
    pub type_context: &'a mut TypeContext<D>,
    pub error_queue: &'a mut Vec<WithInfo<D::Info, QueuedError<D>>>,
    pub errors: &'a mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
    pub variables: &'a mut HashMap<D::Path, Type<D>>,
    pub recursion_stack: &'a mut Vec<D::Info>,
    pub bound_instances: Vec<Vec<WithInfo<D::Info, Instance<D>>>>,
}

pub struct FinalizeContext<'a, D: Driver> {
    pub driver: &'a D,
    pub type_context: &'a mut TypeContext<D>,
    pub bound_instances: Vec<Vec<WithInfo<D::Info, Instance<D>>>>,
    pub error_queue: Option<&'a mut Vec<WithInfo<D::Info, QueuedError<D>>>>,
    pub errors: Option<&'a mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>>,
    pub unresolved_variables: Option<&'a mut HashSet<TypeVariable<D>>>,
    pub contains_unknown: bool,
    pub subexpression_types: Option<&'a mut Vec<crate::Type<D>>>, // in the order of traversal
}

impl<'a, 'b: 'a, D: Driver> InferContext<'a, D> {
    pub fn from_resolve_context(context: &'a mut ResolveContext<'b, D>) -> Self {
        InferContext {
            driver: context.driver,
            type_context: context.type_context,
            error_queue: context.error_queue,
            errors: context.errors,
            variables: context.variables,
        }
    }
}

pub fn resolve<D: Driver>(
    driver: &D,
    item_declaration: impl crate::IntoItemDeclaration<D>,
) -> crate::Result<D> {
    struct Queued<D: Driver> {
        use_info: D::Info,
        type_context: TypeContext<D>,
        bounds: Vec<Vec<WithInfo<D::Info, Instance<D>>>>,
        body: WithInfo<D::Info, Expression<D>>,
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
            return crate::Result {
                item: None,
                captures: Vec::new(),
                diagnostics: errors,
            }
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

    let bounds = vec![item_declaration
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
        .collect()];

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
                item: crate::Diagnostic::RecursionLimit,
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

    let mut errors = errors;
    report_queued_errors(driver, &mut queued.type_context, error_queue, &mut errors);

    // Remove `UnresolvedInstance` errors where all the types are unknown in
    // favor of `UnknownType` errors

    let is_unknown_type_error =
        |error: &WithInfo<_, _>| matches!(error.item, crate::Diagnostic::UnknownType(_));

    let not_unresolved_instance_all_unknown_error = |error: &WithInfo<_, _>| {
        !matches!(
            &error.item,
            crate::Diagnostic::UnresolvedInstance { instance, ..}
                if instance.parameters.iter().all(|r#type| matches!(r#type.item, crate::Type::Unknown))
        )
    };

    if errors.iter().any(is_unknown_type_error)
        && errors.iter().any(not_unresolved_instance_all_unknown_error)
    {
        errors.retain(not_unresolved_instance_all_unknown_error);
    }

    // Remove `UnknownType` errors in favor of other errors

    let not_unknown_type_error =
        |error: &WithInfo<_, _>| !matches!(error.item, crate::Diagnostic::UnknownType(_));

    if errors.iter().any(not_unknown_type_error) {
        errors.retain(not_unknown_type_error);
    }

    crate::Result {
        item: Some(item),
        captures: item_declaration.item.captures,
        diagnostics: errors,
    }
}
