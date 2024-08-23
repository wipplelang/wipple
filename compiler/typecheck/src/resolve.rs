use crate::{
    infer::{
        errors::report_queued_errors,
        expression::{
            finalize_expression, infer_expression, resolve_expression,
            substitute_defaults_in_expression,
        },
        r#type::{infer_instance, infer_type},
        types::{context::TypeContext, unify::try_unify_expression, Instance},
        Expression, FinalizeContext, InferContext, ResolveContext,
    },
    Driver,
};
use wipple_util::WithInfo;
use std::collections::{HashMap, HashSet};

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
