use crate::{
    infer::{
        errors::QueuedError,
        r#trait::resolve_trait,
        r#type::{infer_instance, infer_type},
        types::{
            instantiate::{InstantiationContext, InstantiationOptions},
            unify::{try_unify_expression, unify},
            Instance, Type,
        },
        Expression, ResolveContext,
    },
    Driver,
};
use wipple_util::WithInfo;

pub fn resolve_item<D: Driver>(
    path: &D::Path,
    mut use_expression: WithInfo<D::Info, &mut Expression<D>>,
    allow_unresolved_bounds: bool,
    context: &mut ResolveContext<'_, D>,
) -> Result<
    Option<(
        Vec<Type<D>>,
        Vec<WithInfo<D::Info, Result<D::Path, Instance<D>>>>,
    )>,
    WithInfo<D::Info, QueuedError<D>>,
> {
    let item_declaration = context.driver.get_constant_declaration(path);

    let use_info = use_expression.info.clone();

    // Instantiate the items' type, substituting inferred parameters with opaque
    // type variables

    let mut instantiation_context = InstantiationContext::from_parameters_with_options(
        context.driver,
        item_declaration.item.parameters,
        context.type_context,
        use_info.clone(),
        context.error_queue,
        context.errors,
        InstantiationOptions {
            instantiate_inferred_parameters_as_opaque: true,
        },
    );

    let instantiated_declared_type = infer_type(
        context.driver,
        WithInfo {
            info: use_info.clone(),
            item: &item_declaration.item.r#type.item,
        },
        instantiation_context.type_context,
        instantiation_context.error_queue,
        instantiation_context.errors,
    )
    .instantiate(context.driver, &mut instantiation_context);

    let instantiated_bounds = item_declaration
        .item
        .bounds
        .into_iter()
        .map(|bound| {
            infer_instance(
                context.driver,
                bound,
                instantiation_context.type_context,
                instantiation_context.error_queue,
                instantiation_context.errors,
            )
            .map(|bound| bound.instantiate(context.driver, &mut instantiation_context))
        })
        .collect::<Vec<_>>();

    let parameters = instantiation_context.into_types_for_parameters();

    // Unify instantiated declared type with type of referring expression

    try_unify_expression(
        context.driver,
        use_expression.as_deref_mut(),
        &instantiated_declared_type,
        context.type_context,
        context.error_queue,
    );

    // Check bounds

    let evaluate_bounds = {
        let instantiated_declared_type = instantiated_declared_type.clone();
        let use_type = use_expression.item.r#type.clone();
        let info = &use_info;

        move |bounds: &[WithInfo<D::Info, Instance<D>>],
              allow_unresolved: bool,
              context: &mut ResolveContext<'_, D>| {
            bounds
                .iter()
                .map(|bound| {
                    let query = WithInfo {
                        info: info.clone(),
                        item: bound.item.clone(),
                    };

                    match resolve_trait(query.as_ref(), context) {
                        Ok(candidate) => Ok(candidate),
                        Err(WithInfo {
                            info,
                            item: QueuedError::UnresolvedInstance { instance, .. },
                        }) if allow_unresolved => Ok(WithInfo {
                            info,
                            item: Err(instance),
                        }),
                        Err(error) => {
                            // Attempt to get a better error message by unifying the
                            // declared type with the use type again, now that the
                            // bounds have influenced the type
                            if !unify(
                                context.driver,
                                &use_type,
                                &instantiated_declared_type,
                                context.type_context,
                            ) {
                                Err(WithInfo {
                                    info: info.clone(),
                                    item: QueuedError::Mismatch {
                                        actual: use_type.clone(),
                                        expected: instantiated_declared_type.clone(),
                                    },
                                })
                            } else {
                                Err(error)
                            }
                        }
                    }
                })
                .collect::<Vec<Result<_, _>>>()
        }
    };

    for result in evaluate_bounds(&instantiated_bounds, true, context) {
        result?;
    }

    // Turn the opaque type variables back into regular type variables so they
    // can be inferred now that we've checked the bounds

    let instantiated_declared_type =
        instantiated_declared_type.instantiate_opaque_in_context(context.type_context);

    let instantiated_bounds = instantiated_bounds
        .into_iter()
        .map(|bound| {
            bound
                .as_ref()
                .map(|instance| instance.instantiate_opaque(context.type_context))
        })
        .collect::<Vec<_>>();

    // Now that we've determined the types of the non-inferred parameters from the bounds,
    // determine the types of the inferred parameters by re-evaluating the bounds

    let mut bounds = Some(Vec::new());
    for result in evaluate_bounds(&instantiated_bounds, false, context) {
        match result {
            Ok(bound) => {
                if let Some(bounds) = bounds.as_mut() {
                    bounds.push(bound);
                }
            }
            Err(error) => {
                if allow_unresolved_bounds {
                    bounds = None;
                } else {
                    return Err(error);
                }
            }
        }
    }

    try_unify_expression(
        context.driver,
        use_expression,
        &instantiated_declared_type,
        context.type_context,
        context.error_queue,
    );

    Ok(bounds.map(|bounds| {
        (
            parameters
                .into_iter()
                .map(|parameter| parameter.instantiate_opaque_in_context(context.type_context))
                .collect(),
            bounds,
        )
    }))
}
