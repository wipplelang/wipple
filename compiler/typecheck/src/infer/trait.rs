use crate::{
    infer::{
        errors::QueuedError,
        r#type::{infer_instance, infer_type},
        types::{
            context::TypeContext,
            instantiate::InstantiationContext,
            unify::{unify_instance, unify_instance_with_options, UnifyOptions},
            Instance, Type, TypeKind,
        },
        FormattedText, ResolveContext,
    },
    Driver,
};
use wipple_util::WithInfo;

pub fn resolve_trait<D: Driver>(
    query: WithInfo<D::Info, &Instance<D>>,
    context: &mut ResolveContext<'_, D>,
) -> Result<WithInfo<D::Info, Result<D::Path, Instance<D>>>, WithInfo<D::Info, QueuedError<D>>> {
    type Candidate<D> = (
        TypeContext<D>,
        WithInfo<<D as Driver>::Info, (Option<<D as Driver>::Path>, Instance<D>)>,
        Vec<WithInfo<<D as Driver>::Info, Instance<D>>>,
    );

    pub fn pick_from_candidates<D: Driver>(
        mut candidates: Vec<Candidate<D>>,
        query: WithInfo<D::Info, &Instance<D>>,
        stack: &[WithInfo<D::Info, &Instance<D>>],
        _type_context: &mut TypeContext<D>,
    ) -> Result<Option<Candidate<D>>, WithInfo<D::Info, QueuedError<D>>> {
        match candidates.len() {
            0 => Ok(None),
            1 => Ok(Some(candidates.pop().unwrap())),
            _ => Err(query.map(|query| QueuedError::UnresolvedInstance {
                instance: query.clone(),
                candidates: candidates
                    .into_iter()
                    .map(|(_, candidate, _)| candidate.info)
                    .collect(),
                stack: stack
                    .iter()
                    .map(|instance| instance.as_deref().map(Instance::clone))
                    .collect(),
            })),
        }
    }

    pub fn resolve_trait_inner<D: Driver>(
        query: WithInfo<D::Info, &Instance<D>>,
        context: &mut ResolveContext<'_, D>,
        stack: &[WithInfo<D::Info, &Instance<D>>],
    ) -> Result<WithInfo<D::Info, Result<D::Path, Instance<D>>>, WithInfo<D::Info, QueuedError<D>>>
    {
        let r#trait = query.item.r#trait.clone();

        let recursion_limit = context.driver.recursion_limit();

        if context.recursion_stack.len() as u32 > recursion_limit {
            return Err(query.replace(QueuedError::RecursionLimit));
        }

        // First, check if there are any bound instances that match

        for bound_instances in context.bound_instances.iter() {
            let mut candidates = Vec::new();
            for bound_instance in bound_instances {
                let mut new_type_context = context.type_context.clone();

                let query = query.as_deref().map(Clone::clone);

                let bound = bound_instance.as_ref().map(Clone::clone);

                if unify_instance_with_options(
                    context.driver,
                    query.as_ref(),
                    bound.as_ref(),
                    &mut new_type_context,
                    UnifyOptions {
                        require_equal_type_parameters: true,
                        ..Default::default()
                    },
                ) {
                    candidates.push((
                        new_type_context,
                        bound.map(|bound| (None, bound)),
                        Vec::new(),
                    ));
                }
            }

            if let Some((new_type_context, candidate, _)) =
                pick_from_candidates(candidates, query.clone(), stack, context.type_context)?
            {
                context.type_context.replace_with(new_type_context);
                return Ok(candidate.map(|(path, instance)| path.ok_or(instance)));
            }
        }

        // Then, check if there are any declared instances that match

        // Non-default instances have priority over default instances
        let (default_instances, instances): (Vec<_>, Vec<_>) = context
            .driver
            .get_instances_for_trait(&r#trait)
            .into_iter()
            .map(|path| (context.driver.get_instance_declaration(&path), path))
            .partition(|(instance, _)| instance.item.default);

        for instance_declarations in [instances, default_instances] {
            let mut candidates = Vec::new();
            for (instance_declaration, instance_path) in instance_declarations {
                let mut new_type_context = context.type_context.clone();

                let query = query.as_deref().map(Clone::clone);

                let mut instantiation_context = InstantiationContext::from_parameters(
                    context.driver,
                    instance_declaration.item.parameters,
                    &mut new_type_context,
                    query.info.clone(),
                    context.error_queue,
                    context.errors,
                );

                let parameters = instance_declaration
                    .item
                    .instance
                    .item
                    .parameters
                    .iter()
                    .map(|parameter| {
                        infer_type(
                            context.driver,
                            parameter.as_ref(),
                            instantiation_context.type_context,
                            instantiation_context.error_queue,
                            instantiation_context.errors,
                        )
                        .instantiate(context.driver, &mut instantiation_context)
                    })
                    .collect::<Vec<_>>();

                let bounds = instance_declaration
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

                let instance = WithInfo {
                    info: instance_declaration.info.clone(),
                    item: Instance {
                        r#trait: query.item.r#trait.clone(),
                        parameters,
                    },
                };

                if unify_instance(
                    context.driver,
                    query.as_ref(),
                    instance.as_ref(),
                    &mut new_type_context,
                ) {
                    candidates.push((
                        new_type_context,
                        instance.map(|instance| (Some(instance_path), instance)),
                        bounds,
                    ));
                }
            }

            // If an instance matches, check its bounds

            if let Some((new_type_context, mut candidate, bounds)) =
                pick_from_candidates(candidates, query.clone(), stack, context.type_context)?
            {
                context.type_context.replace_with(new_type_context);
                candidate.item.1.set_source_info(context, &query.info);

                for bound in bounds {
                    let mut stack = stack.to_vec();
                    stack.push(bound.as_ref());

                    context.recursion_stack.push(bound.info.clone());

                    let result = resolve_trait_inner(query.replace(&bound.item), context, &stack);

                    context.recursion_stack.pop();

                    result?;
                }

                // Special case: If the instance resolves to `Error`, display
                // the user-provided message
                if let Some(error_trait_path) = context.driver.path_for_language_trait("error") {
                    if candidate.item.1.r#trait == error_trait_path {
                        if let Some(message_type) = candidate.item.1.parameters.first() {
                            if let Some(error) =
                                resolve_custom_error(&query.info, message_type, context)
                            {
                                context.error_queue.push(error);
                            }
                        }
                    }
                }

                return Ok(candidate.map(|(path, instance)| path.ok_or(instance)));
            }
        }

        // If nothing matches, raise an error

        Err(WithInfo {
            info: stack.first().unwrap().info.clone(),
            item: QueuedError::UnresolvedInstance {
                instance: query.item.clone(),
                candidates: Vec::new(),
                stack: stack
                    .iter()
                    .map(|instance| instance.as_deref().map(Instance::clone))
                    .collect(),
            },
        })
    }

    resolve_trait_inner(query.clone(), context, &[query])
}

pub fn resolve_custom_error<D: Driver>(
    error_info: &D::Info,
    message_type: &Type<D>,
    context: &mut ResolveContext<'_, D>,
) -> Option<WithInfo<D::Info, QueuedError<D>>> {
    let message_type = message_type.apply_in_context(context.type_context);

    let mut error_message = None;
    let mut error_description = None;
    let mut error_location = None;
    match &message_type.kind {
        TypeKind::Message { segments, trailing } => {
            error_message = Some(FormattedText {
                segments: segments.clone(),
                trailing: trailing.clone(),
            });
        }
        TypeKind::Tuple(elements) => {
            for element in elements {
                match &element.kind {
                    // Error message
                    TypeKind::Message { segments, trailing } => {
                        error_message = Some(FormattedText {
                            segments: segments.clone(),
                            trailing: trailing.clone(),
                        });
                    }

                    // Error description
                    TypeKind::Declared { path, parameters }
                        if context
                            .driver
                            .path_for_language_type("error-description")
                            .is_some_and(|error_description_path| {
                                *path == error_description_path
                            }) =>
                    {
                        if let Some(description_type) = parameters.first() {
                            if let TypeKind::Message { segments, trailing } = &description_type.kind
                            {
                                error_description = Some(FormattedText {
                                    segments: segments.clone(),
                                    trailing: trailing.clone(),
                                });
                            }
                        }
                    }

                    // Error location
                    TypeKind::Declared { path, parameters }
                        if context
                            .driver
                            .path_for_language_type("error-location")
                            .is_some_and(|error_location_path| *path == error_location_path) =>
                    {
                        if let Some(location_type) = parameters.first() {
                            error_location = Some(location_type.clone());
                        }
                    }

                    _ => {}
                }
            }
        }
        _ => {}
    }

    error_message.map(|error_message| WithInfo {
        info: error_info.clone(),
        item: QueuedError::Custom {
            message: error_message,
            description: error_description,
            location: error_location,
        },
    })
}
