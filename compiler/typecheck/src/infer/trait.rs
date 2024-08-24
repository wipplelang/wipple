use crate::{
    infer::{
        errors::{ErrorReason, QueuedError},
        r#type::{infer_instance, infer_type},
        types::{
            context::TypeContext,
            instantiate::InstantiationContext,
            unify::{unify, unify_instance, unify_instance_with_options, UnifyOptions},
            Instance, Type, TypeKind,
        },
        ExpressionKind, FinalizeContext, FormattedText, ResolveContext,
    },
    Driver,
};
use itertools::Itertools;
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
        type_context: &mut TypeContext<D>,
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
                reasons: type_context.reasons.clone(),
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

                // Special case: If the instance has any `Because` bounds, add
                // the reason to the type context
                if let Some(because_trait_path) = context.driver.path_for_language_trait("because")
                {
                    if candidate.item.1.r#trait == because_trait_path {
                        if let Some(reason) = candidate.item.1.parameters.first() {
                            resolve_custom_reason(&query.info, reason, context);
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
                reasons: context.type_context.reasons.clone(),
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
    let mut error_fix_message = None;
    let mut error_fix_code = None;
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

                    // Error fix
                    TypeKind::Declared { path, parameters }
                        if context
                            .driver
                            .path_for_language_type("error-fix")
                            .is_some_and(|error_fix_path| *path == error_fix_path) =>
                    {
                        if let Some((message_type, code_type)) = parameters.iter().collect_tuple() {
                            if let TypeKind::Message { segments, trailing } = &message_type.kind {
                                error_fix_message = Some(FormattedText {
                                    segments: segments.clone(),
                                    trailing: trailing.clone(),
                                });
                            }

                            if let TypeKind::Message { segments, trailing } = &code_type.kind {
                                error_fix_code = Some(FormattedText {
                                    segments: segments.clone(),
                                    trailing: trailing.clone(),
                                });
                            }
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
            fix: error_fix_message.zip(error_fix_code),
            location: error_location,
            reasons: context.type_context.reasons.clone(),
        },
    })
}

pub fn resolve_custom_reason<D: Driver>(
    reason_info: &D::Info,
    message_type: &Type<D>,
    context: &mut ResolveContext<'_, D>,
) {
    let message_type = message_type.apply_in_context(context.type_context);

    match &message_type.kind {
        TypeKind::Message { segments, trailing } => {
            context.type_context.add_reason(WithInfo {
                info: reason_info.clone(),
                item: ErrorReason::Custom(FormattedText {
                    segments: segments.clone(),
                    trailing: trailing.clone(),
                }),
            });
        }
        TypeKind::Tuple(elements) => {
            for element in elements {
                match &element.kind {
                    TypeKind::Message { segments, trailing } => {
                        context.type_context.add_reason(WithInfo {
                            info: reason_info.clone(),
                            item: ErrorReason::Custom(FormattedText {
                                segments: segments.clone(),
                                trailing: trailing.clone(),
                            }),
                        });
                    }
                    _ => {
                        context.type_context.add_reason(WithInfo {
                            info: element.info.clone(),
                            item: ErrorReason::Expression(element.clone()),
                        });
                    }
                }
            }
        }
        _ => {}
    }
}

pub fn refine_mismatch_error<D: Driver>(
    info: &mut D::Info,
    actual: &mut Type<D>,
    expected: &mut Type<D>,
    finalize_context: &mut FinalizeContext<'_, D>,
) {
    actual.apply_in_context_mut(finalize_context.type_context);
    expected.apply_in_context_mut(finalize_context.type_context);

    loop {
        let prev_actual = actual.clone();
        let prev_expected = expected.clone();

        // Highlight the last statement in a 'do' block instead of the entire block
        if let Some(actual_expression_id) = actual.expression {
            let actual_expression = finalize_context
                .type_context
                .tracked_expression(actual_expression_id);

            if let ExpressionKind::Do(block_expression) = &actual_expression.item.kind {
                if let ExpressionKind::Block { statements, .. } = &block_expression.item.kind {
                    if let Some(last_statement) = statements.last() {
                        *info = last_statement.info.clone();
                        *actual = last_statement.item.r#type.clone();
                    }
                }
            }
        }

        // Highlight the call expression instead of the function if the output
        // is mismatched
        if let TypeKind::Function {
            inputs: actual_inputs,
            output: actual_output,
        } = &actual.kind
        {
            if let TypeKind::Function {
                inputs: expected_inputs,
                output: expected_output,
            } = &expected.kind
            {
                if actual_inputs.len() == expected_inputs.len()
                    && actual_inputs
                        .iter()
                        .zip(expected_inputs)
                        .all(|(actual, expected)| {
                            unify(
                                finalize_context.driver,
                                actual,
                                expected,
                                finalize_context.type_context,
                            )
                        })
                {
                    if let Some(actual_parent_expression_id) = actual.parent_expression {
                        let actual_parent_expression = finalize_context
                            .type_context
                            .tracked_expression(actual_parent_expression_id);

                        if let ExpressionKind::Call { function, .. } =
                            &actual_parent_expression.item.kind
                        {
                            if function.info == actual.info {
                                let actual_output = actual_output.as_ref().clone();
                                let expected_output = expected_output.as_ref().clone();

                                *info = actual_parent_expression.info.clone();
                                *expected = actual_output;
                                *actual = expected_output;
                            }
                        }
                    }
                }
            }
        }

        // Track down the source of a mismatched function output
        if let TypeKind::Function {
            inputs: actual_inputs,
            output: actual_output,
        } = &actual.kind
        {
            if let TypeKind::Function {
                inputs: expected_inputs,
                output: expected_output,
            } = &expected.kind
            {
                if actual_inputs.len() == expected_inputs.len()
                    && actual_inputs
                        .iter()
                        .zip(expected_inputs)
                        .all(|(actual, expected)| {
                            unify(
                                finalize_context.driver,
                                actual,
                                expected,
                                finalize_context.type_context,
                            )
                        })
                    && !unify(
                        finalize_context.driver,
                        actual_output,
                        expected_output,
                        finalize_context.type_context,
                    )
                {
                    if let Some(actual_expression_id) = actual.expression {
                        let actual_expression = finalize_context
                            .type_context
                            .tracked_expression(actual_expression_id);

                        // Don't look inside constants/traits/etc. to refine the
                        // error message
                        if !actual_expression.item.kind.is_reference() {
                            *info = actual_output.info.clone();
                            *actual = actual_output.as_ref().clone();
                            *expected = expected_output.as_ref().clone();
                        }
                    }
                }
            }
        }

        // TODO: If the error involves `A = B` on both sides, replace it with
        // "expected `A` but found `B`"

        if prev_actual == *actual && prev_expected == *expected {
            break;
        }
    }
}
