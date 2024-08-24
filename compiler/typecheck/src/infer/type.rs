use crate::{
    infer::{
        errors::{ErrorReason, QueuedError},
        types::{context::TypeContext, Instance, Type, TypeKind},
        FinalizeContext, FormatSegment,
    },
    Driver,
};
use wipple_util::WithInfo;

pub fn infer_type<D: Driver>(
    driver: &D,
    r#type: WithInfo<D::Info, &crate::Type<D>>,
    type_context: &mut TypeContext<D>,
    error_queue: &mut Vec<WithInfo<D::Info, QueuedError<D>>>,
    errors: &mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
) -> Type<D> {
    // In case we want to use these in the future
    let _ = driver;
    let _ = error_queue;
    let _ = errors;

    Type::new(
        match r#type.item {
            crate::Type::Parameter(path) => TypeKind::Parameter(path.clone()),
            crate::Type::Declared { path, parameters } => TypeKind::Declared {
                path: path.clone(),
                parameters: parameters
                    .iter()
                    .map(|r#type| {
                        infer_type(driver, r#type.as_ref(), type_context, error_queue, errors)
                    })
                    .collect(),
            },
            crate::Type::Function { inputs, output } => TypeKind::Function {
                inputs: inputs
                    .iter()
                    .map(|r#type| {
                        infer_type(driver, r#type.as_ref(), type_context, error_queue, errors)
                    })
                    .collect(),
                output: Box::new(infer_type(
                    driver,
                    output.as_deref(),
                    type_context,
                    error_queue,
                    errors,
                )),
            },
            crate::Type::Tuple(elements) => TypeKind::Tuple(
                elements
                    .iter()
                    .map(|r#type| {
                        infer_type(driver, r#type.as_ref(), type_context, error_queue, errors)
                    })
                    .collect(),
            ),
            crate::Type::Block(r#type) => TypeKind::Block(Box::new(infer_type(
                driver,
                r#type.as_deref(),
                type_context,
                error_queue,
                errors,
            ))),
            crate::Type::Unknown => TypeKind::Variable(type_context.variable()),
            crate::Type::Intrinsic => TypeKind::Intrinsic,
            crate::Type::Message { segments, trailing } => TypeKind::Message {
                segments: segments
                    .iter()
                    .map(|segment| FormatSegment {
                        text: segment.text.clone(),
                        value: infer_type(
                            driver,
                            segment.r#type.as_ref(),
                            type_context,
                            error_queue,
                            errors,
                        ),
                    })
                    .collect(),
                trailing: trailing.clone(),
            },
            crate::Type::Equal { left, right } => {
                let left = infer_type(driver, left.as_deref(), type_context, error_queue, errors);

                let right = infer_type(driver, right.as_deref(), type_context, error_queue, errors);

                TypeKind::Equal {
                    left: Box::new(left),
                    right: Box::new(right),
                }
            }
        },
        r#type.info,
    )
}

pub fn finalize_type<D: Driver>(
    r#type: Type<D>,
    report_error: bool,
    context: &mut FinalizeContext<'_, D>,
) -> WithInfo<D::Info, crate::Type<D>> {
    pub fn finalize_type_inner<D: Driver>(
        mut r#type: Type<D>,
        context: &mut FinalizeContext<'_, D>,
        fully_resolved: &mut bool,
    ) -> WithInfo<D::Info, crate::Type<D>> {
        r#type.apply_in_context_mut(context.type_context);

        let info = match r#type.expression {
            Some(expression_id) => context
                .type_context
                .tracked_expression(expression_id)
                .info
                .clone(),
            None => r#type.info,
        };

        WithInfo {
            info,
            item: match r#type.kind {
                TypeKind::Variable(var) | TypeKind::Opaque(var) => {
                    if let Some(unresolved_variables) = &mut context.unresolved_variables {
                        // Prevent displaying duplicate errors when a type variable is involved in
                        // more than one expression
                        if !unresolved_variables.contains(&var) {
                            *fully_resolved = false;
                            unresolved_variables.insert(var);
                        }
                    } else {
                        *fully_resolved = false;
                    }

                    crate::Type::Unknown
                }
                TypeKind::Parameter(path) => crate::Type::Parameter(path),
                TypeKind::Declared { path, parameters } => crate::Type::Declared {
                    path,
                    parameters: parameters
                        .into_iter()
                        .map(|r#type| finalize_type_inner(r#type, context, fully_resolved))
                        .collect(),
                },
                TypeKind::Function { inputs, output } => crate::Type::Function {
                    inputs: inputs
                        .into_iter()
                        .map(|input| finalize_type_inner(input, context, fully_resolved))
                        .collect(),
                    output: finalize_type_inner(*output, context, fully_resolved).boxed(),
                },
                TypeKind::Tuple(elements) => crate::Type::Tuple(
                    elements
                        .into_iter()
                        .map(|r#type| finalize_type_inner(r#type, context, fully_resolved))
                        .collect(),
                ),
                TypeKind::Block(r#type) => crate::Type::Block(
                    finalize_type_inner(*r#type, context, fully_resolved).boxed(),
                ),
                TypeKind::Unknown => {
                    context.contains_unknown = true;
                    crate::Type::Unknown
                }
                TypeKind::Intrinsic => crate::Type::Intrinsic,
                TypeKind::Message { segments, trailing } => crate::Type::Message {
                    segments: segments
                        .into_iter()
                        .map(|segment| crate::MessageTypeFormatSegment {
                            text: segment.text,
                            r#type: finalize_type_inner(segment.value, context, fully_resolved),
                        })
                        .collect(),
                    trailing,
                },
                TypeKind::Equal { left, right } => crate::Type::Equal {
                    left: finalize_type_inner(*left, context, fully_resolved).boxed(),
                    right: finalize_type_inner(*right, context, fully_resolved).boxed(),
                },
            },
        }
    }

    let mut fully_resolved = true;
    let finalized_type = finalize_type_inner(r#type, context, &mut fully_resolved);

    if report_error && !fully_resolved {
        if let Some(errors) = context.errors.as_deref_mut() {
            errors.push(WithInfo {
                info: finalized_type.info.clone(),
                item: crate::Diagnostic::UnknownType(finalized_type.item.clone()),
            });
        }
    }

    if let Some(types) = &mut context.subexpression_types {
        types.push(finalized_type.item.clone());
    }

    finalized_type
}

pub fn finalize_type_reason<D: Driver>(
    reason: ErrorReason<D>,
    context: &mut FinalizeContext<'_, D>,
) -> Option<crate::ErrorReason<D>> {
    Some(match reason {
        ErrorReason::Expression(mut r#type) => {
            r#type.apply_in_context_mut(context.type_context);
            let expression = context.type_context.tracked_expression(r#type.expression?);

            crate::ErrorReason::Expression(finalize_type(
                expression.item.r#type.clone(),
                false,
                context,
            ))
        }
        ErrorReason::Custom(message) => crate::ErrorReason::Custom(crate::CustomMessage {
            segments: message
                .segments
                .into_iter()
                .map(|segment| crate::MessageTypeFormatSegment {
                    text: segment.text,
                    r#type: finalize_type(segment.value, false, context),
                })
                .collect(),
            trailing: message.trailing,
        }),
    })
}

pub fn infer_instance<D: Driver>(
    driver: &D,
    instance: WithInfo<D::Info, crate::Instance<D>>,
    type_context: &mut TypeContext<D>,
    error_queue: &mut Vec<WithInfo<D::Info, QueuedError<D>>>,
    errors: &mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
) -> WithInfo<D::Info, Instance<D>> {
    instance.map(|instance| Instance {
        r#trait: instance.r#trait,
        parameters: instance
            .parameters
            .into_iter()
            .map(|r#type| infer_type(driver, r#type.as_ref(), type_context, error_queue, errors))
            .collect(),
    })
}

pub fn finalize_instance<D: Driver>(
    instance: Instance<D>,
    context: &mut FinalizeContext<'_, D>,
) -> crate::Instance<D> {
    crate::Instance {
        r#trait: instance.r#trait,
        parameters: instance
            .parameters
            .into_iter()
            .map(|r#type| finalize_type(r#type, true, context))
            .collect(),
    }
}
