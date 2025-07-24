use crate::{
    typecheck::{
        Driver,
        infer::{
            FinalizeContext, FormatSegment,
            errors::{QueuedError, refine_unknown_type_for_error},
            types::{Instance, Type, TypeKind, context::TypeContext},
        },
    },
    util::WithInfo,
};

pub fn infer_type(
    driver: &dyn Driver,
    r#type: WithInfo<&crate::typecheck::Type>,
    type_context: &mut TypeContext,
    error_queue: &mut Vec<WithInfo<QueuedError>>,
    errors: &mut Vec<WithInfo<crate::typecheck::Diagnostic>>,
) -> Type {
    // In case we want to use these in the future
    let _ = driver;
    let _ = error_queue;
    let _ = errors;

    Type::new(
        match r#type.item {
            crate::typecheck::Type::Parameter(path) => TypeKind::Parameter(path.clone()),
            crate::typecheck::Type::Declared { path, parameters } => TypeKind::Declared {
                path: path.clone(),
                parameters: parameters
                    .iter()
                    .map(|r#type| {
                        infer_type(driver, r#type.as_ref(), type_context, error_queue, errors)
                    })
                    .collect(),
            },
            crate::typecheck::Type::Function { inputs, output } => TypeKind::Function {
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
            crate::typecheck::Type::Tuple(elements) => TypeKind::Tuple(
                elements
                    .iter()
                    .map(|r#type| {
                        infer_type(driver, r#type.as_ref(), type_context, error_queue, errors)
                    })
                    .collect(),
            ),
            crate::typecheck::Type::Block(r#type) => TypeKind::Block(Box::new(infer_type(
                driver,
                r#type.as_deref(),
                type_context,
                error_queue,
                errors,
            ))),
            crate::typecheck::Type::Unknown => TypeKind::Variable(type_context.variable()),
            crate::typecheck::Type::Intrinsic => TypeKind::Intrinsic,
            crate::typecheck::Type::Message { segments, trailing } => TypeKind::Message {
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
            crate::typecheck::Type::Equal { left, right } => {
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

pub fn finalize_type(
    r#type: Type,
    report_error: bool,
    context: &mut FinalizeContext<'_>,
) -> WithInfo<crate::typecheck::Type> {
    pub fn finalize_type_inner(
        mut r#type: Type,
        context: &mut FinalizeContext<'_>,
        fully_resolved: &mut bool,
    ) -> WithInfo<crate::typecheck::Type> {
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

                    crate::typecheck::Type::Unknown
                }
                TypeKind::Parameter(path) => crate::typecheck::Type::Parameter(path),
                TypeKind::Declared { path, parameters } => crate::typecheck::Type::Declared {
                    path,
                    parameters: parameters
                        .into_iter()
                        .map(|r#type| finalize_type_inner(r#type, context, fully_resolved))
                        .collect(),
                },
                TypeKind::Function { inputs, output } => crate::typecheck::Type::Function {
                    inputs: inputs
                        .into_iter()
                        .map(|input| finalize_type_inner(input, context, fully_resolved))
                        .collect(),
                    output: finalize_type_inner(*output, context, fully_resolved).boxed(),
                },
                TypeKind::Tuple(elements) => crate::typecheck::Type::Tuple(
                    elements
                        .into_iter()
                        .map(|r#type| finalize_type_inner(r#type, context, fully_resolved))
                        .collect(),
                ),
                TypeKind::Block(r#type) => crate::typecheck::Type::Block(
                    finalize_type_inner(*r#type, context, fully_resolved).boxed(),
                ),
                TypeKind::Unknown => {
                    context.contains_unknown = true;
                    crate::typecheck::Type::Unknown
                }
                TypeKind::Intrinsic => crate::typecheck::Type::Intrinsic,
                TypeKind::Message { segments, trailing } => crate::typecheck::Type::Message {
                    segments: segments
                        .into_iter()
                        .map(|segment| crate::typecheck::MessageTypeFormatSegment {
                            text: segment.text,
                            r#type: finalize_type_inner(segment.value, context, fully_resolved),
                        })
                        .collect(),
                    trailing,
                },
                TypeKind::Equal { left, right } => crate::typecheck::Type::Equal {
                    left: finalize_type_inner(*left, context, fully_resolved).boxed(),
                    right: finalize_type_inner(*right, context, fully_resolved).boxed(),
                },
            },
        }
    }

    let refined_info = refine_unknown_type_for_error(&r#type, context);

    let mut fully_resolved = true;
    let finalized_type = finalize_type_inner(r#type, context, &mut fully_resolved);

    if report_error && !fully_resolved {
        if let Some(errors) = context.errors.as_deref_mut() {
            errors.push(match refined_info {
                Some(info) => WithInfo {
                    info,
                    item: crate::typecheck::Diagnostic::UnknownType(
                        crate::typecheck::Type::Unknown,
                    ),
                },
                None => WithInfo {
                    info: finalized_type.info.clone(),
                    item: crate::typecheck::Diagnostic::UnknownType(finalized_type.item.clone()),
                },
            });
        }
    }

    if let Some(types) = &mut context.subexpression_types {
        types.push(finalized_type.item.clone());
    }

    finalized_type
}

pub fn infer_instance(
    driver: &dyn Driver,
    instance: WithInfo<crate::typecheck::Instance>,
    type_context: &mut TypeContext,
    error_queue: &mut Vec<WithInfo<QueuedError>>,
    errors: &mut Vec<WithInfo<crate::typecheck::Diagnostic>>,
) -> WithInfo<Instance> {
    instance.map(|instance| Instance {
        r#trait: instance.r#trait,
        parameters: instance
            .parameters
            .into_iter()
            .map(|r#type| infer_type(driver, r#type.as_ref(), type_context, error_queue, errors))
            .collect(),
    })
}

pub fn finalize_instance(
    instance: Instance,
    context: &mut FinalizeContext<'_>,
) -> crate::typecheck::Instance {
    crate::typecheck::Instance {
        r#trait: instance.r#trait,
        parameters: instance
            .parameters
            .into_iter()
            .map(|r#type| finalize_type(r#type, true, context))
            .collect(),
    }
}
