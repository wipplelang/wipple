use crate::{
    infer::{
        r#trait::{refine_mismatch_error, resolve_trait},
        r#type::{finalize_instance, finalize_type, finalize_type_reason},
        types::{context::TypeContext, Instance, Type},
        FinalizeContext, FormattedText, ResolveContext,
    },
    Driver,
};
use derivative::Derivative;
use wipple_util::WithInfo;

// Instead of reporting unification errors immediately, queue them and then
// report them all once all type information has been collected.
#[derive(Derivative)]
#[derivative(Debug(bound = ""), PartialEq(bound = ""))]
pub enum QueuedError<D: Driver> {
    RecursionLimit,

    Mismatch {
        actual: Type<D>,
        expected: Type<D>,
        reasons: Vec<WithInfo<D::Info, ErrorReason<D>>>,
    },

    MissingInputs(Vec<Type<D>>),

    ExtraInput,

    UnresolvedInstance {
        instance: Instance<D>,
        candidates: Vec<D::Info>,
        stack: Vec<WithInfo<D::Info, Instance<D>>>,
        reasons: Vec<WithInfo<D::Info, ErrorReason<D>>>,
    },

    NotAStructure(Type<D>),

    MissingFields(Vec<String>),

    ExtraField,

    Custom {
        message: FormattedText<Type<D>>,
        fix: Option<(FormattedText<Type<D>>, FormattedText<Type<D>>)>,
        location: Option<Type<D>>,
        reasons: Vec<WithInfo<D::Info, ErrorReason<D>>>,
    },
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = "")
)]
pub enum ErrorReason<D: Driver> {
    Expression(Type<D>),
    Custom(FormattedText<Type<D>>),
}

pub fn report_queued_errors<D: Driver>(
    driver: &D,
    type_context: &mut TypeContext<D>,
    mut error_queue: Vec<WithInfo<D::Info, QueuedError<D>>>,
    errors: &mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
) {
    let mut finalize_context = FinalizeContext {
        driver,
        type_context,
        bound_instances: Default::default(),
        error_queue: None,
        errors: None,
        unresolved_variables: None,
        contains_unknown: false,
        subexpression_types: None,
    };

    // We `reverse` before `dedup`ing because if two errors are created for an
    // expression, the second one will have more specific type information, and
    // we want to keep that one when we `dedup`
    error_queue.sort_by_key(|error| error.info.clone());
    error_queue.reverse();
    error_queue.dedup();

    for error in error_queue {
        let mut info = error.info;
        let error = match error.item {
            QueuedError::RecursionLimit => Some(crate::Diagnostic::RecursionLimit),
            QueuedError::Mismatch {
                mut actual,
                mut expected,
                reasons,
            } => (|| {
                refine_mismatch_error(&mut info, &mut actual, &mut expected, &mut finalize_context);

                // Special case: Display a user-provided message if there is a
                // corresponding instance for `Mismatch`
                if try_report_custom_mismatch_error(
                    driver,
                    &info,
                    &actual,
                    &expected,
                    finalize_context.type_context,
                    errors,
                ) {
                    return None;
                }

                let mut reasons = reasons
                    .into_iter()
                    .filter_map(|reason| {
                        reason.filter_map(|reason| {
                            finalize_type_reason(reason, &mut finalize_context)
                        })
                    })
                    .collect::<Vec<_>>();

                reasons.sort_by_key(|reason| reason.info.clone());
                reasons.dedup();

                Some(crate::Diagnostic::Mismatch {
                    actual: finalize_type(actual, false, &mut finalize_context),
                    expected: finalize_type(expected, false, &mut finalize_context),
                    reasons,
                })
            })(),
            QueuedError::MissingInputs(inputs) => Some(crate::Diagnostic::MissingInputs(
                inputs
                    .into_iter()
                    .map(|input| finalize_type(input, false, &mut finalize_context))
                    .collect(),
            )),
            QueuedError::ExtraInput => Some(crate::Diagnostic::ExtraInput),
            QueuedError::UnresolvedInstance {
                instance,
                candidates,
                stack,
                reasons,
            } => {
                let mut reasons = reasons
                    .into_iter()
                    .filter_map(|reason| {
                        reason.filter_map(|reason| {
                            finalize_type_reason(reason, &mut finalize_context)
                        })
                    })
                    .collect::<Vec<_>>();

                reasons.sort_by_key(|reason| reason.info.clone());
                reasons.dedup();

                Some(crate::Diagnostic::UnresolvedInstance {
                    instance: finalize_instance(instance, &mut finalize_context),
                    candidates,
                    stack: stack
                        .into_iter()
                        .map(|instance| {
                            instance
                                .map(|instance| finalize_instance(instance, &mut finalize_context))
                        })
                        .collect(),
                    reasons,
                })
            }
            QueuedError::NotAStructure(r#type) => Some(crate::Diagnostic::NotAStructure(
                finalize_type(r#type, false, &mut finalize_context),
            )),
            QueuedError::MissingFields(fields) => Some(crate::Diagnostic::MissingFields(fields)),
            QueuedError::ExtraField => Some(crate::Diagnostic::ExtraField),
            QueuedError::Custom {
                message,
                fix,
                location,
                reasons,
            } => {
                pub fn report_message<D: Driver>(
                    text: FormattedText<Type<D>>,
                    finalize_context: &mut FinalizeContext<'_, D>,
                ) -> crate::CustomMessage<D> {
                    crate::CustomMessage {
                        segments: text
                            .segments
                            .into_iter()
                            .map(|segment| crate::MessageTypeFormatSegment {
                                text: segment.text,
                                r#type: finalize_type(segment.value, false, finalize_context),
                            })
                            .collect(),
                        trailing: text.trailing,
                    }
                }

                if let Some(location) = location {
                    info = location
                        .apply_in_context(finalize_context.type_context)
                        .info;
                }

                let mut reasons = reasons
                    .into_iter()
                    .filter_map(|reason| {
                        reason.filter_map(|reason| {
                            finalize_type_reason(reason, &mut finalize_context)
                        })
                    })
                    .collect::<Vec<_>>();

                reasons.sort_by_key(|reason| reason.info.clone());
                reasons.dedup();

                Some(crate::Diagnostic::Custom {
                    message: report_message(message, &mut finalize_context),
                    fix: fix.map(|(message, code)| {
                        (
                            report_message(message, &mut finalize_context),
                            report_message(code, &mut finalize_context),
                        )
                    }),
                    reasons,
                })
            }
        };

        if let Some(error) = error {
            errors.push(WithInfo { info, item: error });
        }
    }
}

pub fn try_report_custom_mismatch_error<D: Driver>(
    driver: &D,
    info: &D::Info,
    actual: &Type<D>,
    expected: &Type<D>,
    type_context: &mut TypeContext<D>,
    errors: &mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
) -> bool {
    if let Some(mismatch_trait_path) = driver.path_for_language_trait("mismatch") {
        let query = WithInfo {
            info: info.clone(),
            item: Instance {
                r#trait: mismatch_trait_path,
                parameters: vec![actual.clone(), expected.clone()],
            },
        };

        let mut temp_error_queue = Vec::new();
        let mut temp_errors = Vec::new();
        let mut resolve_context = ResolveContext {
            driver,
            type_context,
            error_queue: &mut temp_error_queue,
            errors: &mut temp_errors,
            variables: &mut Default::default(),
            recursion_stack: &mut Default::default(),
            bound_instances: Default::default(),
        };

        if resolve_trait(query.as_ref(), &mut resolve_context).is_ok() {
            let temp_error_queue = temp_error_queue;
            let mut temp_errors = temp_errors;

            let has_error = !temp_error_queue.is_empty();

            report_queued_errors(driver, type_context, temp_error_queue, &mut temp_errors);
            errors.extend(temp_errors);

            return has_error;
        }
    }

    false
}

pub fn try_report_custom_unused_error<D: Driver>(
    driver: &D,
    info: &D::Info,
    r#type: &Type<D>,
    type_context: &mut TypeContext<D>,
    errors: Option<&mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>>,
) -> bool {
    if let Some(unused_trait_path) = driver.path_for_language_trait("unused") {
        let query = WithInfo {
            info: info.clone(),
            item: Instance {
                r#trait: unused_trait_path,
                parameters: vec![r#type.clone()],
            },
        };

        let mut temp_error_queue = Vec::new();
        let mut temp_errors = Vec::new();
        let mut resolve_context = ResolveContext {
            driver,
            type_context: &mut type_context.clone(),
            error_queue: &mut temp_error_queue,
            errors: &mut temp_errors,
            variables: &mut Default::default(),
            recursion_stack: &mut Default::default(),
            bound_instances: Default::default(),
        };

        if resolve_trait(query.as_ref(), &mut resolve_context).is_ok() {
            let temp_error_queue = temp_error_queue;
            let mut temp_errors = temp_errors;

            let has_error = !temp_error_queue.is_empty();

            report_queued_errors(driver, type_context, temp_error_queue, &mut temp_errors);

            if let Some(errors) = errors {
                errors.extend(temp_errors);
            }

            return has_error;
        }
    }

    false
}
