use crate::{
    syntax::Location,
    typecheck::{
        Driver,
        infer::{
            ExpressionKind, FinalizeContext, FormattedText, ResolveContext,
            r#trait::resolve_trait,
            r#type::{finalize_instance, finalize_type},
            types::{Instance, Type, TypeKind, context::TypeContext, unify::unify},
        },
    },
    util::WithInfo,
};

// Instead of reporting unification errors immediately, queue them and then
// report them all once all type information has been collected.
#[derive(Debug, PartialEq)]
pub enum QueuedError {
    RecursionLimit,

    Mismatch {
        actual: Type,
        expected: Type,
    },

    MissingInputs(Vec<Type>),

    ExtraInput,

    UnresolvedInstance {
        instance: Instance,
        candidates: Vec<Location>,
        stack: Vec<WithInfo<Instance>>,
    },

    NotAStructure(Type),

    MissingFields(Vec<String>),

    ExtraField,

    Custom {
        message: FormattedText<Type>,
        description: Option<FormattedText<Type>>,
        location: Option<Type>,
    },
}

pub fn report_queued_errors(
    driver: &dyn Driver,
    type_context: &mut TypeContext,
    mut error_queue: Vec<WithInfo<QueuedError>>,
    errors: &mut Vec<WithInfo<crate::typecheck::Diagnostic>>,
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
            QueuedError::RecursionLimit => Some(crate::typecheck::Diagnostic::RecursionLimit),
            QueuedError::Mismatch {
                mut actual,
                mut expected,
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

                Some(crate::typecheck::Diagnostic::Mismatch {
                    actual: finalize_type(actual, false, &mut finalize_context),
                    expected: finalize_type(expected, false, &mut finalize_context),
                })
            })(),
            QueuedError::MissingInputs(inputs) => {
                Some(crate::typecheck::Diagnostic::MissingInputs(
                    inputs
                        .into_iter()
                        .map(|input| finalize_type(input, false, &mut finalize_context))
                        .collect(),
                ))
            }
            QueuedError::ExtraInput => Some(crate::typecheck::Diagnostic::ExtraInput),
            QueuedError::UnresolvedInstance {
                instance,
                candidates,
                stack,
            } => Some(crate::typecheck::Diagnostic::UnresolvedInstance {
                instance: finalize_instance(instance, &mut finalize_context),
                candidates,
                stack: stack
                    .into_iter()
                    .map(|instance| {
                        instance.map(|instance| finalize_instance(instance, &mut finalize_context))
                    })
                    .collect(),
            }),
            QueuedError::NotAStructure(r#type) => {
                Some(crate::typecheck::Diagnostic::NotAStructure(finalize_type(
                    r#type,
                    false,
                    &mut finalize_context,
                )))
            }
            QueuedError::MissingFields(fields) => {
                Some(crate::typecheck::Diagnostic::MissingFields(fields))
            }
            QueuedError::ExtraField => Some(crate::typecheck::Diagnostic::ExtraField),
            QueuedError::Custom {
                message,
                description,
                location,
            } => {
                pub fn report_message(
                    text: FormattedText<Type>,
                    finalize_context: &mut FinalizeContext<'_>,
                ) -> crate::typecheck::CustomMessage {
                    crate::typecheck::CustomMessage {
                        segments: text
                            .segments
                            .into_iter()
                            .map(|segment| crate::typecheck::MessageTypeFormatSegment {
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

                Some(crate::typecheck::Diagnostic::Custom {
                    message: report_message(message, &mut finalize_context),
                    description: description
                        .map(|description| report_message(description, &mut finalize_context)),
                })
            }
        };

        if let Some(error) = error {
            errors.push(WithInfo { info, item: error });
        }
    }
}

pub fn try_report_custom_mismatch_error(
    driver: &dyn Driver,
    info: &Location,
    actual: &Type,
    expected: &Type,
    type_context: &mut TypeContext,
    errors: &mut Vec<WithInfo<crate::typecheck::Diagnostic>>,
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

pub fn try_report_custom_unused_error(
    driver: &dyn Driver,
    info: &Location,
    r#type: &Type,
    type_context: &mut TypeContext,
    errors: Option<&mut Vec<WithInfo<crate::typecheck::Diagnostic>>>,
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

pub fn refine_mismatch_error(
    info: &mut Location,
    actual: &mut Type,
    expected: &mut Type,
    finalize_context: &mut FinalizeContext<'_>,
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

pub fn refine_unknown_type_for_error(
    r#type: &Type,
    finalize_context: &mut FinalizeContext<'_>,
) -> Option<Location> {
    let r#type = r#type.apply_in_context(finalize_context.type_context);

    loop {
        let prev_type = r#type.clone();

        // Highlight the call expression instead of the function if the output
        // is unknown
        if let TypeKind::Function { output, .. } = &r#type.kind {
            if output.is_currently_unknown() {
                if let Some(parent_expression_id) = r#type.parent_expression {
                    let parent_expression = finalize_context
                        .type_context
                        .tracked_expression(parent_expression_id);

                    if let ExpressionKind::Call { function, .. } = &parent_expression.item.kind {
                        if function.info == r#type.info {
                            return Some(parent_expression.info.clone());
                        }
                    }
                }
            }
        }

        if prev_type == r#type {
            break;
        }
    }

    None
}
