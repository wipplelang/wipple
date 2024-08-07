use crate::Driver;
use derivative::Derivative;
use itertools::Itertools;
use std::{
    collections::{btree_map, BTreeMap, HashMap, HashSet},
    fmt::Debug,
};
use wipple_util::WithInfo;

pub struct ItemDeclarationInner<D: Driver> {
    bounds: Vec<WithInfo<D::Info, crate::Instance<D>>>,
    r#type: WithInfo<D::Info, crate::Type<D>>,
    body: WithInfo<D::Info, crate::UntypedExpression<D>>,
    captures: Vec<D::Path>,
}

impl<D: Driver> crate::IntoItemDeclaration<D>
    for (
        WithInfo<D::Info, crate::ConstantDeclaration<D>>,
        crate::UntypedItem<D>,
    )
{
    fn into_item_declaration(
        self,
        _driver: &D,
        _errors: &mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
    ) -> WithInfo<D::Info, Option<crate::ItemDeclaration<D>>> {
        let (declaration, item) = self;

        declaration.map(|declaration| {
            Some(crate::ItemDeclaration(ItemDeclarationInner {
                bounds: declaration.bounds,
                r#type: declaration.r#type,
                body: item.body,
                captures: item.captures,
            }))
        })
    }
}

impl<D: Driver> crate::IntoItemDeclaration<D>
    for (
        WithInfo<D::Info, crate::InstanceDeclaration<D>>,
        Option<crate::UntypedItem<D>>,
    )
{
    fn into_item_declaration(
        self,
        driver: &D,
        errors: &mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
    ) -> WithInfo<D::Info, Option<crate::ItemDeclaration<D>>> {
        let (declaration, item) = self;

        let info = declaration.info.clone();

        let r#type = resolve_trait_type_from_instance(driver, declaration.item.instance.as_ref());

        WithInfo {
            info: info.clone(),
            item: match (r#type, item) {
                (Some(r#type), Some(item)) => Some(crate::ItemDeclaration(ItemDeclarationInner {
                    bounds: declaration.item.bounds,
                    r#type,
                    body: item.body,
                    captures: item.captures,
                })),
                (Some(r#type), None) => {
                    errors.push(WithInfo {
                        info,
                        item: crate::Diagnostic::ExpectedInstanceValue,
                    });

                    Some(crate::ItemDeclaration(ItemDeclarationInner {
                        bounds: declaration.item.bounds,
                        r#type,
                        body: WithInfo {
                            info: declaration.info,
                            item: crate::UntypedExpression::Unknown,
                        },
                        captures: Vec::new(),
                    }))
                }
                (None, Some(_)) => {
                    errors.push(WithInfo {
                        info,
                        item: crate::Diagnostic::UnexpectedInstanceValue,
                    });

                    None
                }
                (None, None) => None,
            },
        }
    }
}

impl<D: Driver> crate::IntoItemDeclaration<D> for crate::UntypedTopLevelCode<D> {
    fn into_item_declaration(
        self,
        driver: &D,
        _errors: &mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
    ) -> WithInfo<D::Info, Option<crate::ItemDeclaration<D>>> {
        WithInfo {
            info: driver.top_level_info(),
            item: Some(crate::ItemDeclaration(ItemDeclarationInner {
                bounds: Vec::new(),
                r#type: WithInfo {
                    info: driver.top_level_info(),
                    item: crate::Type::Unknown, // the top level can be any type
                },
                body: WithInfo {
                    info: driver.top_level_info(),
                    item: crate::UntypedExpression::Block {
                        statements: self.statements,
                        captures: Vec::new(),
                    },
                },
                captures: Vec::new(),
            })),
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
        infer_expression(
            item_declaration.item.body,
            Some(expression_id),
            infer_context,
        )
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

            break finalize_expression(queued.body, &mut finalize_context);
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

            let item = finalize_expression(queued.body.clone(), &mut finalize_context);

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

                break finalize_expression(queued.body, &mut finalize_context);
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

pub fn instances_overlap<D: Driver>(
    driver: &D,
    r#trait: &D::Path,
    mut instances: Vec<D::Path>,
) -> Vec<WithInfo<D::Info, crate::Diagnostic<D>>> {
    instances.sort();

    let trait_declaration = driver.get_trait_declaration(r#trait);
    let opaque_parameters = trait_declaration
        .item
        .parameters
        .iter()
        .filter_map(|parameter| {
            let parameter_declaration = driver.get_type_parameter_declaration(parameter);
            parameter_declaration.item.infer.map(|_| parameter)
        })
        .collect::<HashSet<_>>();

    let mut errors = Vec::new();

    let (instances, default_instances): (Vec<_>, Vec<_>) =
        instances.into_iter().partition_map(|path| {
            let declaration = driver.get_instance_declaration(&path);

            let mut type_context = TypeContext::default();
            let mut error_queue = Vec::new();

            let info = (
                path,
                declaration.info.clone(),
                declaration.item.parameters,
                infer_instance(
                    driver,
                    declaration.item.instance,
                    &mut type_context,
                    &mut error_queue,
                    &mut errors,
                ),
            );

            report_queued_errors(driver, &mut type_context, error_queue, &mut errors);

            if declaration.item.default {
                itertools::Either::Right(info)
            } else {
                itertools::Either::Left(info)
            }
        });

    for instances in [instances, default_instances] {
        let mut overlapping = HashSet::new();

        for (index, (path, info, parameters, instance)) in instances.iter().enumerate() {
            for (other_index, (other_path, _, other_parameters, other_instance)) in
                instances.iter().enumerate()
            {
                if index == other_index
                    || overlapping.contains(&(index, other_index))
                    || overlapping.contains(&(other_index, index))
                {
                    continue;
                }

                let mut type_context = TypeContext::default();

                let mut instantiate_instance =
                    |parameters: Vec<<D as Driver>::Path>,
                     instance: WithInfo<D::Info, &mut Instance<D>>| {
                        let mut unused_error_queue = Vec::new();
                        let mut unused_errors = Vec::new();

                        let mut instantiation_context = InstantiationContext::from_parameters(
                            driver,
                            parameters,
                            &mut type_context,
                            instance.info,
                            &mut unused_error_queue,
                            &mut unused_errors,
                        );

                        instance
                            .item
                            .instantiate_mut(driver, &mut instantiation_context);

                        // Only count non-inferred parameters in the overlap check
                        for (trait_parameter, instance_parameter) in trait_declaration
                            .item
                            .parameters
                            .iter()
                            .zip(&mut instance.item.parameters)
                        {
                            if opaque_parameters.contains(trait_parameter) {
                                instance_parameter.kind = TypeKind::Opaque(type_context.variable());
                            }
                        }
                    };

                let mut instance = instance.clone();

                instantiate_instance(parameters.clone(), instance.as_mut());

                let mut other_instance = other_instance.clone();

                instantiate_instance(other_parameters.clone(), other_instance.as_mut());

                if unify_instance(
                    driver,
                    instance.as_ref(),
                    other_instance.as_ref(),
                    &mut type_context,
                ) {
                    overlapping.insert((index, other_index));

                    errors.push(WithInfo {
                        info: info.clone(),
                        item: crate::Diagnostic::OverlappingInstances {
                            instance: path.clone(),
                            other: other_path.clone(),
                        },
                    });
                }
            }
        }
    }

    errors
}

pub fn resolve_trait_type_from_instance<D: Driver>(
    driver: &D,
    instance: WithInfo<D::Info, &crate::Instance<D>>,
) -> Option<WithInfo<D::Info, crate::Type<D>>> {
    let trait_declaration = driver.get_trait_declaration(&instance.item.r#trait);

    let r#type = trait_declaration.item.r#type.as_ref()?.as_ref();

    let mut type_context = TypeContext::default();
    let mut error_queue = Vec::new();
    let mut errors = Vec::new();

    let mut instantiation_context = InstantiationContext::from_parameters(
        driver,
        trait_declaration.item.parameters.clone(),
        &mut type_context,
        instance.info,
        &mut error_queue,
        &mut errors,
    );

    for (trait_parameter, instance_parameter) in trait_declaration
        .item
        .parameters
        .into_iter()
        .zip(&instance.item.parameters)
    {
        let r#type = instantiation_context.type_for_parameter(driver, &trait_parameter);

        assert!(unify(
            driver,
            &r#type,
            &infer_type(
                driver,
                instance_parameter.as_ref(),
                instantiation_context.type_context,
                instantiation_context.error_queue,
                instantiation_context.errors
            ),
            instantiation_context.type_context,
        ));
    }

    let r#type = infer_type(
        driver,
        r#type,
        instantiation_context.type_context,
        instantiation_context.error_queue,
        instantiation_context.errors,
    )
    .instantiate(driver, &mut instantiation_context);

    assert!(errors.is_empty());

    let mut finalize_context = FinalizeContext {
        driver,
        type_context: &mut type_context,
        bound_instances: Default::default(),
        error_queue: None,
        errors: None,
        unresolved_variables: None,
        contains_unknown: false,
        subexpression_types: None,
    };

    let r#type = finalize_type(r#type, false, &mut finalize_context);

    report_queued_errors(driver, &mut type_context, error_queue, &mut errors);

    Some(r#type)
}

pub fn substitute_defaults_in_parameters<D: Driver>(
    driver: &D,
    r#type: WithInfo<D::Info, &mut crate::Type<D>>,
) {
    match r#type.item {
        crate::Type::Parameter(path) => {
            if let Some(default) = driver.get_type_parameter_declaration(path).item.default {
                *r#type.item = default.item;
            }
        }
        crate::Type::Declared { parameters, .. } => {
            for parameter in parameters {
                substitute_defaults_in_parameters(driver, parameter.as_mut());
            }
        }
        crate::Type::Function { inputs, output } => {
            for input in inputs {
                substitute_defaults_in_parameters(driver, input.as_mut());
            }

            substitute_defaults_in_parameters(driver, output.as_deref_mut());
        }
        crate::Type::Tuple(elements) => {
            for element in elements {
                substitute_defaults_in_parameters(driver, element.as_mut());
            }
        }
        crate::Type::Block(body) => {
            substitute_defaults_in_parameters(driver, body.as_deref_mut());
        }
        crate::Type::Message { segments, .. } => {
            for segment in segments {
                substitute_defaults_in_parameters(driver, segment.r#type.as_mut());
            }
        }
        crate::Type::Equal { left, right } => {
            substitute_defaults_in_parameters(driver, left.as_deref_mut());
            substitute_defaults_in_parameters(driver, right.as_deref_mut());
        }
        crate::Type::Unknown | crate::Type::Intrinsic => {}
    }
}

pub fn resolve_attribute_like_trait<D: Driver>(
    driver: &D,
    language_item: &str,
    r#type: WithInfo<D::Info, &crate::Type<D>>,
    number_of_parameters: u32,
) -> Option<Vec<WithInfo<D::Info, crate::Type<D>>>> {
    let mut type_context = TypeContext::default();
    let mut error_queue = Vec::new();
    let mut errors = Vec::new();

    let r#type = infer_type(
        driver,
        r#type.clone(),
        &mut type_context,
        &mut error_queue,
        &mut errors,
    );

    if let Some(describe_type_trait_path) = driver.path_for_language_trait(language_item) {
        let mut resolve_context = ResolveContext {
            driver,
            type_context: &mut type_context,
            error_queue: &mut error_queue,
            errors: &mut errors,
            variables: &mut Default::default(),
            recursion_stack: &mut Default::default(),
            bound_instances: Default::default(),
        };

        let parameter_types = std::iter::repeat_with(|| {
            Type::new(
                TypeKind::Variable(resolve_context.type_context.variable()),
                r#type.info.clone(),
            )
        })
        .take(number_of_parameters as usize)
        .collect::<Vec<_>>();

        let query = WithInfo {
            info: r#type.info.clone(),
            item: Instance {
                r#trait: describe_type_trait_path,
                parameters: std::iter::once(r#type)
                    .chain(parameter_types.clone())
                    .collect(),
            },
        };

        if resolve_trait(query.as_ref(), &mut resolve_context).is_ok() {
            let mut finalize_context = FinalizeContext {
                driver,
                type_context: &mut type_context,
                bound_instances: Default::default(),
                error_queue: None,
                errors: None,
                unresolved_variables: None,
                contains_unknown: false,
                subexpression_types: None,
            };

            let parameter_types = parameter_types
                .into_iter()
                .map(|r#type| finalize_type(r#type, false, &mut finalize_context))
                .collect();

            // FIXME: Return errors
            report_queued_errors(driver, &mut type_context, error_queue, &mut errors);

            return Some(parameter_types);
        }
    }

    // FIXME: Return errors
    report_queued_errors(driver, &mut type_context, error_queue, &mut errors);

    None
}

// Instead of reporting unification errors immediately, queue them and then
// report them all once all type information has been collected.
#[derive(Derivative)]
#[derivative(Debug(bound = ""), PartialEq(bound = ""))]
enum QueuedError<D: Driver> {
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
enum ErrorReason<D: Driver> {
    Expression(Type<D>),
    Custom(FormattedText<Type<D>>),
}

fn report_queued_errors<D: Driver>(
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
                fn report_message<D: Driver>(
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

fn try_report_custom_mismatch_error<D: Driver>(
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

fn try_report_custom_unused_error<D: Driver>(
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

            if let Some(errors) = errors {
                errors.extend(temp_errors);
            }

            return has_error;
        }
    }

    false
}

// MARK: Types and type variables

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""), PartialEq(bound = ""))]
struct Type<D: Driver> {
    kind: TypeKind<D>,
    info: D::Info,
    #[derivative(PartialEq = "ignore")]
    expression: Option<TrackedExpressionId<D>>,
    #[derivative(PartialEq = "ignore")]
    parent_expression: Option<TrackedExpressionId<D>>,
}

impl<D: Driver> Type<D> {
    fn new(kind: TypeKind<D>, info: D::Info) -> Self {
        Type {
            kind,
            info,
            expression: None,
            parent_expression: None,
        }
    }

    fn with_expression(
        mut self,
        expression: TrackedExpressionId<D>,
        parent: Option<TrackedExpressionId<D>>,
    ) -> Self {
        self.expression = Some(expression);
        self.parent_expression = parent;
        self
    }

    fn contains_variable(&self, variable: &TypeVariable<D>) -> bool {
        match &self.kind {
            TypeKind::Variable(var) => var.counter == variable.counter,
            TypeKind::Declared { parameters, .. } => parameters
                .iter()
                .any(|r#type| r#type.contains_variable(variable)),
            TypeKind::Function { inputs, output } => {
                inputs
                    .iter()
                    .any(|r#type| r#type.contains_variable(variable))
                    || output.contains_variable(variable)
            }
            TypeKind::Tuple(elements) => elements
                .iter()
                .any(|r#type| r#type.contains_variable(variable)),
            TypeKind::Block(r#type) => r#type.contains_variable(variable),
            TypeKind::Message { segments, .. } => segments
                .iter()
                .any(|segment| segment.value.contains_variable(variable)),
            TypeKind::Equal { left, right } => {
                left.contains_variable(variable) || right.contains_variable(variable)
            }
            TypeKind::Opaque(_)
            | TypeKind::Parameter(_)
            | TypeKind::Unknown
            | TypeKind::Intrinsic => false,
        }
    }

    #[must_use]
    fn apply_in_context(&self, context: &mut TypeContext<D>) -> Self {
        let mut r#type = self.clone();
        r#type.apply_in_context_mut(context);
        r#type
    }

    fn apply_in_context_mut(&mut self, context: &mut TypeContext<D>) {
        match &mut self.kind {
            TypeKind::Variable(variable) => {
                let r#type =
                    match variable.with_substitution_mut(context, |substitution| match substitution
                    {
                        btree_map::Entry::Vacant(_) => None,
                        btree_map::Entry::Occupied(entry) => Some(entry.get().clone()),
                    }) {
                        Some(r#type) => r#type,
                        _ => return,
                    };

                assert!(!r#type.contains_variable(variable), "recursive type");

                self.kind = r#type.kind;

                if r#type.expression.is_some() {
                    self.info = r#type.info;
                }

                self.expression = self.expression.or(r#type.expression);
                self.parent_expression = self.parent_expression.or(r#type.parent_expression);
                self.apply_in_context_mut(context);
            }
            TypeKind::Opaque(_) => {}
            TypeKind::Parameter(_) => {}
            TypeKind::Declared { parameters, .. } => {
                for r#type in parameters {
                    r#type.apply_in_context_mut(context);
                }
            }
            TypeKind::Function { inputs, output } => {
                for r#type in inputs {
                    r#type.apply_in_context_mut(context);
                }

                output.apply_in_context_mut(context);
            }
            TypeKind::Tuple(elements) => {
                for r#type in elements {
                    r#type.apply_in_context_mut(context);
                }
            }
            TypeKind::Block(r#type) => {
                r#type.apply_in_context_mut(context);
            }
            TypeKind::Message { segments, .. } => {
                for segment in segments {
                    segment.value.apply_in_context_mut(context);
                }
            }
            TypeKind::Equal { left, right } => {
                left.apply_in_context_mut(context);
                right.apply_in_context_mut(context);
            }
            TypeKind::Unknown | TypeKind::Intrinsic => {}
        }
    }

    fn set_source_info(&mut self, context: &mut ResolveContext<'_, D>, info: &D::Info) {
        self.apply_in_context_mut(context.type_context);

        match &mut self.kind {
            TypeKind::Declared { path, parameters } => {
                if context
                    .driver
                    .path_for_language_type("source")
                    .is_some_and(|source_path| *path == source_path)
                {
                    self.info = info.clone();
                }

                for r#type in parameters {
                    r#type.set_source_info(context, info);
                }
            }
            TypeKind::Function { inputs, output } => {
                for r#type in inputs {
                    r#type.set_source_info(context, info);
                }

                output.set_source_info(context, info);
            }
            TypeKind::Tuple(elements) => {
                for r#type in elements {
                    r#type.set_source_info(context, info);
                }
            }
            TypeKind::Block(r#type) => {
                r#type.set_source_info(context, info);
            }
            TypeKind::Message { segments, .. } => {
                for segment in segments {
                    segment.value.set_source_info(context, info);
                }
            }
            TypeKind::Equal { left, right } => {
                left.set_source_info(context, info);
                right.set_source_info(context, info);
            }
            TypeKind::Variable(_)
            | TypeKind::Opaque(_)
            | TypeKind::Parameter(_)
            | TypeKind::Unknown
            | TypeKind::Intrinsic => {}
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""), PartialEq(bound = ""))]
enum TypeKind<D: Driver> {
    Variable(TypeVariable<D>),
    Opaque(TypeVariable<D>),
    Parameter(D::Path),
    Declared {
        path: D::Path,
        parameters: Vec<Type<D>>,
    },
    Function {
        inputs: Vec<Type<D>>,
        output: Box<Type<D>>,
    },
    Tuple(Vec<Type<D>>),
    Block(Box<Type<D>>),
    Intrinsic,
    Message {
        segments: Vec<FormatSegment<Type<D>>>,
        trailing: String,
    },
    Equal {
        left: Box<Type<D>>,
        right: Box<Type<D>>,
    },
    Unknown,
}

#[derive(Derivative)]
#[derivative(
    Copy(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
struct TypeVariable<D: Driver> {
    _driver: std::marker::PhantomData<D>,
    counter: u32,
}

impl<D: Driver> Clone for TypeVariable<D> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<D: Driver> Debug for TypeVariable<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TypeVariable({})", self.counter)
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""), PartialEq(bound = ""))]
struct Instance<D: Driver> {
    r#trait: D::Path,
    parameters: Vec<Type<D>>,
}

// MARK: Type context

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Default(bound = ""))]
struct TypeContext<D: Driver> {
    next_variable: u32,
    tracked_expressions: Vec<Option<WithInfo<D::Info, Expression<D>>>>,
    substitutions: BTreeMap<u32, Type<D>>,
    defaults: BTreeMap<u32, Type<D>>,
    reasons: Vec<WithInfo<D::Info, ErrorReason<D>>>,
}

impl<D: Driver> TypeContext<D> {
    fn replace_with(&mut self, other: Self) {
        self.next_variable = other.next_variable;
        self.substitutions = other.substitutions;
        self.defaults = other.defaults;

        self.reasons = other.reasons;
        self.reasons.sort_by_key(|reason| reason.info.clone());
        self.reasons.dedup();
    }

    fn tracked_expression(&self, id: TrackedExpressionId<D>) -> &WithInfo<D::Info, Expression<D>> {
        self.tracked_expressions[id.counter as usize]
            .as_ref()
            .expect("uninitialized tracked expression")
    }

    fn add_reason(&mut self, reason: WithInfo<D::Info, ErrorReason<D>>) {
        self.reasons.push(reason);
    }
}

#[derive(Derivative)]
#[derivative(
    Copy(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
struct TrackedExpressionId<D: Driver> {
    _driver: std::marker::PhantomData<D>,
    counter: u32,
}

impl<D: Driver> Clone for TrackedExpressionId<D> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<D: Driver> Debug for TrackedExpressionId<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TrackedExpressionId({})", self.counter)
    }
}

impl<D: Driver> InferContext<'_, D> {
    pub fn with_tracked_expression(
        &mut self,
        parent_id: Option<TrackedExpressionId<D>>,
        expression: impl FnOnce(&mut Self, TrackedExpressionId<D>) -> WithInfo<D::Info, Expression<D>>,
    ) -> WithInfo<D::Info, Expression<D>> {
        let id = TrackedExpressionId {
            _driver: std::marker::PhantomData,
            counter: self.type_context.tracked_expressions.len() as u32,
        };

        self.type_context.tracked_expressions.push(None);

        let mut expression = expression(self, id);
        expression.item.r#type = expression.item.r#type.with_expression(id, parent_id);

        self.type_context.tracked_expressions[id.counter as usize] = Some(expression.clone());

        expression
    }
}

// MARK: Instantiation context

struct InstantiationContext<'a, D: Driver> {
    type_context: &'a mut TypeContext<D>,
    types: Vec<(D::Path, Type<D>)>,
    info: D::Info,
    error_queue: &'a mut Vec<WithInfo<D::Info, QueuedError<D>>>,
    errors: &'a mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
}

#[derive(Clone, Copy, Default)]
#[non_exhaustive]
struct InstantiationOptions {
    instantiate_inferred_parameters_as_opaque: bool,
}

impl<'a, D: Driver> InstantiationContext<'a, D> {
    pub fn from_parameters(
        driver: &D,
        parameters: impl IntoIterator<Item = D::Path>,
        type_context: &'a mut TypeContext<D>,
        info: D::Info,
        error_queue: &'a mut Vec<WithInfo<D::Info, QueuedError<D>>>,
        errors: &'a mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
    ) -> Self {
        InstantiationContext::from_parameters_with_options(
            driver,
            parameters,
            type_context,
            info,
            error_queue,
            errors,
            Default::default(),
        )
    }

    pub fn from_parameters_with_options(
        driver: &D,
        parameters: impl IntoIterator<Item = D::Path>,
        type_context: &'a mut TypeContext<D>,
        info: D::Info,
        error_queue: &'a mut Vec<WithInfo<D::Info, QueuedError<D>>>,
        errors: &'a mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
        options: InstantiationOptions,
    ) -> Self {
        let mut context = InstantiationContext {
            type_context,
            types: Vec::new(),
            info: info.clone(),
            error_queue,
            errors,
        };

        for path in parameters {
            let parameter_declaration = driver.get_type_parameter_declaration(&path);

            let default = parameter_declaration
                .as_ref()
                .item
                .default
                .as_ref()
                .map(|r#type| {
                    infer_type(
                        driver,
                        r#type.as_ref(),
                        context.type_context,
                        context.error_queue,
                        context.errors,
                    )
                    .instantiate(driver, &mut context)
                });

            let variable = context.type_context.variable_with_default(default);

            let kind = if parameter_declaration.item.infer.is_some()
                && options.instantiate_inferred_parameters_as_opaque
            {
                TypeKind::Opaque(variable)
            } else {
                TypeKind::Variable(variable)
            };

            context.types.push((path, Type::new(kind, info.clone())));
        }

        context
    }

    pub fn type_for_parameter(&mut self, driver: &D, parameter: &D::Path) -> Type<D> {
        self.types
            .iter()
            .find_map(|(instantiation_path, r#type)| {
                driver
                    .paths_are_equal(parameter, instantiation_path)
                    .then_some(r#type)
            })
            .cloned()
            .unwrap_or_else(|| {
                self.errors.push(WithInfo {
                    info: self.info.clone(),
                    item: crate::Diagnostic::UndeclaredTypeParameter(parameter.clone()),
                });

                Type::new(TypeKind::Unknown, self.info.clone())
            })
    }

    pub fn into_types_for_parameters(self) -> Vec<Type<D>> {
        self.types.into_iter().map(|(_, r#type)| r#type).collect()
    }
}

impl<D: Driver> Type<D> {
    #[must_use]
    fn instantiate(
        &self,
        driver: &D,
        instantiation_context: &mut InstantiationContext<'_, D>,
    ) -> Self {
        let mut r#type = self.clone();
        r#type.instantiate_mut(driver, instantiation_context);
        r#type
    }

    fn instantiate_mut(
        &mut self,
        driver: &D,
        instantiation_context: &mut InstantiationContext<'_, D>,
    ) {
        self.apply_in_context_mut(instantiation_context.type_context);

        match &mut self.kind {
            TypeKind::Variable(_) => {}
            TypeKind::Opaque(_) => {}
            TypeKind::Parameter(path) => {
                *self = instantiation_context.type_for_parameter(driver, path);
            }
            TypeKind::Declared { parameters, .. } => {
                for r#type in parameters {
                    r#type.instantiate_mut(driver, instantiation_context);
                }
            }
            TypeKind::Function { inputs, output } => {
                for r#type in inputs {
                    r#type.instantiate_mut(driver, instantiation_context);
                }

                output.instantiate_mut(driver, instantiation_context);
            }
            TypeKind::Tuple(elements) => {
                for r#type in elements {
                    r#type.instantiate_mut(driver, instantiation_context);
                }
            }
            TypeKind::Block(r#type) => {
                r#type.instantiate_mut(driver, instantiation_context);
            }
            TypeKind::Message { segments, .. } => {
                for segment in segments {
                    segment.value.instantiate_mut(driver, instantiation_context);
                }
            }
            TypeKind::Equal { left, right } => {
                left.instantiate_mut(driver, instantiation_context);
                right.instantiate_mut(driver, instantiation_context);
            }
            TypeKind::Unknown | TypeKind::Intrinsic => {}
        }
    }

    #[must_use]
    fn instantiate_opaque_in_context(&self, context: &mut TypeContext<D>) -> Self {
        let mut r#type = self.clone();
        r#type.instantiate_opaque_in_context_mut(context);
        r#type
    }

    fn instantiate_opaque_in_context_mut(&mut self, context: &mut TypeContext<D>) {
        self.apply_in_context_mut(context);

        match &mut self.kind {
            TypeKind::Variable(_) => {}
            TypeKind::Opaque(variable) => {
                self.kind = TypeKind::Variable(*variable);
            }
            TypeKind::Parameter(_) => {}
            TypeKind::Declared { parameters, .. } => {
                for r#type in parameters {
                    r#type.instantiate_opaque_in_context_mut(context);
                }
            }
            TypeKind::Function { inputs, output } => {
                for r#type in inputs {
                    r#type.instantiate_opaque_in_context_mut(context);
                }

                output.instantiate_opaque_in_context_mut(context);
            }
            TypeKind::Tuple(elements) => {
                for r#type in elements {
                    r#type.instantiate_opaque_in_context_mut(context);
                }
            }
            TypeKind::Block(r#type) => {
                r#type.instantiate_opaque_in_context_mut(context);
            }
            TypeKind::Message { segments, .. } => {
                for segment in segments {
                    segment.value.instantiate_opaque_in_context_mut(context);
                }
            }
            TypeKind::Equal { left, right } => {
                left.instantiate_opaque_in_context_mut(context);
                right.instantiate_opaque_in_context_mut(context);
            }
            TypeKind::Unknown | TypeKind::Intrinsic => {}
        }
    }
}

impl<D: Driver> Instance<D> {
    #[must_use]
    fn instantiate(
        &self,
        driver: &D,
        instantiation_context: &mut InstantiationContext<'_, D>,
    ) -> Self {
        let mut instance = self.clone();
        instance.instantiate_mut(driver, instantiation_context);
        instance
    }

    fn instantiate_mut(
        &mut self,
        driver: &D,
        instantiation_context: &mut InstantiationContext<'_, D>,
    ) {
        for parameter in &mut self.parameters {
            parameter.instantiate_mut(driver, instantiation_context);
        }
    }

    #[must_use]
    fn instantiate_opaque(&self, context: &mut TypeContext<D>) -> Self {
        let mut instance = self.clone();
        instance.instantiate_opaque_mut(context);
        instance
    }

    fn instantiate_opaque_mut(&mut self, context: &mut TypeContext<D>) {
        for parameter in &mut self.parameters {
            parameter.instantiate_opaque_in_context_mut(context);
        }
    }

    fn set_source_info(&mut self, context: &mut ResolveContext<'_, D>, info: &D::Info) {
        for parameter in &mut self.parameters {
            parameter.set_source_info(context, info);
        }
    }
}

// MARK: Unification

impl<D: Driver> TypeContext<D> {
    pub fn variable(&mut self) -> TypeVariable<D> {
        self.variable_with_default(None)
    }

    pub fn variable_with_default(
        &mut self,
        default: impl Into<Option<Type<D>>>,
    ) -> TypeVariable<D> {
        let counter = self.next_variable;
        self.next_variable += 1;

        if let Some(default) = default.into() {
            self.defaults.insert(counter, default);
        }

        TypeVariable {
            _driver: std::marker::PhantomData,
            counter,
        }
    }
}

impl<D: Driver> TypeVariable<D> {
    fn with_substitution_mut<T>(
        &self,
        context: &mut TypeContext<D>,
        f: impl FnOnce(btree_map::Entry<'_, u32, Type<D>>) -> T,
    ) -> T {
        f(context.substitutions.entry(self.counter))
    }

    fn default(&self, context: &mut TypeContext<D>) -> Option<Type<D>> {
        context.defaults.get(&self.counter).cloned()
    }
}

#[derive(Clone, Copy, Default)]
#[non_exhaustive]
struct UnifyOptions {
    require_equal_type_parameters: bool,
}

#[must_use]
fn unify_with_options<D: Driver>(
    driver: &D,
    r#type: &Type<D>,
    expected_type: &Type<D>,
    context: &mut TypeContext<D>,
    options: UnifyOptions,
) -> bool {
    fn unify_variable<D: Driver>(
        variable: &TypeVariable<D>,
        r#type: &Type<D>,
        context: &mut TypeContext<D>,
    ) -> bool {
        if r#type.contains_variable(variable) {
            return false;
        }

        variable.with_substitution_mut(context, |substitution| match substitution {
            btree_map::Entry::Vacant(entry) => {
                entry.insert(r#type.clone());
            }
            btree_map::Entry::Occupied(_) => panic!("variable already has substitution"),
        });

        if let TypeKind::Variable(other) = &r#type.kind {
            if let Some(default) = variable.default(context) {
                context.defaults.insert(other.counter, default);
            }
        }

        true
    }

    fn unify_inner<D: Driver>(
        driver: &D,
        r#type: &Type<D>,
        expected_type: &Type<D>,
        context: &mut TypeContext<D>,
        options: UnifyOptions,
        ignore_type_parameters: bool,
    ) -> bool {
        let r#type = r#type.apply_in_context(context);
        let expected_type = expected_type.apply_in_context(context);

        match (&r#type.kind, &expected_type.kind) {
            // Opaque types don't unify with anything
            (TypeKind::Opaque(_), _) | (_, TypeKind::Opaque(_)) => true,

            // Type variables unify with anything and are substituted with the
            // other type in `apply_in_context`
            (TypeKind::Variable(variable), _) => {
                unify_variable(variable, &expected_type, context);
                true
            }
            (_, TypeKind::Variable(variable)) => {
                unify_variable(variable, &r#type, context);
                true
            }

            // Unify the right side of an equal type, and the left side if it's
            // a type variable (ie. an instantiated type parameter)
            (TypeKind::Equal { left, right }, _) => {
                let left_context = if let TypeKind::Variable(_) = left.kind {
                    &mut *context
                } else {
                    &mut context.clone()
                };

                unify_inner(driver, left, &expected_type, left_context, options, true)
                    & unify_inner(driver, right, &expected_type, context, options, true)
            }
            (_, TypeKind::Equal { left, right }) => {
                let left_context = if let TypeKind::Variable(_) = left.kind {
                    &mut *context
                } else {
                    &mut context.clone()
                };

                unify_inner(driver, &r#type, left, left_context, options, true)
                    & unify_inner(driver, &r#type, right, context, options, true)
            }

            // Type parameters are equal to themselves, but otherwise must be
            // instantiated
            (TypeKind::Parameter(parameter), TypeKind::Parameter(expected_parameter)) => {
                ignore_type_parameters
                    || driver.paths_are_equal(parameter, expected_parameter)
                    || !options.require_equal_type_parameters
            }
            (_, TypeKind::Parameter(_)) | (TypeKind::Parameter(_), _) => ignore_type_parameters,

            // Unify declared types, functions, tuples, and blocks structurally
            (
                TypeKind::Declared { path, parameters },
                TypeKind::Declared {
                    path: expected_path,
                    parameters: expected_parameters,
                },
            ) => {
                if !driver.paths_are_equal(path, expected_path) {
                    return false;
                }

                let mut unified = true;
                for (r#type, expected_type) in parameters.iter().zip(expected_parameters) {
                    unified &= unify_inner(
                        driver,
                        r#type,
                        expected_type,
                        context,
                        options,
                        ignore_type_parameters,
                    );
                }

                unified
            }
            (
                TypeKind::Function { inputs, output },
                TypeKind::Function {
                    inputs: expected_inputs,
                    output: expected_output,
                },
            ) => {
                if inputs.len() != expected_inputs.len() {
                    return false;
                }

                let mut unified = true;
                for (r#type, expected_type) in inputs.iter().zip(expected_inputs) {
                    unified &= unify_inner(
                        driver,
                        r#type,
                        expected_type,
                        context,
                        options,
                        ignore_type_parameters,
                    );
                }

                unified
                    & unify_inner(
                        driver,
                        output,
                        expected_output,
                        context,
                        options,
                        ignore_type_parameters,
                    )
            }
            (TypeKind::Tuple(elements), TypeKind::Tuple(expected_elements)) => {
                if elements.len() != expected_elements.len() {
                    return false;
                }

                let mut unified = true;
                for (r#type, expected_type) in elements.iter().zip(expected_elements) {
                    unified &= unify_inner(
                        driver,
                        r#type,
                        expected_type,
                        context,
                        options,
                        ignore_type_parameters,
                    );
                }

                unified
            }
            (TypeKind::Block(r#type), TypeKind::Block(expected_type)) => unify_inner(
                driver,
                r#type,
                expected_type,
                context,
                options,
                ignore_type_parameters,
            ),

            // Intrinsic types unify with other intrinsic types (they're
            // supposed to be wrapped in another type)
            (TypeKind::Intrinsic, TypeKind::Intrinsic) => true,

            // Unknown types unify with everything
            (TypeKind::Unknown, _) | (_, TypeKind::Unknown) => true,

            // Any other combination of types is an error
            _ => false,
        }
    }

    unify_inner(driver, r#type, expected_type, context, options, false)
}

#[must_use]
fn unify<D: Driver>(
    driver: &D,
    r#type: &Type<D>,
    expected_type: &Type<D>,
    context: &mut TypeContext<D>,
) -> bool {
    unify_with_options(
        driver,
        r#type,
        expected_type,
        context,
        UnifyOptions::default(),
    )
}

#[must_use]
fn unify_parameters_with_options<D: Driver>(
    driver: &D,
    parameters: &[Type<D>],
    expected_parameters: &[Type<D>],
    context: &mut TypeContext<D>,
    options: UnifyOptions,
) -> bool {
    let mut unified = true;
    for (r#type, expected_type) in parameters.iter().zip(expected_parameters) {
        unified &= unify_with_options(driver, r#type, expected_type, context, options);
    }

    unified
}

#[must_use]
fn unify_instance_with_options<D: Driver>(
    driver: &D,
    instance: WithInfo<D::Info, &Instance<D>>,
    expected_instance: WithInfo<D::Info, &Instance<D>>,
    context: &mut TypeContext<D>,
    options: UnifyOptions,
) -> bool {
    driver.paths_are_equal(&instance.item.r#trait, &expected_instance.item.r#trait)
        && unify_parameters_with_options(
            driver,
            &instance.item.parameters,
            &expected_instance.item.parameters,
            context,
            options,
        )
}

#[must_use]
fn unify_instance<D: Driver>(
    driver: &D,
    actual_instance: WithInfo<D::Info, &Instance<D>>,
    expected_instance: WithInfo<D::Info, &Instance<D>>,
    context: &mut TypeContext<D>,
) -> bool {
    unify_instance_with_options(
        driver,
        actual_instance,
        expected_instance,
        context,
        UnifyOptions::default(),
    )
}

fn try_unify_expression<D: Driver>(
    driver: &D,
    expression: WithInfo<D::Info, &mut Expression<D>>,
    expected_type: &Type<D>,
    context: &mut TypeContext<D>,
    error_queue: &mut Vec<WithInfo<D::Info, QueuedError<D>>>,
) -> bool {
    let unified = unify(driver, &expression.item.r#type, expected_type, context);
    if !unified {
        error_queue.push(WithInfo {
            info: expression.info.clone(),
            item: QueuedError::Mismatch {
                actual: expression.item.r#type.clone(),
                expected: expected_type.clone(),
                reasons: context.reasons.clone(),
            },
        });
    }

    unified
}

fn try_unify<D: Driver>(
    driver: &D,
    r#type: WithInfo<D::Info, &Type<D>>,
    expected_type: &Type<D>,
    context: &mut TypeContext<D>,
    error_queue: &mut Vec<WithInfo<D::Info, QueuedError<D>>>,
) {
    if !unify(driver, r#type.item, expected_type, context) {
        error_queue.push(WithInfo {
            info: r#type.info.clone(),
            item: QueuedError::Mismatch {
                actual: r#type.item.clone(),
                expected: expected_type.clone(),
                reasons: context.reasons.clone(),
            },
        });
    }
}

fn substitute_defaults<D: Driver>(
    driver: &D,
    r#type: &mut Type<D>,
    context: &mut TypeContext<D>,
) -> bool {
    r#type.apply_in_context_mut(context);

    match &mut r#type.kind {
        TypeKind::Variable(variable) => variable
            .default(context)
            .is_some_and(|default| unify(driver, r#type, &default, context)),
        TypeKind::Declared { parameters, .. } => {
            for r#type in parameters {
                if substitute_defaults(driver, r#type, context) {
                    return true;
                }
            }

            false
        }
        TypeKind::Function { inputs, output } => {
            for r#type in inputs {
                if substitute_defaults(driver, r#type, context) {
                    return true;
                }
            }

            substitute_defaults(driver, output, context)
        }
        TypeKind::Tuple(elements) => {
            for r#type in elements {
                if substitute_defaults(driver, r#type, context) {
                    return true;
                }
            }

            false
        }
        TypeKind::Block(r#type) => substitute_defaults(driver, r#type, context),
        TypeKind::Message { segments, .. } => {
            for segment in segments {
                if substitute_defaults(driver, &mut segment.value, context) {
                    return true;
                }
            }

            false
        }
        TypeKind::Equal { left, right } => {
            substitute_defaults(driver, left, context)
                || substitute_defaults(driver, right, context)
        }
        TypeKind::Opaque(_) | TypeKind::Parameter(_) | TypeKind::Unknown | TypeKind::Intrinsic => {
            false
        }
    }
}

// MARK: Resolution

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
struct Expression<D: Driver> {
    r#type: Type<D>,
    kind: ExpressionKind<D>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
enum ExpressionKind<D: Driver> {
    Unknown(Option<D::Path>),
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
    fn is_reference(&self) -> bool {
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
struct FormattedText<T> {
    segments: Vec<FormatSegment<T>>,
    trailing: String,
}

#[derive(Debug, Clone, PartialEq)]
struct FormatSegment<T> {
    text: String,
    value: T,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
struct StructureFieldValue<D: Driver> {
    name: String,
    value: WithInfo<D::Info, Expression<D>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
struct Arm<D: Driver> {
    pattern: WithInfo<D::Info, crate::Pattern<D>>,
    body: WithInfo<D::Info, Expression<D>>,
}

// MARK: Infer

struct InferContext<'a, D: Driver> {
    driver: &'a D,
    type_context: &'a mut TypeContext<D>,
    error_queue: &'a mut Vec<WithInfo<D::Info, QueuedError<D>>>,
    errors: &'a mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
    variables: &'a mut HashMap<D::Path, Type<D>>,
}

impl<'a, 'b: 'a, D: Driver> InferContext<'a, D> {
    fn from_resolve_context(context: &'a mut ResolveContext<'b, D>) -> Self {
        InferContext {
            driver: context.driver,
            type_context: context.type_context,
            error_queue: context.error_queue,
            errors: context.errors,
            variables: context.variables,
        }
    }
}

fn infer_type<D: Driver>(
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

fn infer_instance<D: Driver>(
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

fn infer_expression<D: Driver>(
    expression: WithInfo<D::Info, crate::UntypedExpression<D>>,
    parent_id: Option<TrackedExpressionId<D>>,
    context: &mut InferContext<'_, D>,
) -> WithInfo<<D as Driver>::Info, Expression<D>> {
    context.with_tracked_expression(parent_id, |context, expression_id| {
        let info = expression.info.clone();

        let mut expression = expression.map(|expression| match expression {
            crate::UntypedExpression::Unknown => Expression {
                r#type: Type::new(TypeKind::Unknown, info.clone()),
                kind: ExpressionKind::Unknown(None),
            },
            crate::UntypedExpression::Annotate { value, r#type } => {
                let mut value = infer_expression(value.unboxed(), Some(expression_id), context);

                let r#type = infer_type(
                    context.driver,
                    r#type.as_ref(),
                    context.type_context,
                    context.error_queue,
                    context.errors,
                );

                try_unify_expression(
                    context.driver,
                    value.as_mut(),
                    &r#type,
                    context.type_context,
                    context.error_queue,
                );

                value.item
            }
            crate::UntypedExpression::Variable(name, variable) => {
                let mut r#type = context
                    .variables
                    .get(&variable)
                    .cloned()
                    .unwrap_or_else(|| Type::new(TypeKind::Unknown, info.clone()));

                r#type.info = info.clone();

                Expression {
                    r#type,
                    kind: ExpressionKind::Variable(name, variable),
                }
            }
            crate::UntypedExpression::Constant(path) => {
                let constant_declaration = context.driver.get_constant_declaration(&path);

                let mut instantiation_context = InstantiationContext::from_parameters(
                    context.driver,
                    constant_declaration.item.parameters.clone(),
                    context.type_context,
                    info.clone(),
                    context.error_queue,
                    context.errors,
                );

                let r#type = infer_type(
                    context.driver,
                    constant_declaration.item.r#type.as_ref(),
                    instantiation_context.type_context,
                    instantiation_context.error_queue,
                    instantiation_context.errors,
                )
                .instantiate(context.driver, &mut instantiation_context);

                Expression {
                    r#type,
                    kind: ExpressionKind::UnresolvedConstant(path),
                }
            }
            crate::UntypedExpression::Trait(path) => {
                let trait_declaration = context.driver.get_trait_declaration(&path);

                let mut instantiation_context = InstantiationContext::from_parameters(
                    context.driver,
                    trait_declaration.item.parameters.clone(),
                    context.type_context,
                    info.clone(),
                    context.error_queue,
                    context.errors,
                );

                let r#type = match trait_declaration.item.r#type.as_ref() {
                    Some(r#type) => infer_type(
                        context.driver,
                        r#type.as_ref(),
                        instantiation_context.type_context,
                        instantiation_context.error_queue,
                        instantiation_context.errors,
                    )
                    .instantiate(context.driver, &mut instantiation_context),
                    None => {
                        context.errors.push(WithInfo {
                            info: info.clone(),
                            item: crate::Diagnostic::TraitHasNoValue(path.clone()),
                        });

                        Type::new(
                            TypeKind::Variable(context.type_context.variable()),
                            info.clone(),
                        )
                    }
                };

                Expression {
                    r#type,
                    kind: ExpressionKind::UnresolvedTrait(path),
                }
            }
            crate::UntypedExpression::Number(number) => {
                let r#type = instantiated_language_type(
                    "number",
                    info.clone(),
                    context.driver,
                    context.type_context,
                    context.error_queue,
                    context.errors,
                )
                .unwrap_or_else(|| {
                    Type::new(
                        TypeKind::Variable(context.type_context.variable()),
                        info.clone(),
                    )
                });

                Expression {
                    r#type,
                    kind: ExpressionKind::Number(number),
                }
            }
            crate::UntypedExpression::Text(text) => {
                let r#type = instantiated_language_type(
                    "text",
                    info.clone(),
                    context.driver,
                    context.type_context,
                    context.error_queue,
                    context.errors,
                )
                .unwrap_or_else(|| {
                    Type::new(
                        TypeKind::Variable(context.type_context.variable()),
                        info.clone(),
                    )
                });

                Expression {
                    r#type,
                    kind: ExpressionKind::Text(text),
                }
            }
            crate::UntypedExpression::Block {
                statements,
                captures,
            } => {
                let statements = statements
                    .into_iter()
                    .map(|expression| infer_expression(expression, Some(expression_id), context))
                    .collect::<Vec<_>>();

                let r#type = statements.last().map_or_else(
                    || Type::new(TypeKind::Tuple(Vec::new()), info.clone()),
                    |expression| expression.item.r#type.clone(),
                );

                Expression {
                    r#type: Type::new(TypeKind::Block(Box::new(r#type)), info.clone()),
                    kind: ExpressionKind::Block {
                        statements,
                        top_level: parent_id.is_none(),
                        captures,
                    },
                }
            }
            crate::UntypedExpression::Do(block) => {
                let output_type = Type::new(
                    TypeKind::Variable(context.type_context.variable()),
                    info.clone(),
                );

                let block_type =
                    Type::new(TypeKind::Block(Box::new(output_type.clone())), info.clone());

                let mut block = infer_expression(block.unboxed(), Some(expression_id), context);

                try_unify_expression(
                    context.driver,
                    block.as_mut(),
                    &block_type,
                    context.type_context,
                    context.error_queue,
                );

                Expression {
                    r#type: output_type,
                    kind: ExpressionKind::Do(block.boxed()),
                }
            }
            crate::UntypedExpression::Function {
                mut inputs,
                body,
                captures,
            } => {
                let input_types = inputs
                    .iter()
                    .map(|_| {
                        Type::new(
                            TypeKind::Variable(context.type_context.variable()),
                            info.clone(),
                        )
                    })
                    .collect::<Vec<_>>();

                for (pattern, input_type) in inputs.iter_mut().zip(&input_types) {
                    let input_type = pattern.replace(input_type);
                    resolve_pattern(pattern.as_mut(), input_type, context);
                }

                let body = infer_expression(body.unboxed(), Some(expression_id), context);

                Expression {
                    r#type: Type::new(
                        TypeKind::Function {
                            inputs: input_types,
                            output: Box::new(body.item.r#type.clone()),
                        },
                        info.clone(),
                    ),
                    kind: ExpressionKind::Function {
                        inputs,
                        body: body.boxed(),
                        captures,
                    },
                }
            }
            crate::UntypedExpression::Call { function, inputs } => {
                let function = infer_expression(function.unboxed(), Some(expression_id), context);

                let inputs = inputs
                    .into_iter()
                    .map(|input| infer_expression(input, Some(expression_id), context))
                    .collect::<Vec<_>>();

                let r#type = match &function.item.r#type.kind {
                    TypeKind::Function { output, .. } => output.as_ref().clone(),
                    _ => Type::new(
                        TypeKind::Variable(context.type_context.variable()),
                        info.clone(),
                    ),
                };

                Expression {
                    r#type,
                    kind: ExpressionKind::Call {
                        function: function.boxed(),
                        inputs,
                    },
                }
            }
            crate::UntypedExpression::When { input, arms } => {
                let input = infer_expression(input.unboxed(), Some(expression_id), context);

                let r#type = Type::new(
                    TypeKind::Variable(context.type_context.variable()),
                    info.clone(),
                );

                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        arm.map(|mut arm| {
                            resolve_pattern(
                                arm.pattern.as_mut(),
                                input.as_ref().map(|input| &input.r#type),
                                context,
                            );

                            let mut arm = Arm {
                                pattern: arm.pattern,
                                body: infer_expression(arm.body, Some(expression_id), context),
                            };

                            try_unify_expression(
                                context.driver,
                                arm.body.as_mut(),
                                &r#type,
                                context.type_context,
                                context.error_queue,
                            );

                            arm
                        })
                    })
                    .collect();

                Expression {
                    r#type,
                    kind: ExpressionKind::When {
                        input: input.boxed(),
                        arms,
                    },
                }
            }
            crate::UntypedExpression::Intrinsic { name, inputs } => Expression {
                r#type: Type::new(
                    TypeKind::Variable(context.type_context.variable()),
                    info.clone(),
                ),
                kind: ExpressionKind::Intrinsic {
                    name,
                    inputs: inputs
                        .into_iter()
                        .map(|expression| {
                            infer_expression(expression, Some(expression_id), context)
                        })
                        .collect(),
                },
            },
            crate::UntypedExpression::Initialize { mut pattern, value } => {
                let value = infer_expression(value.unboxed(), Some(expression_id), context);

                resolve_pattern(
                    pattern.as_mut(),
                    value.as_ref().map(|value| &value.r#type),
                    context,
                );

                Expression {
                    r#type: Type::new(TypeKind::Tuple(Vec::new()), info.clone()),
                    kind: ExpressionKind::Initialize {
                        pattern,
                        value: value.boxed(),
                    },
                }
            }
            crate::UntypedExpression::Mutate { name, path, value } => {
                let mut value = infer_expression(value.unboxed(), Some(expression_id), context);

                let r#type = context
                    .variables
                    .get(&path.item)
                    .cloned()
                    .unwrap_or_else(|| Type::new(TypeKind::Unknown, info.clone()));

                try_unify_expression(
                    context.driver,
                    value.as_mut(),
                    &r#type,
                    context.type_context,
                    context.error_queue,
                );

                Expression {
                    r#type: Type::new(TypeKind::Tuple(Vec::new()), info.clone()),
                    kind: ExpressionKind::Mutate {
                        name,
                        path,
                        value: value.boxed(),
                    },
                }
            }
            crate::UntypedExpression::Marker(path) => {
                let type_declaration = context.driver.get_type_declaration(&path);

                let mut instantiation_context = InstantiationContext::from_parameters(
                    context.driver,
                    type_declaration.item.parameters.clone(),
                    context.type_context,
                    info.clone(),
                    context.error_queue,
                    context.errors,
                );

                let r#type = Type::new(
                    TypeKind::Declared {
                        path: path.clone(),
                        parameters: type_declaration
                            .item
                            .parameters
                            .into_iter()
                            .map(|path| {
                                instantiation_context.type_for_parameter(context.driver, &path)
                            })
                            .collect(),
                    },
                    info.clone(),
                );

                Expression {
                    r#type,
                    kind: ExpressionKind::Marker(path),
                }
            }
            crate::UntypedExpression::Structure(fields) => Expression {
                r#type: Type::new(
                    TypeKind::Variable(context.type_context.variable()),
                    info.clone(),
                ),
                kind: ExpressionKind::UnresolvedStructure(
                    fields
                        .into_iter()
                        .map(|field| {
                            field.map(|field| StructureFieldValue {
                                name: field.name,
                                value: infer_expression(field.value, Some(expression_id), context),
                            })
                        })
                        .collect(),
                ),
            },
            crate::UntypedExpression::Variant { variant, values } => {
                let enumeration = context.driver.get_enumeration_for_variant(&variant.item);
                let type_declaration = context.driver.get_type_declaration(&enumeration);

                let mut instantiation_context = InstantiationContext::from_parameters(
                    context.driver,
                    type_declaration.item.parameters.clone(),
                    context.type_context,
                    info.clone(),
                    context.error_queue,
                    context.errors,
                );

                let values = match type_declaration.item.representation.item {
                    crate::TypeRepresentation::Enumeration(declared_variants) => {
                        if let Some(declared_variant) = declared_variants.get(&variant.item) {
                            values
                                .into_iter()
                                .zip(&declared_variant.item.value_types)
                                .map(|(value, declared_type)| {
                                    let declared_type = infer_type(
                                        context.driver,
                                        declared_type.as_ref(),
                                        instantiation_context.type_context,
                                        instantiation_context.error_queue,
                                        instantiation_context.errors,
                                    )
                                    .instantiate(context.driver, &mut instantiation_context);

                                    (value, declared_type)
                                })
                                .collect()
                        } else {
                            Vec::new()
                        }
                    }
                    _ => Vec::new(),
                };

                let parameters = type_declaration
                    .item
                    .parameters
                    .into_iter()
                    .map(|path| instantiation_context.type_for_parameter(context.driver, &path))
                    .collect::<Vec<_>>();

                let r#type = Type::new(
                    TypeKind::Declared {
                        path: enumeration,
                        parameters,
                    },
                    info.clone(),
                );

                let values = values
                    .into_iter()
                    .map(|(value, declared_type)| {
                        let mut value = infer_expression(value, Some(expression_id), context);

                        try_unify_expression(
                            context.driver,
                            value.as_mut(),
                            &declared_type,
                            context.type_context,
                            context.error_queue,
                        );

                        value
                    })
                    .collect::<Vec<_>>();

                Expression {
                    r#type,
                    kind: ExpressionKind::Variant { variant, values },
                }
            }
            crate::UntypedExpression::Wrapper {
                r#type: path,
                value,
            } => {
                let type_declaration = context.driver.get_type_declaration(&path);

                let mut instantiation_context = InstantiationContext::from_parameters(
                    context.driver,
                    type_declaration.item.parameters.clone(),
                    context.type_context,
                    info.clone(),
                    context.error_queue,
                    context.errors,
                );

                let r#type = Type::new(
                    TypeKind::Declared {
                        path,
                        parameters: type_declaration
                            .item
                            .parameters
                            .into_iter()
                            .map(|path| {
                                instantiation_context.type_for_parameter(context.driver, &path)
                            })
                            .collect(),
                    },
                    info.clone(),
                );

                let value = match type_declaration.item.representation.item {
                    crate::TypeRepresentation::Wrapper(declared_type) => {
                        let declared_type = infer_type(
                            context.driver,
                            declared_type.as_ref(),
                            instantiation_context.type_context,
                            instantiation_context.error_queue,
                            instantiation_context.errors,
                        )
                        .instantiate(context.driver, &mut instantiation_context);

                        let mut value =
                            infer_expression(value.unboxed(), Some(expression_id), context);

                        try_unify_expression(
                            context.driver,
                            value.as_mut(),
                            &declared_type,
                            context.type_context,
                            context.error_queue,
                        );

                        value
                    }
                    _ => WithInfo {
                        info: info.clone(),
                        item: Expression {
                            r#type: Type::new(
                                TypeKind::Variable(context.type_context.variable()),
                                info.clone(),
                            ),
                            kind: ExpressionKind::Unknown(None),
                        },
                    },
                };

                Expression {
                    r#type,
                    kind: ExpressionKind::Wrapper(value.boxed()),
                }
            }
            crate::UntypedExpression::Tuple(elements) => {
                let elements = elements
                    .into_iter()
                    .map(|expression| infer_expression(expression, Some(expression_id), context))
                    .collect::<Vec<_>>();

                Expression {
                    r#type: Type::new(
                        TypeKind::Tuple(
                            elements
                                .iter()
                                .map(|element| element.item.r#type.clone())
                                .collect(),
                        ),
                        info.clone(),
                    ),
                    kind: ExpressionKind::Tuple(elements),
                }
            }
            crate::UntypedExpression::Collection(elements) => {
                let element_type = Type::new(
                    TypeKind::Variable(context.type_context.variable()),
                    info.clone(),
                );

                let initial_collection = instantiated_language_constant(
                    "initial-collection",
                    &info,
                    Some(expression_id),
                    context,
                );

                let collection_type = initial_collection.item.r#type.clone();

                elements
                    .into_iter()
                    .fold(initial_collection, |current, expression| {
                        let mut expression =
                            infer_expression(expression, Some(expression_id), context);

                        try_unify_expression(
                            context.driver,
                            expression.as_mut(),
                            &element_type,
                            context.type_context,
                            context.error_queue,
                        );

                        expression.item.r#type = element_type.clone();

                        let build_collection_trait = instantiated_language_trait(
                            "build-collection",
                            &info,
                            Some(expression_id),
                            context,
                        );

                        WithInfo {
                            info: expression.info.clone(),
                            item: Expression {
                                r#type: collection_type.clone(),
                                kind: ExpressionKind::Call {
                                    function: WithInfo {
                                        info: current.info.clone(),
                                        item: Box::new(Expression {
                                            r#type: Type::new(
                                                TypeKind::Function {
                                                    inputs: vec![collection_type.clone()],
                                                    output: Box::new(collection_type.clone()),
                                                },
                                                current.info.clone(),
                                            ),
                                            kind: ExpressionKind::Call {
                                                function: build_collection_trait.boxed(),
                                                inputs: vec![expression],
                                            },
                                        }),
                                    },
                                    inputs: vec![current],
                                },
                            },
                        }
                    })
                    .item
            }
            crate::UntypedExpression::Format { segments, trailing } => {
                let text_type = instantiated_language_type(
                    "text",
                    info.clone(),
                    context.driver,
                    context.type_context,
                    context.error_queue,
                    context.errors,
                )
                .unwrap_or_else(|| {
                    Type::new(
                        TypeKind::Variable(context.type_context.variable()),
                        info.clone(),
                    )
                });

                let segments = segments
                    .into_iter()
                    .map(|segment| {
                        let value = infer_expression(segment.value, Some(expression_id), context);

                        let describe_trait = instantiated_language_trait(
                            "describe",
                            &info,
                            Some(expression_id),
                            context,
                        );

                        FormatSegment {
                            text: segment.text,
                            value: WithInfo {
                                info: value.info.clone(),
                                item: Expression {
                                    r#type: Type::new(
                                        TypeKind::Variable(context.type_context.variable()),
                                        value.info.clone(),
                                    ),
                                    kind: ExpressionKind::Call {
                                        function: describe_trait.boxed(),
                                        inputs: vec![value],
                                    },
                                },
                            },
                        }
                    })
                    .collect();

                Expression {
                    r#type: text_type,
                    kind: ExpressionKind::Format { segments, trailing },
                }
            }
        });

        expression.item.r#type.info = info;

        expression
    })
}

fn resolve_pattern<D: Driver>(
    pattern: WithInfo<D::Info, &mut crate::Pattern<D>>,
    r#type: WithInfo<D::Info, &Type<D>>,
    context: &mut InferContext<'_, D>,
) {
    let mut r#type = r#type.map(|r#type| r#type.clone());
    r#type.item.apply_in_context_mut(context.type_context);

    match pattern.item {
        crate::Pattern::Unknown => {}
        crate::Pattern::Wildcard => {}
        crate::Pattern::Number(_) => {
            let number_type = instantiated_language_type(
                "number",
                pattern.info.clone(),
                context.driver,
                context.type_context,
                context.error_queue,
                context.errors,
            )
            .map_or_else(
                || {
                    Type::new(
                        TypeKind::Variable(context.type_context.variable()),
                        pattern.info.clone(),
                    )
                },
                |r#type| r#type,
            );

            try_unify(
                context.driver,
                r#type.replace(&number_type),
                &r#type.item,
                context.type_context,
                context.error_queue,
            );
        }
        crate::Pattern::Text(_) => {
            let text_type = instantiated_language_type(
                "text",
                pattern.info.clone(),
                context.driver,
                context.type_context,
                context.error_queue,
                context.errors,
            )
            .map_or_else(
                || {
                    Type::new(
                        TypeKind::Variable(context.type_context.variable()),
                        pattern.info.clone(),
                    )
                },
                |r#type| r#type,
            );

            try_unify(
                context.driver,
                r#type.replace(&text_type),
                &r#type.item,
                context.type_context,
                context.error_queue,
            );
        }
        crate::Pattern::Variable(_, variable) => {
            use std::collections::hash_map::Entry;

            match context.variables.entry(variable.clone()) {
                Entry::Occupied(entry) => {
                    try_unify(
                        context.driver,
                        r#type.as_ref(),
                        entry.get(),
                        context.type_context,
                        context.error_queue,
                    );
                }
                Entry::Vacant(entry) => {
                    entry.insert(r#type.item);
                }
            }
        }
        crate::Pattern::Destructure {
            field_patterns,
            structure,
        } => {
            let field_types = match &r#type.item.kind {
                TypeKind::Declared { path, parameters } => {
                    let type_declaration = context.driver.get_type_declaration(path);

                    let mut instantiation_context = InstantiationContext::from_parameters(
                        context.driver,
                        type_declaration.item.parameters.clone(),
                        context.type_context,
                        pattern.info,
                        context.error_queue,
                        context.errors,
                    );

                    for (path, r#type) in type_declaration.item.parameters.iter().zip(parameters) {
                        assert!(unify(
                            context.driver,
                            &instantiation_context.type_for_parameter(context.driver, path),
                            r#type,
                            instantiation_context.type_context,
                        ));
                    }

                    match type_declaration.item.representation.item {
                        crate::TypeRepresentation::Structure(field_types) => {
                            *structure = Some(r#type.replace(path.clone()));

                            field_patterns
                                .iter()
                                .map(|field| {
                                    infer_type(
                                        context.driver,
                                        field_types
                                            .get(&field.item.name)
                                            .unwrap()
                                            .item
                                            .r#type
                                            .as_ref(),
                                        instantiation_context.type_context,
                                        instantiation_context.error_queue,
                                        instantiation_context.errors,
                                    )
                                    .instantiate(context.driver, &mut instantiation_context)
                                })
                                .collect::<Vec<_>>()
                        }
                        _ => return,
                    }
                }
                _ => field_patterns
                    .iter()
                    .map(|field| {
                        Type::new(
                            TypeKind::Variable(context.type_context.variable()),
                            field.info.clone(),
                        )
                    })
                    .collect::<Vec<_>>(),
            };

            for (field, r#type) in field_patterns.iter_mut().zip(field_types) {
                let r#type = field.replace(&r#type);
                resolve_pattern(field.item.pattern.as_mut(), r#type, context);
            }
        }
        crate::Pattern::Variant {
            variant,
            value_patterns,
        } => {
            let enumeration = context.driver.get_enumeration_for_variant(&variant.item);
            let type_declaration = context.driver.get_type_declaration(&enumeration);

            let mut instantiation_context = InstantiationContext::from_parameters(
                context.driver,
                type_declaration.item.parameters.clone(),
                context.type_context,
                pattern.info.clone(),
                context.error_queue,
                context.errors,
            );

            let value_types = match type_declaration.item.representation.item {
                crate::TypeRepresentation::Enumeration(variants) => {
                    let variant = variants.get(&variant.item).unwrap();

                    variant
                        .item
                        .value_types
                        .iter()
                        .map(|r#type| {
                            infer_type(
                                context.driver,
                                r#type.as_ref(),
                                instantiation_context.type_context,
                                instantiation_context.error_queue,
                                instantiation_context.errors,
                            )
                            .instantiate(context.driver, &mut instantiation_context)
                        })
                        .collect::<Vec<_>>()
                }
                _ => return,
            };

            let enumeration_type = Type::new(
                TypeKind::Declared {
                    path: enumeration,
                    parameters: instantiation_context.into_types_for_parameters(),
                },
                pattern.info,
            );

            try_unify(
                context.driver,
                r#type.as_ref(),
                &enumeration_type,
                context.type_context,
                context.error_queue,
            );

            for (pattern, r#type) in value_patterns.iter_mut().zip(value_types) {
                let r#type = pattern.replace(&r#type);
                resolve_pattern(pattern.as_mut(), r#type, context);
            }
        }
        crate::Pattern::Wrapper {
            path,
            value_pattern,
        } => {
            let type_declaration = context.driver.get_type_declaration(&path.item);

            let mut instantiation_context = InstantiationContext::from_parameters(
                context.driver,
                type_declaration.item.parameters.clone(),
                context.type_context,
                pattern.info.clone(),
                context.error_queue,
                context.errors,
            );

            let value_type = match type_declaration.item.representation.item {
                crate::TypeRepresentation::Wrapper(wrapped) => infer_type(
                    context.driver,
                    wrapped.as_ref(),
                    instantiation_context.type_context,
                    instantiation_context.error_queue,
                    instantiation_context.errors,
                )
                .instantiate(context.driver, &mut instantiation_context),
                _ => return,
            };

            let wrapper_type = Type::new(
                TypeKind::Declared {
                    path: path.item.clone(),
                    parameters: instantiation_context.into_types_for_parameters(),
                },
                pattern.info,
            );

            try_unify(
                context.driver,
                r#type.as_ref(),
                &wrapper_type,
                context.type_context,
                context.error_queue,
            );

            let value_type = value_pattern.replace(&value_type);
            resolve_pattern(value_pattern.as_deref_mut(), value_type, context);
        }
        crate::Pattern::Tuple(elements) => {
            let element_types = match &r#type.item.kind {
                TypeKind::Tuple(element_types) => {
                    element_types.iter().map(Type::clone).collect::<Vec<_>>()
                }
                _ => elements
                    .iter()
                    .map(|element| {
                        Type::new(
                            TypeKind::Variable(context.type_context.variable()),
                            element.info.clone(),
                        )
                    })
                    .collect::<Vec<_>>(),
            };

            let tuple_type = Type::new(
                TypeKind::Tuple(element_types.iter().map(Type::clone).collect()),
                pattern.info,
            );

            try_unify(
                context.driver,
                r#type.replace(&tuple_type),
                &r#type.item,
                context.type_context,
                context.error_queue,
            );

            for (element, r#type) in elements.iter_mut().zip(element_types) {
                let r#type = element.replace(&r#type);
                resolve_pattern(element.as_mut(), r#type, context);
            }
        }
        crate::Pattern::Or { left, right } => {
            resolve_pattern(left.as_deref_mut(), r#type.as_ref(), context);
            resolve_pattern(right.as_deref_mut(), r#type.as_ref(), context);
        }
        crate::Pattern::Annotate {
            pattern,
            r#type: annotated_type,
        } => {
            let annotated_type = infer_type(
                context.driver,
                annotated_type.as_ref(),
                context.type_context,
                context.error_queue,
                context.errors,
            );

            try_unify(
                context.driver,
                r#type.as_ref(),
                &annotated_type,
                context.type_context,
                context.error_queue,
            );

            resolve_pattern(pattern.as_deref_mut(), r#type.as_ref(), context)
        }
    }
}

fn instantiated_language_type<D: Driver>(
    language_item: &str,
    info: D::Info,
    driver: &D,
    type_context: &mut TypeContext<D>,
    error_queue: &mut Vec<WithInfo<D::Info, QueuedError<D>>>,
    errors: &mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
) -> Option<Type<D>> {
    match try_instantiated_language_type(
        language_item,
        info.clone(),
        driver,
        type_context,
        error_queue,
        errors,
    ) {
        Some(path) => Some(path),
        None => {
            errors.push(WithInfo {
                info,
                item: crate::Diagnostic::MissingLanguageItem(language_item.to_string()),
            });

            None
        }
    }
}

fn try_instantiated_language_type<D: Driver>(
    language_item: &str,
    info: D::Info,
    driver: &D,
    type_context: &mut TypeContext<D>,
    error_queue: &mut Vec<WithInfo<D::Info, QueuedError<D>>>,
    errors: &mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
) -> Option<Type<D>> {
    let path = driver.path_for_language_type(language_item)?;
    let type_declaration = driver.get_type_declaration(&path);

    let instantiated_parameters = InstantiationContext::from_parameters(
        driver,
        type_declaration.item.parameters,
        type_context,
        info.clone(),
        error_queue,
        errors,
    )
    .into_types_for_parameters();

    Some(Type::new(
        TypeKind::Declared {
            path,
            parameters: instantiated_parameters,
        },
        info,
    ))
}

fn instantiated_language_trait<D: Driver>(
    language_item: &str,
    info: &D::Info,
    parent_id: Option<TrackedExpressionId<D>>,
    context: &mut InferContext<'_, D>,
) -> WithInfo<D::Info, Expression<D>> {
    match context.driver.path_for_language_constructor(language_item) {
        Some(path) => context.with_tracked_expression(parent_id, |context, expression_id| {
            infer_expression(
                WithInfo {
                    info: info.clone(),
                    item: crate::UntypedExpression::Constant(path),
                },
                Some(expression_id),
                context,
            )
        }),
        None => {
            context.errors.push(WithInfo {
                info: info.clone(),
                item: crate::Diagnostic::MissingLanguageItem(language_item.to_string()),
            });

            WithInfo {
                info: info.clone(),
                item: Expression {
                    r#type: Type::new(TypeKind::Unknown, info.clone()),
                    kind: ExpressionKind::Unknown(None),
                },
            }
        }
    }
}

fn instantiated_language_constant<D: Driver>(
    language_item: &str,
    info: &D::Info,
    parent_id: Option<TrackedExpressionId<D>>,
    context: &mut InferContext<'_, D>,
) -> WithInfo<D::Info, Expression<D>> {
    match context.driver.path_for_language_constant(language_item) {
        Some(path) => context.with_tracked_expression(parent_id, |context, expression_id| {
            infer_expression(
                WithInfo {
                    info: info.clone(),
                    item: crate::UntypedExpression::Constant(path),
                },
                Some(expression_id),
                context,
            )
        }),
        None => {
            context.errors.push(WithInfo {
                info: info.clone(),
                item: crate::Diagnostic::MissingLanguageItem(language_item.to_string()),
            });

            WithInfo {
                info: info.clone(),
                item: Expression {
                    r#type: Type::new(TypeKind::Unknown, info.clone()),
                    kind: ExpressionKind::Unknown(None),
                },
            }
        }
    }
}

// MARK: Resolve

struct ResolveContext<'a, D: Driver> {
    driver: &'a D,
    type_context: &'a mut TypeContext<D>,
    error_queue: &'a mut Vec<WithInfo<D::Info, QueuedError<D>>>,
    errors: &'a mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
    variables: &'a mut HashMap<D::Path, Type<D>>,
    recursion_stack: &'a mut Vec<D::Info>,
    bound_instances: Vec<Vec<WithInfo<D::Info, Instance<D>>>>,
}

fn resolve_expression<D: Driver>(
    mut expression: WithInfo<D::Info, Expression<D>>,
    context: &mut ResolveContext<'_, D>,
) -> WithInfo<D::Info, Expression<D>> {
    let kind = match expression.item.kind {
        ExpressionKind::Unknown(path) => ExpressionKind::Unknown(path),
        ExpressionKind::Variable(ref name, ref variable) => {
            let mut variable_type = context
                .variables
                .get(variable)
                .cloned()
                .unwrap_or_else(|| Type::new(TypeKind::Unknown, expression.info.clone()));

            variable_type.apply_in_context_mut(context.type_context);
            variable_type.info = expression.info.clone();

            let name = name.clone();
            let variable = variable.clone();

            try_unify_expression(
                context.driver,
                expression.as_mut(),
                &variable_type,
                context.type_context,
                context.error_queue,
            );

            ExpressionKind::Variable(name, variable)
        }
        ExpressionKind::UnresolvedConstant(ref path) => {
            let path = path.clone();

            match resolve_item(&path, expression.as_mut(), true, context) {
                Ok(Some((parameters, bounds))) => ExpressionKind::ResolvedConstant {
                    path,
                    parameters,
                    bounds,
                },
                Ok(None) => ExpressionKind::UnresolvedConstant(path), // try again with more type information
                Err(error) => {
                    context.error_queue.push(error);
                    ExpressionKind::Unknown(Some(path))
                }
            }
        }
        ExpressionKind::UnresolvedTrait(ref path) => {
            let path = path.clone();

            match resolve_trait_parameters_from_type(&path, expression.as_mut(), context) {
                Some(parameters) => {
                    let query = expression.replace(Instance {
                        r#trait: path,
                        parameters,
                    });

                    match resolve_trait(query.as_ref(), context) {
                        Ok(instance) => ExpressionKind::ResolvedTrait {
                            trait_path: query.item.r#trait,
                            parameters: query.item.parameters,
                            instance,
                        },
                        Err(error) => {
                            context.error_queue.push(error);
                            ExpressionKind::Unknown(Some(query.item.r#trait))
                        }
                    }
                }
                None => {
                    context.errors.push(WithInfo {
                        info: expression.info.clone(),
                        item: crate::Diagnostic::TraitHasNoValue(path.clone()),
                    });

                    ExpressionKind::Unknown(Some(path))
                }
            }
        }
        ExpressionKind::ResolvedConstant {
            path,
            parameters,
            bounds,
        } => ExpressionKind::ResolvedConstant {
            path,
            parameters,
            bounds,
        },
        ExpressionKind::ResolvedTrait {
            trait_path,
            parameters,
            instance,
        } => ExpressionKind::ResolvedTrait {
            trait_path,
            parameters,
            instance,
        },
        ExpressionKind::Number(number) => ExpressionKind::Number(number),
        ExpressionKind::Text(text) => ExpressionKind::Text(text),
        ExpressionKind::Block {
            statements,
            top_level,
            captures,
        } => ExpressionKind::Block {
            statements: statements
                .into_iter()
                .map(|expression| {
                    // Don't persist error reasons across statements; statements
                    // can't directly influence each other
                    let prev_reasons = context.type_context.reasons.clone();

                    let expression = resolve_expression(expression, context);
                    context.type_context.reasons = prev_reasons;

                    expression
                })
                .collect(),
            top_level,
            captures,
        },
        ExpressionKind::Do(block) => {
            ExpressionKind::Do(resolve_expression(block.unboxed(), context).boxed())
        }
        ExpressionKind::Function {
            mut inputs,
            body,
            captures,
        } => {
            if let TypeKind::Function {
                inputs: input_types,
                ..
            } = &expression.item.r#type.kind
            {
                for (pattern, input_type) in inputs.iter_mut().zip(input_types) {
                    let mut infer_context = InferContext::from_resolve_context(context);

                    let input_type = pattern.replace(input_type);

                    resolve_pattern(pattern.as_mut(), input_type, &mut infer_context);
                }
            }

            ExpressionKind::Function {
                inputs,
                body: resolve_expression(body.unboxed(), context).boxed(),
                captures,
            }
        }
        ExpressionKind::Call {
            mut function,
            mut inputs,
        } => {
            function
                .item
                .r#type
                .apply_in_context_mut(context.type_context);

            for input in &mut inputs {
                input.item.r#type.apply_in_context_mut(context.type_context);
            }

            // If we encounter a unit after a number, treat the unit as a
            // function
            let is_number_with_unit = (|| {
                if inputs.len() != 1 {
                    return false;
                }

                let input = inputs.first().unwrap();

                let number_type = match try_instantiated_language_type(
                    "number",
                    expression.info.clone(),
                    context.driver,
                    context.type_context,
                    context.error_queue,
                    context.errors,
                ) {
                    Some(number_type) => number_type,
                    None => return false,
                };

                let number_type_path = match number_type.kind {
                    TypeKind::Declared { path, .. } => path,
                    _ => return false,
                };

                // Return true if the expression in function position is a
                // number...
                if matches!(
                    &function.item.r#type.kind,
                    TypeKind::Declared { path, .. }
                        if context.driver.paths_are_equal(path, &number_type_path),
                ) {
                    return true;
                }

                // ...or if the expression in input position is a function that
                // accepts a number
                if let TypeKind::Function { inputs, .. } = &input.item.r#type.kind {
                    if inputs.len() != 1 {
                        return false;
                    }

                    return matches!(
                        &inputs.first().unwrap().kind,
                        TypeKind::Declared { path, .. }
                            if context.driver.paths_are_equal(path, &number_type_path),
                    ) && !matches!(&function.item.r#type.kind, TypeKind::Function { .. });
                }

                false
            })();

            if is_number_with_unit {
                let input = inputs.pop().unwrap();

                let mut unit = resolve_expression(input, context);
                let number = resolve_expression(function.unboxed(), context);

                let unit_type = Type::new(
                    TypeKind::Function {
                        inputs: vec![number.item.r#type.clone()],
                        output: Box::new(expression.item.r#type.clone()),
                    },
                    expression.info.clone(),
                );

                if try_unify_expression(
                    context.driver,
                    unit.as_mut(),
                    &unit_type,
                    context.type_context,
                    context.error_queue,
                ) {
                    ExpressionKind::Call {
                        function: unit.boxed(),
                        inputs: vec![number],
                    }
                } else {
                    // Prevent duplicate errors caused by recreating the call
                    // expression
                    ExpressionKind::Unknown(None)
                }
            } else if let TypeKind::Function {
                inputs: input_types,
                output: output_type,
            } = &function.item.r#type.kind
            {
                match inputs.len().cmp(&input_types.len()) {
                    std::cmp::Ordering::Equal => {
                        for (input, input_type) in inputs.iter_mut().zip(input_types) {
                            try_unify_expression(
                                context.driver,
                                input.as_mut(),
                                input_type,
                                context.type_context,
                                context.error_queue,
                            );
                        }

                        try_unify(
                            context.driver,
                            WithInfo {
                                info: expression.info.clone(),
                                item: &expression.item.r#type,
                            },
                            output_type,
                            context.type_context,
                            context.error_queue,
                        );

                        let inputs = inputs
                            .into_iter()
                            .map(|input| resolve_expression(input, context))
                            .collect::<Vec<_>>();

                        let function = resolve_expression(function.unboxed(), context);

                        ExpressionKind::Call {
                            function: function.boxed(),
                            inputs,
                        }
                    }
                    std::cmp::Ordering::Less => {
                        context.error_queue.push(WithInfo {
                            info: expression.info.clone(),
                            item: QueuedError::MissingInputs(input_types[inputs.len()..].to_vec()),
                        });

                        ExpressionKind::Unknown(None)
                    }
                    std::cmp::Ordering::Greater => {
                        for _ in &inputs[input_types.len()..] {
                            context.error_queue.push(WithInfo {
                                info: expression.info.clone(),
                                item: QueuedError::ExtraInput,
                            });
                        }

                        ExpressionKind::Unknown(None)
                    }
                }
            } else {
                let inputs = inputs
                    .into_iter()
                    .map(|input| resolve_expression(input, context))
                    .collect::<Vec<_>>();

                let mut function = resolve_expression(function.unboxed(), context);

                let function_type = Type::new(
                    TypeKind::Function {
                        inputs: inputs
                            .iter()
                            .map(|input| input.item.r#type.clone())
                            .collect(),
                        output: Box::new(expression.item.r#type.clone()),
                    },
                    function.info.clone(),
                );

                try_unify_expression(
                    context.driver,
                    function.as_mut(),
                    &function_type,
                    context.type_context,
                    context.error_queue,
                );

                ExpressionKind::Call {
                    function: function.boxed(),
                    inputs,
                }
            }
        }
        ExpressionKind::When { input, arms } => {
            let input = resolve_expression(input.unboxed(), context).boxed();

            let arms = arms
                .into_iter()
                .map(|arm| {
                    arm.map(|mut arm| {
                        resolve_pattern(
                            arm.pattern.as_mut(),
                            input.as_ref().map(|input| &input.r#type),
                            &mut InferContext::from_resolve_context(context),
                        );

                        Arm {
                            pattern: arm.pattern,
                            body: resolve_expression(arm.body, context),
                        }
                    })
                })
                .collect::<Vec<_>>();

            ExpressionKind::When { input, arms }
        }
        ExpressionKind::Intrinsic { name, inputs } => ExpressionKind::Intrinsic {
            name,
            inputs: inputs
                .into_iter()
                .map(|expression| resolve_expression(expression, context))
                .collect(),
        },
        ExpressionKind::Initialize { mut pattern, value } => {
            let value = resolve_expression(value.unboxed(), context).boxed();

            resolve_pattern(
                pattern.as_mut(),
                value.as_ref().map(|value| &value.r#type),
                &mut InferContext::from_resolve_context(context),
            );

            ExpressionKind::Initialize { pattern, value }
        }
        ExpressionKind::Mutate { name, path, value } => {
            let value = resolve_expression(value.unboxed(), context).boxed();

            resolve_pattern(
                path.as_ref()
                    .map(|path| crate::Pattern::Variable(name.clone(), path.clone()))
                    .as_mut(),
                value.as_ref().map(|value| &value.r#type),
                &mut InferContext::from_resolve_context(context),
            );

            ExpressionKind::Mutate { name, path, value }
        }
        ExpressionKind::Marker(r#type) => ExpressionKind::Marker(r#type),
        ExpressionKind::UnresolvedStructure(fields) => {
            expression
                .item
                .r#type
                .apply_in_context_mut(context.type_context);

            match &expression.item.r#type.kind {
                TypeKind::Variable(_) | TypeKind::Opaque(_) => ExpressionKind::UnresolvedStructure(
                    fields
                        .into_iter()
                        .map(|field_value| {
                            field_value.map(|field_value| StructureFieldValue {
                                name: field_value.name,
                                value: resolve_expression(field_value.value, context),
                            })
                        })
                        .collect(),
                ),
                TypeKind::Declared { path, .. } => {
                    let path = path.clone();

                    let type_declaration = context.driver.get_type_declaration(&path);

                    if let crate::TypeRepresentation::Structure(mut field_types) =
                        type_declaration.item.representation.item
                    {
                        let mut instantiation_context = InstantiationContext::from_parameters(
                            context.driver,
                            type_declaration.item.parameters.clone(),
                            context.type_context,
                            expression.info.clone(),
                            context.error_queue,
                            context.errors,
                        );

                        let fields = fields
                            .into_iter()
                            .filter_map(|field_value| {
                                let declared_type = match field_types.remove(&field_value.item.name)
                                {
                                    Some(field) => infer_type(
                                        context.driver,
                                        field.item.r#type.as_ref(),
                                        instantiation_context.type_context,
                                        instantiation_context.error_queue,
                                        instantiation_context.errors,
                                    )
                                    .instantiate(context.driver, &mut instantiation_context),
                                    None => {
                                        instantiation_context.error_queue.push(WithInfo {
                                            info: field_value.info.clone(),
                                            item: QueuedError::ExtraField,
                                        });

                                        return None;
                                    }
                                };

                                Some((field_value, declared_type))
                            })
                            .collect::<Vec<_>>();

                        let missing_fields = field_types.into_keys().collect::<Vec<_>>();
                        if !missing_fields.is_empty() {
                            instantiation_context.error_queue.push(WithInfo {
                                info: expression.info.clone(),
                                item: QueuedError::MissingFields(missing_fields),
                            });
                        }

                        let instantiated_declared_type = Type::new(
                            TypeKind::Declared {
                                path: path.clone(),
                                parameters: instantiation_context.into_types_for_parameters(),
                            },
                            expression.info.clone(),
                        );

                        try_unify(
                            context.driver,
                            WithInfo {
                                info: expression.info.clone(),
                                item: &instantiated_declared_type,
                            },
                            &expression.item.r#type,
                            context.type_context,
                            context.error_queue,
                        );

                        let fields = fields
                            .into_iter()
                            .map(|(field_value, declared_type)| {
                                field_value.map(|field_value| {
                                    let mut value = resolve_expression(field_value.value, context);

                                    try_unify_expression(
                                        context.driver,
                                        value.as_mut(),
                                        &declared_type,
                                        context.type_context,
                                        context.error_queue,
                                    );

                                    StructureFieldValue {
                                        name: field_value.name,
                                        value,
                                    }
                                })
                            })
                            .collect();

                        ExpressionKind::ResolvedStructure {
                            structure: path,
                            fields,
                        }
                    } else {
                        context.error_queue.push(WithInfo {
                            info: expression.info.clone(),
                            item: QueuedError::NotAStructure(expression.item.r#type.clone()),
                        });

                        ExpressionKind::Unknown(None)
                    }
                }
                _ => {
                    context.error_queue.push(WithInfo {
                        info: expression.info.clone(),
                        item: QueuedError::NotAStructure(expression.item.r#type.clone()),
                    });

                    ExpressionKind::Unknown(None)
                }
            }
        }
        ExpressionKind::ResolvedStructure { structure, fields } => {
            ExpressionKind::ResolvedStructure {
                structure,
                fields: fields
                    .into_iter()
                    .map(|field| {
                        field.map(|field| StructureFieldValue {
                            name: field.name,
                            value: resolve_expression(field.value, context),
                        })
                    })
                    .collect(),
            }
        }
        ExpressionKind::Variant { variant, values } => ExpressionKind::Variant {
            variant,
            values: values
                .into_iter()
                .map(|expression| resolve_expression(expression, context))
                .collect(),
        },
        ExpressionKind::Wrapper(value) => {
            ExpressionKind::Wrapper(resolve_expression(value.unboxed(), context).boxed())
        }
        ExpressionKind::Tuple(elements) => ExpressionKind::Tuple(
            elements
                .into_iter()
                .map(|expression| resolve_expression(expression, context))
                .collect(),
        ),
        ExpressionKind::Format { segments, trailing } => ExpressionKind::Format {
            segments: segments
                .into_iter()
                .map(|segment| FormatSegment {
                    text: segment.text,
                    value: resolve_expression(segment.value, context),
                })
                .collect(),
            trailing,
        },
    };

    WithInfo {
        info: expression.info,
        item: Expression {
            r#type: expression.item.r#type,
            kind,
        },
    }
}

fn resolve_item<D: Driver>(
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
                                        reasons: context.type_context.reasons.clone(),
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

fn resolve_trait_parameters_from_type<D: Driver>(
    path: &D::Path,
    use_expression: WithInfo<D::Info, &mut Expression<D>>,
    context: &mut ResolveContext<'_, D>,
) -> Option<Vec<Type<D>>> {
    let trait_declaration = context.driver.get_trait_declaration(path);

    let use_info = use_expression.info.clone();

    let mut instantiation_context = InstantiationContext::from_parameters(
        context.driver,
        trait_declaration.item.parameters,
        context.type_context,
        use_info,
        context.error_queue,
        context.errors,
    );

    let trait_type = infer_type(
        context.driver,
        trait_declaration.item.r#type.as_ref()?.as_ref(),
        instantiation_context.type_context,
        instantiation_context.error_queue,
        instantiation_context.errors,
    )
    .instantiate(context.driver, &mut instantiation_context);

    try_unify_expression(
        context.driver,
        use_expression,
        &trait_type,
        instantiation_context.type_context,
        instantiation_context.error_queue,
    );

    Some(instantiation_context.into_types_for_parameters())
}

fn resolve_trait<D: Driver>(
    query: WithInfo<D::Info, &Instance<D>>,
    context: &mut ResolveContext<'_, D>,
) -> Result<WithInfo<D::Info, Result<D::Path, Instance<D>>>, WithInfo<D::Info, QueuedError<D>>> {
    type Candidate<D> = (
        TypeContext<D>,
        WithInfo<<D as Driver>::Info, (Option<<D as Driver>::Path>, Instance<D>)>,
        Vec<WithInfo<<D as Driver>::Info, Instance<D>>>,
    );

    fn pick_from_candidates<D: Driver>(
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

    fn resolve_trait_inner<D: Driver>(
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

fn resolve_custom_error<D: Driver>(
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

fn resolve_custom_reason<D: Driver>(
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

fn substitute_defaults_in_expression<D: Driver>(
    driver: &D,
    expression: WithInfo<D::Info, &mut Expression<D>>,
    context: &mut ResolveContext<'_, D>,
) -> bool {
    let substituted_subexpression = match &mut expression.item.kind {
        ExpressionKind::Block { statements, .. } => statements.iter_mut().any(|statement| {
            substitute_defaults_in_expression(driver, statement.as_mut(), context)
        }),
        ExpressionKind::Do(block) => {
            substitute_defaults_in_expression(driver, block.as_deref_mut(), context)
        }
        ExpressionKind::Function { body, .. } => {
            substitute_defaults_in_expression(driver, body.as_deref_mut(), context)
        }
        ExpressionKind::Call { function, inputs } => {
            inputs
                .iter_mut()
                .any(|input| substitute_defaults_in_expression(driver, input.as_mut(), context))
                || substitute_defaults_in_expression(driver, function.as_deref_mut(), context)
        }
        ExpressionKind::When { input, arms } => {
            substitute_defaults_in_expression(driver, input.as_deref_mut(), context)
                || arms.iter_mut().any(|arm| {
                    substitute_defaults_in_expression(driver, arm.item.body.as_mut(), context)
                })
        }
        ExpressionKind::Intrinsic { inputs, .. } => inputs
            .iter_mut()
            .any(|input| substitute_defaults_in_expression(driver, input.as_mut(), context)),
        ExpressionKind::Initialize { value, .. } => {
            substitute_defaults_in_expression(driver, value.as_deref_mut(), context)
        }
        ExpressionKind::Mutate { value, .. } => {
            substitute_defaults_in_expression(driver, value.as_deref_mut(), context)
        }
        ExpressionKind::UnresolvedStructure(fields) => fields.iter_mut().any(|field| {
            substitute_defaults_in_expression(driver, field.item.value.as_mut(), context)
        }),
        ExpressionKind::ResolvedStructure { fields, .. } => fields.iter_mut().any(|field| {
            substitute_defaults_in_expression(driver, field.item.value.as_mut(), context)
        }),
        ExpressionKind::Variant { values, .. } => values
            .iter_mut()
            .any(|value| substitute_defaults_in_expression(driver, value.as_mut(), context)),
        ExpressionKind::Wrapper(value) => {
            substitute_defaults_in_expression(driver, value.as_deref_mut(), context)
        }
        ExpressionKind::Tuple(elements) => elements
            .iter_mut()
            .any(|element| substitute_defaults_in_expression(driver, element.as_mut(), context)),
        ExpressionKind::Format { segments, .. } => segments.iter_mut().any(|segment| {
            substitute_defaults_in_expression(driver, segment.value.as_mut(), context)
        }),
        ExpressionKind::ResolvedConstant { parameters, .. } => parameters
            .iter_mut()
            .any(|r#type| substitute_defaults(driver, r#type, context.type_context)),
        ExpressionKind::Unknown(_)
        | ExpressionKind::Variable(_, _)
        | ExpressionKind::UnresolvedConstant(_)
        | ExpressionKind::UnresolvedTrait(_)
        | ExpressionKind::ResolvedTrait { .. }
        | ExpressionKind::Marker(_)
        | ExpressionKind::Number(_)
        | ExpressionKind::Text(_) => false,
    };

    substituted_subexpression
        || substitute_defaults(driver, &mut expression.item.r#type, context.type_context)
}

// MARK: Finalize

struct FinalizeContext<'a, D: Driver> {
    driver: &'a D,
    type_context: &'a mut TypeContext<D>,
    bound_instances: Vec<Vec<WithInfo<D::Info, Instance<D>>>>,
    error_queue: Option<&'a mut Vec<WithInfo<D::Info, QueuedError<D>>>>,
    errors: Option<&'a mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>>,
    unresolved_variables: Option<&'a mut HashSet<TypeVariable<D>>>,
    contains_unknown: bool,
    subexpression_types: Option<&'a mut Vec<crate::Type<D>>>, // in the order of traversal
}

fn finalize_type<D: Driver>(
    r#type: Type<D>,
    report_error: bool,
    context: &mut FinalizeContext<'_, D>,
) -> WithInfo<D::Info, crate::Type<D>> {
    fn finalize_type_inner<D: Driver>(
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

fn finalize_type_reason<D: Driver>(
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

fn finalize_expression<D: Driver>(
    mut expression: WithInfo<D::Info, Expression<D>>,
    context: &mut FinalizeContext<'_, D>,
) -> WithInfo<D::Info, crate::TypedExpression<D>> {
    let mut report_errors = true;
    let kind = match expression.item.kind {
        ExpressionKind::Unknown(path) => {
            report_errors = false;
            crate::TypedExpressionKind::Unknown(path)
        }
        ExpressionKind::Variable(name, variable) => {
            crate::TypedExpressionKind::Variable(name, variable)
        }
        ExpressionKind::UnresolvedConstant(ref path) => {
            let path = path.clone();

            if let Some((error_queue, errors)) = context
                .error_queue
                .as_deref_mut()
                .zip(context.errors.as_deref_mut())
            {
                let mut resolve_context = ResolveContext {
                    driver: context.driver,
                    type_context: context.type_context,
                    error_queue,
                    errors,
                    variables: &mut Default::default(),
                    recursion_stack: &mut Default::default(),
                    bound_instances: context.bound_instances.clone(),
                };

                match resolve_item(&path, expression.as_mut(), false, &mut resolve_context) {
                    Ok(Some((parameters, bounds))) => {
                        finalize_type(expression.item.r#type.clone(), true, context);

                        crate::TypedExpressionKind::Constant {
                            path: path.clone(),
                            parameters: parameters
                                .into_iter()
                                .map(|r#type| finalize_type(r#type, true, context).item)
                                .collect(),
                            bounds: bounds
                                .into_iter()
                                .map(|bound| {
                                    bound.map(|bound| {
                                        bound.map_err(|instance| {
                                            finalize_instance(instance, context)
                                        })
                                    })
                                })
                                .collect(),
                        }
                    }
                    Ok(None) => crate::TypedExpressionKind::Unknown(Some(path)),
                    Err(error) => {
                        error_queue.push(error);
                        crate::TypedExpressionKind::Unknown(Some(path))
                    }
                }
            } else {
                crate::TypedExpressionKind::Unknown(Some(path))
            }
        }
        ExpressionKind::UnresolvedTrait(path) => {
            let error = WithInfo {
                info: expression.info.clone(),
                item: crate::Diagnostic::UnknownType(
                    finalize_type(expression.item.r#type.clone(), true, context).item,
                ),
            };

            if let Some(errors) = context.errors.as_deref_mut() {
                errors.push(error);
            }

            crate::TypedExpressionKind::Unknown(Some(path))
        }
        ExpressionKind::ResolvedConstant {
            path,
            parameters,
            bounds,
        } => crate::TypedExpressionKind::Constant {
            path,
            parameters: parameters
                .into_iter()
                .map(|r#type| finalize_type(r#type, true, context).item)
                .collect(),
            bounds: bounds
                .into_iter()
                .map(|bound| {
                    bound
                        .map(|bound| bound.map_err(|instance| finalize_instance(instance, context)))
                })
                .collect(),
        },
        ExpressionKind::ResolvedTrait {
            trait_path,
            parameters,
            instance,
        } => crate::TypedExpressionKind::Trait {
            path: trait_path,
            parameters: parameters
                .into_iter()
                .map(|r#type| finalize_type(r#type, true, context).item)
                .collect(),
            instance: instance
                .map(|instance| instance.map_err(|bound| finalize_instance(bound, context))),
        },
        ExpressionKind::Number(number) => crate::TypedExpressionKind::Number(number),
        ExpressionKind::Text(text) => crate::TypedExpressionKind::Text(text),
        ExpressionKind::Block {
            statements,
            top_level,
            captures,
        } => {
            let statement_count = statements.len();

            crate::TypedExpressionKind::Block {
                statements: statements
                    .into_iter()
                    .enumerate()
                    .map(|(index, statement)| {
                        let is_last_statement = index + 1 == statement_count;

                        let statement_type = statement.item.r#type.clone();
                        let statement = finalize_expression(statement, context);

                        // Report errors for unused values in statement position...
                        if top_level
                            || !is_last_statement
                                && !matches!(statement.item.r#type, crate::Type::Unknown)
                        {
                            let reported_custom_error = try_report_custom_unused_error(
                                context.driver,
                                &statement.info,
                                &statement_type,
                                context.type_context,
                                context.errors.as_deref_mut(),
                            );

                            // ...as well as uncalled functions
                            if !reported_custom_error {
                                if let crate::Type::Function { inputs, .. } = &statement.item.r#type
                                {
                                    if let crate::TypedExpressionKind::Constant { .. }
                                    | crate::TypedExpressionKind::Trait { .. }
                                    | crate::TypedExpressionKind::Variable { .. } =
                                        &statement.item.kind
                                    {
                                        if let Some(errors) = context.errors.as_deref_mut() {
                                            errors.push(WithInfo {
                                                info: statement.info.clone(),
                                                item: crate::Diagnostic::MissingInputs(
                                                    inputs.clone(),
                                                ),
                                            });
                                        }
                                    }
                                }
                            }
                        }

                        statement
                    })
                    .collect(),
                captures,
            }
        }
        ExpressionKind::Do(block) => {
            crate::TypedExpressionKind::Do(finalize_expression(block.unboxed(), context).boxed())
        }
        ExpressionKind::Function {
            inputs,
            body,
            captures,
        } => crate::TypedExpressionKind::Function {
            inputs,
            body: finalize_expression(body.unboxed(), context).boxed(),
            captures,
        },
        ExpressionKind::Call { function, inputs } => {
            let inputs = inputs
                .into_iter()
                .map(|input| finalize_expression(input, context))
                .collect::<Vec<_>>();

            let function = finalize_expression(function.unboxed(), context);

            crate::TypedExpressionKind::Call {
                function: function.boxed(),
                inputs,
            }
        }
        ExpressionKind::When { input, arms } => crate::TypedExpressionKind::When {
            input: finalize_expression(input.unboxed(), context).boxed(),
            arms: arms
                .into_iter()
                .map(|arm| {
                    arm.map(|arm| crate::TypedArm {
                        pattern: arm.pattern,
                        body: finalize_expression(arm.body, context),
                    })
                })
                .collect(),
        },
        ExpressionKind::Intrinsic { name, inputs } => crate::TypedExpressionKind::Intrinsic {
            name,
            inputs: inputs
                .into_iter()
                .map(|expression| finalize_expression(expression, context))
                .collect(),
        },
        ExpressionKind::Initialize { pattern, value } => crate::TypedExpressionKind::Initialize {
            pattern,
            value: finalize_expression(value.unboxed(), context).boxed(),
        },
        ExpressionKind::Mutate { name, path, value } => crate::TypedExpressionKind::Mutate {
            name,
            path,
            value: finalize_expression(value.unboxed(), context).boxed(),
        },
        ExpressionKind::Marker(r#type) => crate::TypedExpressionKind::Marker(r#type),
        ExpressionKind::UnresolvedStructure(_) => {
            let error = WithInfo {
                info: expression.info.clone(),
                item: crate::Diagnostic::UnknownType(
                    finalize_type(expression.item.r#type.clone(), true, context).item,
                ),
            };

            if let Some(errors) = context.errors.as_deref_mut() {
                errors.push(error);
            }

            crate::TypedExpressionKind::Unknown(None)
        }
        ExpressionKind::ResolvedStructure { structure, fields } => {
            let field_indices = match context
                .driver
                .get_type_declaration(&structure)
                .item
                .representation
                .item
            {
                crate::TypeRepresentation::Structure(fields) => fields
                    .into_iter()
                    .map(|(name, field)| (name, field.item.index))
                    .collect::<HashMap<_, _>>(),
                _ => HashMap::new(),
            };

            crate::TypedExpressionKind::Structure {
                structure,
                fields: fields
                    .into_iter()
                    .map(|field_value| {
                        field_value.map(|field_value| crate::TypedStructureFieldValue {
                            name: field_value.name.clone(),
                            index: field_indices.get(&field_value.name).copied(),
                            value: finalize_expression(field_value.value, context),
                        })
                    })
                    .collect(),
            }
        }
        ExpressionKind::Wrapper(value) => crate::TypedExpressionKind::Wrapper(
            finalize_expression(value.unboxed(), context).boxed(),
        ),
        ExpressionKind::Variant { variant, values } => crate::TypedExpressionKind::Variant {
            variant,
            values: values
                .into_iter()
                .map(|expression| finalize_expression(expression, context))
                .collect(),
        },
        ExpressionKind::Tuple(elements) => crate::TypedExpressionKind::Tuple(
            elements
                .into_iter()
                .map(|expression| finalize_expression(expression, context))
                .collect(),
        ),
        ExpressionKind::Format { segments, trailing } => crate::TypedExpressionKind::Format {
            segments: segments
                .into_iter()
                .map(|segment| crate::TypedFormatSegment {
                    text: segment.text.clone(),
                    value: finalize_expression(segment.value, context),
                })
                .collect(),
            trailing,
        },
    };

    let r#type = finalize_type(expression.item.r#type.clone(), report_errors, context).item;

    WithInfo {
        info: expression.info,
        item: crate::TypedExpression { r#type, kind },
    }
}

fn finalize_instance<D: Driver>(
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

fn refine_mismatch_error<D: Driver>(
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

#[allow(dead_code)]
fn debug_instance<D: Driver>(instance: &Instance<D>, context: &mut TypeContext<D>) -> String {
    format!(
        "({:?}{})",
        debug_path(&instance.r#trait),
        instance
            .parameters
            .iter()
            .fold(String::new(), |mut result, parameter| {
                use std::fmt::Write;
                write!(&mut result, " {}", debug_type(parameter, context)).unwrap();
                result
            })
    )
}

#[allow(dead_code)]
fn debug_type<D: Driver>(r#type: &Type<D>, context: &mut TypeContext<D>) -> String {
    let r#type = r#type.apply_in_context(context);

    match r#type.kind {
        TypeKind::Variable(variable) => format!("{{{:?}}}", variable),
        TypeKind::Opaque(_) => String::from("{opaque}"),
        TypeKind::Parameter(path) => format!("({})", debug_path(&path)),
        TypeKind::Declared { path, parameters } => format!(
            "({:?}{})",
            debug_path(&path),
            parameters
                .into_iter()
                .fold(String::new(), |mut result, parameter| {
                    use std::fmt::Write;
                    write!(&mut result, " {}", debug_type(&parameter, context)).unwrap();
                    result
                })
        ),
        TypeKind::Function { inputs, output } => {
            format!(
                "({}-> {})",
                inputs.into_iter().fold(String::new(), |mut result, input| {
                    use std::fmt::Write;
                    write!(&mut result, "{} ", debug_type(&input, context)).unwrap();
                    result
                }),
                debug_type(&output, context),
            )
        }
        TypeKind::Tuple(elements) => format!(
            "({})",
            elements.iter().fold(String::new(), |mut result, element| {
                use std::fmt::Write;
                write!(&mut result, " {} ,", debug_type(element, context)).unwrap();
                result
            })
        ),
        TypeKind::Block(r#type) => format!("{{{}}}", debug_type(&r#type, context)),
        TypeKind::Unknown => String::from("_"),
        TypeKind::Intrinsic => String::from("intrinsic"),
        TypeKind::Equal { left, right } => format!(
            "({} = {})",
            debug_type(&left, context),
            debug_type(&right, context)
        ),
        TypeKind::Message { segments, trailing } => {
            let mut message = String::new();
            for segment in segments {
                use std::fmt::Write;
                message.push_str(&segment.text);
                write!(&mut message, "{}", debug_type(&segment.value, context)).unwrap();
            }

            message.push_str(&trailing);

            message
        }
    }
}

#[allow(dead_code)]
fn debug_path(path: &(impl Debug + serde::Serialize)) -> String {
    serde_json::to_value(path)
        .ok()
        .and_then(|value| value.as_str().map(ToString::to_string))
        .unwrap_or_else(|| format!("{path:?}"))
}
