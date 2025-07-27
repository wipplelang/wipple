use crate::{
    lower::Path,
    syntax::Location,
    typecheck::{
        Driver,
        infer::{
            Expression, ExpressionKind, FinalizeContext, InferContext, ResolveContext,
            errors::{QueuedError, report_queued_errors},
            expression::infer_expression,
            r#trait::resolve_trait,
            r#type::{finalize_type, infer_instance, infer_type},
            types::{
                Instance, Type, TypeKind,
                context::{TrackedExpressionId, TypeContext},
                instantiate::InstantiationContext,
                unify::{try_unify_expression, unify, unify_instance},
            },
        },
    },
    util::WithInfo,
};
use itertools::Itertools;
use std::collections::HashSet;

pub fn instances_overlap(
    driver: &dyn Driver,
    r#trait: &Path,
    mut instances: Vec<Path>,
) -> Vec<WithInfo<crate::typecheck::Diagnostic>> {
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
                    |parameters: Vec<Path>, instance: WithInfo<&mut Instance>| {
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
                        item: crate::typecheck::Diagnostic::OverlappingInstances {
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

pub fn resolve_trait_type_from_instance(
    driver: &dyn Driver,
    instance: WithInfo<&crate::typecheck::Instance>,
) -> Option<WithInfo<crate::typecheck::Type>> {
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

pub fn substitute_defaults_in_parameters(
    driver: &dyn Driver,
    r#type: WithInfo<&mut crate::typecheck::Type>,
) {
    match r#type.item {
        crate::typecheck::Type::Parameter(path) => {
            if let Some(default) = driver.get_type_parameter_declaration(path).item.default {
                *r#type.item = default.item;
            }
        }
        crate::typecheck::Type::Declared { parameters, .. } => {
            for parameter in parameters {
                substitute_defaults_in_parameters(driver, parameter.as_mut());
            }
        }
        crate::typecheck::Type::Function { inputs, output } => {
            for input in inputs {
                substitute_defaults_in_parameters(driver, input.as_mut());
            }

            substitute_defaults_in_parameters(driver, output.as_deref_mut());
        }
        crate::typecheck::Type::Tuple(elements) => {
            for element in elements {
                substitute_defaults_in_parameters(driver, element.as_mut());
            }
        }
        crate::typecheck::Type::Block(body) => {
            substitute_defaults_in_parameters(driver, body.as_deref_mut());
        }
        crate::typecheck::Type::Message { segments, .. } => {
            for segment in segments {
                substitute_defaults_in_parameters(driver, segment.r#type.as_mut());
            }
        }
        crate::typecheck::Type::Equal { left, right } => {
            substitute_defaults_in_parameters(driver, left.as_deref_mut());
            substitute_defaults_in_parameters(driver, right.as_deref_mut());
        }
        crate::typecheck::Type::Unknown | crate::typecheck::Type::Intrinsic => {}
    }
}

pub fn resolve_attribute_like_trait(
    driver: &dyn Driver,
    language_item: &str,
    r#type: WithInfo<&crate::typecheck::Type>,
    number_of_parameters: u32,
) -> Option<Vec<WithInfo<crate::typecheck::Type>>> {
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

pub fn instantiated_language_type(
    language_item: &str,
    info: Location,
    driver: &dyn Driver,
    type_context: &mut TypeContext,
    error_queue: &mut Vec<WithInfo<QueuedError>>,
    errors: &mut Vec<WithInfo<crate::typecheck::Diagnostic>>,
) -> Option<Type> {
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
                item: crate::typecheck::Diagnostic::MissingLanguageItem(language_item.to_string()),
            });

            None
        }
    }
}

pub fn try_instantiated_language_type(
    language_item: &str,
    info: Location,
    driver: &dyn Driver,
    type_context: &mut TypeContext,
    error_queue: &mut Vec<WithInfo<QueuedError>>,
    errors: &mut Vec<WithInfo<crate::typecheck::Diagnostic>>,
) -> Option<Type> {
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

pub fn instantiated_language_trait(
    language_item: &str,
    info: &Location,
    parent_id: Option<TrackedExpressionId>,
    context: &mut InferContext<'_>,
) -> WithInfo<Expression> {
    match context.driver.path_for_language_constructor(language_item) {
        Some(path) => context.with_tracked_expression(parent_id, |context, expression_id| {
            infer_expression(
                WithInfo {
                    info: info.clone(),
                    item: crate::typecheck::UntypedExpression::Constant(path),
                },
                expression_id,
                context,
            )
        }),
        None => {
            context.errors.push(WithInfo {
                info: info.clone(),
                item: crate::typecheck::Diagnostic::MissingLanguageItem(language_item.to_string()),
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

pub fn instantiated_language_constant(
    language_item: &str,
    info: &Location,
    parent_id: Option<TrackedExpressionId>,
    context: &mut InferContext<'_>,
) -> WithInfo<Expression> {
    match context.driver.path_for_language_constant(language_item) {
        Some(path) => context.with_tracked_expression(parent_id, |context, expression_id| {
            infer_expression(
                WithInfo {
                    info: info.clone(),
                    item: crate::typecheck::UntypedExpression::Constant(path),
                },
                expression_id,
                context,
            )
        }),
        None => {
            context.errors.push(WithInfo {
                info: info.clone(),
                item: crate::typecheck::Diagnostic::MissingLanguageItem(language_item.to_string()),
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

pub fn resolve_trait_parameters_from_type(
    path: &Path,
    use_expression: WithInfo<&mut Expression>,
    context: &mut ResolveContext<'_>,
) -> Option<Vec<(Path, Type)>> {
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

    Some(instantiation_context.into_substitutions())
}
