use crate::{
    infer::{
        r#type::infer_type,
        types::{
            instantiate::InstantiationContext,
            unify::{try_unify, unify},
            Type, TypeKind,
        },
        InferContext,
    },
    utils::instantiated_language_type,
    Driver,
};
use wipple_util::WithInfo;

pub fn infer_pattern<D: Driver>(
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
                infer_pattern(field.item.pattern.as_mut(), r#type, context);
            }
        }
        crate::Pattern::Marker(path) => {
            let type_declaration = context.driver.get_type_declaration(path);

            let instantiation_context = InstantiationContext::from_parameters(
                context.driver,
                type_declaration.item.parameters,
                context.type_context,
                pattern.info.clone(),
                context.error_queue,
                context.errors,
            );

            let marker_type = Type::new(
                TypeKind::Declared {
                    path: path.clone(),
                    parameters: instantiation_context.into_types_for_parameters(),
                },
                pattern.info,
            );

            try_unify(
                context.driver,
                r#type.as_ref(),
                &marker_type,
                context.type_context,
                context.error_queue,
            );
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
                infer_pattern(pattern.as_mut(), r#type, context);
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
            infer_pattern(value_pattern.as_deref_mut(), value_type, context);
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
                infer_pattern(element.as_mut(), r#type, context);
            }
        }
        crate::Pattern::Or { left, right } => {
            infer_pattern(left.as_deref_mut(), r#type.as_ref(), context);
            infer_pattern(right.as_deref_mut(), r#type.as_ref(), context);
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

            infer_pattern(pattern.as_deref_mut(), r#type.as_ref(), context)
        }
    }
}
