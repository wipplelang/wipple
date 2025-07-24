use crate::{
    lower::{name::resolve_name, resolve::Info, utils::check_parameter_count},
    util::WithInfo,
};

pub fn resolve_type(
    r#type: WithInfo<crate::lower::UnresolvedType>,
    info: &mut Info,
) -> WithInfo<crate::lower::Type> {
    r#type.map(|r#type| match r#type {
        crate::lower::UnresolvedType::Error => crate::lower::Type::Error,
        crate::lower::UnresolvedType::Placeholder => crate::lower::Type::Placeholder,
        crate::lower::UnresolvedType::Declared { name, parameters } => {
            let name = match name.try_unwrap() {
                Some(name) => name,
                None => return crate::lower::Type::Error,
            };

            let filter = |candidates: &[WithInfo<crate::lower::Path>]| {
                let mut candidates = candidates.to_vec();
                candidates.sort_by_key(|path| match path.item.last().unwrap() {
                    crate::lower::PathComponent::Type(_)
                    | crate::lower::PathComponent::TypeParameter(_) => 0,
                    crate::lower::PathComponent::Constant(_) => 1,
                    _ => 2,
                });

                match candidates.into_iter().next() {
                    Some(candidate) => vec![(candidate.item.clone(), candidate)],
                    None => Vec::new(),
                }
            };

            let name_info = name.info.clone();

            match resolve_name(name, info, filter) {
                Some(path) => {
                    match path.item.last().unwrap() {
                        crate::lower::PathComponent::Type(_) => {
                            let (type_declaration_info, _) =
                                &info.type_declarations.get(&path.item).unwrap().item;

                            check_parameter_count(
                                name_info,
                                type_declaration_info.parameters,
                                &parameters,
                                info,
                            );

                            crate::lower::Type::Declared {
                                path,
                                parameters: parameters
                                    .into_iter()
                                    .map(|parameter| resolve_type(parameter, info))
                                    .collect(),
                            }
                        }
                        crate::lower::PathComponent::TypeParameter(_) => {
                            // FIXME: disallow parameters passed to type parameters
                            crate::lower::Type::Parameter(path.item)
                        }
                        _ => crate::lower::Type::Error,
                    }
                }
                None => crate::lower::Type::Error,
            }
        }
        crate::lower::UnresolvedType::Function { inputs, output } => crate::lower::Type::Function {
            inputs: inputs
                .into_iter()
                .map(|input| resolve_type(input, info))
                .collect(),
            output: resolve_type(output.unboxed(), info).boxed(),
        },
        crate::lower::UnresolvedType::Tuple(elements) => crate::lower::Type::Tuple(
            elements
                .into_iter()
                .map(|element| resolve_type(element, info))
                .collect(),
        ),
        crate::lower::UnresolvedType::Block(r#type) => {
            let r#type = resolve_type(r#type.unboxed(), info);
            crate::lower::Type::Block(r#type.boxed())
        }
        crate::lower::UnresolvedType::Intrinsic => crate::lower::Type::Intrinsic,
        crate::lower::UnresolvedType::Message { segments, trailing } => {
            crate::lower::Type::Message {
                segments: segments
                    .into_iter()
                    .map(|segment| crate::lower::FormatSegment {
                        text: segment.text,
                        value: resolve_type(segment.value, info),
                    })
                    .collect(),
                trailing,
            }
        }
        crate::lower::UnresolvedType::Equal { left, right } => crate::lower::Type::Equal {
            left: resolve_type(left.unboxed(), info).boxed(),
            right: resolve_type(right.unboxed(), info).boxed(),
        },
    })
}

pub fn resolve_type_parameter(
    type_parameter: WithInfo<crate::lower::UnresolvedTypeParameter>,
    info: &mut Info,
) -> Option<crate::lower::Path> {
    let type_parameter_info = type_parameter.info.clone();

    let name = type_parameter.item.name.try_unwrap()?;

    let mut path = info.path.clone();
    path.push(crate::lower::PathComponent::TypeParameter(
        name.item.clone(),
    ));

    info.scopes.define(
        name.item,
        WithInfo {
            info: type_parameter_info.clone(),
            item: path.clone(),
        },
    );

    let type_parameter_declaration = crate::lower::TypeParameterDeclaration {
        infer: type_parameter.item.infer,
        default: type_parameter
            .item
            .default
            .map(|default| resolve_type(default, info)),
    };

    info.type_parameter_declarations.insert(
        path.clone(),
        WithInfo {
            info: type_parameter_info,
            item: Some(type_parameter_declaration),
        },
    );

    Some(path)
}

pub fn resolve_instance(
    instance: WithInfo<crate::lower::UnresolvedInstance>,
    info: &mut Info,
) -> Option<WithInfo<crate::lower::Instance>> {
    instance.filter_map(|instance| {
        let r#trait = instance.r#trait.try_unwrap()?;
        let trait_info = r#trait.info.clone();

        let r#trait = match resolve_name(r#trait, info, |candidates| {
            candidates
                .iter()
                .filter_map(|path| match path.item.last().unwrap() {
                    crate::lower::PathComponent::Trait(_) => {
                        Some((path.item.clone(), path.clone()))
                    }
                    _ => None,
                })
                .collect::<Vec<_>>()
        }) {
            Some(r#trait) => r#trait,
            None => return None,
        };

        let (trait_declaration_info, _) = &info.trait_declarations.get(&r#trait.item).unwrap().item;

        check_parameter_count(
            trait_info,
            trait_declaration_info.parameters,
            &instance.parameters,
            info,
        );

        Some(crate::lower::Instance {
            r#trait,
            parameters: instance
                .parameters
                .into_iter()
                .map(|parameter| resolve_type(parameter, info))
                .collect(),
        })
    })
}
