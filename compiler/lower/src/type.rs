use crate::{name::resolve_name, resolve::Info, utils::check_parameter_count, Driver};
use wipple_util::WithInfo;

pub fn resolve_type<D: Driver>(
    r#type: WithInfo<D::Info, crate::UnresolvedType<D>>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::Type<D>> {
    r#type.map(|r#type| match r#type {
        crate::UnresolvedType::Error => crate::Type::Error,
        crate::UnresolvedType::Placeholder => crate::Type::Placeholder,
        crate::UnresolvedType::Declared { name, parameters } => {
            let name = match name.try_unwrap() {
                Some(name) => name,
                None => return crate::Type::Error,
            };

            let filter = |candidates: &[WithInfo<D::Info, crate::Path>]| {
                let mut candidates = candidates.to_vec();
                candidates.sort_by_key(|path| match path.item.last().unwrap() {
                    crate::PathComponent::Type(_) | crate::PathComponent::TypeParameter(_) => 0,
                    crate::PathComponent::Constant(_) => 1,
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
                        crate::PathComponent::Type(_) => {
                            let (type_declaration_info, _) =
                                &info.type_declarations.get(&path.item).unwrap().item;

                            check_parameter_count(
                                name_info,
                                type_declaration_info.parameters,
                                &parameters,
                                info,
                            );

                            crate::Type::Declared {
                                path,
                                parameters: parameters
                                    .into_iter()
                                    .map(|parameter| resolve_type(parameter, info))
                                    .collect(),
                            }
                        }
                        crate::PathComponent::TypeParameter(_) => {
                            // FIXME: disallow parameters passed to type parameters
                            crate::Type::Parameter(path.item)
                        }
                        _ => crate::Type::Error,
                    }
                }
                None => crate::Type::Error,
            }
        }
        crate::UnresolvedType::Function { inputs, output } => crate::Type::Function {
            inputs: inputs
                .into_iter()
                .map(|input| resolve_type(input, info))
                .collect(),
            output: resolve_type(output.unboxed(), info).boxed(),
        },
        crate::UnresolvedType::Tuple(elements) => crate::Type::Tuple(
            elements
                .into_iter()
                .map(|element| resolve_type(element, info))
                .collect(),
        ),
        crate::UnresolvedType::Block(r#type) => {
            let r#type = resolve_type(r#type.unboxed(), info);
            crate::Type::Block(r#type.boxed())
        }
        crate::UnresolvedType::Intrinsic => crate::Type::Intrinsic,
        crate::UnresolvedType::Message { segments, trailing } => crate::Type::Message {
            segments: segments
                .into_iter()
                .map(|segment| crate::FormatSegment {
                    text: segment.text,
                    value: resolve_type(segment.value, info),
                })
                .collect(),
            trailing,
        },
        crate::UnresolvedType::Equal { left, right } => crate::Type::Equal {
            left: resolve_type(left.unboxed(), info).boxed(),
            right: resolve_type(right.unboxed(), info).boxed(),
        },
    })
}

pub fn resolve_type_parameter<D: Driver>(
    type_parameter: WithInfo<D::Info, crate::UnresolvedTypeParameter<D>>,
    info: &mut Info<D>,
) -> Option<crate::Path> {
    let type_parameter_info = type_parameter.info.clone();

    let name = type_parameter.item.name.try_unwrap()?;

    let mut path = info.path.clone();
    path.push(crate::PathComponent::TypeParameter(name.item.clone()));

    info.scopes.define(
        name.item,
        WithInfo {
            info: type_parameter_info.clone(),
            item: path.clone(),
        },
    );

    let type_parameter_declaration = crate::TypeParameterDeclaration {
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

pub fn resolve_instance<D: Driver>(
    instance: WithInfo<D::Info, crate::UnresolvedInstance<D>>,
    info: &mut Info<D>,
) -> Option<WithInfo<D::Info, crate::Instance<D>>> {
    instance.filter_map(|instance| {
        let r#trait = instance.r#trait.try_unwrap()?;
        let trait_info = r#trait.info.clone();

        let r#trait = match resolve_name(r#trait, info, |candidates| {
            candidates
                .iter()
                .filter_map(|path| match path.item.last().unwrap() {
                    crate::PathComponent::Trait(_) => Some((path.item.clone(), path.clone())),
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

        Some(crate::Instance {
            r#trait,
            parameters: instance
                .parameters
                .into_iter()
                .map(|parameter| resolve_type(parameter, info))
                .collect(),
        })
    })
}
