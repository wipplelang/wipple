use crate::{
    name::{resolve_name, try_resolve_name},
    r#type::resolve_type,
    resolve::Info,
    Driver,
};
use wipple_util::WithInfo;

pub fn resolve_pattern<D: Driver>(
    pattern: WithInfo<D::Info, crate::UnresolvedPattern<D>>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::Pattern<D>> {
    let mut defines = Vec::new();
    let pattern = resolve_pattern_inner(pattern, &mut defines, info);

    for (name, path) in defines {
        info.scopes.define(name, path);
    }

    pattern
}

fn resolve_pattern_inner<D: Driver>(
    pattern: WithInfo<D::Info, crate::UnresolvedPattern<D>>,
    defines: &mut Vec<(String, WithInfo<D::Info, crate::Path>)>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::Pattern<D>> {
    let pattern_info = pattern.info.clone();

    pattern.map(|pattern| match pattern {
        crate::UnresolvedPattern::Error => crate::Pattern::Error,
        crate::UnresolvedPattern::Wildcard => crate::Pattern::Wildcard,
        crate::UnresolvedPattern::Number(number) => crate::Pattern::Number(number),
        crate::UnresolvedPattern::Text(text) => crate::Pattern::Text(text),
        crate::UnresolvedPattern::Name(name) => {
            let index = info.next_variable;
            info.next_variable += 1;

            let path = info.make_path(crate::PathComponent::Variable(index));
            defines.push((
                name.clone(),
                WithInfo {
                    info: pattern_info,
                    item: path.clone(),
                },
            ));

            info.declare_variable(&path);

            crate::Pattern::Variable(name, path)
        }
        crate::UnresolvedPattern::VariantOrName(name) => {
            let name = match name {
                Some(name) => name,
                None => return crate::Pattern::Error,
            };

            macro_rules! get {
                ($kind:ident) => {
                    try_resolve_name(
                        WithInfo {
                            info: pattern_info.clone(),
                            item: name.clone(),
                        },
                        info,
                        |candidates| {
                            candidates
                                .iter()
                                .filter_map(|path| match path.item.last().unwrap() {
                                    crate::PathComponent::$kind(_) => {
                                        Some((path.item.clone(), path.clone()))
                                    }
                                    _ => None,
                                })
                                .collect::<Vec<_>>()
                        },
                    )
                };
            }

            if let Some(variant) = get!(Variant) {
                return crate::Pattern::Variant {
                    variant,
                    value_patterns: Vec::new(),
                };
            } else if let Some(r#type) = get!(Type) {
                let type_declaration = info
                    .type_declarations
                    .get(&r#type.item)
                    .unwrap()
                    .as_ref()
                    .map(|(_, declaration)| declaration.as_ref().unwrap());

                if matches!(
                    type_declaration.item.representation.item,
                    crate::TypeRepresentation::Marker
                ) {
                    return crate::Pattern::Marker(r#type.item);
                }
            }

            resolve_pattern_inner(
                WithInfo {
                    info: pattern_info.clone(),
                    item: crate::UnresolvedPattern::Name(name),
                },
                defines,
                info,
            )
            .item
        }
        crate::UnresolvedPattern::Destructure(fields) => crate::Pattern::Destructure(
            fields
                .into_iter()
                .map(|field| {
                    field.map(|field| crate::FieldPattern {
                        name: field.name,
                        pattern: resolve_pattern_inner(field.pattern, defines, info),
                    })
                })
                .collect(),
        ),
        crate::UnresolvedPattern::Variant {
            variant,
            value_patterns,
        } => {
            let name = match variant.try_unwrap() {
                Some(name) => name,
                None => return crate::Pattern::Error,
            };

            if let Some(variant) = try_resolve_name(name.clone(), info, |candidates| {
                candidates
                    .iter()
                    .filter_map(|path| match path.item.last().unwrap() {
                        crate::PathComponent::Variant(_) => Some((path.item.clone(), path.clone())),
                        _ => None,
                    })
                    .collect::<Vec<_>>()
            }) {
                let value_patterns = value_patterns
                    .into_iter()
                    .map(|pattern| resolve_pattern_inner(pattern, defines, info))
                    .collect::<Vec<_>>();

                crate::Pattern::Variant {
                    variant,
                    value_patterns,
                }
            } else if let Some(r#type) = resolve_name(name.clone(), info, |candidates| {
                candidates
                    .iter()
                    .filter_map(|path| match path.item.last().unwrap() {
                        crate::PathComponent::Type(_) => Some((path.item.clone(), path.clone())),
                        _ => None,
                    })
                    .collect::<Vec<_>>()
            }) {
                let type_declaration = info
                    .type_declarations
                    .get(&r#type.item)
                    .unwrap()
                    .as_ref()
                    .map(|(_, declaration)| declaration.as_ref().unwrap());

                if !matches!(
                    type_declaration.item.representation.item,
                    crate::TypeRepresentation::Wrapper(_)
                ) {
                    info.errors
                        .push(name.replace(crate::Diagnostic::NotAWrapper));

                    return crate::Pattern::Error;
                }

                if value_patterns.len() != 1 {
                    info.errors
                        .push(name.replace(crate::Diagnostic::WrapperExpectsASinglePattern));

                    return crate::Pattern::Error;
                }

                crate::Pattern::Wrapper {
                    path: r#type,
                    value_pattern: resolve_pattern_inner(
                        value_patterns.into_iter().next().unwrap(),
                        defines,
                        info,
                    )
                    .boxed(),
                }
            } else {
                crate::Pattern::Error
            }
        }
        crate::UnresolvedPattern::Tuple(patterns) => crate::Pattern::Tuple(
            patterns
                .into_iter()
                .map(|pattern| resolve_pattern_inner(pattern, defines, info))
                .collect(),
        ),
        crate::UnresolvedPattern::Or { left, right } => crate::Pattern::Or {
            left: resolve_pattern_inner(left.unboxed(), defines, info).boxed(),
            right: resolve_pattern_inner(right.unboxed(), defines, info).boxed(),
        },
        crate::UnresolvedPattern::Mutate(name) => {
            info.errors
                .push(name.replace(crate::Diagnostic::InvalidMutatePattern));

            crate::Pattern::Error
        }
        crate::UnresolvedPattern::Annotate { pattern, r#type } => crate::Pattern::Annotate {
            pattern: resolve_pattern_inner(pattern.unboxed(), defines, info).boxed(),
            r#type: resolve_type(r#type, info),
        },
    })
}
