use crate::{
    lower::{
        name::{resolve_name, try_resolve_name},
        resolve::Info,
        r#type::resolve_type,
    },
    util::WithInfo,
};

pub fn resolve_pattern(
    pattern: WithInfo<crate::lower::UnresolvedPattern>,
    info: &mut Info,
) -> WithInfo<crate::lower::Pattern> {
    let mut defines = Vec::new();
    let pattern = resolve_pattern_inner(pattern, &mut defines, info);

    for (name, path) in defines {
        info.scopes.define(name, path);
    }

    pattern
}

fn resolve_pattern_inner(
    pattern: WithInfo<crate::lower::UnresolvedPattern>,
    defines: &mut Vec<(String, WithInfo<crate::lower::Path>)>,
    info: &mut Info,
) -> WithInfo<crate::lower::Pattern> {
    let pattern_info = pattern.info.clone();

    pattern.map(|pattern| match pattern {
        crate::lower::UnresolvedPattern::Error => crate::lower::Pattern::Error,
        crate::lower::UnresolvedPattern::Wildcard => crate::lower::Pattern::Wildcard,
        crate::lower::UnresolvedPattern::Number(number) => crate::lower::Pattern::Number(number),
        crate::lower::UnresolvedPattern::Text(text) => crate::lower::Pattern::Text(text),
        crate::lower::UnresolvedPattern::Name(name) => {
            let index = info.next_variable;
            info.next_variable += 1;

            let path = info.make_path(crate::lower::PathComponent::Variable(index));
            defines.push((
                name.clone(),
                WithInfo {
                    info: pattern_info,
                    item: path.clone(),
                },
            ));

            info.declare_variable(&path);

            crate::lower::Pattern::Variable(name, path)
        }
        crate::lower::UnresolvedPattern::VariantOrName(name) => {
            let name = match name {
                Some(name) => name,
                None => return crate::lower::Pattern::Error,
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
                                    crate::lower::PathComponent::$kind(_) => {
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
                return crate::lower::Pattern::Variant {
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
                    crate::lower::TypeRepresentation::Marker
                ) {
                    return crate::lower::Pattern::Marker(r#type.item);
                }
            }

            resolve_pattern_inner(
                WithInfo {
                    info: pattern_info.clone(),
                    item: crate::lower::UnresolvedPattern::Name(name),
                },
                defines,
                info,
            )
            .item
        }
        crate::lower::UnresolvedPattern::Destructure(fields) => crate::lower::Pattern::Destructure(
            fields
                .into_iter()
                .map(|field| {
                    field.map(|field| crate::lower::FieldPattern {
                        name: field.name,
                        pattern: resolve_pattern_inner(field.pattern, defines, info),
                    })
                })
                .collect(),
        ),
        crate::lower::UnresolvedPattern::Variant {
            variant,
            value_patterns,
        } => {
            let name = match variant.try_unwrap() {
                Some(name) => name,
                None => return crate::lower::Pattern::Error,
            };

            if let Some(variant) = try_resolve_name(name.clone(), info, |candidates| {
                candidates
                    .iter()
                    .filter_map(|path| match path.item.last().unwrap() {
                        crate::lower::PathComponent::Variant(_) => {
                            Some((path.item.clone(), path.clone()))
                        }
                        _ => None,
                    })
                    .collect::<Vec<_>>()
            }) {
                let value_patterns = value_patterns
                    .into_iter()
                    .map(|pattern| resolve_pattern_inner(pattern, defines, info))
                    .collect::<Vec<_>>();

                crate::lower::Pattern::Variant {
                    variant,
                    value_patterns,
                }
            } else if let Some(r#type) = resolve_name(name.clone(), info, |candidates| {
                candidates
                    .iter()
                    .filter_map(|path| match path.item.last().unwrap() {
                        crate::lower::PathComponent::Type(_) => {
                            Some((path.item.clone(), path.clone()))
                        }
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
                    crate::lower::TypeRepresentation::Wrapper(_)
                ) {
                    info.errors
                        .push(name.replace(crate::lower::Diagnostic::NotAWrapper));

                    return crate::lower::Pattern::Error;
                }

                if value_patterns.len() != 1 {
                    info.errors
                        .push(name.replace(crate::lower::Diagnostic::WrapperExpectsASinglePattern));

                    return crate::lower::Pattern::Error;
                }

                crate::lower::Pattern::Wrapper {
                    path: r#type,
                    value_pattern: resolve_pattern_inner(
                        value_patterns.into_iter().next().unwrap(),
                        defines,
                        info,
                    )
                    .boxed(),
                }
            } else {
                crate::lower::Pattern::Error
            }
        }
        crate::lower::UnresolvedPattern::Tuple(patterns) => crate::lower::Pattern::Tuple(
            patterns
                .into_iter()
                .map(|pattern| resolve_pattern_inner(pattern, defines, info))
                .collect(),
        ),
        crate::lower::UnresolvedPattern::Or { left, right } => crate::lower::Pattern::Or {
            left: resolve_pattern_inner(left.unboxed(), defines, info).boxed(),
            right: resolve_pattern_inner(right.unboxed(), defines, info).boxed(),
        },
        crate::lower::UnresolvedPattern::Mutate(name) => {
            info.errors
                .push(name.replace(crate::lower::Diagnostic::InvalidMutatePattern));

            crate::lower::Pattern::Error
        }
        crate::lower::UnresolvedPattern::Annotate { pattern, r#type } => {
            crate::lower::Pattern::Annotate {
                pattern: resolve_pattern_inner(pattern.unboxed(), defines, info).boxed(),
                r#type: resolve_type(r#type, info),
            }
        }
    })
}
