use crate::{
    lower::{
        attribute::resolve_attributes,
        constructors::{
            generate_marker_constructor, generate_structure_constructor,
            generate_trait_constructor, generate_variant_constructor, generate_wrapper_constructor,
        },
        expression::resolve_expression,
        name::resolve_name,
        pattern::resolve_pattern,
        resolve::Info,
        scope::Captures,
        r#type::{resolve_instance, resolve_type, resolve_type_parameter},
    },
    util::WithInfo,
};
use std::mem;

#[derive(Debug, Clone)]
pub struct EagerTypeDeclarationInfo {
    pub parameters: u32,
}

impl From<&crate::lower::TypeDeclaration> for EagerTypeDeclarationInfo {
    fn from(declaration: &crate::lower::TypeDeclaration) -> Self {
        EagerTypeDeclarationInfo {
            parameters: declaration.parameters.len() as u32,
        }
    }
}

#[derive(Debug, Clone)]
pub struct EagerTraitDeclarationInfo {
    pub parameters: u32,
}

impl From<&crate::lower::TraitDeclaration> for EagerTraitDeclarationInfo {
    fn from(declaration: &crate::lower::TraitDeclaration) -> Self {
        EagerTraitDeclarationInfo {
            parameters: declaration.parameters.len() as u32,
        }
    }
}

pub fn split_executable_statements(
    statements: Vec<WithInfo<crate::lower::UnresolvedStatement>>,
    info: &mut Info,
) -> (
    Vec<Vec<WithInfo<crate::lower::UnresolvedStatement>>>,
    Vec<WithInfo<crate::lower::UnresolvedStatement>>,
) {
    let (declaration_statements, executable_statements): (Vec<_>, Vec<_>) = statements
        .into_iter()
        .filter_map(|statement| {
            macro_rules! insert_declaration {
                ($declarations:ident, $name:expr, $path:expr, $default:expr,) => {{
                    use std::collections::hash_map::Entry;

                    let path = $path;

                    match info.$declarations.entry(path.clone()) {
                        Entry::Occupied(entry) => {
                            info.errors.push($name.replace(
                                crate::lower::Diagnostic::AlreadyDefined(entry.key().clone()),
                            ));

                            None
                        }
                        Entry::Vacant(entry) => {
                            entry.insert($name.replace($default));

                            info.scopes.define($name.item.clone(), $name.replace(path));

                            Some(statement)
                        }
                    }
                }};
            }

            match &statement.item {
                crate::lower::UnresolvedStatement::Syntax { name, .. } => {
                    insert_declaration!(
                        syntax_declarations,
                        name,
                        info.make_path(crate::lower::PathComponent::Syntax(name.item.clone())),
                        None,
                    )
                }
                crate::lower::UnresolvedStatement::Type {
                    name, parameters, ..
                } => insert_declaration!(
                    type_declarations,
                    name,
                    info.make_path(crate::lower::PathComponent::Type(name.item.clone())),
                    (
                        EagerTypeDeclarationInfo {
                            parameters: parameters.len() as u32,
                        },
                        None,
                    ),
                ),
                crate::lower::UnresolvedStatement::Trait {
                    name, parameters, ..
                } => insert_declaration!(
                    trait_declarations,
                    name,
                    info.make_path(crate::lower::PathComponent::Trait(name.item.clone())),
                    (
                        EagerTraitDeclarationInfo {
                            parameters: parameters.len() as u32,
                        },
                        None,
                    ),
                ),
                crate::lower::UnresolvedStatement::Constant { name, .. } => insert_declaration!(
                    constant_declarations,
                    name,
                    info.make_path(crate::lower::PathComponent::Constant(name.item.clone())),
                    None,
                ),
                crate::lower::UnresolvedStatement::Instance { .. }
                | crate::lower::UnresolvedStatement::Assignment { .. }
                | crate::lower::UnresolvedStatement::Expression(_) => Some(statement),
            }
        })
        .partition(|statement| {
            matches!(
                statement.item,
                crate::lower::UnresolvedStatement::Syntax { .. }
                    | crate::lower::UnresolvedStatement::Type { .. }
                    | crate::lower::UnresolvedStatement::Trait { .. }
            )
        });

    let (type_statements, language_statements): (Vec<_>, Vec<_>) =
        declaration_statements.into_iter().partition(|statement| {
            matches!(
                statement.item,
                crate::lower::UnresolvedStatement::Syntax { .. }
                    | crate::lower::UnresolvedStatement::Type { .. }
                    | crate::lower::UnresolvedStatement::Trait { .. }
            )
        });

    (
        vec![type_statements, language_statements],
        executable_statements,
    )
}

pub fn resolve_statements(
    statements: Vec<WithInfo<crate::lower::UnresolvedStatement>>,
    info: &mut Info,
) -> Vec<WithInfo<crate::lower::Expression>> {
    let mut queued_constants = Vec::new();

    let statements = statements
        .into_iter()
        .filter_map(|statement| match statement.item {
            crate::lower::UnresolvedStatement::Syntax { attributes, name } => {
                info.path
                    .push(crate::lower::PathComponent::Syntax(name.item));

                resolve_attributes(&attributes, info);

                let declaration = info.syntax_declarations.get_mut(&info.path).unwrap();

                declaration.item = Some(crate::lower::SyntaxDeclaration { attributes });

                info.path.pop().unwrap();

                None
            }
            crate::lower::UnresolvedStatement::Type {
                attributes,
                name,
                parameters,
                representation,
            } => {
                info.path
                    .push(crate::lower::PathComponent::Type(name.item.clone()));

                info.scopes.push_constant_scope();

                let parameters = parameters
                    .into_iter()
                    .filter_map(|type_parameter| resolve_type_parameter(type_parameter, info))
                    .collect::<Vec<_>>();

                let mut constructors = Vec::new();
                let representation = representation.map(|representation| match representation {
                    crate::lower::UnresolvedTypeRepresentation::Marker => {
                        constructors.push(generate_marker_constructor(
                            attributes.clone(),
                            name.clone(),
                            parameters.clone(),
                            info,
                        ));

                        crate::lower::TypeRepresentation::Marker
                    }
                    crate::lower::UnresolvedTypeRepresentation::Structure(fields) => {
                        constructors.push(generate_structure_constructor(
                            attributes.clone(),
                            name,
                            parameters.clone(),
                            info,
                        ));

                        crate::lower::TypeRepresentation::Structure(
                            fields
                                .into_iter()
                                .map(|field| {
                                    field.map(|field| crate::lower::Field {
                                        index: field.index,
                                        attributes: field.attributes,
                                        name: field.name,
                                        r#type: resolve_type(field.r#type, info),
                                    })
                                })
                                .collect(),
                        )
                    }
                    crate::lower::UnresolvedTypeRepresentation::Enumeration(variants) => {
                        crate::lower::TypeRepresentation::Enumeration(
                            variants
                                .into_iter()
                                .map(|variant| {
                                    variant.map(|variant| {
                                        let variant_path = variant.name.clone().map(|name| {
                                            info.make_path(crate::lower::PathComponent::Variant(
                                                name,
                                            ))
                                        });

                                        let value_types = variant
                                            .types
                                            .into_iter()
                                            .map(|r#type| resolve_type(r#type, info))
                                            .collect::<Vec<_>>();

                                        constructors.push(generate_variant_constructor(
                                            variant.attributes.clone(),
                                            variant.name.clone(),
                                            parameters.clone(),
                                            variant_path.clone(),
                                            value_types.clone(),
                                            info,
                                        ));

                                        constructors
                                            .push((variant.name.item, variant_path.clone()));

                                        crate::lower::Variant {
                                            index: variant.index,
                                            attributes: variant.attributes,
                                            name: variant_path,
                                            types: value_types,
                                        }
                                    })
                                })
                                .collect(),
                        )
                    }
                    crate::lower::UnresolvedTypeRepresentation::Wrapper(r#type) => {
                        let value_type = resolve_type(r#type, info);

                        constructors.push(generate_wrapper_constructor(
                            attributes.clone(),
                            name.clone(),
                            value_type.clone(),
                            parameters.clone(),
                            info,
                        ));

                        crate::lower::TypeRepresentation::Wrapper(value_type)
                    }
                });

                resolve_attributes(&attributes, info);

                info.scopes.pop_scope();

                let declaration = info.type_declarations.get_mut(&info.path).unwrap();

                declaration.item.1 = Some(crate::lower::TypeDeclaration {
                    attributes,
                    parameters,
                    representation,
                });

                info.path.pop().unwrap();

                for (name, path) in constructors {
                    info.scopes.define(name, path);
                }

                None
            }
            crate::lower::UnresolvedStatement::Trait {
                attributes,
                name,
                parameters,
                r#type,
            } => {
                info.path
                    .push(crate::lower::PathComponent::Trait(name.item.clone()));

                info.scopes.push_constant_scope();

                let parameters = parameters
                    .into_iter()
                    .filter_map(|type_parameter| resolve_type_parameter(type_parameter, info))
                    .collect::<Vec<_>>();

                let r#type = r#type.map(|r#type| resolve_type(r#type, info));

                resolve_attributes(&attributes, info);

                info.scopes.pop_scope();

                let declaration = info.trait_declarations.get_mut(&info.path).unwrap();

                declaration.item.1 = Some(crate::lower::TraitDeclaration {
                    attributes: attributes.clone(),
                    parameters: parameters.clone(),
                    r#type: r#type.clone(),
                });

                if let Some(r#type) = r#type {
                    let (name, constructor) =
                        generate_trait_constructor(attributes, name, parameters, r#type, info);

                    info.scopes.define(name, constructor);
                }

                info.path.pop().unwrap();

                None
            }
            crate::lower::UnresolvedStatement::Constant {
                attributes,
                name,
                parameters,
                bounds,
                r#type,
                body,
            } => {
                info.path
                    .push(crate::lower::PathComponent::Constant(name.item));

                info.scopes.push_constant_scope();

                let parameters = parameters
                    .into_iter()
                    .filter_map(|type_parameter| resolve_type_parameter(type_parameter, info))
                    .collect::<Vec<_>>();

                let bounds = bounds
                    .into_iter()
                    .filter_map(|bound| resolve_instance(bound, info))
                    .collect::<Vec<_>>();

                let r#type = resolve_type(r#type, info);

                resolve_attributes(&attributes, info);

                let scope = info.scopes.pop_scope();

                queued_constants.push((info.path.clone(), scope, body));

                let declaration = info
                    .constant_declarations
                    .get_mut(&info.path)
                    .unwrap_or_else(|| panic!("{:?}", info.path));

                declaration.item = Some(crate::lower::ConstantDeclaration {
                    attributes,
                    parameters,
                    bounds,
                    r#type,
                });

                info.path.pop().unwrap();

                None
            }
            crate::lower::UnresolvedStatement::Instance {
                pattern,
                parameters,
                bounds,
                instance,
                body,
                default,
            } => {
                info.path.push(crate::lower::PathComponent::Instance(
                    info.instance_declarations.len() as u32,
                ));

                info.scopes.push_constant_scope();
                let prev_next_variable = info.reset_next_variable();
                info.captures.push(Captures::default());

                let parameters = parameters
                    .into_iter()
                    .filter_map(|type_parameter| resolve_type_parameter(type_parameter, info))
                    .collect::<Vec<_>>();

                let bounds = bounds
                    .into_iter()
                    .filter_map(|bound| resolve_instance(bound, info))
                    .collect::<Vec<_>>();

                let instance = match resolve_instance(instance, info) {
                    Some(instance) => instance,
                    None => {
                        info.captures.pop().unwrap();
                        info.next_variable = prev_next_variable;
                        info.scopes.pop_scope();
                        info.path.pop().unwrap();

                        return None;
                    }
                };

                let body = body.map(|body| resolve_expression(body, info));

                let captures = info.captures.pop().unwrap();
                info.next_variable = prev_next_variable;
                info.scopes.pop_scope();

                info.instance_declarations.insert(
                    info.path.clone(),
                    pattern.replace(Some(crate::lower::InstanceDeclaration {
                        parameters,
                        bounds,
                        instance,
                        default,
                    })),
                );

                info.library.items.insert(
                    info.path.clone(),
                    body.map(|body| crate::lower::Item {
                        body,
                        captures: Vec::from_iter(captures.used),
                        top_level: info.top_level_path(),
                    }),
                );

                info.path.pop().unwrap();

                None
            }
            crate::lower::UnresolvedStatement::Assignment { pattern, value } => {
                let value = resolve_expression(value, info);

                match pattern.item {
                    crate::lower::UnresolvedPattern::Mutate(name) => {
                        let name = match name.clone().try_unwrap() {
                            Some(name) => name,
                            None => {
                                return Some(WithInfo {
                                    info: name.info.clone(),
                                    item: crate::lower::Expression::Assign {
                                        pattern: name.replace(crate::lower::Pattern::Error),
                                        value: value.boxed(),
                                    },
                                });
                            }
                        };

                        match resolve_name(name.clone(), info, |candidates| {
                            candidates
                                .iter()
                                .filter_map(|path| match path.item.last().unwrap() {
                                    crate::lower::PathComponent::Variable(_) => {
                                        Some((path.item.clone(), path.clone()))
                                    }
                                    _ => None,
                                })
                                .collect::<Vec<_>>()
                        }) {
                            Some(path) => Some(WithInfo {
                                info: name.info.clone(),
                                item: crate::lower::Expression::Mutate {
                                    name,
                                    path,
                                    value: value.boxed(),
                                },
                            }),
                            None => Some(WithInfo {
                                info: name.info.clone(),
                                item: crate::lower::Expression::Assign {
                                    pattern: name.replace(crate::lower::Pattern::Error),
                                    value: value.boxed(),
                                },
                            }),
                        }
                    }
                    _ => {
                        let pattern = resolve_pattern(pattern, info);

                        Some(WithInfo {
                            info: pattern.info.clone(),
                            item: crate::lower::Expression::Assign {
                                pattern,
                                value: value.boxed(),
                            },
                        })
                    }
                }
            }
            crate::lower::UnresolvedStatement::Expression(expression) => {
                Some(resolve_expression(expression, info))
            }
        })
        .collect();

    for (path, scope, body) in queued_constants {
        let prev_next_variable = info.reset_next_variable();
        let prev_path = mem::replace(&mut info.path, path);
        info.scopes.0.push(scope);
        info.captures.push(Captures::default());

        let body = resolve_expression(body, info);

        let captures = info.captures.pop().unwrap();
        info.scopes.pop_scope();
        let path = mem::replace(&mut info.path, prev_path);
        info.next_variable = prev_next_variable;

        info.library.items.insert(
            path,
            Some(crate::lower::Item {
                body,
                captures: Vec::from_iter(captures.used),
                top_level: info.top_level_path(),
            }),
        );
    }

    statements
}
