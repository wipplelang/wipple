use crate::{
    attribute::resolve_attributes,
    constructors::{
        generate_marker_constructor, generate_structure_constructor, generate_trait_constructor,
        generate_variant_constructor, generate_wrapper_constructor,
    },
    expression::resolve_expression,
    name::resolve_name,
    pattern::resolve_pattern,
    r#type::{resolve_instance, resolve_type, resolve_type_parameter},
    resolve::Info,
    scope::Captures,
    Driver,
};
use std::mem;
use wipple_util::WithInfo;

#[derive(Debug, Clone)]
pub struct EagerTypeDeclarationInfo {
    pub parameters: u32,
}

impl<D: Driver> From<&crate::TypeDeclaration<D>> for EagerTypeDeclarationInfo {
    fn from(declaration: &crate::TypeDeclaration<D>) -> Self {
        EagerTypeDeclarationInfo {
            parameters: declaration.parameters.len() as u32,
        }
    }
}

#[derive(Debug, Clone)]
pub struct EagerTraitDeclarationInfo {
    pub parameters: u32,
}

impl<D: Driver> From<&crate::TraitDeclaration<D>> for EagerTraitDeclarationInfo {
    fn from(declaration: &crate::TraitDeclaration<D>) -> Self {
        EagerTraitDeclarationInfo {
            parameters: declaration.parameters.len() as u32,
        }
    }
}

pub fn split_executable_statements<D: Driver>(
    statements: Vec<WithInfo<D::Info, crate::UnresolvedStatement<D>>>,
    info: &mut Info<D>,
) -> (
    Vec<Vec<WithInfo<D::Info, crate::UnresolvedStatement<D>>>>,
    Vec<WithInfo<D::Info, crate::UnresolvedStatement<D>>>,
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
                            info.errors.push(
                                $name.replace(crate::Diagnostic::AlreadyDefined(
                                    entry.key().clone(),
                                )),
                            );

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
                crate::UnresolvedStatement::Syntax { name, .. } => {
                    insert_declaration!(
                        syntax_declarations,
                        name,
                        info.make_path(crate::PathComponent::Syntax(name.item.clone())),
                        None,
                    )
                }
                crate::UnresolvedStatement::Type {
                    name, parameters, ..
                } => insert_declaration!(
                    type_declarations,
                    name,
                    info.make_path(crate::PathComponent::Type(name.item.clone())),
                    (
                        EagerTypeDeclarationInfo {
                            parameters: parameters.len() as u32,
                        },
                        None,
                    ),
                ),
                crate::UnresolvedStatement::Trait {
                    name, parameters, ..
                } => insert_declaration!(
                    trait_declarations,
                    name,
                    info.make_path(crate::PathComponent::Trait(name.item.clone())),
                    (
                        EagerTraitDeclarationInfo {
                            parameters: parameters.len() as u32,
                        },
                        None,
                    ),
                ),
                crate::UnresolvedStatement::Constant { name, .. } => insert_declaration!(
                    constant_declarations,
                    name,
                    info.make_path(crate::PathComponent::Constant(name.item.clone())),
                    None,
                ),
                crate::UnresolvedStatement::Instance { .. }
                | crate::UnresolvedStatement::Assignment { .. }
                | crate::UnresolvedStatement::Expression(_) => Some(statement),
            }
        })
        .partition(|statement| {
            matches!(
                statement.item,
                crate::UnresolvedStatement::Syntax { .. }
                    | crate::UnresolvedStatement::Type { .. }
                    | crate::UnresolvedStatement::Trait { .. }
            )
        });

    let (type_statements, language_statements): (Vec<_>, Vec<_>) =
        declaration_statements.into_iter().partition(|statement| {
            matches!(
                statement.item,
                crate::UnresolvedStatement::Syntax { .. }
                    | crate::UnresolvedStatement::Type { .. }
                    | crate::UnresolvedStatement::Trait { .. }
            )
        });

    (
        vec![type_statements, language_statements],
        executable_statements,
    )
}

pub fn resolve_statements<D: Driver>(
    statements: Vec<WithInfo<D::Info, crate::UnresolvedStatement<D>>>,
    info: &mut Info<D>,
) -> Vec<WithInfo<D::Info, crate::Expression<D>>> {
    let mut queued_constants = Vec::new();

    let statements = statements
        .into_iter()
        .filter_map(|statement| match statement.item {
            crate::UnresolvedStatement::Syntax { attributes, name } => {
                info.path.push(crate::PathComponent::Syntax(name.item));

                resolve_attributes(&attributes, info);

                let declaration = info.syntax_declarations.get_mut(&info.path).unwrap();

                declaration.item = Some(crate::SyntaxDeclaration { attributes });

                info.path.pop().unwrap();

                None
            }
            crate::UnresolvedStatement::Type {
                attributes,
                name,
                parameters,
                representation,
            } => {
                info.path
                    .push(crate::PathComponent::Type(name.item.clone()));

                info.scopes.push_constant_scope();

                let parameters = parameters
                    .into_iter()
                    .filter_map(|type_parameter| resolve_type_parameter(type_parameter, info))
                    .collect::<Vec<_>>();

                let mut constructors = Vec::new();
                let representation = representation.map(|representation| match representation {
                    crate::UnresolvedTypeRepresentation::Marker => {
                        constructors.push(generate_marker_constructor(
                            attributes.clone(),
                            name.clone(),
                            parameters.clone(),
                            info,
                        ));

                        crate::TypeRepresentation::Marker
                    }
                    crate::UnresolvedTypeRepresentation::Structure(fields) => {
                        constructors.push(generate_structure_constructor(
                            attributes.clone(),
                            name,
                            parameters.clone(),
                            info,
                        ));

                        crate::TypeRepresentation::Structure(
                            fields
                                .into_iter()
                                .map(|field| {
                                    field.map(|field| crate::Field {
                                        index: field.index,
                                        attributes: field.attributes,
                                        name: field.name,
                                        r#type: resolve_type(field.r#type, info),
                                    })
                                })
                                .collect(),
                        )
                    }
                    crate::UnresolvedTypeRepresentation::Enumeration(variants) => {
                        crate::TypeRepresentation::Enumeration(
                            variants
                                .into_iter()
                                .map(|variant| {
                                    variant.map(|variant| {
                                        let variant_path = variant.name.clone().map(|name| {
                                            info.make_path(crate::PathComponent::Variant(name))
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

                                        crate::Variant {
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
                    crate::UnresolvedTypeRepresentation::Wrapper(r#type) => {
                        let value_type = resolve_type(r#type, info);

                        constructors.push(generate_wrapper_constructor(
                            attributes.clone(),
                            name.clone(),
                            value_type.clone(),
                            parameters.clone(),
                            info,
                        ));

                        crate::TypeRepresentation::Wrapper(value_type)
                    }
                });

                resolve_attributes(&attributes, info);

                info.scopes.pop_scope();

                let declaration = info.type_declarations.get_mut(&info.path).unwrap();

                declaration.item.1 = Some(crate::TypeDeclaration {
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
            crate::UnresolvedStatement::Trait {
                attributes,
                name,
                parameters,
                r#type,
            } => {
                info.path
                    .push(crate::PathComponent::Trait(name.item.clone()));

                info.scopes.push_constant_scope();

                let parameters = parameters
                    .into_iter()
                    .filter_map(|type_parameter| resolve_type_parameter(type_parameter, info))
                    .collect::<Vec<_>>();

                let r#type = r#type.map(|r#type| resolve_type(r#type, info));

                resolve_attributes(&attributes, info);

                info.scopes.pop_scope();

                let declaration = info.trait_declarations.get_mut(&info.path).unwrap();

                declaration.item.1 = Some(crate::TraitDeclaration {
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
            crate::UnresolvedStatement::Constant {
                attributes,
                name,
                parameters,
                bounds,
                r#type,
                body,
            } => {
                info.path.push(crate::PathComponent::Constant(name.item));

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

                declaration.item = Some(crate::ConstantDeclaration {
                    attributes,
                    parameters,
                    bounds,
                    r#type,
                });

                info.path.pop().unwrap();

                None
            }
            crate::UnresolvedStatement::Instance {
                pattern,
                parameters,
                bounds,
                instance,
                body,
                default,
            } => {
                info.path.push(crate::PathComponent::Instance(
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
                    pattern.replace(Some(crate::InstanceDeclaration {
                        parameters,
                        bounds,
                        instance,
                        default,
                    })),
                );

                info.library.items.insert(
                    info.path.clone(),
                    body.map(|body| crate::Item {
                        body,
                        captures: Vec::from_iter(captures.used),
                        top_level: info.top_level_path(),
                    }),
                );

                info.path.pop().unwrap();

                None
            }
            crate::UnresolvedStatement::Assignment { pattern, value } => {
                let value = resolve_expression(value, info);

                match pattern.item {
                    crate::UnresolvedPattern::Mutate(name) => {
                        let name = match name.clone().try_unwrap() {
                            Some(name) => name,
                            None => {
                                return Some(WithInfo {
                                    info: name.info.clone(),
                                    item: crate::Expression::Assign {
                                        pattern: name.replace(crate::Pattern::Error),
                                        value: value.boxed(),
                                    },
                                });
                            }
                        };

                        match resolve_name(name.clone(), info, |candidates| {
                            candidates
                                .iter()
                                .filter_map(|path| match path.item.last().unwrap() {
                                    crate::PathComponent::Variable(_) => {
                                        Some((path.item.clone(), path.clone()))
                                    }
                                    _ => None,
                                })
                                .collect::<Vec<_>>()
                        }) {
                            Some(path) => Some(WithInfo {
                                info: name.info.clone(),
                                item: crate::Expression::Mutate {
                                    name,
                                    path,
                                    value: value.boxed(),
                                },
                            }),
                            None => Some(WithInfo {
                                info: name.info.clone(),
                                item: crate::Expression::Assign {
                                    pattern: name.replace(crate::Pattern::Error),
                                    value: value.boxed(),
                                },
                            }),
                        }
                    }
                    _ => {
                        let pattern = resolve_pattern(pattern, info);

                        Some(WithInfo {
                            info: pattern.info.clone(),
                            item: crate::Expression::Assign {
                                pattern,
                                value: value.boxed(),
                            },
                        })
                    }
                }
            }
            crate::UnresolvedStatement::Expression(expression) => {
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
            Some(crate::Item {
                body,
                captures: Vec::from_iter(captures.used),
                top_level: info.top_level_path(),
            }),
        );
    }

    statements
}
