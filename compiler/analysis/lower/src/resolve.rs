use crate::Driver;
use derivative::Derivative;
use std::{collections::HashMap, mem};
use wipple_util::WithInfo;

/// Resolve a list of files into an interface and a library.
pub fn resolve<D: Driver>(
    files: impl IntoIterator<Item = WithInfo<D::Info, crate::UnresolvedFile<D>>>,
    dependencies: crate::Interface<D>,
) -> crate::Result<D> {
    let mut info = Info::default();
    info.scopes.push_block_scope();

    macro_rules! add {
        ($declarations:ident) => {
            for (path, declaration) in dependencies.$declarations {
                info.$declarations
                    .insert(path.clone(), declaration.map(Some));
            }
        };
        ($($declarations:ident),* $(,)?) => {
            $(add!($declarations);)*
        }
    }

    add!(
        type_declarations,
        trait_declarations,
        constant_declarations,
        type_parameter_declarations,
        instance_declarations,
    );

    info.language_declarations.extend(
        dependencies
            .language_declarations
            .into_iter()
            .map(|(item, path)| (item, Some(path))),
    );

    for (name, paths) in dependencies.top_level {
        for path in paths {
            info.scopes.define(name.clone(), path);
        }
    }

    let statements = files
        .into_iter()
        .flat_map(|file| file.item.statements)
        .collect::<Vec<_>>();

    let statements = resolve_statements(statements, &mut info);
    info.library.code.extend(statements);

    let mut interface = crate::Interface::default();

    macro_rules! unwrap {
        ($declarations:ident) => {
            for (name, declaration) in info.$declarations {
                interface.$declarations.insert(name, declaration.map(Option::unwrap));
            }
        };
        ($($declarations:ident),* $(,)?) => {
            $(unwrap!($declarations);)*
        }
    }

    unwrap!(
        type_declarations,
        trait_declarations,
        constant_declarations,
        type_parameter_declarations,
        instance_declarations,
    );

    interface.language_declarations.extend(
        info.language_declarations
            .into_iter()
            .filter_map(|(name, path)| Some((name, path?))),
    );

    assert!(info.scopes.0.len() == 1);
    for (name, paths) in info.scopes.pop_scope().1 {
        for path in paths {
            interface
                .top_level
                .entry(name.clone())
                .or_default()
                .push(path);
        }
    }

    crate::Result {
        interface,
        library: info.library,
        errors: info.errors,
    }
}

#[derive(Debug, Derivative)]
#[derivative(Default(bound = ""))]
struct Info<D: Driver> {
    errors: Vec<WithInfo<D::Info, crate::Error>>,
    dependencies: crate::Interface<D>,
    type_declarations: HashMap<crate::Path, WithInfo<D::Info, Option<crate::TypeDeclaration<D>>>>,
    trait_declarations: HashMap<crate::Path, WithInfo<D::Info, Option<crate::TraitDeclaration<D>>>>,
    constant_declarations:
        HashMap<crate::Path, WithInfo<D::Info, Option<crate::ConstantDeclaration<D>>>>,
    type_parameter_declarations:
        HashMap<crate::Path, WithInfo<D::Info, Option<crate::TypeParameterDeclaration<D>>>>,
    language_declarations: HashMap<String, Option<crate::Path>>,
    instance_declarations:
        HashMap<crate::Path, WithInfo<D::Info, Option<crate::InstanceDeclaration<D>>>>,
    library: crate::Library<D>,
    path: crate::Path,
    scopes: Scopes<D>,
    next_variable: u32,
}

impl<D: Driver> Info<D> {
    fn reset_next_variable(&mut self) -> u32 {
        mem::take(&mut self.next_variable)
    }

    fn make_path(&self, component: crate::PathComponent) -> crate::Path {
        let mut path = self.path.clone();
        path.push(component);
        path
    }
}

pub type Scope<D> = (
    ScopeKind,
    HashMap<String, Vec<WithInfo<<D as Driver>::Info, crate::Path>>>,
);

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""), Default(bound = ""))]
pub struct Scopes<D: Driver>(Vec<Scope<D>>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    Block,
    Constant,
}

impl<D: Driver> Scopes<D> {
    pub fn push_block_scope(&mut self) {
        self.0.push((ScopeKind::Block, HashMap::new()));
    }

    pub fn push_constant_scope(&mut self) {
        self.0.push((ScopeKind::Constant, HashMap::new()));
    }

    pub fn pop_scope(&mut self) -> Scope<D> {
        self.0.pop().unwrap()
    }

    pub fn define(&mut self, name: String, path: WithInfo<D::Info, crate::Path>) {
        let (_, paths) = self.0.last_mut().unwrap();
        let paths = paths.entry(name).or_default();

        if path.item.last().unwrap().is_local() {
            paths.retain(|path| !path.item.last().unwrap().is_local());
        }

        paths.push(path);
    }
}

fn resolve_statements<D: Driver>(
    statements: Vec<WithInfo<D::Info, crate::UnresolvedStatement<D>>>,
    info: &mut Info<D>,
) -> Vec<WithInfo<D::Info, crate::Expression<D>>> {
    let mut statements = statements
        .into_iter()
        .filter_map(|statement| {
            macro_rules! insert_declaration {
                ($declarations:ident, $name:expr, $path:expr $(,)?) => {{
                    use std::collections::hash_map::Entry;

                    let path = $path;

                    match info.$declarations.entry(path.clone()) {
                        Entry::Occupied(entry) => {
                            info.errors.push(
                                statement
                                    .replace(crate::Error::AlreadyDefined(entry.key().clone())),
                            );

                            None
                        }
                        Entry::Vacant(entry) => {
                            entry.insert(statement.replace(None));
                            info.scopes.define($name.clone(), statement.replace(path));
                            Some(statement)
                        }
                    }
                }};
            }

            match &statement.item {
                crate::UnresolvedStatement::Type { name, .. } => insert_declaration!(
                    type_declarations,
                    &name.item,
                    info.make_path(crate::PathComponent::Type(name.item.clone())),
                ),
                crate::UnresolvedStatement::Trait { name, .. } => insert_declaration!(
                    trait_declarations,
                    &name.item,
                    info.make_path(crate::PathComponent::Trait(name.item.clone())),
                ),
                crate::UnresolvedStatement::Constant { name, .. } => insert_declaration!(
                    constant_declarations,
                    &name.item,
                    info.make_path(crate::PathComponent::Constant(name.item.clone())),
                ),
                crate::UnresolvedStatement::Language { name, .. } => {
                    if info.path.len() > 1 {
                        info.errors.push(WithInfo {
                            info: statement.info,
                            item: crate::Error::NestedLanguageDeclaration,
                        });

                        return None;
                    }

                    info.language_declarations.insert(name.item.clone(), None);

                    Some(statement)
                }
                crate::UnresolvedStatement::Instance { .. }
                | crate::UnresolvedStatement::Assignment { .. }
                | crate::UnresolvedStatement::Expression(_) => Some(statement),
            }
        })
        .collect::<Vec<_>>();

    // Ensure that executable code is resolved after type and trait definitions,
    // so that references to types and traits can use the resolved definition
    statements.sort_by_key(|statement| match statement.item {
        crate::UnresolvedStatement::Type { .. } | crate::UnresolvedStatement::Trait { .. } => 0,
        crate::UnresolvedStatement::Constant { .. } => 1,
        crate::UnresolvedStatement::Language { .. } => 2,
        crate::UnresolvedStatement::Instance { .. }
        | crate::UnresolvedStatement::Assignment { .. }
        | crate::UnresolvedStatement::Expression(_) => 3,
    });

    let mut queued_constants = Vec::new();
    let statements = statements
        .into_iter()
        .filter_map(|statement| match statement.item {
            crate::UnresolvedStatement::Type {
                name,
                parameters,
                representation,
            } => {
                info.path
                    .push(crate::PathComponent::Type(name.item.clone()));

                info.scopes.push_constant_scope();

                let parameters = parameters
                    .into_iter()
                    .map(|type_parameter| resolve_type_parameter(type_parameter, info))
                    .collect::<Vec<_>>();

                let mut constructors = Vec::new();
                let representation = representation.map(|representation| match representation {
                    crate::UnresolvedTypeRepresentation::Marker => {
                        constructors.push(generate_marker_constructor(
                            name,
                            parameters.clone(),
                            info,
                        ));

                        crate::TypeRepresentation::Marker
                    }
                    crate::UnresolvedTypeRepresentation::Structure(fields) => {
                        constructors.push(generate_structure_constructor(
                            name,
                            parameters.clone(),
                            info,
                        ));

                        crate::TypeRepresentation::Structure(
                            fields
                                .into_iter()
                                .map(|field| {
                                    field.map(|field| crate::Field {
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
                                            variant.name.clone(),
                                            parameters.clone(),
                                            variant_path.clone(),
                                            value_types.clone(),
                                            info,
                                        ));

                                        constructors
                                            .push((variant.name.item, variant_path.clone()));

                                        crate::Variant {
                                            name: variant_path,
                                            types: value_types,
                                        }
                                    })
                                })
                                .collect(),
                        )
                    }
                });

                info.scopes.pop_scope();

                let declaration = info.type_declarations.get_mut(&info.path).unwrap();

                declaration.item = Some(crate::TypeDeclaration {
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
                name,
                parameters,
                r#type,
            } => {
                info.path
                    .push(crate::PathComponent::Trait(name.item.clone()));

                info.scopes.push_constant_scope();

                let parameters = parameters
                    .into_iter()
                    .map(|type_parameter| resolve_type_parameter(type_parameter, info))
                    .collect::<Vec<_>>();

                let r#type = resolve_type(r#type, info);

                info.scopes.pop_scope();

                let declaration = info.trait_declarations.get_mut(&info.path).unwrap();

                declaration.item = Some(crate::TraitDeclaration {
                    parameters: parameters.clone(),
                    r#type: r#type.clone(),
                });

                let (name, constructor) =
                    generate_trait_constructor(name, parameters, r#type, info);

                info.scopes.define(name, constructor);

                info.path.pop().unwrap();

                None
            }
            crate::UnresolvedStatement::Constant {
                name,
                parameters,
                bounds,
                r#type,
                body,
            } => {
                info.path.push(crate::PathComponent::Constant(name.item));

                info.scopes.push_constant_scope();
                let prev_next_variable = info.reset_next_variable();

                let parameters = parameters
                    .into_iter()
                    .map(|type_parameter| resolve_type_parameter(type_parameter, info))
                    .collect::<Vec<_>>();

                let bounds = bounds
                    .into_iter()
                    .filter_map(|bound| resolve_instance(bound, info))
                    .collect::<Vec<_>>();

                let r#type = resolve_type(r#type, info);

                queued_constants.push((info.path.clone(), body));

                info.next_variable = prev_next_variable;
                info.scopes.pop_scope();

                let declaration = info.constant_declarations.get_mut(&info.path).unwrap();

                declaration.item = Some(crate::ConstantDeclaration {
                    parameters,
                    bounds,
                    r#type,
                });

                info.path.pop().unwrap();

                None
            }
            crate::UnresolvedStatement::Instance {
                parameters,
                bounds,
                instance,
                body,
            } => {
                info.path.push(crate::PathComponent::Instance(
                    info.instance_declarations.len() as u32,
                ));

                info.scopes.push_constant_scope();
                let prev_next_variable = info.reset_next_variable();

                let parameters = parameters
                    .into_iter()
                    .map(|type_parameter| resolve_type_parameter(type_parameter, info))
                    .collect::<Vec<_>>();

                let bounds = bounds
                    .into_iter()
                    .filter_map(|bound| resolve_instance(bound, info))
                    .collect::<Vec<_>>();

                let instance = match resolve_instance(instance, info) {
                    Some(instance) => instance,
                    None => {
                        info.scopes.pop_scope();

                        return None;
                    }
                };

                let body = resolve_expression(body, info);

                info.next_variable = prev_next_variable;
                info.scopes.pop_scope();

                info.instance_declarations.insert(
                    info.path.clone(),
                    WithInfo {
                        info: statement.info,
                        item: Some(crate::InstanceDeclaration {
                            parameters,
                            bounds,
                            instance,
                        }),
                    },
                );

                info.library.items.insert(info.path.clone(), body);

                info.path.pop().unwrap();

                None
            }
            crate::UnresolvedStatement::Language { name, item } => {
                let path = resolve_name(item, info, |candidates| {
                    let mut candidates = candidates
                        .iter()
                        .filter(|path| !path.item.last().unwrap().is_local())
                        .cloned()
                        .collect::<Vec<_>>();

                    candidates.sort_by_key(|path| match path.item.last().unwrap() {
                        crate::PathComponent::Type(_) | crate::PathComponent::Trait(_) => 0,
                        crate::PathComponent::Constant(_)
                        | crate::PathComponent::Constructor(_)
                        | crate::PathComponent::Variant(_) => 1,
                        crate::PathComponent::Instance(_)
                        | crate::PathComponent::Language(_)
                        | crate::PathComponent::TypeParameter(_)
                        | crate::PathComponent::Variable(_) => 2,
                    });

                    vec![candidates.into_iter().next().unwrap()]
                })?;

                let declaration = info.language_declarations.get_mut(&name.item).unwrap();
                *declaration = Some(path.item);

                None
            }
            crate::UnresolvedStatement::Assignment { pattern, value } => {
                let value = resolve_expression(value, info);
                let pattern = resolve_pattern(pattern, info);

                Some(WithInfo {
                    info: statement.info,
                    item: crate::Expression::Assign {
                        pattern,
                        value: value.boxed(),
                    },
                })
            }
            crate::UnresolvedStatement::Expression(expression) => {
                Some(resolve_expression(expression, info))
            }
        })
        .collect();

    for (path, body) in queued_constants {
        let prev_path = mem::replace(&mut info.path, path);
        let body = resolve_expression(body, info);
        let path = mem::replace(&mut info.path, prev_path);
        info.library.items.insert(path, body);
    }

    statements
}

fn generate_marker_constructor<D: Driver>(
    name: WithInfo<D::Info, String>,
    parameters: Vec<crate::Path>,
    info: &mut Info<D>,
) -> (String, WithInfo<D::Info, crate::Path>) {
    let constructor_path = info.make_path(crate::PathComponent::Constructor(name.item.clone()));

    let constructor_declaration = crate::ConstantDeclaration {
        parameters: parameters.clone(),
        bounds: Vec::new(),
        r#type: WithInfo {
            info: name.info.clone(),
            item: crate::Type::Declared {
                path: WithInfo {
                    info: name.info.clone(),
                    item: info.path.clone(),
                },
                parameters: parameters
                    .into_iter()
                    .map(|parameter| WithInfo {
                        info: name.info.clone(),
                        item: crate::Type::Parameter(parameter),
                    })
                    .collect(),
            },
        },
    };

    let constructor_body = WithInfo {
        info: name.info.clone(),
        item: crate::Expression::Marker(info.path.clone()),
    };

    info.constant_declarations.insert(
        constructor_path.clone(),
        WithInfo {
            info: name.info.clone(),
            item: Some(constructor_declaration),
        },
    );

    info.library
        .items
        .insert(constructor_path.clone(), constructor_body);

    (
        name.item,
        WithInfo {
            info: name.info,
            item: constructor_path,
        },
    )
}

fn generate_structure_constructor<D: Driver>(
    name: WithInfo<D::Info, String>,
    parameters: Vec<crate::Path>,
    info: &mut Info<D>,
) -> (String, WithInfo<D::Info, crate::Path>) {
    let constructor_path = info.make_path(crate::PathComponent::Constructor(name.item.clone()));

    let constructor_declaration = crate::ConstantDeclaration {
        parameters: parameters.clone(),
        bounds: Vec::new(),
        r#type: WithInfo {
            info: name.info.clone(),
            item: crate::Type::Function {
                input: WithInfo {
                    info: name.info.clone(),
                    item: crate::Type::Declared {
                        path: WithInfo {
                            info: name.info.clone(),
                            item: info.path.clone(),
                        },
                        parameters: parameters
                            .clone()
                            .into_iter()
                            .map(|parameter| WithInfo {
                                info: name.info.clone(),
                                item: crate::Type::Parameter(parameter),
                            })
                            .collect(),
                    },
                }
                .boxed(),
                output: WithInfo {
                    info: name.info.clone(),
                    item: crate::Type::Declared {
                        path: WithInfo {
                            info: name.info.clone(),
                            item: info.path.clone(),
                        },
                        parameters: parameters
                            .into_iter()
                            .map(|parameter| WithInfo {
                                info: name.info.clone(),
                                item: crate::Type::Parameter(parameter),
                            })
                            .collect(),
                    },
                }
                .boxed(),
            },
        },
    };

    let constructor_body = {
        let input_variable = info.next_variable;
        info.next_variable += 1;
        let input_variable = info.make_path(crate::PathComponent::Variable(input_variable));

        WithInfo {
            info: name.info.clone(),
            item: crate::Expression::Function {
                pattern: WithInfo {
                    info: name.info.clone(),
                    item: crate::Pattern::Variable(name.item.clone(), input_variable.clone()),
                },
                body: WithInfo {
                    info: name.info.clone(),
                    item: crate::Expression::Variable(name.item.clone(), input_variable),
                }
                .boxed(),
            },
        }
    };

    info.constant_declarations.insert(
        constructor_path.clone(),
        WithInfo {
            info: name.info.clone(),
            item: Some(constructor_declaration),
        },
    );

    info.library
        .items
        .insert(constructor_path.clone(), constructor_body);

    (
        name.item,
        WithInfo {
            info: name.info,
            item: constructor_path,
        },
    )
}

fn generate_variant_constructor<D: Driver>(
    name: WithInfo<D::Info, String>,
    parameters: Vec<crate::Path>,
    variant_path: WithInfo<D::Info, crate::Path>,
    value_types: Vec<WithInfo<D::Info, crate::Type<D>>>,
    info: &mut Info<D>,
) -> (String, WithInfo<D::Info, crate::Path>) {
    let constructor_path = info.make_path(crate::PathComponent::Constructor(name.item.clone()));

    let constructor_declaration = crate::ConstantDeclaration {
        parameters: parameters.clone(),
        bounds: Vec::new(),
        r#type: value_types.clone().into_iter().rfold(
            WithInfo {
                info: name.info.clone(),
                item: crate::Type::Declared {
                    path: WithInfo {
                        info: name.info.clone(),
                        item: info.path.clone(),
                    },
                    parameters: parameters
                        .into_iter()
                        .map(|parameter| WithInfo {
                            info: name.info.clone(),
                            item: crate::Type::Parameter(parameter),
                        })
                        .collect(),
                },
            },
            |result, next| WithInfo {
                info: name.info.clone(),
                item: crate::Type::Function {
                    input: next.boxed(),
                    output: result.boxed(),
                },
            },
        ),
    };

    let constructor_body = {
        let input_variables = value_types
            .iter()
            .map(|_| {
                let input_variable = info.next_variable;
                info.next_variable += 1;
                info.make_path(crate::PathComponent::Variable(input_variable))
            })
            .collect::<Vec<_>>();

        input_variables.clone().into_iter().rfold(
            WithInfo {
                info: name.info.clone(),
                item: crate::Expression::Variant {
                    variant: variant_path,
                    values: input_variables
                        .into_iter()
                        .map(|input_variable| WithInfo {
                            info: name.info.clone(),
                            item: crate::Expression::Variable(name.item.clone(), input_variable),
                        })
                        .collect(),
                },
            },
            |result, next| WithInfo {
                info: name.info.clone(),
                item: crate::Expression::Function {
                    pattern: WithInfo {
                        info: name.info.clone(),
                        item: crate::Pattern::Variable(name.item.clone(), next),
                    },
                    body: result.boxed(),
                },
            },
        )
    };

    info.constant_declarations.insert(
        constructor_path.clone(),
        WithInfo {
            info: name.info.clone(),
            item: Some(constructor_declaration),
        },
    );

    info.library
        .items
        .insert(constructor_path.clone(), constructor_body);

    (
        name.item,
        WithInfo {
            info: name.info,
            item: constructor_path,
        },
    )
}

fn generate_trait_constructor<D: Driver>(
    name: WithInfo<D::Info, String>,
    parameters: Vec<crate::Path>,
    r#type: WithInfo<D::Info, crate::Type<D>>,
    info: &mut Info<D>,
) -> (String, WithInfo<D::Info, crate::Path>) {
    let constructor_path = info.make_path(crate::PathComponent::Constructor(name.item.clone()));

    let constructor_declaration = crate::ConstantDeclaration {
        parameters: parameters.clone(),
        bounds: vec![name.replace(crate::Instance {
            r#trait: name.replace(info.path.clone()),
            parameters: parameters
                .into_iter()
                .map(|parameter| WithInfo {
                    info: name.info.clone(),
                    item: crate::Type::Parameter(parameter),
                })
                .collect(),
        })],
        r#type,
    };

    let constructor_body = name.replace(crate::Expression::Trait(info.path.clone()));

    info.constant_declarations.insert(
        constructor_path.clone(),
        WithInfo {
            info: name.info.clone(),
            item: Some(constructor_declaration),
        },
    );

    info.library
        .items
        .insert(constructor_path.clone(), constructor_body);

    (
        name.item,
        WithInfo {
            info: name.info,
            item: constructor_path,
        },
    )
}

fn resolve_expression<D: Driver>(
    expression: WithInfo<D::Info, crate::UnresolvedExpression<D>>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::Expression<D>> {
    let expression_info = expression.info.clone();

    expression.map(|expression| match expression {
        crate::UnresolvedExpression::Error => crate::Expression::Error,
        crate::UnresolvedExpression::Annotate { value, r#type } => crate::Expression::Annotate {
            value: resolve_expression(value.unboxed(), info).boxed(),
            r#type: resolve_type(r#type, info),
        },
        crate::UnresolvedExpression::Name(name) => resolve_name(
            WithInfo {
                info: expression_info,
                item: name.clone(),
            },
            info,
            |candidates| {
                let mut candidates = candidates.to_vec();
                candidates.sort_by_key(|path| match path.item.last().unwrap() {
                    crate::PathComponent::Variable(_) => 0,
                    crate::PathComponent::Trait(_) => 1,
                    crate::PathComponent::Constant(_) | crate::PathComponent::Constructor(_) => 2,
                    _ => 3,
                });

                let path = match candidates.first() {
                    Some(candidate) => candidate,
                    None => return Vec::new(),
                };

                match path.item.last().unwrap() {
                    crate::PathComponent::Variable(_) => {
                        vec![crate::Expression::Variable(name.clone(), path.item.clone())]
                    }
                    crate::PathComponent::Trait(_) => {
                        vec![crate::Expression::Trait(path.item.clone())]
                    }
                    crate::PathComponent::Constant(_) | crate::PathComponent::Constructor(_) => {
                        vec![crate::Expression::Constant(path.item.clone())]
                    }
                    _ => Vec::new(),
                }
            },
        )
        .unwrap_or(crate::Expression::Error),
        crate::UnresolvedExpression::Number(number) => crate::Expression::Number(number),
        crate::UnresolvedExpression::Text(text) => crate::Expression::Text(text),
        crate::UnresolvedExpression::Format { segments, trailing } => crate::Expression::Format {
            segments: segments
                .into_iter()
                .map(|segment| crate::FormatSegment {
                    text: segment.text,
                    value: resolve_expression(segment.value, info),
                })
                .collect(),
            trailing,
        },
        crate::UnresolvedExpression::Block(statements) => {
            info.scopes.push_block_scope();
            let block = resolve_statements(statements, info);
            info.scopes.pop_scope();

            crate::Expression::Block(block)
        }
        crate::UnresolvedExpression::Function { pattern, body } => {
            info.scopes.push_block_scope();
            let pattern = resolve_pattern(pattern, info);
            let body = resolve_expression(body.unboxed(), info);
            info.scopes.pop_scope();

            crate::Expression::Function {
                pattern,
                body: body.boxed(),
            }
        }
        crate::UnresolvedExpression::Call { function, input } => crate::Expression::Call {
            function: resolve_expression(function.unboxed(), info).boxed(),
            input: resolve_expression(input.unboxed(), info).boxed(),
        },
        crate::UnresolvedExpression::Apply { input, function } => match (input, function) {
            (Some(input), Some(function)) => crate::Expression::Call {
                function: resolve_expression(function.unboxed(), info).boxed(),
                input: resolve_expression(input.unboxed(), info).boxed(),
            },
            (Some(input), None) => {
                let input_variable = info.next_variable;
                info.next_variable += 1;
                let input_variable = info.make_path(crate::PathComponent::Variable(input_variable));

                crate::Expression::Function {
                    pattern: input.replace(crate::Pattern::Variable(
                        String::from("function"),
                        input_variable.clone(),
                    )),
                    body: WithInfo {
                        info: input.info.clone(),
                        item: crate::Expression::Call {
                            function: input
                                .replace(crate::Expression::Variable(
                                    String::from("function"),
                                    input_variable,
                                ))
                                .boxed(),
                            input: resolve_expression(input.unboxed(), info).boxed(),
                        },
                    }
                    .boxed(),
                }
            }
            (None, Some(function)) => resolve_expression(function.unboxed(), info).item,
            (None, None) => {
                let function_variable = info.next_variable;
                info.next_variable += 1;
                let function_variable =
                    info.make_path(crate::PathComponent::Variable(function_variable));

                let input_variable = info.next_variable;
                info.next_variable += 1;
                let input_variable = info.make_path(crate::PathComponent::Variable(input_variable));

                crate::Expression::Function {
                    pattern: WithInfo {
                        info: expression_info.clone(),
                        item: crate::Pattern::Variable(
                            String::from("function"),
                            function_variable.clone(),
                        ),
                    },
                    body: WithInfo {
                        info: expression_info.clone(),
                        item: crate::Expression::Function {
                            pattern: WithInfo {
                                info: expression_info.clone(),
                                item: crate::Pattern::Variable(
                                    String::from("input"),
                                    input_variable.clone(),
                                ),
                            },
                            body: WithInfo {
                                info: expression_info.clone(),
                                item: crate::Expression::Call {
                                    function: WithInfo {
                                        info: expression_info.clone(),
                                        item: crate::Expression::Variable(
                                            String::from("function"),
                                            function_variable,
                                        ),
                                    }
                                    .boxed(),
                                    input: WithInfo {
                                        info: expression_info,
                                        item: crate::Expression::Variable(
                                            String::from("input"),
                                            input_variable,
                                        ),
                                    }
                                    .boxed(),
                                },
                            }
                            .boxed(),
                        },
                    }
                    .boxed(),
                }
            }
        },
        crate::UnresolvedExpression::Compose { outer, inner } => match (outer, inner) {
            (Some(outer), Some(inner)) => {
                let outer = resolve_expression(outer.unboxed(), info);
                let inner = resolve_expression(inner.unboxed(), info);

                let input_variable = info.next_variable;
                info.next_variable += 1;
                let input_variable = info.make_path(crate::PathComponent::Variable(input_variable));

                crate::Expression::Function {
                    pattern: WithInfo {
                        info: expression_info.clone(),
                        item: crate::Pattern::Variable(
                            String::from("input"),
                            input_variable.clone(),
                        ),
                    },
                    body: WithInfo {
                        info: expression_info.clone(),
                        item: crate::Expression::Call {
                            function: outer.boxed(),
                            input: WithInfo {
                                info: expression_info.clone(),
                                item: crate::Expression::Call {
                                    function: inner.boxed(),
                                    input: WithInfo {
                                        info: expression_info,
                                        item: crate::Expression::Variable(
                                            String::from("input"),
                                            input_variable,
                                        ),
                                    }
                                    .boxed(),
                                },
                            }
                            .boxed(),
                        },
                    }
                    .boxed(),
                }
            }
            _ => {
                info.errors.push(WithInfo {
                    info: expression_info,
                    item: crate::Error::InvalidComposition,
                });

                crate::Expression::Error
            }
        },
        crate::UnresolvedExpression::BinaryOperator {
            operator,
            left,
            right,
        } => resolve_binary_operator(
            operator,
            left.map(WithInfo::unboxed),
            right.map(WithInfo::unboxed),
            info,
        ),
        crate::UnresolvedExpression::As { value, r#type } => {
            let operator = WithInfo {
                info: expression_info,
                item: String::from("as"),
            };

            let operator_trait =
                operator.replace(match resolve_language_item(operator.clone(), info) {
                    Some(path) => crate::Expression::Trait(path),
                    None => crate::Expression::Error,
                });

            let operator_trait = crate::Expression::Annotate {
                value: operator_trait.boxed(),
                r#type: resolve_type(r#type, info),
            };

            match value {
                Some(value) => crate::Expression::Call {
                    function: operator.replace(operator_trait).boxed(),
                    input: resolve_expression(value.unboxed(), info).boxed(),
                },
                None => operator_trait,
            }
        }
        crate::UnresolvedExpression::Is { value, pattern } => {
            let r#true = WithInfo {
                info: expression_info.clone(),
                item: String::from("true"),
            };

            let true_value = r#true.replace(match resolve_language_item(r#true.clone(), info) {
                Some(path) => crate::Expression::Constant(path),
                None => crate::Expression::Error,
            });

            let r#false = WithInfo {
                info: expression_info.clone(),
                item: String::from("false"),
            };

            let false_value = r#false.replace(match resolve_language_item(r#false.clone(), info) {
                Some(path) => crate::Expression::Constant(path),
                None => crate::Expression::Error,
            });

            let when_expression = |value: WithInfo<D::Info, crate::Expression<D>>,
                                   info: &mut Info<D>| {
                let value_info = value.info.clone();

                crate::Expression::When {
                    input: value.boxed(),
                    arms: vec![
                        r#true.replace(crate::Arm {
                            pattern: resolve_pattern(pattern, info),
                            condition: None,
                            body: true_value,
                        }),
                        r#false.replace(crate::Arm {
                            pattern: WithInfo {
                                info: value_info,
                                item: crate::Pattern::Wildcard,
                            },
                            condition: None,
                            body: false_value,
                        }),
                    ],
                }
            };

            let input_variable = info.next_variable;
            info.next_variable += 1;
            let input_variable = info.make_path(crate::PathComponent::Variable(input_variable));

            match value {
                Some(value) => when_expression(resolve_expression(value.unboxed(), info), info),
                None => crate::Expression::Function {
                    pattern: WithInfo {
                        info: expression_info.clone(),
                        item: crate::Pattern::Variable(
                            String::from("input"),
                            input_variable.clone(),
                        ),
                    },
                    body: WithInfo {
                        info: expression_info.clone(),
                        item: when_expression(
                            WithInfo {
                                info: expression_info,
                                item: crate::Expression::Variable(
                                    String::from("input"),
                                    input_variable,
                                ),
                            },
                            info,
                        ),
                    }
                    .boxed(),
                },
            }
        }
        crate::UnresolvedExpression::When { input, arms } => {
            let input = resolve_expression(input.unboxed(), info);

            let arms = arms
                .into_iter()
                .map(|arm| {
                    arm.map(|arm| {
                        info.scopes.push_block_scope();

                        let pattern = resolve_pattern(arm.pattern, info);

                        let condition = arm
                            .condition
                            .map(|condition| resolve_expression(condition, info));

                        let body = resolve_expression(arm.body, info);

                        info.scopes.pop_scope();

                        crate::Arm {
                            pattern,
                            condition,
                            body,
                        }
                    })
                })
                .collect::<Vec<_>>();

            crate::Expression::When {
                input: input.boxed(),
                arms,
            }
        }
        crate::UnresolvedExpression::Intrinsic { name, inputs } => {
            let inputs = inputs
                .into_iter()
                .map(|input| resolve_expression(input, info))
                .collect::<Vec<_>>();
            crate::Expression::Intrinsic { name, inputs }
        }
        crate::UnresolvedExpression::Tuple(tuple) => {
            let tuple = tuple
                .into_iter()
                .map(|element| resolve_expression(element, info))
                .collect::<Vec<_>>();
            crate::Expression::Tuple(tuple)
        }
        crate::UnresolvedExpression::Collection(collection) => {
            let collection = collection
                .into_iter()
                .map(|element| resolve_expression(element, info))
                .collect::<Vec<_>>();
            crate::Expression::Collection(collection)
        }
        crate::UnresolvedExpression::Structure(structure) => {
            let structure = structure
                .into_iter()
                .map(|field_value| {
                    field_value.map(|field_value| crate::FieldValue {
                        name: field_value.name,
                        value: resolve_expression(field_value.value, info),
                    })
                })
                .collect::<Vec<_>>();
            crate::Expression::Structure(structure)
        }
        crate::UnresolvedExpression::Semantics { name, body } => crate::Expression::Semantics {
            name,
            body: resolve_expression(body.unboxed(), info).boxed(),
        },
    })
}

fn resolve_pattern<D: Driver>(
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

            crate::Pattern::Variable(name, path)
        }
        crate::UnresolvedPattern::VariantOrName(name) => {
            match try_resolve_name(
                WithInfo {
                    info: pattern_info.clone(),
                    item: name.clone(),
                },
                info,
                |candidates| {
                    candidates
                        .iter()
                        .filter_map(|path| match path.item.last().unwrap() {
                            crate::PathComponent::Variant(_) => Some(path.clone()),
                            _ => None,
                        })
                        .collect::<Vec<_>>()
                },
            ) {
                Some(variant) => crate::Pattern::Variant {
                    variant,
                    value_patterns: Vec::new(),
                },
                None => {
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
            }
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
            let variant = match resolve_name(variant, info, |candidates| {
                candidates
                    .iter()
                    .filter_map(|path| match path.item.last().unwrap() {
                        crate::PathComponent::Variant(_) => Some(path.clone()),
                        _ => None,
                    })
                    .collect::<Vec<_>>()
            }) {
                Some(variant) => variant,
                None => return crate::Pattern::Error,
            };

            let value_patterns = value_patterns
                .into_iter()
                .map(|pattern| resolve_pattern_inner(pattern, defines, info))
                .collect::<Vec<_>>();

            crate::Pattern::Variant {
                variant,
                value_patterns,
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
    })
}

fn resolve_type<D: Driver>(
    r#type: WithInfo<D::Info, crate::UnresolvedType<D>>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::Type<D>> {
    r#type.map(|r#type| match r#type {
        crate::UnresolvedType::Error => crate::Type::Error,
        crate::UnresolvedType::Placeholder => crate::Type::Placeholder,
        crate::UnresolvedType::Declared { name, parameters } => {
            match resolve_name(name, info, |candidates| {
                candidates
                    .iter()
                    .filter_map(|path| match path.item.last().unwrap() {
                        crate::PathComponent::Type(_) | crate::PathComponent::TypeParameter(_) => {
                            Some(path.clone())
                        }
                        _ => None,
                    })
                    .collect::<Vec<_>>()
            }) {
                Some(path) => match path.item.last().unwrap() {
                    crate::PathComponent::Type(_) => crate::Type::Declared {
                        path,
                        parameters: parameters
                            .into_iter()
                            .map(|parameter| resolve_type(parameter, info))
                            .collect(),
                    },
                    crate::PathComponent::TypeParameter(_) => crate::Type::Parameter(path.item), // FIXME: disallow parameters passed to type parameters
                    _ => unreachable!(),
                },
                None => crate::Type::Error,
            }
        }
        crate::UnresolvedType::Function { input, output } => crate::Type::Function {
            input: resolve_type(input.unboxed(), info).boxed(),
            output: resolve_type(output.unboxed(), info).boxed(),
        },
        crate::UnresolvedType::Tuple(elements) => crate::Type::Tuple(
            elements
                .into_iter()
                .map(|element| resolve_type(element, info))
                .collect(),
        ),
        crate::UnresolvedType::Lazy(r#type) => {
            crate::Type::Lazy(resolve_type(r#type.unboxed(), info).boxed())
        }
    })
}

fn resolve_type_parameter<D: Driver>(
    type_parameter: WithInfo<D::Info, crate::UnresolvedTypeParameter<D>>,
    info: &mut Info<D>,
) -> crate::Path {
    let type_parameter_info = type_parameter.info.clone();

    let mut path = info.path.clone();
    path.push(crate::PathComponent::TypeParameter(
        type_parameter.item.name.item.clone(),
    ));

    info.scopes.define(
        type_parameter.item.name.item,
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

    path
}

fn resolve_instance<D: Driver>(
    instance: WithInfo<D::Info, crate::UnresolvedInstance<D>>,
    info: &mut Info<D>,
) -> Option<WithInfo<D::Info, crate::Instance<D>>> {
    instance.filter_map(|instance| {
        let r#trait = match resolve_name(instance.r#trait, info, |candidates| {
            candidates
                .iter()
                .filter_map(|path| match path.item.last().unwrap() {
                    crate::PathComponent::Trait(_) => Some(path.clone()),
                    _ => None,
                })
                .collect::<Vec<_>>()
        }) {
            Some(r#trait) => r#trait,
            None => return None,
        };

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

fn resolve_name<D: Driver, T>(
    name: WithInfo<D::Info, String>,
    info: &mut Info<D>,
    filter: impl FnMut(&[WithInfo<D::Info, crate::Path>]) -> Vec<T>,
) -> Option<T> {
    let result = try_resolve_name(name.clone(), info, filter);

    if result.is_none() {
        info.errors.push(name.map(crate::Error::UnresolvedName));
    }

    result
}

fn try_resolve_name<D: Driver, T>(
    name: WithInfo<D::Info, String>,
    info: &mut Info<D>,
    mut filter: impl FnMut(&[WithInfo<D::Info, crate::Path>]) -> Vec<T>,
) -> Option<T> {
    let mut allow_locals = true;
    for (kind, scope) in info.scopes.0.iter().rev() {
        if let Some(paths) = scope.get(&name.item) {
            let paths = paths
                .iter()
                .filter(|path| allow_locals || !path.item.last().unwrap().is_local())
                .cloned()
                .collect::<Vec<_>>();

            let mut candidates = filter(&paths);

            match candidates.len() {
                0 => {
                    // Don't allow accessing locals outside a constant boundary
                    if matches!(kind, ScopeKind::Constant) {
                        allow_locals = false;
                    }

                    continue;
                }
                1 => return Some(candidates.pop().unwrap()),
                _ => {
                    info.errors.push(name.replace(crate::Error::AmbiguousName {
                        name: name.item.clone(),
                        candidates: paths.into_iter().map(|path| path.item).collect(),
                    }));

                    return Some(candidates.pop().unwrap()); // try the last candidate defined
                }
            }
        }
    }

    None
}

fn resolve_binary_operator<D: Driver>(
    operator: WithInfo<D::Info, crate::UnresolvedBinaryOperator>,
    left: Option<WithInfo<D::Info, crate::UnresolvedExpression<D>>>,
    right: Option<WithInfo<D::Info, crate::UnresolvedExpression<D>>>,
    info: &mut Info<D>,
) -> crate::Expression<D> {
    let operator_trait = operator.replace(
        match resolve_language_item(operator.as_ref().map(ToString::to_string), info) {
            Some(mut path) => {
                let name = path.last().unwrap().name().unwrap().to_string();
                path.push(crate::PathComponent::Constructor(name));
                crate::Expression::Constant(path)
            }
            None => crate::Expression::Error,
        },
    );

    match (left, right) {
        (Some(left), Some(right)) => crate::Expression::Call {
            function: operator
                .replace(crate::Expression::Call {
                    function: operator_trait.boxed(),
                    input: resolve_expression(right, info).boxed(),
                })
                .boxed(),
            input: resolve_expression(left, info).boxed(),
        },
        (Some(left), None) => {
            let input_variable = info.next_variable;
            info.next_variable += 1;
            let input_variable = info.make_path(crate::PathComponent::Variable(input_variable));

            crate::Expression::Function {
                pattern: operator.replace(crate::Pattern::Variable(
                    String::from("input"),
                    input_variable.clone(),
                )),
                body: operator
                    .replace(crate::Expression::Call {
                        function: operator
                            .replace(crate::Expression::Call {
                                function: operator_trait.boxed(),
                                input: operator
                                    .replace(crate::Expression::Variable(
                                        String::from("input"),
                                        input_variable,
                                    ))
                                    .boxed(),
                            })
                            .boxed(),
                        input: resolve_expression(left, info).boxed(),
                    })
                    .boxed(),
            }
        }
        (None, Some(right)) => crate::Expression::Call {
            function: operator_trait.boxed(),
            input: resolve_expression(right, info).boxed(),
        },
        (None, None) => operator_trait.item,
    }
}

fn resolve_language_item<D: Driver>(
    name: WithInfo<D::Info, String>,
    info: &mut Info<D>,
) -> Option<crate::Path> {
    match info
        .language_declarations
        .get(&name.item)
        .map(|declaration| declaration.as_ref())
        .or_else(|| {
            info.dependencies
                .language_declarations
                .get(&name.item)
                .map(Some)
        }) {
        Some(item) => Some(
            item.unwrap_or_else(|| {
                panic!(
                    "language items are resolved before everything else: {:?}",
                    name.item
                )
            })
            .clone(),
        ),
        None => {
            info.errors
                .push(name.map(crate::Error::UnresolvedLanguageItem));

            None
        }
    }
}
