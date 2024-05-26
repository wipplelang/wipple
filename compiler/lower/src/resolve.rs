use crate::Driver;
use derivative::Derivative;
use std::{
    collections::{HashMap, HashSet},
    mem,
};
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
        type_alias_declarations,
        trait_declarations,
        constant_declarations,
        type_parameter_declarations,
        instance_declarations,
    );

    for (name, paths) in dependencies.language_declarations {
        info.language_declarations
            .entry(name)
            .or_default()
            .extend(paths);
    }

    for (name, paths) in dependencies.top_level {
        for path in paths {
            info.scopes.define(name.clone(), path);
        }
    }

    let statements_by_file = files
        .into_iter()
        .map(|file| {
            info.path
                .push(crate::PathComponent::File(file.item.name.clone()));

            info.scopes.push_block_scope();

            let (declaration_statements, executable_statements) =
                split_executable_statements(file.item.statements, &mut info);

            let path_component = info.path.pop().unwrap();
            assert!(info.path.is_empty());

            let scope = info.scopes.pop_scope();
            info.scopes.merge(&scope);

            (
                path_component,
                declaration_statements,
                executable_statements,
            )
        })
        .collect::<Vec<_>>();

    let executable_statements_by_file = statements_by_file
        .into_iter()
        .map(
            |(file_path_component, declaration_statements, executable_statements)| {
                info.path.push(file_path_component);

                for statements in declaration_statements {
                    resolve_statements(statements, &mut info);
                }

                let file_path_component = info.path.pop().unwrap();
                assert!(info.path.is_empty());

                (file_path_component, executable_statements)
            },
        )
        .collect::<Vec<_>>();

    let mut interface = crate::Interface::default();
    for (file_path_component, executable_statements) in executable_statements_by_file {
        info.path.push(file_path_component);
        info.scopes.push_block_scope();

        let statements = resolve_statements(executable_statements, &mut info);

        info.scopes.pop_scope();
        assert!(info.scopes.0.len() == 1);

        let path = crate::Path(vec![info.path.pop().unwrap()]);
        assert!(info.path.is_empty());

        info.library
            .code
            .insert(path, crate::TopLevelCode { statements });
    }

    for (name, paths) in info.scopes.pop_scope().into_paths() {
        for path in paths {
            if !path.item.last().unwrap().is_local() {
                interface
                    .top_level
                    .entry(name.clone())
                    .or_default()
                    .push(path);
            }
        }
    }

    assert!(info.scopes.0.is_empty());

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
        type_alias_declarations,
        trait_declarations,
        constant_declarations,
        type_parameter_declarations,
        instance_declarations,
    );

    for (name, paths) in info.language_declarations {
        interface
            .language_declarations
            .entry(name)
            .or_default()
            .extend(paths);
    }

    crate::Result {
        interface,
        library: info.library,
        diagnostics: info.errors,
    }
}

#[derive(Debug, Derivative)]
#[derivative(Default(bound = ""))]
struct Info<D: Driver> {
    errors: Vec<WithInfo<D::Info, crate::Diagnostic>>,
    dependencies: crate::Interface<D>,
    type_declarations: HashMap<crate::Path, WithInfo<D::Info, Option<crate::TypeDeclaration<D>>>>,
    type_alias_declarations:
        HashMap<crate::Path, WithInfo<D::Info, Option<crate::TypeAliasDeclaration<D>>>>,
    trait_declarations: HashMap<crate::Path, WithInfo<D::Info, Option<crate::TraitDeclaration<D>>>>,
    constant_declarations:
        HashMap<crate::Path, WithInfo<D::Info, Option<crate::ConstantDeclaration<D>>>>,
    type_parameter_declarations:
        HashMap<crate::Path, WithInfo<D::Info, Option<crate::TypeParameterDeclaration<D>>>>,
    language_declarations: HashMap<String, Vec<crate::Path>>,
    instance_declarations:
        HashMap<crate::Path, WithInfo<D::Info, Option<crate::InstanceDeclaration<D>>>>,
    library: crate::Library<D>,
    path: crate::Path,
    scopes: Scopes<D>,
    next_variable: u32,
    captures: Vec<Captures>,
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

    fn top_level_path(&self) -> crate::Path {
        crate::Path(vec![self.path.first().unwrap().clone()])
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
pub struct Scope<D: Driver> {
    kind: ScopeKind,
    paths: HashMap<String, Vec<WithInfo<<D as Driver>::Info, crate::Path>>>,
}

impl<D: Driver> Scope<D> {
    fn new(kind: ScopeKind) -> Self {
        Self {
            kind,
            paths: HashMap::new(),
        }
    }

    fn into_paths(self) -> HashMap<String, Vec<WithInfo<D::Info, crate::Path>>> {
        self.paths
    }

    fn filters_locals(&self) -> bool {
        matches!(self.kind, ScopeKind::Constant)
    }
}

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
        self.0.push(Scope::new(ScopeKind::Block));
    }

    pub fn push_constant_scope(&mut self) {
        self.0.push(Scope::new(ScopeKind::Constant));
    }

    pub fn pop_scope(&mut self) -> Scope<D> {
        self.0.pop().unwrap()
    }

    pub fn define(&mut self, name: String, path: WithInfo<D::Info, crate::Path>) {
        let paths = &mut self.0.last_mut().unwrap().paths;
        let paths = paths.entry(name).or_default();

        // Overwrite existing locals with the same name
        if path.item.last().unwrap().is_local() {
            paths.retain(|path| !path.item.last().unwrap().is_local());
        }

        paths.push(path);
    }

    pub fn merge(&mut self, scope: &Scope<D>) {
        for (name, paths) in &scope.paths {
            for path in paths {
                if !path.item.last().unwrap().is_local() {
                    self.define(name.clone(), path.clone());
                }
            }
        }
    }
}

#[derive(Debug, Default)]
struct Captures {
    declared: HashSet<crate::Path>,
    used: HashSet<crate::Path>,
}

impl<D: Driver> Info<D> {
    fn declare_variable(&mut self, path: &crate::Path) {
        if let Some(captures) = self.captures.last_mut() {
            captures.declared.insert(path.clone());
        }
    }

    fn capture_if_variable(&mut self, path: &crate::Path) {
        if !matches!(path.last(), Some(crate::PathComponent::Variable(_))) {
            return;
        }

        for captures in self.captures.iter_mut().rev() {
            if captures.declared.contains(path) {
                break;
            }

            captures.used.insert(path.clone());
        }
    }
}

fn split_executable_statements<D: Driver>(
    statements: Vec<WithInfo<D::Info, crate::UnresolvedStatement<D>>>,
    info: &mut Info<D>,
) -> (
    Vec<Vec<WithInfo<D::Info, crate::UnresolvedStatement<D>>>>,
    Vec<WithInfo<D::Info, crate::UnresolvedStatement<D>>>,
) {
    let (declaration_statements, executable_statements): (Vec<_>, Vec<_>) =
        statements
            .into_iter()
            .filter_map(|statement| {
                macro_rules! insert_declaration {
                    ($declarations:ident, $name:expr, $path:expr $(,)?) => {{
                        use std::collections::hash_map::Entry;

                        let path = $path;

                        match info.$declarations.entry(path.clone()) {
                            Entry::Occupied(entry) => {
                                info.errors.push(statement.replace(
                                    crate::Diagnostic::AlreadyDefined(entry.key().clone()),
                                ));

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
                    crate::UnresolvedStatement::TypeAlias { name, .. } => insert_declaration!(
                        type_alias_declarations,
                        &name.item,
                        info.make_path(crate::PathComponent::TypeAlias(name.item.clone())),
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
                    crate::UnresolvedStatement::Instance { .. }
                    | crate::UnresolvedStatement::Assignment { .. }
                    | crate::UnresolvedStatement::Expression(_) => Some(statement),
                }
            })
            .partition(|statement| {
                matches!(
                    statement.item,
                    crate::UnresolvedStatement::Type { .. }
                        | crate::UnresolvedStatement::TypeAlias { .. }
                        | crate::UnresolvedStatement::Trait { .. }
                )
            });

    let (type_statements, language_statements): (Vec<_>, Vec<_>) =
        declaration_statements.into_iter().partition(|statement| {
            matches!(
                statement.item,
                crate::UnresolvedStatement::Type { .. } | crate::UnresolvedStatement::Trait { .. }
            )
        });

    (
        vec![type_statements, language_statements],
        executable_statements,
    )
}

fn resolve_statements<D: Driver>(
    statements: Vec<WithInfo<D::Info, crate::UnresolvedStatement<D>>>,
    info: &mut Info<D>,
) -> Vec<WithInfo<D::Info, crate::Expression<D>>> {
    let mut queued_constants = Vec::new();

    let statements = statements
        .into_iter()
        .filter_map(|statement| match statement.item {
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

                declaration.item = Some(crate::TypeDeclaration {
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
            crate::UnresolvedStatement::TypeAlias {
                attributes,
                name,
                parameters,
                r#type,
            } => {
                info.path.push(crate::PathComponent::TypeAlias(name.item));

                info.scopes.push_constant_scope();

                let parameters = parameters
                    .into_iter()
                    .filter_map(|type_parameter| resolve_type_parameter(type_parameter, info))
                    .collect::<Vec<_>>();

                let r#type = resolve_type(r#type, info);

                resolve_attributes(&attributes, info);

                info.scopes.pop_scope();

                let declaration = info.type_alias_declarations.get_mut(&info.path).unwrap();

                declaration.item = Some(crate::TypeAliasDeclaration {
                    attributes,
                    parameters,
                    r#type,
                });

                info.path.pop().unwrap();

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

                declaration.item = Some(crate::TraitDeclaration {
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
                    WithInfo {
                        info: statement.info,
                        item: Some(crate::InstanceDeclaration {
                            parameters,
                            bounds,
                            instance,
                            default,
                        }),
                    },
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
                                    info: statement.info,
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
                                info: statement.info,
                                item: crate::Expression::Mutate {
                                    name,
                                    path,
                                    value: value.boxed(),
                                },
                            }),
                            None => Some(WithInfo {
                                info: statement.info,
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
                            info: statement.info,
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

fn resolve_attributes<D: Driver>(
    attributes: &[WithInfo<D::Info, crate::Attribute<D>>],
    info: &mut Info<D>,
) {
    for attribute in attributes {
        if let crate::Attribute::Valued { name, value } = &attribute.item {
            if name.item.as_str() == "language" {
                if let crate::AttributeValue::Text(language_item) = &value.item {
                    info.language_declarations
                        .entry(language_item.item.clone())
                        .or_default()
                        .push(info.path.clone());
                }
            }
        }
    }
}

fn generate_marker_constructor<D: Driver>(
    attributes: Vec<WithInfo<D::Info, crate::Attribute<D>>>,
    name: WithInfo<D::Info, String>,
    parameters: Vec<crate::Path>,
    info: &mut Info<D>,
) -> (String, WithInfo<D::Info, crate::Path>) {
    info.path
        .push(crate::PathComponent::Constructor(name.item.clone()));

    resolve_attributes(&attributes, info);

    let constructor_path = info.path.clone();
    info.path.pop().unwrap();

    let constructor_declaration = crate::ConstantDeclaration {
        attributes,
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

    info.library.items.insert(
        constructor_path.clone(),
        Some(crate::Item {
            body: constructor_body,
            captures: Vec::new(),
            top_level: info.top_level_path(),
        }),
    );

    (
        name.item,
        WithInfo {
            info: name.info,
            item: constructor_path,
        },
    )
}

fn generate_structure_constructor<D: Driver>(
    attributes: Vec<WithInfo<D::Info, crate::Attribute<D>>>,
    name: WithInfo<D::Info, String>,
    parameters: Vec<crate::Path>,
    info: &mut Info<D>,
) -> (String, WithInfo<D::Info, crate::Path>) {
    info.path
        .push(crate::PathComponent::Constructor(name.item.clone()));

    resolve_attributes(&attributes, info);

    let constructor_path = info.path.clone();
    info.path.pop().unwrap();

    let constructor_declaration = crate::ConstantDeclaration {
        attributes,
        parameters: parameters.clone(),
        bounds: Vec::new(),
        r#type: WithInfo {
            info: name.info.clone(),
            item: crate::Type::Function {
                inputs: vec![WithInfo {
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
                }],
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
                inputs: vec![WithInfo {
                    info: name.info.clone(),
                    item: crate::Pattern::Variable(name.item.clone(), input_variable.clone()),
                }],
                body: WithInfo {
                    info: name.info.clone(),
                    item: crate::Expression::Variable(name.item.clone(), input_variable),
                }
                .boxed(),
                captures: Vec::new(),
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

    info.library.items.insert(
        constructor_path.clone(),
        Some(crate::Item {
            body: constructor_body,
            captures: Vec::new(),
            top_level: info.top_level_path(),
        }),
    );

    (
        name.item,
        WithInfo {
            info: name.info,
            item: constructor_path,
        },
    )
}

fn generate_variant_constructor<D: Driver>(
    attributes: Vec<WithInfo<D::Info, crate::Attribute<D>>>,
    name: WithInfo<D::Info, String>,
    parameters: Vec<crate::Path>,
    variant_path: WithInfo<D::Info, crate::Path>,
    value_types: Vec<WithInfo<D::Info, crate::Type<D>>>,
    info: &mut Info<D>,
) -> (String, WithInfo<D::Info, crate::Path>) {
    info.path
        .push(crate::PathComponent::Constructor(name.item.clone()));

    resolve_attributes(&attributes, info);

    let constructor_path = info.path.clone();
    info.path.pop().unwrap();

    let constructor_declaration = {
        let output_type = WithInfo {
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
        };

        crate::ConstantDeclaration {
            attributes,
            parameters,
            bounds: Vec::new(),
            r#type: if value_types.is_empty() {
                output_type
            } else {
                WithInfo {
                    info: name.info.clone(),
                    item: crate::Type::Function {
                        inputs: value_types.clone(),
                        output: output_type.boxed(),
                    },
                }
            },
        }
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

        let output = WithInfo {
            info: name.info.clone(),
            item: crate::Expression::Variant {
                variant: variant_path,
                values: input_variables
                    .iter()
                    .map(|input_variable| WithInfo {
                        info: name.info.clone(),
                        item: crate::Expression::Variable(
                            name.item.clone(),
                            input_variable.clone(),
                        ),
                    })
                    .collect(),
            },
        };

        if value_types.is_empty() {
            output
        } else {
            WithInfo {
                info: name.info.clone(),
                item: crate::Expression::Function {
                    inputs: input_variables
                        .into_iter()
                        .map(|variable| WithInfo {
                            info: name.info.clone(),
                            item: crate::Pattern::Variable(name.item.clone(), variable),
                        })
                        .collect(),
                    body: output.boxed(),
                    captures: Vec::new(),
                },
            }
        }
    };

    info.constant_declarations.insert(
        constructor_path.clone(),
        WithInfo {
            info: name.info.clone(),
            item: Some(constructor_declaration),
        },
    );

    info.library.items.insert(
        constructor_path.clone(),
        Some(crate::Item {
            body: constructor_body,
            captures: Vec::new(),
            top_level: info.top_level_path(),
        }),
    );

    (
        name.item,
        WithInfo {
            info: name.info,
            item: constructor_path,
        },
    )
}

fn generate_wrapper_constructor<D: Driver>(
    attributes: Vec<WithInfo<D::Info, crate::Attribute<D>>>,
    name: WithInfo<D::Info, String>,
    value_type: WithInfo<D::Info, crate::Type<D>>,
    parameters: Vec<crate::Path>,
    info: &mut Info<D>,
) -> (String, WithInfo<D::Info, crate::Path>) {
    info.path
        .push(crate::PathComponent::Constructor(name.item.clone()));

    resolve_attributes(&attributes, info);

    let constructor_path = info.path.clone();
    info.path.pop().unwrap();

    let constructor_declaration = crate::ConstantDeclaration {
        attributes,
        parameters: parameters.clone(),
        bounds: Vec::new(),
        r#type: WithInfo {
            info: name.info.clone(),
            item: crate::Type::Function {
                inputs: vec![value_type],
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
                inputs: vec![WithInfo {
                    info: name.info.clone(),
                    item: crate::Pattern::Variable(name.item.clone(), input_variable.clone()),
                }],
                body: WithInfo {
                    info: name.info.clone(),
                    item: crate::Expression::Wrapper {
                        r#type: info.path.clone(),
                        value: WithInfo {
                            info: name.info.clone(),
                            item: crate::Expression::Variable(name.item.clone(), input_variable),
                        }
                        .boxed(),
                    },
                }
                .boxed(),
                captures: Vec::new(),
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

    info.library.items.insert(
        constructor_path.clone(),
        Some(crate::Item {
            body: constructor_body,
            captures: Vec::new(),
            top_level: info.top_level_path(),
        }),
    );

    (
        name.item,
        WithInfo {
            info: name.info,
            item: constructor_path,
        },
    )
}

fn generate_trait_constructor<D: Driver>(
    attributes: Vec<WithInfo<D::Info, crate::Attribute<D>>>,
    name: WithInfo<D::Info, String>,
    parameters: Vec<crate::Path>,
    r#type: WithInfo<D::Info, crate::Type<D>>,
    info: &mut Info<D>,
) -> (String, WithInfo<D::Info, crate::Path>) {
    info.path
        .push(crate::PathComponent::Constructor(name.item.clone()));

    resolve_attributes(&attributes, info);

    let constructor_path = info.path.clone();
    info.path.pop().unwrap();

    let constructor_declaration = crate::ConstantDeclaration {
        attributes,
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

    info.library.items.insert(
        constructor_path.clone(),
        Some(crate::Item {
            body: constructor_body,
            captures: Vec::new(),
            top_level: info.top_level_path(),
        }),
    );

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
                    crate::PathComponent::Constant(_) | crate::PathComponent::Constructor(_) => 1,
                    _ => 3,
                });

                let path = match candidates.first() {
                    Some(candidate) => candidate,
                    None => return Vec::new(),
                };

                match path.item.last().unwrap() {
                    crate::PathComponent::Variable(_) => {
                        vec![(
                            path.item.clone(),
                            crate::Expression::Variable(name.clone(), path.item.clone()),
                        )]
                    }
                    crate::PathComponent::Constant(_) | crate::PathComponent::Constructor(_) => {
                        vec![(
                            path.item.clone(),
                            crate::Expression::Constant(path.item.clone()),
                        )]
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
            info.captures.push(Captures::default());

            let (declaration_statements, executable_statements) =
                split_executable_statements(statements, info);

            for statements in declaration_statements {
                resolve_statements(statements, info);
            }

            let statements = resolve_statements(executable_statements, info);

            let captures = info.captures.pop().unwrap();
            info.scopes.pop_scope();

            crate::Expression::Block {
                statements,
                captures: Vec::from_iter(captures.used),
            }
        }
        crate::UnresolvedExpression::Function { inputs, body } => {
            info.scopes.push_block_scope();
            info.captures.push(Captures::default());

            let inputs = inputs
                .into_iter()
                .map(|pattern| resolve_pattern(pattern, info))
                .collect();

            let body = resolve_expression(body.unboxed(), info);

            let captures = info.captures.pop().unwrap();
            info.scopes.pop_scope();

            crate::Expression::Function {
                inputs,
                body: body.boxed(),
                captures: Vec::from_iter(captures.used),
            }
        }
        crate::UnresolvedExpression::Do(block) => {
            crate::Expression::Do(resolve_expression(block.unboxed(), info).boxed())
        }
        crate::UnresolvedExpression::Call { function, inputs } => crate::Expression::Call {
            function: resolve_expression(function.unboxed(), info).boxed(),
            inputs: inputs
                .into_iter()
                .map(|input| resolve_expression(input, info))
                .collect(),
        },
        crate::UnresolvedExpression::Apply { input, function } => crate::Expression::Call {
            function: resolve_expression(function.unboxed(), info).boxed(),
            inputs: vec![resolve_expression(input.unboxed(), info)],
        },
        crate::UnresolvedExpression::BinaryOperator {
            operator,
            left,
            right,
        } => resolve_binary_operator(operator, left.unboxed(), right.unboxed(), info),
        crate::UnresolvedExpression::As { value, r#type } => {
            let operator = WithInfo {
                info: expression_info,
                item: String::from("as"),
            };

            let operator_trait = operator.replace(
                match resolve_language_item(operator.clone(), info)
                    .into_iter()
                    .find(|path| matches!(path.last().unwrap(), crate::PathComponent::Trait(_)))
                {
                    Some(path) => crate::Expression::Trait(path),
                    None => crate::Expression::Error,
                },
            );

            let r#type = resolve_type(r#type, info);

            crate::Expression::Annotate {
                value: operator
                    .replace(crate::Expression::Call {
                        function: operator_trait.boxed(),
                        inputs: vec![resolve_expression(value.unboxed(), info)],
                    })
                    .boxed(),
                r#type,
            }
        }
        crate::UnresolvedExpression::Is { value, pattern } => {
            let r#true = WithInfo {
                info: expression_info.clone(),
                item: String::from("true"),
            };

            let true_value = r#true.replace(
                match resolve_language_item(r#true.clone(), info)
                    .into_iter()
                    .find(|path| {
                        matches!(path.last().unwrap(), crate::PathComponent::Constructor(_))
                    }) {
                    Some(path) => crate::Expression::Constant(path),
                    None => crate::Expression::Error,
                },
            );

            let r#false = WithInfo {
                info: expression_info.clone(),
                item: String::from("false"),
            };

            let false_value = r#false.replace(
                match resolve_language_item(r#false.clone(), info)
                    .into_iter()
                    .find(|path| {
                        matches!(path.last().unwrap(), crate::PathComponent::Constructor(_))
                    }) {
                    Some(path) => crate::Expression::Constant(path),
                    None => crate::Expression::Error,
                },
            );

            let when_expression = |value: WithInfo<D::Info, crate::Expression<D>>,
                                   info: &mut Info<D>| {
                let value_info = value.info.clone();

                crate::Expression::When {
                    input: value.boxed(),
                    arms: vec![
                        r#true.replace(crate::Arm {
                            pattern: resolve_pattern(pattern, info),
                            body: true_value,
                        }),
                        r#false.replace(crate::Arm {
                            pattern: WithInfo {
                                info: value_info,
                                item: crate::Pattern::Wildcard,
                            },
                            body: false_value,
                        }),
                    ],
                }
            };

            when_expression(resolve_expression(value.unboxed(), info), info)
        }
        crate::UnresolvedExpression::When { input, arms } => {
            let input = resolve_expression(input.unboxed(), info);

            let arms = arms
                .into_iter()
                .map(|arm| {
                    arm.map(|arm| {
                        info.scopes.push_block_scope();

                        let pattern = resolve_pattern(arm.pattern, info);

                        let body = resolve_expression(arm.body, info);

                        info.scopes.pop_scope();

                        crate::Arm { pattern, body }
                    })
                })
                .collect::<Vec<_>>();

            crate::Expression::When {
                input: input.boxed(),
                arms,
            }
        }
        crate::UnresolvedExpression::Intrinsic { name, inputs } => {
            let name = match name.try_unwrap() {
                Some(name) => name,
                None => return crate::Expression::Error,
            };

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

            info.declare_variable(&path);

            crate::Pattern::Variable(name, path)
        }
        crate::UnresolvedPattern::VariantOrName(name) => {
            let name = match name {
                Some(name) => name,
                None => return crate::Pattern::Error,
            };

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
                            crate::PathComponent::Variant(_) => {
                                Some((path.item.clone(), path.clone()))
                            }
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
                    .map(|declaration| declaration.as_ref().unwrap());

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

fn resolve_type<D: Driver>(
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
                    crate::PathComponent::Type(_)
                    | crate::PathComponent::TypeAlias(_)
                    | crate::PathComponent::TypeParameter(_) => 0,
                    crate::PathComponent::Constant(_) => 1,
                    _ => 2,
                });

                match candidates.into_iter().next() {
                    Some(candidate) => vec![(candidate.item.clone(), candidate)],
                    None => Vec::new(),
                }
            };

            match resolve_name(name, info, filter) {
                Some(path) => {
                    match path.item.last().unwrap() {
                        crate::PathComponent::Type(_) => crate::Type::Declared {
                            path,
                            parameters: parameters
                                .into_iter()
                                .map(|parameter| resolve_type(parameter, info))
                                .collect(),
                        },
                        crate::PathComponent::TypeAlias(_) => crate::Type::Alias {
                            path,
                            parameters: parameters
                                .into_iter()
                                .map(|parameter| resolve_type(parameter, info))
                                .collect(),
                        },
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
    })
}

fn resolve_type_parameter<D: Driver>(
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

fn resolve_instance<D: Driver>(
    instance: WithInfo<D::Info, crate::UnresolvedInstance<D>>,
    info: &mut Info<D>,
) -> Option<WithInfo<D::Info, crate::Instance<D>>> {
    instance.filter_map(|instance| {
        let r#trait = match resolve_name(instance.r#trait.try_unwrap()?, info, |candidates| {
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
    filter: impl FnMut(&[WithInfo<D::Info, crate::Path>]) -> Vec<(crate::Path, T)>,
) -> Option<T> {
    let result = try_resolve_name(name.clone(), info, filter);

    if result.is_none() {
        info.errors
            .push(name.map(crate::Diagnostic::UnresolvedName));
    }

    result
}

fn try_resolve_name<D: Driver, T>(
    name: WithInfo<D::Info, String>,
    info: &mut Info<D>,
    mut filter: impl FnMut(&[WithInfo<D::Info, crate::Path>]) -> Vec<(crate::Path, T)>,
) -> Option<T> {
    let mut allow_locals = true;
    for scope in info.scopes.0.iter().rev() {
        if let Some(paths) = scope.paths.get(&name.item) {
            let paths = paths
                .iter()
                .filter(|path| allow_locals || !path.item.last().unwrap().is_local())
                .cloned()
                .collect::<Vec<_>>();

            let mut candidates = filter(&paths);

            match candidates.len() {
                0 => {
                    if scope.filters_locals() {
                        allow_locals = false;
                    }

                    continue;
                }
                1 => {
                    let (path, candidate) = candidates.pop().unwrap();
                    info.capture_if_variable(&path);

                    return Some(candidate);
                }
                _ => {
                    info.errors
                        .push(name.replace(crate::Diagnostic::AmbiguousName {
                            name: name.item.clone(),
                            candidates: paths.iter().map(|path| path.item.clone()).collect(),
                        }));

                    // Try the last candidate defined
                    let (path, candidate) = candidates.pop().unwrap();
                    info.capture_if_variable(&path);

                    return Some(candidate);
                }
            }
        } else if scope.filters_locals() {
            allow_locals = false;
        }
    }

    None
}

fn resolve_binary_operator<D: Driver>(
    operator: WithInfo<D::Info, crate::UnresolvedBinaryOperator>,
    left: WithInfo<D::Info, crate::UnresolvedExpression<D>>,
    right: WithInfo<D::Info, crate::UnresolvedExpression<D>>,
    info: &mut Info<D>,
) -> crate::Expression<D> {
    let operator_trait = operator.replace(
        match resolve_language_item(operator.as_ref().map(ToString::to_string), info)
            .into_iter()
            .find(|path| matches!(path.last().unwrap(), crate::PathComponent::Trait(_)))
        {
            Some(mut path) => {
                let name = match path.last().unwrap() {
                    crate::PathComponent::Trait(name) => name.clone(),
                    _ => unreachable!(),
                };

                path.push(crate::PathComponent::Constructor(name));

                crate::Expression::Constant(path)
            }
            None => crate::Expression::Error,
        },
    );

    let left = resolve_expression(left, info);

    let right = if operator.item.short_circuits() {
        let right_info = right.info.clone();
        resolve_expression(
            right.map(|right| {
                crate::UnresolvedExpression::Block(vec![WithInfo {
                    info: right_info.clone(),
                    item: crate::UnresolvedStatement::Expression(WithInfo {
                        info: right_info,
                        item: right,
                    }),
                }])
            }),
            info,
        )
    } else {
        resolve_expression(right, info)
    };

    crate::Expression::Call {
        function: operator_trait.boxed(),
        inputs: vec![left, right],
    }
}

fn resolve_language_item<D: Driver>(
    name: WithInfo<D::Info, String>,
    info: &mut Info<D>,
) -> Vec<crate::Path> {
    match info
        .language_declarations
        .get(&name.item)
        .or_else(|| info.dependencies.language_declarations.get(&name.item))
    {
        Some(paths) => paths.clone(),
        None => {
            info.errors
                .push(name.map(crate::Diagnostic::UnresolvedLanguageItem));

            Vec::new()
        }
    }
}
