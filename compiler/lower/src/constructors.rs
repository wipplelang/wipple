use crate::{attribute::resolve_attributes, resolve::Info, Driver};
use wipple_util::WithInfo;

pub fn generate_marker_constructor<D: Driver>(
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

pub fn generate_structure_constructor<D: Driver>(
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

pub fn generate_variant_constructor<D: Driver>(
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

pub fn generate_wrapper_constructor<D: Driver>(
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

pub fn generate_trait_constructor<D: Driver>(
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
