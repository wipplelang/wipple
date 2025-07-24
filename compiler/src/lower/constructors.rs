use crate::{
    lower::{attribute::resolve_attributes, resolve::Info},
    util::WithInfo,
};

pub fn generate_marker_constructor(
    attributes: Vec<WithInfo<crate::lower::Attribute>>,
    name: WithInfo<String>,
    parameters: Vec<crate::lower::Path>,
    info: &mut Info,
) -> (String, WithInfo<crate::lower::Path>) {
    info.path
        .push(crate::lower::PathComponent::Constructor(name.item.clone()));

    resolve_attributes(&attributes, info);

    let constructor_path = info.path.clone();
    info.path.pop().unwrap();

    let constructor_declaration = crate::lower::ConstantDeclaration {
        attributes,
        parameters: parameters.clone(),
        bounds: Vec::new(),
        r#type: WithInfo {
            info: name.info.clone(),
            item: crate::lower::Type::Declared {
                path: WithInfo {
                    info: name.info.clone(),
                    item: info.path.clone(),
                },
                parameters: parameters
                    .into_iter()
                    .map(|parameter| WithInfo {
                        info: name.info.clone(),
                        item: crate::lower::Type::Parameter(parameter),
                    })
                    .collect(),
            },
        },
    };

    let constructor_body = WithInfo {
        info: name.info.clone(),
        item: crate::lower::Expression::Marker(info.path.clone()),
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
        Some(crate::lower::Item {
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

pub fn generate_structure_constructor(
    attributes: Vec<WithInfo<crate::lower::Attribute>>,
    name: WithInfo<String>,
    parameters: Vec<crate::lower::Path>,
    info: &mut Info,
) -> (String, WithInfo<crate::lower::Path>) {
    info.path
        .push(crate::lower::PathComponent::Constructor(name.item.clone()));

    resolve_attributes(&attributes, info);

    let constructor_path = info.path.clone();
    info.path.pop().unwrap();

    let constructor_declaration = crate::lower::ConstantDeclaration {
        attributes,
        parameters: parameters.clone(),
        bounds: Vec::new(),
        r#type: WithInfo {
            info: name.info.clone(),
            item: crate::lower::Type::Function {
                inputs: vec![WithInfo {
                    info: name.info.clone(),
                    item: crate::lower::Type::Declared {
                        path: WithInfo {
                            info: name.info.clone(),
                            item: info.path.clone(),
                        },
                        parameters: parameters
                            .clone()
                            .into_iter()
                            .map(|parameter| WithInfo {
                                info: name.info.clone(),
                                item: crate::lower::Type::Parameter(parameter),
                            })
                            .collect(),
                    },
                }],
                output: WithInfo {
                    info: name.info.clone(),
                    item: crate::lower::Type::Declared {
                        path: WithInfo {
                            info: name.info.clone(),
                            item: info.path.clone(),
                        },
                        parameters: parameters
                            .into_iter()
                            .map(|parameter| WithInfo {
                                info: name.info.clone(),
                                item: crate::lower::Type::Parameter(parameter),
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
        let input_variable = info.make_path(crate::lower::PathComponent::Variable(input_variable));

        WithInfo {
            info: name.info.clone(),
            item: crate::lower::Expression::Function {
                inputs: vec![WithInfo {
                    info: name.info.clone(),
                    item: crate::lower::Pattern::Variable(
                        name.item.clone(),
                        input_variable.clone(),
                    ),
                }],
                body: WithInfo {
                    info: name.info.clone(),
                    item: crate::lower::Expression::Variable(name.item.clone(), input_variable),
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
        Some(crate::lower::Item {
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

pub fn generate_variant_constructor(
    attributes: Vec<WithInfo<crate::lower::Attribute>>,
    name: WithInfo<String>,
    parameters: Vec<crate::lower::Path>,
    variant_path: WithInfo<crate::lower::Path>,
    value_types: Vec<WithInfo<crate::lower::Type>>,
    info: &mut Info,
) -> (String, WithInfo<crate::lower::Path>) {
    info.path
        .push(crate::lower::PathComponent::Constructor(name.item.clone()));

    resolve_attributes(&attributes, info);

    let constructor_path = info.path.clone();
    info.path.pop().unwrap();

    let constructor_declaration = {
        let output_type = WithInfo {
            info: name.info.clone(),
            item: crate::lower::Type::Declared {
                path: WithInfo {
                    info: name.info.clone(),
                    item: info.path.clone(),
                },
                parameters: parameters
                    .clone()
                    .into_iter()
                    .map(|parameter| WithInfo {
                        info: name.info.clone(),
                        item: crate::lower::Type::Parameter(parameter),
                    })
                    .collect(),
            },
        };

        crate::lower::ConstantDeclaration {
            attributes,
            parameters,
            bounds: Vec::new(),
            r#type: if value_types.is_empty() {
                output_type
            } else {
                WithInfo {
                    info: name.info.clone(),
                    item: crate::lower::Type::Function {
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
                info.make_path(crate::lower::PathComponent::Variable(input_variable))
            })
            .collect::<Vec<_>>();

        let output = WithInfo {
            info: name.info.clone(),
            item: crate::lower::Expression::Variant {
                variant: variant_path,
                values: input_variables
                    .iter()
                    .map(|input_variable| WithInfo {
                        info: name.info.clone(),
                        item: crate::lower::Expression::Variable(
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
                item: crate::lower::Expression::Function {
                    inputs: input_variables
                        .into_iter()
                        .map(|variable| WithInfo {
                            info: name.info.clone(),
                            item: crate::lower::Pattern::Variable(name.item.clone(), variable),
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
        Some(crate::lower::Item {
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

pub fn generate_wrapper_constructor(
    attributes: Vec<WithInfo<crate::lower::Attribute>>,
    name: WithInfo<String>,
    value_type: WithInfo<crate::lower::Type>,
    parameters: Vec<crate::lower::Path>,
    info: &mut Info,
) -> (String, WithInfo<crate::lower::Path>) {
    info.path
        .push(crate::lower::PathComponent::Constructor(name.item.clone()));

    resolve_attributes(&attributes, info);

    let constructor_path = info.path.clone();
    info.path.pop().unwrap();

    let constructor_declaration = crate::lower::ConstantDeclaration {
        attributes,
        parameters: parameters.clone(),
        bounds: Vec::new(),
        r#type: WithInfo {
            info: name.info.clone(),
            item: crate::lower::Type::Function {
                inputs: vec![value_type],
                output: WithInfo {
                    info: name.info.clone(),
                    item: crate::lower::Type::Declared {
                        path: WithInfo {
                            info: name.info.clone(),
                            item: info.path.clone(),
                        },
                        parameters: parameters
                            .into_iter()
                            .map(|parameter| WithInfo {
                                info: name.info.clone(),
                                item: crate::lower::Type::Parameter(parameter),
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
        let input_variable = info.make_path(crate::lower::PathComponent::Variable(input_variable));

        WithInfo {
            info: name.info.clone(),
            item: crate::lower::Expression::Function {
                inputs: vec![WithInfo {
                    info: name.info.clone(),
                    item: crate::lower::Pattern::Variable(
                        name.item.clone(),
                        input_variable.clone(),
                    ),
                }],
                body: WithInfo {
                    info: name.info.clone(),
                    item: crate::lower::Expression::Wrapper {
                        r#type: info.path.clone(),
                        value: WithInfo {
                            info: name.info.clone(),
                            item: crate::lower::Expression::Variable(
                                name.item.clone(),
                                input_variable,
                            ),
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
        Some(crate::lower::Item {
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

pub fn generate_trait_constructor(
    attributes: Vec<WithInfo<crate::lower::Attribute>>,
    name: WithInfo<String>,
    parameters: Vec<crate::lower::Path>,
    r#type: WithInfo<crate::lower::Type>,
    info: &mut Info,
) -> (String, WithInfo<crate::lower::Path>) {
    info.path
        .push(crate::lower::PathComponent::Constructor(name.item.clone()));

    resolve_attributes(&attributes, info);

    let constructor_path = info.path.clone();
    info.path.pop().unwrap();

    let constructor_declaration = crate::lower::ConstantDeclaration {
        attributes,
        parameters: parameters.clone(),
        bounds: vec![
            name.replace(crate::lower::Instance {
                r#trait: name.replace(info.path.clone()),
                parameters: parameters
                    .into_iter()
                    .map(|parameter| WithInfo {
                        info: name.info.clone(),
                        item: crate::lower::Type::Parameter(parameter),
                    })
                    .collect(),
            }),
        ],
        r#type,
    };

    let constructor_body = name.replace(crate::lower::Expression::Trait(info.path.clone()));

    info.constant_declarations.insert(
        constructor_path.clone(),
        WithInfo {
            info: name.info.clone(),
            item: Some(constructor_declaration),
        },
    );

    info.library.items.insert(
        constructor_path.clone(),
        Some(crate::lower::Item {
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
