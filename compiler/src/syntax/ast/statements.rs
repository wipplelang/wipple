use crate::{
    syntax::{
        ast::{
            Info, PartialConstant,
            attribute::attribute,
            expression::expression,
            pattern::pattern,
            r#type::{instance, r#type, type_function},
        },
        parse,
    },
    util::WithInfo,
};

pub fn statements(
    statement_syntaxes: impl IntoIterator<Item = WithInfo<parse::Statement>>,
    info: &mut Info,
) -> Vec<WithInfo<crate::syntax::Statement>> {
    statement_syntaxes
        .into_iter()
        .map(Some)
        .chain(std::iter::once(None))
        .filter_map({
            |statement_syntax| {
                let current_constant = info.current_constant.take();

                macro_rules! expected_constant_value {
                    ($statement_info:expr) => {
                        if let Some(constant) = current_constant.as_ref() {
                            info.errors.push(WithInfo {
                                info: $statement_info.unwrap_or(constant.name.info.clone()),
                                item: crate::syntax::Diagnostic::ExpectedConstantValue(
                                    constant.name.item.clone(),
                                ),
                            });
                        }
                    };
                }

                let disallow_bounds = |bounds: Vec<WithInfo<crate::syntax::Instance>>,
                                       info: &mut Info| {
                    for bound in bounds {
                        info.errors
                            .push(bound.replace(crate::syntax::Diagnostic::UnexpectedBound));
                    }
                };

                match statement_syntax {
                    Some(statement_syntax) => {
                        let mut statement_info = statement_syntax.info.clone();

                        statement_syntax
                            .filter_map(|statement_syntax| match statement_syntax {
                                parse::Statement::Error => None,
                                parse::Statement::SyntaxDeclaration {
                                    attributes: attributes_syntaxes,
                                    name,
                                } => {
                                    let attributes = attributes_syntaxes
                                        .into_iter()
                                        .map(attribute)
                                        .collect();

                                    expected_constant_value!(Some(statement_info.clone()));

                                    Some(crate::syntax::Statement::Syntax {
                                        attributes,
                                        name,
                                    })
                                }
                                parse::Statement::TypeDeclaration {
                                    attributes: attributes_syntaxes,
                                    name,
                                    parameters: type_function_syntax,
                                    representation: representation_syntax,
                                } => {
                                    let attributes = attributes_syntaxes
                                        .into_iter()
                                        .map(attribute)
                                        .collect();

                                    let name = name.try_unwrap()?;

                                    expected_constant_value!(Some(statement_info.clone()));

                                    let (parameters, bounds) =
                                        type_function(type_function_syntax, info);

                                    disallow_bounds(bounds, info);

                                    let representation =
                                        representation_syntax.filter_map(|representation_syntax| {
                                            Some(match representation_syntax {
                                                parse::TypeRepresentation::Marker => {
                                                    crate::syntax::TypeRepresentation::Marker
                                                }
                                                parse::TypeRepresentation::Compound(member_syntaxes) => {
                                                    let mut member_syntaxes = member_syntaxes.into_iter();

                                                    let mut type_representation = match member_syntaxes.next() {
                                                        Some(member_syntax) => {
                                                            let attributes = member_syntax
                                                                .item
                                                                .attributes
                                                                .into_iter()
                                                                .map(attribute)
                                                                .collect::<Vec<_>>();

                                                            let name = match member_syntax.item.name.item {
                                                                Some(name) => WithInfo {
                                                                    info: member_syntax.item.name.info,
                                                                    item: name,
                                                                },
                                                                None => return None,
                                                            };

                                                            match member_syntax.item.kind {
                                                                parse::TypeMemberKind::Error => return None,
                                                                parse::TypeMemberKind::Field(type_syntax) => {
                                                                    crate::syntax::TypeRepresentation::Structure(vec![
                                                                        WithInfo {
                                                                            info: member_syntax.info,
                                                                            item: crate::syntax::Field {
                                                                                attributes,
                                                                                name,
                                                                                r#type: r#type(type_syntax, info),
                                                                            },
                                                                        },
                                                                    ])
                                                                }
                                                                parse::TypeMemberKind::Variant(type_syntaxes) => {
                                                                    crate::syntax::TypeRepresentation::Enumeration(vec![
                                                                        WithInfo {
                                                                            info: member_syntax.info,
                                                                            item: crate::syntax::Variant {
                                                                                attributes,
                                                                                name,
                                                                                types: type_syntaxes
                                                                                    .into_iter()
                                                                                    .map(|type_syntax| {
                                                                                        r#type(type_syntax, info)
                                                                                    })
                                                                                    .collect(),
                                                                            },
                                                                        },
                                                                    ])
                                                                }
                                                            }
                                                        }
                                                        None => {
                                                            info.errors.push(WithInfo {
                                                                info: name.info.clone(),
                                                                item: crate::syntax::Diagnostic::EmptyTypeRepresentation,
                                                            });

                                                            crate::syntax::TypeRepresentation::Marker
                                                        }
                                                    };

                                                    for member_syntax in member_syntaxes {
                                                        let attributes = member_syntax
                                                                .item
                                                                .attributes
                                                                .into_iter()
                                                                .map(attribute)
                                                                .collect::<Vec<_>>();

                                                        let name = match member_syntax.item.name.item {
                                                            Some(name) => WithInfo {
                                                                info: member_syntax.item.name.info,
                                                                item: name,
                                                            },
                                                            None => continue,
                                                        };

                                                        match member_syntax.item.kind {
                                                            parse::TypeMemberKind::Error => continue,
                                                            parse::TypeMemberKind::Field(type_syntax) => {
                                                                let fields = match &mut type_representation {
                                                                    crate::syntax::TypeRepresentation::Structure(
                                                                        fields,
                                                                    ) => fields,
                                                                    crate::syntax::TypeRepresentation::Enumeration(
                                                                        _,
                                                                    ) => {
                                                                        info.errors.push(WithInfo {
                                                                            info: member_syntax.info.clone(),
                                                                            item: crate::syntax::Diagnostic::ExpectedVariant,
                                                                        });

                                                                        continue;
                                                                    }
                                                                    _ => unreachable!(),
                                                                };

                                                                fields.push(WithInfo {
                                                                    info: member_syntax.info,
                                                                    item: crate::syntax::Field {
                                                                        attributes,
                                                                        name,
                                                                        r#type: r#type(type_syntax, info),
                                                                    },
                                                                });
                                                            }
                                                            parse::TypeMemberKind::Variant(type_syntaxes) => {
                                                                let variants = match &mut type_representation {
                                                                    crate::syntax::TypeRepresentation::Structure(_) => {
                                                                        info.errors.push(WithInfo {
                                                                            info: member_syntax.info.clone(),
                                                                            item: crate::syntax::Diagnostic::ExpectedField,
                                                                        });

                                                                        continue;
                                                                    }
                                                                    crate::syntax::TypeRepresentation::Enumeration(
                                                                        variants,
                                                                    ) => variants,
                                                                    _ => unreachable!(),
                                                                };

                                                                variants.push(WithInfo {
                                                                    info: member_syntax.info,
                                                                    item: crate::syntax::Variant {
                                                                        attributes,
                                                                        name,
                                                                        types: type_syntaxes
                                                                            .into_iter()
                                                                            .map(|type_syntax| {
                                                                                r#type(type_syntax, info)
                                                                            })
                                                                            .collect(),
                                                                    },
                                                                });
                                                            }
                                                        }
                                                    }

                                                    type_representation
                                                }
                                                parse::TypeRepresentation::Wrapper(type_syntax) => {
                                                    crate::syntax::TypeRepresentation::Wrapper(r#type(
                                                        type_syntax,
                                                        info,
                                                    ))
                                                }
                                            })
                                        });

                                    representation.map(|representation| crate::syntax::Statement::Type {
                                        attributes,
                                        name,
                                        parameters,
                                        representation,
                                    })
                                }
                                parse::Statement::TraitDeclaration {
                                    attributes: attributes_syntaxes,
                                    name,
                                    parameters: type_function_syntax,
                                    r#type: type_syntax,
                                } => {
                                    let attributes = attributes_syntaxes
                                        .into_iter()
                                        .map(attribute)
                                        .collect();

                                    let name = name.try_unwrap()?;

                                    expected_constant_value!(Some(statement_info.clone()));

                                    let (parameters, bounds) =
                                        type_function(type_function_syntax, info);

                                    disallow_bounds(bounds, info);

                                    let r#type = type_syntax.map(|type_syntax| r#type(type_syntax, info));

                                    Some(crate::syntax::Statement::Trait {
                                        attributes,
                                        name,
                                        parameters,
                                        r#type,
                                    })
                                }
                                parse::Statement::DefaultInstanceDeclaration {
                                    pattern: pattern_syntax,
                                    parameters: type_function_syntax,
                                    instance: instance_syntax,
                                    body: body_syntax,
                                } => {
                                    let instance_syntax = instance_syntax.try_unwrap()?;

                                    expected_constant_value!(Some(statement_info.clone()));

                                    let (parameters, bounds) =
                                        type_function(type_function_syntax, info);

                                    let instance = instance(instance_syntax, info);

                                    let body = body_syntax.map(|body_syntax| expression(body_syntax, info));

                                    Some(crate::syntax::Statement::Instance {
                                        pattern: pattern_syntax,
                                        parameters,
                                        bounds,
                                        instance,
                                        body,
                                        default: true,
                                    })
                                }
                                parse::Statement::InstanceDeclaration {
                                    pattern: pattern_syntax,
                                    parameters: type_function_syntax,
                                    instance: instance_syntax,
                                    body: body_syntax,
                                } => {
                                    let instance_syntax = instance_syntax.try_unwrap()?;

                                    expected_constant_value!(Some(statement_info.clone()));

                                    let (parameters, bounds) =
                                        type_function(type_function_syntax, info);

                                    let instance = instance(instance_syntax, info);

                                    let body = body_syntax.map(|body_syntax| expression(body_syntax, info));

                                    Some(crate::syntax::Statement::Instance {
                                        pattern: pattern_syntax,
                                        parameters,
                                        bounds,
                                        instance,
                                        body,
                                        default: false,
                                    })
                                }
                                parse::Statement::ConstantDeclaration {
                                    attributes: attributes_syntaxes,
                                    name,
                                    parameters: type_function_syntax,
                                    r#type: type_syntax,
                                } => {
                                    let attributes = attributes_syntaxes
                                        .into_iter()
                                        .map(attribute)
                                        .collect();

                                    let name = name.try_unwrap()?;

                                    expected_constant_value!(Some(statement_info.clone()));

                                    let (parameters, bounds) =
                                        type_function(type_function_syntax, info);

                                    let r#type = r#type(type_syntax, info);

                                    info.current_constant = Some(PartialConstant {
                                        info: statement_info.clone(),
                                        attributes,
                                        name,
                                        parameters,
                                        bounds,
                                        r#type,
                                    });

                                    None
                                }
                                parse::Statement::Assignment {
                                    pattern: pattern_syntax,
                                    value: value_syntax,
                                } => {
                                    let value = expression(value_syntax, info);

                                    if let Some(constant) = current_constant {
                                        if let parse::Pattern::VariantOrName(name) =
                                            &pattern_syntax.item
                                        {
                                            if name.item.as_ref().is_some_and(|name| *name == constant.name.item) {
                                                statement_info = constant.info;

                                                return Some(crate::syntax::Statement::Constant {
                                                    attributes: constant.attributes,
                                                    name: constant.name,
                                                    parameters: constant.parameters,
                                                    bounds: constant.bounds,
                                                    r#type: constant.r#type,
                                                    body: value,
                                                });
                                            }
                                        }

                                        info.errors.push(WithInfo {
                                            info: statement_info.clone(),
                                            item: crate::syntax::Diagnostic::ExpectedConstantValue(
                                                constant.name.item,
                                            ),
                                        });
                                    }

                                    let pattern = pattern(pattern_syntax, info);

                                    Some(crate::syntax::Statement::Assignment { pattern, value })
                                }
                                parse::Statement::Expression(expression_syntax) => {
                                    expected_constant_value!(Some(statement_info.clone()));

                                    let expression = expression(expression_syntax, info);

                                    Some(crate::syntax::Statement::Expression(expression))
                                }
                            })
                            .map(|statement_syntax| WithInfo {
                                info: statement_info,
                                item: statement_syntax.item,
                            })
                    }
                    None => {
                        expected_constant_value!(None);

                        None
                    }
                }
            }
        })
        .collect()
}
