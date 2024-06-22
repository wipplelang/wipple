//! Construct an abstract syntax tree from the concrete syntax tree generated
//! by [`parse`].

use crate::{parse, Driver};
use derivative::Derivative;
use wipple_util::WithInfo;

/// Convert a [`parse::TopLevel`] into a [`crate::TopLevel`].
pub fn top_level<D: Driver>(
    top_level_syntax: WithInfo<D::Info, parse::TopLevel<D>>,
) -> crate::Result<D> {
    let mut info = Info::default();

    let top_level = top_level_syntax.map(|top_level_syntax| crate::TopLevel {
        statements: statements(top_level_syntax.statements, &mut info),
    });

    crate::Result {
        top_level,
        diagnostics: info.errors,
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct PartialConstant<D: Driver> {
    info: D::Info,
    attributes: Vec<WithInfo<D::Info, crate::Attribute<D>>>,
    name: WithInfo<D::Info, String>,
    parameters: Vec<WithInfo<D::Info, crate::TypeParameter<D>>>,
    bounds: Vec<WithInfo<D::Info, crate::Instance<D>>>,
    r#type: WithInfo<D::Info, crate::Type<D>>,
}

struct Info<D: Driver> {
    errors: Vec<WithInfo<D::Info, crate::Diagnostic>>,
    current_constant: Option<PartialConstant<D>>,
}

impl<D: Driver> Default for Info<D> {
    fn default() -> Self {
        Info {
            errors: Default::default(),
            current_constant: Default::default(),
        }
    }
}

fn attribute<D: Driver>(
    attribute_syntax: WithInfo<D::Info, parse::Attribute<D>>,
) -> WithInfo<D::Info, crate::Attribute<D>> {
    attribute_syntax.map(|attribute_syntax| match attribute_syntax {
        parse::Attribute::Error => crate::Attribute::Error,
        parse::Attribute::Name(name) => crate::Attribute::Name(name),
        parse::Attribute::Valued { name, value } => crate::Attribute::Valued {
            name,
            value: attribute_value(value),
        },
    })
}

fn attribute_value<D: Driver>(
    attribute_value_syntax: WithInfo<D::Info, parse::AttributeValue<D>>,
) -> WithInfo<D::Info, crate::AttributeValue<D>> {
    attribute_value_syntax.map(|attribute_value_syntax| match attribute_value_syntax {
        parse::AttributeValue::Error => crate::AttributeValue::Error,
        parse::AttributeValue::Name(name) => crate::AttributeValue::Name(name),
        parse::AttributeValue::Text(text) => crate::AttributeValue::Text(text),
        parse::AttributeValue::Number(number) => crate::AttributeValue::Number(number),
    })
}

fn expression<D: Driver>(
    expression_syntax: WithInfo<D::Info, parse::Expression<D>>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::Expression<D>> {
    expression_syntax.map(|expression_syntax| match expression_syntax {
        parse::Expression::Error => crate::Expression::Error,
        parse::Expression::Annotate {
            value: value_syntax,
            r#type: type_syntax,
        } => crate::Expression::Annotate {
            value: expression(value_syntax.unboxed(), info).boxed(),
            r#type: r#type(type_syntax, info),
        },
        parse::Expression::Name(name) => crate::Expression::Name(name),
        parse::Expression::Number(number) => crate::Expression::Number(number),
        parse::Expression::Text(text) => crate::Expression::Text(text),
        parse::Expression::Unit => crate::Expression::Tuple(Vec::new()),
        parse::Expression::Block(statement_syntaxes) => {
            crate::Expression::Block(statements(statement_syntaxes, info))
        }
        parse::Expression::Do(block_syntax) => {
            crate::Expression::Do(expression(block_syntax.unboxed(), info).boxed())
        }
        parse::Expression::Function {
            inputs: input_syntaxes,
            body: body_syntax,
        } => crate::Expression::Function {
            inputs: input_syntaxes
                .into_iter()
                .map(|input_syntax| pattern(input_syntax, info))
                .collect(),
            body: expression(body_syntax.unboxed(), info).boxed(),
        },
        parse::Expression::Call {
            function: function_syntax,
            inputs: input_syntaxes,
        } => {
            if let parse::Expression::Text(text) = function_syntax.as_deref().item {
                let result = crate::text::parse_format_expression::<D, _>(
                    WithInfo {
                        info: function_syntax.info.clone(),
                        item: text,
                    },
                    input_syntaxes,
                    &mut info.errors,
                );

                crate::Expression::Format {
                    segments: result
                        .segments
                        .into_iter()
                        .map(|segment| crate::FormatSegment {
                            text: segment.text,
                            value: expression(segment.value, info),
                        })
                        .collect(),
                    trailing: result.trailing,
                }
            } else {
                crate::Expression::Call {
                    function: expression(function_syntax.unboxed(), info).boxed(),
                    inputs: input_syntaxes
                        .into_iter()
                        .map(|input_syntax| expression(input_syntax, info))
                        .collect(),
                }
            }
        }
        parse::Expression::Apply {
            input: input_syntax,
            function: function_syntax,
        } => crate::Expression::Apply {
            input: expression(input_syntax.unboxed(), info).boxed(),
            function: expression(function_syntax.unboxed(), info).boxed(),
        },
        parse::Expression::BinaryOperator {
            operator,
            left: left_syntax,
            right: right_syntax,
        } => crate::Expression::BinaryOperator {
            operator,
            left: expression(left_syntax.unboxed(), info).boxed(),
            right: expression(right_syntax.unboxed(), info).boxed(),
        },
        parse::Expression::As {
            value: value_syntax,
            r#type: type_syntax,
        } => crate::Expression::As {
            value: expression(value_syntax.unboxed(), info).boxed(),
            r#type: r#type(type_syntax, info),
        },
        parse::Expression::Is {
            value: value_syntax,
            pattern: pattern_syntax,
        } => crate::Expression::Is {
            value: expression(value_syntax.unboxed(), info).boxed(),
            pattern: pattern(pattern_syntax, info),
        },
        parse::Expression::When {
            input,
            arms: arm_syntaxes,
        } => crate::Expression::When {
            input: expression(input.unboxed(), info).boxed(),
            arms: arm_syntaxes
                .into_iter()
                .map(|arm_syntax| arm(arm_syntax, info))
                .collect(),
        },
        parse::Expression::Intrinsic {
            name,
            inputs: input_syntaxes,
        } => crate::Expression::Intrinsic {
            name,
            inputs: input_syntaxes
                .into_iter()
                .map(|input| expression(input, info))
                .collect(),
        },
        parse::Expression::Tuple(element_syntaxes) => crate::Expression::Tuple(
            element_syntaxes
                .into_iter()
                .map(|element_syntax| expression(element_syntax, info))
                .collect(),
        ),
        parse::Expression::Collection(element_syntaxes) => crate::Expression::Collection(
            element_syntaxes
                .into_iter()
                .map(|element_syntax| expression(element_syntax, info))
                .collect(),
        ),
        parse::Expression::Structure(fields) => {
            // HACK: Actually parse an empty block as an expression
            if fields.is_empty() {
                crate::Expression::Block(Vec::new())
            } else {
                crate::Expression::Structure(
                    fields
                        .into_iter()
                        .map(|field| {
                            field.map(|field| crate::FieldValue {
                                name: field.name,
                                value: expression(field.value, info),
                            })
                        })
                        .collect(),
                )
            }
        }
    })
}

fn statements<D: Driver>(
    statement_syntaxes: impl IntoIterator<Item = WithInfo<D::Info, parse::Statement<D>>>,
    info: &mut Info<D>,
) -> Vec<WithInfo<D::Info, crate::Statement<D>>> {
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
                                item: crate::Diagnostic::ExpectedConstantValue(
                                    constant.name.item.clone(),
                                ),
                            });
                        }
                    };
                }

                let disallow_bounds = |bounds: Vec<WithInfo<D::Info, crate::Instance<D>>>,
                                       info: &mut Info<D>| {
                    for bound in bounds {
                        info.errors
                            .push(bound.replace(crate::Diagnostic::UnexpectedBound));
                    }
                };

                match statement_syntax {
                    Some(statement_syntax) => {
                        let mut statement_info = statement_syntax.info.clone();

                        statement_syntax
                            .filter_map(|statement_syntax| match statement_syntax {
                                parse::Statement::Error => None,
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
                                                    crate::TypeRepresentation::Marker
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
                                                                    crate::TypeRepresentation::Structure(vec![
                                                                        WithInfo {
                                                                            info: member_syntax.info,
                                                                            item: crate::Field {
                                                                                attributes,
                                                                                name,
                                                                                r#type: r#type(type_syntax, info),
                                                                            },
                                                                        },
                                                                    ])
                                                                }
                                                                parse::TypeMemberKind::Variant(type_syntaxes) => {
                                                                    crate::TypeRepresentation::Enumeration(vec![
                                                                        WithInfo {
                                                                            info: member_syntax.info,
                                                                            item: crate::Variant {
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
                                                                item: crate::Diagnostic::EmptyTypeRepresentation,
                                                            });

                                                            crate::TypeRepresentation::Marker
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
                                                                    crate::TypeRepresentation::Structure(
                                                                        fields,
                                                                    ) => fields,
                                                                    crate::TypeRepresentation::Enumeration(
                                                                        _,
                                                                    ) => {
                                                                        info.errors.push(WithInfo {
                                                                            info: member_syntax.info.clone(),
                                                                            item: crate::Diagnostic::ExpectedVariant,
                                                                        });

                                                                        continue;
                                                                    }
                                                                    _ => unreachable!(),
                                                                };

                                                                fields.push(WithInfo {
                                                                    info: member_syntax.info,
                                                                    item: crate::Field {
                                                                        attributes,
                                                                        name,
                                                                        r#type: r#type(type_syntax, info),
                                                                    },
                                                                });
                                                            }
                                                            parse::TypeMemberKind::Variant(type_syntaxes) => {
                                                                let variants = match &mut type_representation {
                                                                    crate::TypeRepresentation::Structure(_) => {
                                                                        info.errors.push(WithInfo {
                                                                            info: member_syntax.info.clone(),
                                                                            item: crate::Diagnostic::ExpectedField,
                                                                        });

                                                                        continue;
                                                                    }
                                                                    crate::TypeRepresentation::Enumeration(
                                                                        variants,
                                                                    ) => variants,
                                                                    _ => unreachable!(),
                                                                };

                                                                variants.push(WithInfo {
                                                                    info: member_syntax.info,
                                                                    item: crate::Variant {
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
                                                    crate::TypeRepresentation::Wrapper(r#type(
                                                        type_syntax,
                                                        info,
                                                    ))
                                                }
                                            })
                                        });

                                    representation.map(|representation| crate::Statement::Type {
                                        attributes,
                                        name,
                                        parameters,
                                        representation,
                                    })
                                }
                                parse::Statement::TypeAliasDeclaration {
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

                                    let r#type = r#type(type_syntax, info);

                                    Some(crate::Statement::TypeAlias {
                                        attributes,
                                        name,
                                        parameters,
                                        r#type,
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

                                    Some(crate::Statement::Trait {
                                        attributes,
                                        name,
                                        parameters,
                                        r#type,
                                    })
                                }
                                parse::Statement::DefaultInstanceDeclaration {
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

                                    Some(crate::Statement::Instance {
                                        parameters,
                                        bounds,
                                        instance,
                                        body,
                                        default: true,
                                    })
                                }
                                parse::Statement::InstanceDeclaration {
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

                                    Some(crate::Statement::Instance {
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

                                                return Some(crate::Statement::Constant {
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
                                            item: crate::Diagnostic::ExpectedConstantValue(
                                                constant.name.item,
                                            ),
                                        });
                                    }

                                    let pattern = pattern(pattern_syntax, info);

                                    Some(crate::Statement::Assignment { pattern, value })
                                }
                                parse::Statement::Expression(expression_syntax) => {
                                    expected_constant_value!(Some(statement_info.clone()));

                                    let expression = expression(expression_syntax, info);

                                    Some(crate::Statement::Expression(expression))
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

fn pattern<D: Driver>(
    pattern_syntax: WithInfo<D::Info, parse::Pattern<D>>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::Pattern<D>> {
    pattern_syntax.map(|pattern_syntax| match pattern_syntax {
        parse::Pattern::Error => crate::Pattern::Error,
        parse::Pattern::Wildcard => crate::Pattern::Wildcard,
        parse::Pattern::Unit => crate::Pattern::Tuple(Vec::new()),
        parse::Pattern::Number(num) => crate::Pattern::Number(num),
        parse::Pattern::Text(text) => crate::Pattern::Text(text),
        parse::Pattern::Name(name) => crate::Pattern::Name(name),
        parse::Pattern::VariantOrName(variant) => crate::Pattern::VariantOrName(variant),
        parse::Pattern::Destructure(field_syntaxes) => crate::Pattern::Destructure(
            field_syntaxes
                .into_iter()
                .map(|field_pattern_syntax| field_pattern(field_pattern_syntax, info))
                .collect(),
        ),
        parse::Pattern::Variant {
            variant,
            value_patterns: value_syntaxes,
        } => crate::Pattern::Variant {
            variant,
            value_patterns: value_syntaxes
                .into_iter()
                .map(|pattern_syntax| pattern(pattern_syntax, info))
                .collect(),
        },
        parse::Pattern::Tuple(pattern_syntaxes) => crate::Pattern::Tuple(
            pattern_syntaxes
                .into_iter()
                .map(|pattern_syntax| pattern(pattern_syntax, info))
                .collect(),
        ),
        parse::Pattern::Or {
            left: left_syntax,
            right: right_syntax,
        } => crate::Pattern::Or {
            left: pattern(left_syntax.unboxed(), info).boxed(),
            right: pattern(right_syntax.unboxed(), info).boxed(),
        },
        parse::Pattern::Mutate(name) => crate::Pattern::Mutate(name),
        parse::Pattern::Annotate {
            pattern: pattern_syntax,
            r#type: type_syntax,
        } => crate::Pattern::Annotate {
            pattern: pattern(pattern_syntax.unboxed(), info).boxed(),
            r#type: r#type(type_syntax, info),
        },
    })
}

#[allow(clippy::only_used_in_recursion)]
fn r#type<D: Driver>(
    type_syntax: WithInfo<D::Info, parse::Type<D>>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::Type<D>> {
    type_syntax.map(|type_syntax| match type_syntax {
        parse::Type::Error => crate::Type::Error,
        parse::Type::Placeholder => crate::Type::Placeholder,
        parse::Type::Unit => crate::Type::Tuple(Vec::new()),
        parse::Type::Declared {
            name,
            parameters: parameter_syntaxes,
        } => crate::Type::Declared {
            name,
            parameters: parameter_syntaxes
                .into_iter()
                .map(|parameter_syntax| r#type(parameter_syntax, info))
                .collect(),
        },
        parse::Type::Function { inputs, output } => crate::Type::Function {
            inputs: inputs
                .into_iter()
                .map(|input_syntax| r#type(input_syntax, info))
                .collect(),
            output: r#type(output.unboxed(), info).boxed(),
        },
        parse::Type::Tuple(types) => crate::Type::Tuple(
            types
                .into_iter()
                .map(|type_syntax| r#type(type_syntax, info))
                .collect(),
        ),
        parse::Type::Block(type_syntax) => {
            crate::Type::Block(r#type(type_syntax.unboxed(), info).boxed())
        }
        parse::Type::Intrinsic => crate::Type::Intrinsic,
        parse::Type::Message {
            message,
            inputs: input_syntaxes,
        } => {
            let result = crate::text::parse_format_expression::<D, _>(
                message.as_deref(),
                input_syntaxes,
                &mut info.errors,
            );

            crate::Type::Message {
                segments: result
                    .segments
                    .into_iter()
                    .map(|segment| crate::FormatSegment {
                        text: segment.text,
                        value: r#type(segment.value, info),
                    })
                    .collect(),
                trailing: result.trailing,
            }
        }
        parse::Type::Equal {
            left: left_syntax,
            right: right_syntax,
        } => crate::Type::Equal {
            left: r#type(left_syntax.unboxed(), info).boxed(),
            right: r#type(right_syntax.unboxed(), info).boxed(),
        },
    })
}

fn type_function<D: Driver>(
    type_function_syntax: WithInfo<D::Info, parse::TypeFunction<D>>,
    info: &mut Info<D>,
) -> (
    Vec<WithInfo<D::Info, crate::TypeParameter<D>>>,
    Vec<WithInfo<D::Info, crate::Instance<D>>>,
) {
    let parameters = type_function_syntax.item.parameters;
    let bounds = type_function_syntax.item.bounds;

    (
        parameters
            .into_iter()
            .map(|type_parameter_syntax| type_parameter(type_parameter_syntax, info))
            .collect(),
        bounds
            .into_iter()
            .map(|instance_syntax| instance(instance_syntax, info))
            .collect(),
    )
}

fn type_parameter<D: Driver>(
    type_parameter_syntax: WithInfo<D::Info, parse::TypeParameter<D>>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::TypeParameter<D>> {
    type_parameter_syntax.map(|type_parameter_syntax| crate::TypeParameter {
        name: type_parameter_syntax.name,
        infer: type_parameter_syntax.infer,
        default: type_parameter_syntax
            .default
            .map(|type_syntax| r#type(type_syntax, info)),
    })
}

fn instance<D: Driver>(
    instance_syntax: WithInfo<D::Info, parse::Instance<D>>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::Instance<D>> {
    instance_syntax.map(|instance_syntax| crate::Instance {
        r#trait: instance_syntax.r#trait,
        parameters: instance_syntax
            .parameters
            .into_iter()
            .map(|type_syntax| r#type(type_syntax, info))
            .collect(),
    })
}

fn field_pattern<D: Driver>(
    field_pattern_syntax: WithInfo<D::Info, parse::FieldPattern<D>>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::FieldPattern<D>> {
    field_pattern_syntax.map(|field_pattern_syntax| crate::FieldPattern {
        name: field_pattern_syntax.name,
        pattern: pattern(field_pattern_syntax.pattern, info),
    })
}

fn arm<D: Driver>(
    arm_syntax: WithInfo<D::Info, parse::Arm<D>>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::Arm<D>> {
    arm_syntax.map(|arm_syntax| crate::Arm {
        pattern: pattern(arm_syntax.pattern, info),
        body: expression(arm_syntax.body, info),
    })
}
