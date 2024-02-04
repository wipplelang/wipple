use crate::{syntax, Driver};
use derivative::Derivative;
use wipple_util::WithInfo;

pub fn top_level<D: Driver>(
    top_level_syntax: WithInfo<D::Info, syntax::TopLevel<D>>,
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

fn expression<D: Driver>(
    expression_syntax: WithInfo<D::Info, syntax::Expression<D>>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::Expression<D>> {
    expression_syntax.map(|expression_syntax| match expression_syntax {
        syntax::Expression::Error => crate::Expression::Error,
        syntax::Expression::Annotate {
            value: value_syntax,
            r#type: type_syntax,
        } => crate::Expression::Annotate {
            value: expression(value_syntax.unboxed(), info).boxed(),
            r#type: r#type(type_syntax, info),
        },
        syntax::Expression::Name(name) => crate::Expression::Name(name),
        syntax::Expression::Number(number) => crate::Expression::Number(number),
        syntax::Expression::Text(text) => crate::Expression::Text(text),
        syntax::Expression::Unit => crate::Expression::Tuple(Vec::new()),
        syntax::Expression::Format { text, inputs } => {
            let result = crate::text::parse_format_expression(&text, inputs, &mut info.errors);

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
        }
        syntax::Expression::Block(statement_syntaxes) => {
            crate::Expression::Block(statements(statement_syntaxes, info))
        }
        syntax::Expression::Function {
            pattern: pattern_syntax,
            body: body_syntax,
        } => crate::Expression::Function {
            pattern: pattern(pattern_syntax, info),
            body: expression(body_syntax.unboxed(), info).boxed(),
        },
        syntax::Expression::Call {
            function: function_syntax,
            input: input_syntax,
        } => crate::Expression::Call {
            function: expression(function_syntax.unboxed(), info).boxed(),
            input: expression(input_syntax.unboxed(), info).boxed(),
        },
        syntax::Expression::Apply {
            input: input_syntax,
            function: function_syntax,
        } => crate::Expression::Apply {
            input: input_syntax
                .map(|input_syntax| expression(input_syntax.unboxed(), info).boxed()),
            function: function_syntax
                .map(|function_syntax| expression(function_syntax.unboxed(), info).boxed()),
        },
        syntax::Expression::Compose {
            outer: outer_syntax,
            inner: inner_syntax,
        } => crate::Expression::Compose {
            outer: outer_syntax
                .map(|outer_syntax| expression(outer_syntax.unboxed(), info).boxed()),
            inner: inner_syntax
                .map(|inner_syntax| expression(inner_syntax.unboxed(), info).boxed()),
        },
        syntax::Expression::BinaryOperator {
            operator,
            left: left_syntax,
            right: right_syntax,
        } => crate::Expression::BinaryOperator {
            operator,
            left: left_syntax.map(|left_syntax| expression(left_syntax.unboxed(), info).boxed()),
            right: right_syntax
                .map(|right_syntax| expression(right_syntax.unboxed(), info).boxed()),
        },
        syntax::Expression::As {
            value: value_syntax,
            r#type: type_syntax,
        } => crate::Expression::As {
            value: value_syntax
                .map(|value_syntax| expression(value_syntax.unboxed(), info).boxed()),
            r#type: r#type(type_syntax, info),
        },
        syntax::Expression::Is {
            value: value_syntax,
            pattern: pattern_syntax,
        } => crate::Expression::Is {
            value: value_syntax
                .map(|value_syntax| expression(value_syntax.unboxed(), info).boxed()),
            pattern: pattern(pattern_syntax, info),
        },
        syntax::Expression::When {
            input,
            arms: arm_syntaxes,
        } => crate::Expression::When {
            input: expression(input.unboxed(), info).boxed(),
            arms: arm_syntaxes
                .into_iter()
                .map(|arm_syntax| arm(arm_syntax, info))
                .collect(),
        },
        syntax::Expression::Intrinsic {
            name,
            inputs: input_syntaxes,
        } => crate::Expression::Intrinsic {
            name,
            inputs: input_syntaxes
                .into_iter()
                .map(|input| expression(input, info))
                .collect(),
        },
        syntax::Expression::Tuple(element_syntaxes) => crate::Expression::Tuple(
            element_syntaxes
                .into_iter()
                .map(|element_syntax| expression(element_syntax, info))
                .collect(),
        ),
        syntax::Expression::Collection(element_syntaxes) => crate::Expression::Collection(
            element_syntaxes
                .into_iter()
                .map(|element_syntax| expression(element_syntax, info))
                .collect(),
        ),
        syntax::Expression::Semantics { name, body } => crate::Expression::Semantics {
            name,
            body: expression(body.unboxed(), info).boxed(),
        },
        syntax::Expression::Structure(fields) => crate::Expression::Structure(
            fields
                .into_iter()
                .map(|field| {
                    field.map(|field| crate::FieldValue {
                        name: field.name,
                        value: expression(field.value, info),
                    })
                })
                .collect(),
        ),
    })
}

fn statements<D: Driver>(
    statement_syntaxes: impl IntoIterator<Item = WithInfo<D::Info, syntax::Statement<D>>>,
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
                                syntax::Statement::Error => None,
                                syntax::Statement::TypeDeclaration {
                                    name,
                                    parameters: type_function_syntax,
                                    representation: representation_syntax,
                                } => {
                                    expected_constant_value!(Some(statement_info.clone()));

                                    let (parameters, bounds) =
                                        type_function(type_function_syntax, info);

                                    disallow_bounds(bounds, info);

                                    let representation =
                                            representation_syntax.map(|representation_syntax| {
                                                match representation_syntax {
                                    syntax::TypeRepresentation::Opaque => {
                                        crate::TypeRepresentation::Opaque
                                    }
                                    syntax::TypeRepresentation::SimpleEnumeration(variants) => {
                                        crate::TypeRepresentation::Enumeration(
                                            variants
                                                .into_iter()
                                                .map(|name| WithInfo {
                                                    info: name.info.clone(),
                                                    item: crate::Variant {
                                                        name,
                                                        types: Vec::new(),
                                                    },
                                                })
                                                .collect(),
                                        )
                                    }
                                    syntax::TypeRepresentation::Compound(member_syntaxes) => {
                                        let mut member_syntaxes = member_syntaxes.into_iter();

                                        let mut type_representation = match member_syntaxes.next() {
                                            Some(member_syntax) => match member_syntax.item.kind {
                                                syntax::TypeMemberKind::Field(type_syntax) => {
                                                    crate::TypeRepresentation::Structure(vec![
                                                        WithInfo {
                                                            info: member_syntax.info,
                                                            item: crate::Field {
                                                                name: member_syntax.item.name,
                                                                r#type: r#type(type_syntax, info),
                                                            },
                                                        },
                                                    ])
                                                }
                                                syntax::TypeMemberKind::Variant(type_syntaxes) => {
                                                    crate::TypeRepresentation::Enumeration(vec![
                                                        WithInfo {
                                                            info: member_syntax.info,
                                                            item: crate::Variant {
                                                                name: member_syntax.item.name,
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
                                            },
                                            None => {
                                                info.errors.push(WithInfo {
                                                    info: name.info.clone(),
                                                    item: crate::Diagnostic::EmptyTypeRepresentation,
                                                });

                                                crate::TypeRepresentation::Opaque
                                            }
                                        };

                                        for member_syntax in member_syntaxes {
                                            match member_syntax.item.kind {
                                                syntax::TypeMemberKind::Field(type_syntax) => {
                                                    let fields = match &mut type_representation {
                                                        crate::TypeRepresentation::Opaque => {
                                                            unreachable!()
                                                        }
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
                                                    };

                                                    fields.push(WithInfo {
                                                        info: member_syntax.info,
                                                        item: crate::Field {
                                                            name: member_syntax.item.name,
                                                            r#type: r#type(type_syntax, info),
                                                        },
                                                    });
                                                }
                                                syntax::TypeMemberKind::Variant(type_syntaxes) => {
                                                    let variants = match &mut type_representation {
                                                        crate::TypeRepresentation::Opaque => {
                                                            unreachable!()
                                                        }
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
                                                    };

                                                    variants.push(WithInfo {
                                                        info: member_syntax.info,
                                                        item: crate::Variant {
                                                            name: member_syntax.item.name,
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
                                }
                                            });

                                    Some(crate::Statement::Type {
                                        name,
                                        parameters,
                                        representation,
                                    })
                                }
                                syntax::Statement::TraitDeclaration {
                                    name,
                                    parameters: type_function_syntax,
                                    r#type: type_syntax,
                                } => {
                                    expected_constant_value!(Some(statement_info.clone()));

                                    let (parameters, bounds) =
                                        type_function(type_function_syntax, info);

                                    disallow_bounds(bounds, info);

                                    let r#type = r#type(type_syntax, info);

                                    Some(crate::Statement::Trait {
                                        name,
                                        parameters,
                                        r#type,
                                    })
                                }
                                syntax::Statement::InstanceDeclaration {
                                    parameters: type_function_syntax,
                                    instance: instance_syntax,
                                    body: body_syntax,
                                } => {
                                    expected_constant_value!(Some(statement_info.clone()));

                                    let (parameters, bounds) =
                                        type_function(type_function_syntax, info);

                                    let instance = instance(instance_syntax, info);

                                    let body = expression(body_syntax, info);

                                    Some(crate::Statement::Instance {
                                        parameters,
                                        bounds,
                                        instance,
                                        body,
                                    })
                                }
                                syntax::Statement::ConstantDeclaration {
                                    name,
                                    parameters: type_function_syntax,
                                    r#type: type_syntax,
                                } => {
                                    expected_constant_value!(Some(statement_info.clone()));

                                    let (parameters, bounds) =
                                        type_function(type_function_syntax, info);

                                    let r#type = r#type(type_syntax, info);

                                    info.current_constant = Some(PartialConstant {
                                        info: statement_info.clone(),
                                        name,
                                        parameters,
                                        bounds,
                                        r#type,
                                    });

                                    None
                                }
                                syntax::Statement::LanguageDeclaration { item, kind, name } => {
                                    expected_constant_value!(Some(statement_info.clone()));

                                    let kind = kind.map(|kind| match kind {
                                        syntax::LanguageDeclarationKind::Type => {
                                            crate::LanguageDeclarationKind::Type
                                        }
                                        syntax::LanguageDeclarationKind::Trait => {
                                            crate::LanguageDeclarationKind::Trait
                                        }
                                        syntax::LanguageDeclarationKind::Constant => {
                                            crate::LanguageDeclarationKind::Constant
                                        }
                                    });

                                    Some(crate::Statement::Language { item, kind, name })
                                }
                                syntax::Statement::Assignment {
                                    pattern: pattern_syntax,
                                    value: value_syntax,
                                } => {
                                    let value = expression(value_syntax, info);

                                    if let Some(constant) = current_constant {
                                        if let syntax::Pattern::VariantOrName(name) =
                                            &pattern_syntax.item
                                        {
                                            if *name == constant.name.item {
                                                statement_info = constant.info;

                                                return Some(crate::Statement::Constant {
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
                                syntax::Statement::Expression(expression_syntax) => {
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
    pattern_syntax: WithInfo<D::Info, syntax::Pattern<D>>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::Pattern<D>> {
    pattern_syntax.map(|pattern_syntax| match pattern_syntax {
        syntax::Pattern::Error => crate::Pattern::Error,
        syntax::Pattern::Wildcard => crate::Pattern::Wildcard,
        syntax::Pattern::Unit => crate::Pattern::Tuple(Vec::new()),
        syntax::Pattern::Number(num) => crate::Pattern::Number(num),
        syntax::Pattern::Text(text) => crate::Pattern::Text(text),
        syntax::Pattern::Name(name) => crate::Pattern::Name(name),
        syntax::Pattern::VariantOrName(variant) => crate::Pattern::VariantOrName(variant),
        syntax::Pattern::Destructure(field_syntaxes) => crate::Pattern::Destructure(
            field_syntaxes
                .into_iter()
                .map(|field_pattern_syntax| field_pattern(field_pattern_syntax, info))
                .collect(),
        ),
        syntax::Pattern::Variant {
            variant,
            value_patterns: value_syntaxes,
        } => crate::Pattern::Variant {
            variant,
            value_patterns: value_syntaxes
                .into_iter()
                .map(|pattern_syntax| pattern(pattern_syntax, info))
                .collect(),
        },
        syntax::Pattern::Tuple(pattern_syntaxes) => crate::Pattern::Tuple(
            pattern_syntaxes
                .into_iter()
                .map(|pattern_syntax| pattern(pattern_syntax, info))
                .collect(),
        ),
        syntax::Pattern::Or {
            left: left_syntax,
            right: right_syntax,
        } => crate::Pattern::Or {
            left: pattern(left_syntax.unboxed(), info).boxed(),
            right: pattern(right_syntax.unboxed(), info).boxed(),
        },
    })
}

#[allow(clippy::only_used_in_recursion)]
fn r#type<D: Driver>(
    type_syntax: WithInfo<D::Info, syntax::Type<D>>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::Type<D>> {
    type_syntax.map(|type_syntax| match type_syntax {
        syntax::Type::Error => crate::Type::Error,
        syntax::Type::Placeholder => crate::Type::Placeholder,
        syntax::Type::Unit => crate::Type::Tuple(Vec::new()),
        syntax::Type::Declared {
            name,
            parameters: parameter_syntaxes,
        } => crate::Type::Declared {
            name,
            parameters: parameter_syntaxes
                .into_iter()
                .map(|parameter_syntax| r#type(parameter_syntax, info))
                .collect(),
        },
        syntax::Type::Function { input, output } => crate::Type::Function {
            input: r#type(input.unboxed(), info).boxed(),
            output: r#type(output.unboxed(), info).boxed(),
        },
        syntax::Type::Tuple(types) => crate::Type::Tuple(
            types
                .into_iter()
                .map(|type_syntax| r#type(type_syntax, info))
                .collect(),
        ),
        syntax::Type::Deferred(type_syntax) => {
            crate::Type::Deferred(r#type(type_syntax.unboxed(), info).boxed())
        }
    })
}

fn type_function<D: Driver>(
    type_function_syntax: WithInfo<D::Info, syntax::TypeFunction<D>>,
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
    type_parameter_syntax: WithInfo<D::Info, syntax::TypeParameter<D>>,
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
    instance_syntax: WithInfo<D::Info, syntax::Instance<D>>,
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
    field_pattern_syntax: WithInfo<D::Info, syntax::FieldPattern<D>>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::FieldPattern<D>> {
    field_pattern_syntax.map(|field_pattern_syntax| crate::FieldPattern {
        name: field_pattern_syntax.name,
        pattern: pattern(field_pattern_syntax.pattern, info),
    })
}

fn arm<D: Driver>(
    arm_syntax: WithInfo<D::Info, syntax::Arm<D>>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::Arm<D>> {
    arm_syntax.map(|arm_syntax| crate::Arm {
        pattern: pattern(arm_syntax.pattern, info),
        condition: arm_syntax
            .condition
            .map(|condition_syntax| expression(condition_syntax, info)),
        body: expression(arm_syntax.body, info),
    })
}
