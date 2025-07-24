use crate::{
    lower::{
        language::resolve_language_item,
        name::resolve_name,
        operator::resolve_binary_operator,
        pattern::resolve_pattern,
        resolve::Info,
        scope::Captures,
        statements::{resolve_statements, split_executable_statements},
        r#type::resolve_type,
    },
    util::WithInfo,
};

pub fn resolve_expression(
    expression: WithInfo<crate::lower::UnresolvedExpression>,
    info: &mut Info,
) -> WithInfo<crate::lower::Expression> {
    let expression_info = expression.info.clone();

    expression.map(|expression| match expression {
        crate::lower::UnresolvedExpression::Error => crate::lower::Expression::Error,
        crate::lower::UnresolvedExpression::Annotate { value, r#type } => {
            crate::lower::Expression::Annotate {
                value: resolve_expression(value.unboxed(), info).boxed(),
                r#type: resolve_type(r#type, info),
            }
        }
        crate::lower::UnresolvedExpression::Name(name) => resolve_name(
            WithInfo {
                info: expression_info,
                item: name.clone(),
            },
            info,
            |candidates| {
                let mut candidates = candidates.to_vec();
                candidates.sort_by_key(|path| match path.item.last().unwrap() {
                    crate::lower::PathComponent::Variable(_) => 0,
                    crate::lower::PathComponent::Constant(_)
                    | crate::lower::PathComponent::Constructor(_) => 1,
                    _ => 3,
                });

                let path = match candidates.first() {
                    Some(candidate) => candidate,
                    None => return Vec::new(),
                };

                match path.item.last().unwrap() {
                    crate::lower::PathComponent::Variable(_) => {
                        vec![(
                            path.item.clone(),
                            crate::lower::Expression::Variable(name.clone(), path.item.clone()),
                        )]
                    }
                    crate::lower::PathComponent::Constant(_)
                    | crate::lower::PathComponent::Constructor(_) => {
                        vec![(
                            path.item.clone(),
                            crate::lower::Expression::Constant(path.item.clone()),
                        )]
                    }
                    _ => Vec::new(),
                }
            },
        )
        .unwrap_or(crate::lower::Expression::Error),
        crate::lower::UnresolvedExpression::Number(number) => {
            crate::lower::Expression::Number(number)
        }
        crate::lower::UnresolvedExpression::Text(text) => crate::lower::Expression::Text(text),
        crate::lower::UnresolvedExpression::Format { segments, trailing } => {
            crate::lower::Expression::Format {
                segments: segments
                    .into_iter()
                    .map(|segment| crate::lower::FormatSegment {
                        text: segment.text,
                        value: resolve_expression(segment.value, info),
                    })
                    .collect(),
                trailing,
            }
        }
        crate::lower::UnresolvedExpression::Block(statements) => {
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

            crate::lower::Expression::Block {
                statements,
                captures: Vec::from_iter(captures.used),
            }
        }
        crate::lower::UnresolvedExpression::Function { inputs, body } => {
            info.scopes.push_block_scope();
            info.captures.push(Captures::default());

            let inputs = inputs
                .into_iter()
                .map(|pattern| resolve_pattern(pattern, info))
                .collect();

            let body = resolve_expression(body.unboxed(), info);

            let captures = info.captures.pop().unwrap();
            info.scopes.pop_scope();

            crate::lower::Expression::Function {
                inputs,
                body: body.boxed(),
                captures: Vec::from_iter(captures.used),
            }
        }
        crate::lower::UnresolvedExpression::Do(block) => {
            crate::lower::Expression::Do(resolve_expression(block.unboxed(), info).boxed())
        }
        crate::lower::UnresolvedExpression::Call { function, inputs } => {
            crate::lower::Expression::Call {
                function: resolve_expression(function.unboxed(), info).boxed(),
                inputs: inputs
                    .into_iter()
                    .map(|input| resolve_expression(input, info))
                    .collect(),
            }
        }
        crate::lower::UnresolvedExpression::Apply { input, function } => {
            crate::lower::Expression::Call {
                function: resolve_expression(function.unboxed(), info).boxed(),
                inputs: vec![resolve_expression(input.unboxed(), info)],
            }
        }
        crate::lower::UnresolvedExpression::BinaryOperator {
            operator,
            left,
            right,
        } => resolve_binary_operator(operator, left.unboxed(), right.unboxed(), info),
        crate::lower::UnresolvedExpression::As { value, r#type } => {
            let operator = WithInfo {
                info: expression_info,
                item: String::from("as"),
            };

            let operator_trait = operator.replace(
                match resolve_language_item(operator.clone(), info)
                    .into_iter()
                    .find(|path| {
                        matches!(path.last().unwrap(), crate::lower::PathComponent::Trait(_))
                    }) {
                    Some(path) => crate::lower::Expression::Trait(path),
                    None => crate::lower::Expression::Error,
                },
            );

            let r#type = resolve_type(r#type, info);

            crate::lower::Expression::Annotate {
                value: operator
                    .replace(crate::lower::Expression::Call {
                        function: operator_trait.boxed(),
                        inputs: vec![resolve_expression(value.unboxed(), info)],
                    })
                    .boxed(),
                r#type,
            }
        }
        crate::lower::UnresolvedExpression::Is { value, pattern } => {
            let r#true = WithInfo {
                info: expression_info.clone(),
                item: String::from("true"),
            };

            let true_value = r#true.replace(
                match resolve_language_item(r#true.clone(), info)
                    .into_iter()
                    .find(|path| {
                        matches!(
                            path.last().unwrap(),
                            crate::lower::PathComponent::Constructor(_)
                        )
                    }) {
                    Some(path) => crate::lower::Expression::Constant(path),
                    None => crate::lower::Expression::Error,
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
                        matches!(
                            path.last().unwrap(),
                            crate::lower::PathComponent::Constructor(_)
                        )
                    }) {
                    Some(path) => crate::lower::Expression::Constant(path),
                    None => crate::lower::Expression::Error,
                },
            );

            let when_expression = |value: WithInfo<crate::lower::Expression>, info: &mut Info| {
                let value_info = value.info.clone();

                crate::lower::Expression::When {
                    input: value.boxed(),
                    arms: vec![
                        r#true.replace(crate::lower::Arm {
                            pattern: resolve_pattern(pattern, info),
                            body: true_value,
                        }),
                        r#false.replace(crate::lower::Arm {
                            pattern: WithInfo {
                                info: value_info,
                                item: crate::lower::Pattern::Wildcard,
                            },
                            body: false_value,
                        }),
                    ],
                }
            };

            when_expression(resolve_expression(value.unboxed(), info), info)
        }
        crate::lower::UnresolvedExpression::When { input, arms } => {
            let input = resolve_expression(input.unboxed(), info);

            let arms = arms
                .into_iter()
                .map(|arm| {
                    arm.map(|arm| {
                        info.scopes.push_block_scope();

                        let pattern = resolve_pattern(arm.pattern, info);

                        let body = resolve_expression(arm.body, info);

                        info.scopes.pop_scope();

                        crate::lower::Arm { pattern, body }
                    })
                })
                .collect::<Vec<_>>();

            crate::lower::Expression::When {
                input: input.boxed(),
                arms,
            }
        }
        crate::lower::UnresolvedExpression::Intrinsic { name, inputs } => {
            let name = match name.try_unwrap() {
                Some(name) => name,
                None => return crate::lower::Expression::Error,
            };

            let inputs = inputs
                .into_iter()
                .map(|input| resolve_expression(input, info))
                .collect::<Vec<_>>();

            crate::lower::Expression::Intrinsic { name, inputs }
        }
        crate::lower::UnresolvedExpression::Tuple(tuple) => {
            let tuple = tuple
                .into_iter()
                .map(|element| resolve_expression(element, info))
                .collect::<Vec<_>>();

            crate::lower::Expression::Tuple(tuple)
        }
        crate::lower::UnresolvedExpression::Collection(collection) => {
            let collection = collection
                .into_iter()
                .map(|element| resolve_expression(element, info))
                .collect::<Vec<_>>();

            crate::lower::Expression::Collection(collection)
        }
        crate::lower::UnresolvedExpression::Structure(structure) => {
            let structure = structure
                .into_iter()
                .map(|field_value| {
                    field_value.map(|field_value| crate::lower::FieldValue {
                        name: field_value.name,
                        value: resolve_expression(field_value.value, info),
                    })
                })
                .collect::<Vec<_>>();
            crate::lower::Expression::Structure(structure)
        }
    })
}
