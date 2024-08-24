use crate::{
    language::resolve_language_item,
    name::resolve_name,
    operator::resolve_binary_operator,
    pattern::resolve_pattern,
    r#type::resolve_type,
    resolve::Info,
    scope::Captures,
    statements::{resolve_statements, split_executable_statements},
    Driver,
};
use wipple_util::WithInfo;

pub fn resolve_expression<D: Driver>(
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
