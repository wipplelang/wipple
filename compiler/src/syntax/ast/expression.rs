use crate::{
    syntax::{
        ast::{
            Info,
            pattern::{arm, pattern},
            statements::statements,
            r#type::r#type,
        },
        parse,
    },
    util::WithInfo,
};

pub fn expression(
    expression_syntax: WithInfo<parse::Expression>,
    info: &mut Info,
) -> WithInfo<crate::syntax::Expression> {
    expression_syntax.map(|expression_syntax| match expression_syntax {
        parse::Expression::Error => crate::syntax::Expression::Error,
        parse::Expression::Annotate {
            value: value_syntax,
            r#type: type_syntax,
        } => crate::syntax::Expression::Annotate {
            value: expression(value_syntax.unboxed(), info).boxed(),
            r#type: r#type(type_syntax, info),
        },
        parse::Expression::Name(name) => crate::syntax::Expression::Name(name),
        parse::Expression::Number(number) => crate::syntax::Expression::Number(number),
        parse::Expression::Text(text) => crate::syntax::Expression::Text(text),
        parse::Expression::Block(statement_syntaxes) => {
            crate::syntax::Expression::Block(statements(statement_syntaxes, info))
        }
        parse::Expression::Do(block_syntax) => {
            crate::syntax::Expression::Do(expression(block_syntax.unboxed(), info).boxed())
        }
        parse::Expression::Function {
            inputs: input_syntaxes,
            body: body_syntax,
        } => crate::syntax::Expression::Function {
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
                let result = crate::syntax::text::parse_format_expression::<_>(
                    WithInfo {
                        info: function_syntax.info.clone(),
                        item: text,
                    },
                    input_syntaxes,
                    &mut info.errors,
                );

                crate::syntax::Expression::Format {
                    segments: result
                        .segments
                        .into_iter()
                        .map(|segment| crate::syntax::FormatSegment {
                            text: segment.text,
                            value: expression(segment.value, info),
                        })
                        .collect(),
                    trailing: result.trailing,
                }
            } else {
                crate::syntax::Expression::Call {
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
        } => crate::syntax::Expression::Apply {
            input: expression(input_syntax.unboxed(), info).boxed(),
            function: expression(function_syntax.unboxed(), info).boxed(),
        },
        parse::Expression::BinaryOperator {
            operator,
            left: left_syntax,
            right: right_syntax,
        } => crate::syntax::Expression::BinaryOperator {
            operator,
            left: expression(left_syntax.unboxed(), info).boxed(),
            right: expression(right_syntax.unboxed(), info).boxed(),
        },
        parse::Expression::As {
            value: value_syntax,
            r#type: type_syntax,
        } => crate::syntax::Expression::As {
            value: expression(value_syntax.unboxed(), info).boxed(),
            r#type: r#type(type_syntax, info),
        },
        parse::Expression::Is {
            value: value_syntax,
            pattern: pattern_syntax,
        } => crate::syntax::Expression::Is {
            value: expression(value_syntax.unboxed(), info).boxed(),
            pattern: pattern(pattern_syntax, info),
        },
        parse::Expression::When {
            input,
            arms: arm_syntaxes,
        } => crate::syntax::Expression::When {
            input: expression(input.unboxed(), info).boxed(),
            arms: arm_syntaxes
                .into_iter()
                .map(|arm_syntax| arm(arm_syntax, info))
                .collect(),
        },
        parse::Expression::Intrinsic {
            name,
            inputs: input_syntaxes,
        } => crate::syntax::Expression::Intrinsic {
            name,
            inputs: input_syntaxes
                .into_iter()
                .map(|input| expression(input, info))
                .collect(),
        },
        parse::Expression::Tuple(element_syntaxes) => crate::syntax::Expression::Tuple(
            element_syntaxes
                .into_iter()
                .map(|element_syntax| expression(element_syntax, info))
                .collect(),
        ),
        parse::Expression::Collection(element_syntaxes) => crate::syntax::Expression::Collection(
            element_syntaxes
                .into_iter()
                .map(|element_syntax| expression(element_syntax, info))
                .collect(),
        ),
        parse::Expression::Structure(fields) => crate::syntax::Expression::Structure(
            fields
                .into_iter()
                .map(|field| {
                    field.map(|field| crate::syntax::FieldValue {
                        name: field.name,
                        value: expression(field.value, info),
                    })
                })
                .collect(),
        ),
    })
}
