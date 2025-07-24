use crate::{
    syntax::{ast::Info, parse},
    util::WithInfo,
};

pub fn r#type(
    type_syntax: WithInfo<parse::Type>,
    info: &mut Info,
) -> WithInfo<crate::syntax::Type> {
    type_syntax.map(|type_syntax| match type_syntax {
        parse::Type::Error => crate::syntax::Type::Error,
        parse::Type::Placeholder => crate::syntax::Type::Placeholder,
        parse::Type::Declared {
            name,
            parameters: parameter_syntaxes,
        } => crate::syntax::Type::Declared {
            name,
            parameters: parameter_syntaxes
                .into_iter()
                .map(|parameter_syntax| r#type(parameter_syntax, info))
                .collect(),
        },
        parse::Type::Function { inputs, output } => crate::syntax::Type::Function {
            inputs: inputs
                .into_iter()
                .map(|input_syntax| r#type(input_syntax, info))
                .collect(),
            output: r#type(output.unboxed(), info).boxed(),
        },
        parse::Type::Tuple(types) => crate::syntax::Type::Tuple(
            types
                .into_iter()
                .map(|type_syntax| r#type(type_syntax, info))
                .collect(),
        ),
        parse::Type::Block(type_syntax) => {
            crate::syntax::Type::Block(r#type(type_syntax.unboxed(), info).boxed())
        }
        parse::Type::Intrinsic => crate::syntax::Type::Intrinsic,
        parse::Type::Message {
            message,
            inputs: input_syntaxes,
        } => {
            let result = crate::syntax::text::parse_format_expression::<_>(
                message.as_deref(),
                input_syntaxes,
                &mut info.errors,
            );

            crate::syntax::Type::Message {
                segments: result
                    .segments
                    .into_iter()
                    .map(|segment| crate::syntax::FormatSegment {
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
        } => crate::syntax::Type::Equal {
            left: r#type(left_syntax.unboxed(), info).boxed(),
            right: r#type(right_syntax.unboxed(), info).boxed(),
        },
    })
}

pub fn type_function(
    type_function_syntax: WithInfo<parse::TypeFunction>,
    info: &mut Info,
) -> (
    Vec<WithInfo<crate::syntax::TypeParameter>>,
    Vec<WithInfo<crate::syntax::Instance>>,
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

pub fn type_parameter(
    type_parameter_syntax: WithInfo<parse::TypeParameter>,
    info: &mut Info,
) -> WithInfo<crate::syntax::TypeParameter> {
    type_parameter_syntax.map(|type_parameter_syntax| crate::syntax::TypeParameter {
        name: type_parameter_syntax.name,
        infer: type_parameter_syntax.infer,
        default: type_parameter_syntax
            .default
            .map(|type_syntax| r#type(type_syntax, info)),
    })
}

pub fn instance(
    instance_syntax: WithInfo<parse::Instance>,
    info: &mut Info,
) -> WithInfo<crate::syntax::Instance> {
    instance_syntax.map(|instance_syntax| crate::syntax::Instance {
        r#trait: instance_syntax.r#trait,
        parameters: instance_syntax
            .parameters
            .into_iter()
            .map(|type_syntax| r#type(type_syntax, info))
            .collect(),
    })
}
