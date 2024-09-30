use crate::{ast::Info, parse, Driver};
use wipple_util::WithInfo;

#[allow(clippy::only_used_in_recursion)]
pub fn r#type<D: Driver>(
    type_syntax: WithInfo<D::Info, parse::Type<D>>,
    info: &mut Info<D>,
) -> WithInfo<D::Info, crate::Type<D>> {
    type_syntax.map(|type_syntax| match type_syntax {
        parse::Type::Error => crate::Type::Error,
        parse::Type::Placeholder => crate::Type::Placeholder,
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

pub fn type_function<D: Driver>(
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

pub fn type_parameter<D: Driver>(
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

pub fn instance<D: Driver>(
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
