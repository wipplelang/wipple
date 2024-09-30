#![allow(dead_code)]

use crate::{
    infer::types::{context::TypeContext, Instance, Type, TypeKind},
    Driver,
};
use std::fmt::Debug;

pub fn debug_instance<D: Driver>(instance: &Instance<D>, context: &mut TypeContext<D>) -> String {
    format!(
        "({:?}{})",
        debug_path(&instance.r#trait),
        instance
            .parameters
            .iter()
            .fold(String::new(), |mut result, parameter| {
                use std::fmt::Write;
                write!(&mut result, " {}", debug_type(parameter, context)).unwrap();
                result
            })
    )
}

pub fn debug_type<D: Driver>(r#type: &Type<D>, context: &mut TypeContext<D>) -> String {
    let r#type = r#type.apply_in_context(context);

    match r#type.kind {
        TypeKind::Variable(variable) => format!("{{{:?}}}", variable),
        TypeKind::Opaque(_) => String::from("{opaque}"),
        TypeKind::Parameter(path) => format!("({})", debug_path(&path)),
        TypeKind::Declared { path, parameters } => format!(
            "({:?}{})",
            debug_path(&path),
            parameters
                .into_iter()
                .fold(String::new(), |mut result, parameter| {
                    use std::fmt::Write;
                    write!(&mut result, " {}", debug_type(&parameter, context)).unwrap();
                    result
                })
        ),
        TypeKind::Function { inputs, output } => {
            format!(
                "({}-> {})",
                inputs.into_iter().fold(String::new(), |mut result, input| {
                    use std::fmt::Write;
                    write!(&mut result, "{} ", debug_type(&input, context)).unwrap();
                    result
                }),
                debug_type(&output, context),
            )
        }
        TypeKind::Tuple(elements) => format!(
            "({})",
            elements.iter().fold(String::new(), |mut result, element| {
                use std::fmt::Write;
                write!(&mut result, " {} ;", debug_type(element, context)).unwrap();
                result
            })
        ),
        TypeKind::Block(r#type) => format!("{{{}}}", debug_type(&r#type, context)),
        TypeKind::Unknown => String::from("_"),
        TypeKind::Intrinsic => String::from("intrinsic"),
        TypeKind::Equal { left, right } => format!(
            "({} = {})",
            debug_type(&left, context),
            debug_type(&right, context)
        ),
        TypeKind::Message { segments, trailing } => {
            let mut message = String::new();
            for segment in segments {
                use std::fmt::Write;
                message.push_str(&segment.text);
                write!(&mut message, "{}", debug_type(&segment.value, context)).unwrap();
            }

            message.push_str(&trailing);

            message
        }
    }
}

pub fn debug_path(path: &(impl Debug + serde::Serialize)) -> String {
    serde_json::to_value(path)
        .ok()
        .and_then(|value| value.as_str().map(ToString::to_string))
        .unwrap_or_else(|| format!("{path:?}"))
}
