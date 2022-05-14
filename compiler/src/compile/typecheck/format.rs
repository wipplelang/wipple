use super::engine::{BuiltinType, UnresolvedType};
use crate::{TypeId, TypeParameterId};

pub fn format_type(
    ty: &UnresolvedType,
    type_names: impl Fn(TypeId) -> String,
    param_names: impl Fn(TypeParameterId) -> String,
) -> String {
    format_type_with(ty, type_names, param_names)
}

fn format_type_with(
    ty: &UnresolvedType,
    type_names: impl Fn(TypeId) -> String,
    param_names: impl Fn(TypeParameterId) -> String,
) -> String {
    fn format_type(
        ty: &UnresolvedType,
        type_names: &impl Fn(TypeId) -> String,
        param_names: &impl Fn(TypeParameterId) -> String,
        is_top_level: bool,
        is_return: bool,
    ) -> String {
        match ty {
            UnresolvedType::Variable(id) => format!("{{{}}}", id.0), // String::from("_"),
            UnresolvedType::Parameter(param) => param_names(*param),
            UnresolvedType::Bottom(_) => String::from("!"),
            UnresolvedType::Named(id) => {
                let name = type_names(*id);

                if is_top_level {
                    name
                } else {
                    format!("({name})")
                }
            }
            UnresolvedType::Builtin(ty) => match ty {
                BuiltinType::Unit => String::from("()"),
                BuiltinType::Text => String::from("Text"),
                BuiltinType::Number => String::from("Number"),
            },
            UnresolvedType::Function(input, output) => {
                let input = format_type(input, type_names, param_names, true, false);
                let output = format_type(output, type_names, param_names, true, true);

                if is_top_level && is_return {
                    format!("{input} -> {output}")
                } else {
                    format!("({input} -> {output})")
                }
            }
        }
    }

    let formatted = format_type(ty, &type_names, &param_names, true, true);

    let mut names = Vec::new();
    for param in ty.params() {
        names.push(param);
    }

    if names.is_empty() {
        formatted
    } else {
        format!(
            "{}=> {}",
            names
                .iter()
                .map(|param| param_names(*param) + " ")
                .collect::<String>(),
            formatted
        )
    }
}
