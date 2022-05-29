use super::engine::{BuiltinType, UnresolvedType};
use crate::{TypeId, TypeParameterId};

pub fn format_type(
    ty: impl Into<UnresolvedType>,
    type_names: impl Fn(TypeId) -> String,
    param_names: impl Fn(TypeParameterId) -> String,
) -> String {
    format_type_with(ty, type_names, param_names)
}

fn format_type_with(
    ty: impl Into<UnresolvedType>,
    type_names: impl Fn(TypeId) -> String,
    param_names: impl Fn(TypeParameterId) -> String,
) -> String {
    let ty = ty.into();

    fn format_type(
        ty: UnresolvedType,
        type_names: &impl Fn(TypeId) -> String,
        param_names: &impl Fn(TypeParameterId) -> String,
        is_top_level: bool,
        is_return: bool,
    ) -> String {
        match ty {
            UnresolvedType::Variable(_) => String::from("_"),
            UnresolvedType::Parameter(param) => param_names(param),
            UnresolvedType::Bottom(_) => String::from("!"),
            UnresolvedType::Named(id, params) => {
                let name = std::iter::once(type_names(id))
                    .chain(params.into_iter().map(|param| {
                        format!(
                            " {}",
                            format_type(param, type_names, param_names, true, false)
                        )
                    }))
                    .collect::<String>();

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
                BuiltinType::List(ty) => {
                    let ty = format_type(*ty, type_names, param_names, true, false);

                    if is_top_level {
                        format!("List {}", ty)
                    } else {
                        format!("(List {})", ty)
                    }
                }
            },
            UnresolvedType::Function(input, output) => {
                let input = format_type(*input, type_names, param_names, true, false);
                let output = format_type(*output, type_names, param_names, true, true);

                if is_top_level && is_return {
                    format!("{input} -> {output}")
                } else {
                    format!("({input} -> {output})")
                }
            }
        }
    }

    let mut names = Vec::new();
    for param in ty.params() {
        names.push(param);
    }

    let formatted = format_type(ty, &type_names, &param_names, true, true);

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
