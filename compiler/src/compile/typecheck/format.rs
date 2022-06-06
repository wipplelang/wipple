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
        macro_rules! format_named_type {
            ($name:expr, $params:expr) => {{
                let params = $params;
                let has_params = !params.is_empty();

                let name = std::iter::once(String::from($name))
                    .chain(params.into_iter().map(|param| {
                        format!(
                            " {}",
                            format_type(param, type_names, param_names, false, false)
                        )
                    }))
                    .collect::<String>();

                if is_top_level || !has_params {
                    name
                } else {
                    format!("({})", name)
                }
            }};
        }

        match ty {
            UnresolvedType::Variable(_) => String::from("_"),
            UnresolvedType::Parameter(param) => param_names(param),
            UnresolvedType::Bottom(_) => String::from("!"),
            UnresolvedType::Named(id, params) => format_named_type!(type_names(id), params),
            UnresolvedType::Builtin(ty) => match ty {
                BuiltinType::Unit => format_named_type!("()", Vec::new()),
                BuiltinType::Text => format_named_type!("Text", Vec::new()),
                BuiltinType::Number => format_named_type!("Number", Vec::new()),
                BuiltinType::List(ty) => format_named_type!("List", vec![*ty]),
                BuiltinType::Mutable(ty) => format_named_type!("Mutable", vec![*ty]),
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
