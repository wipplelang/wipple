use itertools::Itertools;

use super::engine::{BuiltinType, UnresolvedType};
use crate::{TraitId, TypeId, TypeParameterId};

pub enum FormattableType {
    Type(UnresolvedType),
    Trait(TraitId, Vec<UnresolvedType>),
}

impl From<UnresolvedType> for FormattableType {
    fn from(ty: UnresolvedType) -> Self {
        FormattableType::Type(ty)
    }
}

impl FormattableType {
    fn params(&self) -> Vec<UnresolvedType> {
        match self {
            FormattableType::Type(ty) => ty
                .params()
                .into_iter()
                .map(UnresolvedType::Parameter)
                .collect(),
            FormattableType::Trait(_, params) => params.clone(),
        }
    }
}

pub fn format_type(
    ty: impl Into<FormattableType>,
    type_names: impl Fn(TypeId) -> String,
    trait_names: impl Fn(TraitId) -> String,
    param_names: impl Fn(TypeParameterId) -> String,
) -> String {
    format_type_with(ty, type_names, trait_names, param_names)
}

fn format_type_with(
    ty: impl Into<FormattableType>,
    type_names: impl Fn(TypeId) -> String,
    trait_names: impl Fn(TraitId) -> String,
    param_names: impl Fn(TypeParameterId) -> String,
) -> String {
    let ty: FormattableType = ty.into();

    fn format_type(
        ty: FormattableType,
        type_names: &impl Fn(TypeId) -> String,
        trait_names: &impl Fn(TraitId) -> String,
        param_names: &impl Fn(TypeParameterId) -> String,
        is_top_level: bool,
        is_return: bool,
    ) -> String {
        macro_rules! format_named_type {
            ($name:expr, $params:expr) => {{
                let params = $params;
                let has_params = !params.is_empty();

                let name = std::iter::once(String::from($name))
                    .chain(params.into_iter().map(|param: UnresolvedType| {
                        format!(
                            " {}",
                            format_type(
                                param.into(),
                                type_names,
                                trait_names,
                                param_names,
                                false,
                                false
                            )
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
            FormattableType::Type(UnresolvedType::Variable(_)) => String::from("_"),
            FormattableType::Type(UnresolvedType::Parameter(param)) => param_names(param),
            FormattableType::Type(UnresolvedType::Bottom(_)) => String::from("!"),
            FormattableType::Type(UnresolvedType::Named(id, params)) => {
                format_named_type!(type_names(id), params)
            }
            FormattableType::Type(UnresolvedType::Builtin(ty)) => match ty {
                BuiltinType::Unit => format_named_type!("()", Vec::new()),
                BuiltinType::Text => format_named_type!("Text", Vec::new()),
                BuiltinType::Number => format_named_type!("Number", Vec::new()),
                BuiltinType::List(ty) => format_named_type!("List", vec![*ty]),
                BuiltinType::Mutable(ty) => format_named_type!("Mutable", vec![*ty]),
            },
            FormattableType::Type(UnresolvedType::Function(input, output)) => {
                let input = format_type(
                    (*input).into(),
                    type_names,
                    trait_names,
                    param_names,
                    true,
                    false,
                );

                let output = format_type(
                    (*output).into(),
                    type_names,
                    trait_names,
                    param_names,
                    true,
                    true,
                );

                if is_top_level && is_return {
                    format!("{input} -> {output}")
                } else {
                    format!("({input} -> {output})")
                }
            }
            FormattableType::Type(UnresolvedType::Tuple(tys)) => tys
                .into_iter()
                .map(|ty| {
                    format_type(
                        ty.into(),
                        type_names,
                        trait_names,
                        param_names,
                        is_top_level,
                        is_return,
                    )
                })
                .join(" , "),
            FormattableType::Trait(tr, params) => {
                format_named_type!(trait_names(tr), params)
            }
        }
    }

    let mut names = Vec::new();
    for param in ty.params() {
        names.push(param);
    }

    let show_params = names.is_empty() || matches!(ty, FormattableType::Trait(_, _));

    let formatted = format_type(ty, &type_names, &trait_names, &param_names, true, true);

    if show_params {
        formatted
    } else {
        format!(
            "{}=> {}",
            names
                .iter()
                .map(|param| format_type(
                    param.clone().into(),
                    &type_names,
                    &trait_names,
                    &param_names,
                    false,
                    false
                ) + " ")
                .collect::<String>(),
            formatted
        )
    }
}
