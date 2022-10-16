use itertools::Itertools;

use super::engine::{BuiltinType, Type, UnresolvedType};
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

impl From<Type> for FormattableType {
    fn from(ty: Type) -> Self {
        FormattableType::Type(ty.into())
    }
}

impl FormattableType {
    fn params(&self) -> Vec<UnresolvedType> {
        let mut params = match self {
            FormattableType::Type(ty) => ty
                .params()
                .into_iter()
                .map(UnresolvedType::Parameter)
                .collect(),
            FormattableType::Trait(_, params) => params.clone(),
        };

        params.dedup_by_key(|ty| match ty {
            UnresolvedType::Parameter(param) => Some(*param),
            _ => None,
        });

        params
    }
}

pub fn format_type(
    ty: impl Into<FormattableType>,
    type_names: impl Fn(TypeId) -> String,
    trait_names: impl Fn(TraitId) -> String,
    param_names: impl Fn(TypeParameterId) -> String,
    surround_in_backticks: bool,
) -> String {
    format_type_with(
        ty,
        type_names,
        trait_names,
        param_names,
        surround_in_backticks,
    )
}

fn format_type_with(
    ty: impl Into<FormattableType>,
    type_names: impl Fn(TypeId) -> String,
    trait_names: impl Fn(TraitId) -> String,
    param_names: impl Fn(TypeParameterId) -> String,
    surround_in_backticks: bool,
) -> String {
    let ty: FormattableType = ty.into();

    fn format_type(
        ty: FormattableType,
        type_names: &impl Fn(TypeId) -> String,
        trait_names: &impl Fn(TraitId) -> String,
        param_names: &impl Fn(TypeParameterId) -> String,
        is_top_level: bool,
        is_return: bool,
        surround_in_backticks: bool,
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
                                false,
                                false,
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

        let formatted = match ty {
            FormattableType::Type(UnresolvedType::Variable(_)) => String::from("_"),
            FormattableType::Type(UnresolvedType::NumericVariable(_)) => {
                format_named_type!("Number", Vec::new())
            }
            FormattableType::Type(UnresolvedType::Parameter(param)) => param_names(param),
            FormattableType::Type(UnresolvedType::Bottom(_)) => String::from("!"),
            FormattableType::Type(UnresolvedType::Named(id, params, _)) => {
                format_named_type!(type_names(id), params)
            }
            FormattableType::Type(UnresolvedType::Builtin(ty)) => match ty {
                BuiltinType::Number => format_named_type!("Number", Vec::new()),
                BuiltinType::Integer => format_named_type!("Integer", Vec::new()),
                BuiltinType::Natural => format_named_type!("Natural", Vec::new()),
                BuiltinType::Byte => format_named_type!("Byte", Vec::new()),
                BuiltinType::Signed => format_named_type!("Signed", Vec::new()),
                BuiltinType::Unsigned => format_named_type!("Unsigned", Vec::new()),
                BuiltinType::Float => format_named_type!("Float", Vec::new()),
                BuiltinType::Double => format_named_type!("Double", Vec::new()),
                BuiltinType::Text => format_named_type!("Text", Vec::new()),
                BuiltinType::List(ty) => format_named_type!("List", vec![*ty]),
                BuiltinType::Mutable(ty) => format_named_type!("Mutable", vec![*ty]),
            },
            FormattableType::Type(UnresolvedType::Function(input, output)) => {
                let input = format_type(
                    (*input).into(),
                    type_names,
                    trait_names,
                    param_names,
                    is_top_level,
                    false,
                    false,
                );

                let output = format_type(
                    (*output).into(),
                    type_names,
                    trait_names,
                    param_names,
                    is_top_level,
                    true,
                    false,
                );

                if is_top_level && is_return {
                    format!("{input} -> {output}")
                } else {
                    format!("({input} -> {output})")
                }
            }
            FormattableType::Type(UnresolvedType::Tuple(mut tys)) => {
                let is_empty = tys.is_empty();

                let ty = match tys.len() {
                    0 => String::from("()"),
                    1 => {
                        format_type(
                            tys.pop().unwrap().into(),
                            type_names,
                            trait_names,
                            param_names,
                            is_top_level,
                            is_return,
                            false,
                        ) + " ,"
                    }
                    _ => tys
                        .into_iter()
                        .map(|ty| {
                            format_type(
                                ty.into(),
                                type_names,
                                trait_names,
                                param_names,
                                is_top_level,
                                is_return,
                                false,
                            )
                        })
                        .join(" , "),
                };

                if is_top_level || is_empty {
                    ty
                } else {
                    format!("({})", ty)
                }
            }
            FormattableType::Trait(tr, params) => {
                format_named_type!(trait_names(tr), params)
            }
        };

        if surround_in_backticks {
            format!("`{}`", formatted)
        } else {
            formatted
        }
    }

    let params = ty.params();

    let show_params = params.is_empty() || matches!(ty, FormattableType::Trait(_, _));

    let formatted = format_type(
        ty,
        &type_names,
        &trait_names,
        &param_names,
        true,
        true,
        show_params,
    );

    if show_params {
        formatted
    } else {
        let formatted = format!(
            "{}=> {}",
            params
                .iter()
                .map(|param| format_type(
                    param.clone().into(),
                    &type_names,
                    &trait_names,
                    &param_names,
                    false,
                    false,
                    false
                ) + " ")
                .collect::<String>(),
            formatted
        );

        if surround_in_backticks {
            format!("`{}`", formatted)
        } else {
            formatted
        }
    }
}
