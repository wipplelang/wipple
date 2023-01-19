use super::engine::{BuiltinType, Type, TypeVariable, UnresolvedType};
use crate::{TraitId, TypeId, TypeParameterId};
use itertools::Itertools;
use std::{collections::BTreeMap, mem};

#[derive(Debug, Clone)]
pub struct FormattableType {
    vars: BTreeMap<TypeVariable, String>,
    kind: FormattableTypeKind,
}

#[derive(Debug, Clone)]
enum FormattableTypeKind {
    Type(UnresolvedType),
    Trait(TraitId, Vec<UnresolvedType>),
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Format {
    pub type_function: TypeFunctionFormat,
    pub type_variable: TypeVariableFormat,
    pub surround_in_backticks: bool,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum TypeFunctionFormat {
    #[default]
    None,
    Arrow,
    Description,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum TypeVariableFormat {
    #[default]
    None,
    Numeric,
    Description,
}

impl From<UnresolvedType> for FormattableType {
    fn from(ty: UnresolvedType) -> Self {
        let mut vars = BTreeMap::new();
        collect_vars(&ty, &mut vars);

        FormattableType {
            vars,
            kind: FormattableTypeKind::Type(ty),
        }
    }
}

impl From<Type> for FormattableType {
    fn from(ty: Type) -> Self {
        FormattableType::from(UnresolvedType::from(ty))
    }
}

impl FormattableType {
    pub fn r#trait(id: TraitId, params: Vec<UnresolvedType>) -> Self {
        let mut vars = BTreeMap::new();
        for ty in &params {
            collect_vars(ty, &mut vars);
        }

        FormattableType {
            vars,
            kind: FormattableTypeKind::Trait(id, params),
        }
    }
}

impl FormattableType {
    fn var_names(&self) -> Vec<(TypeVariable, String)> {
        let mut vars = self
            .vars
            .iter()
            .map(|(var, s)| (*var, s.clone()))
            .collect::<Vec<_>>();

        vars.sort_by_key(|(var, _)| *var);

        vars
    }

    fn param_names(&self, get_name: impl Fn(TypeParameterId) -> Option<String>) -> Vec<String> {
        let mut names = BTreeMap::new();
        match &self.kind {
            FormattableTypeKind::Type(ty) => {
                for param in ty.params() {
                    let name = match get_name(param) {
                        Some(name) => name,
                        None => continue,
                    };

                    names.insert(param, name);
                }
            }
            FormattableTypeKind::Trait(_, tys) => {
                for ty in tys {
                    for param in ty.params() {
                        let name = match get_name(param) {
                            Some(name) => name,
                            None => continue,
                        };

                        names.insert(param, name);
                    }
                }
            }
        }

        let mut names = names.into_values().collect::<Vec<_>>();
        names.sort();
        names
    }

    fn params(&self) -> Vec<UnresolvedType> {
        let params = match &self.kind {
            FormattableTypeKind::Type(ty) => ty
                .params()
                .into_iter()
                .map(UnresolvedType::Parameter)
                .collect(),
            FormattableTypeKind::Trait(_, params) => params.clone(),
        };

        params
            .into_iter()
            .unique_by(|ty| match ty {
                UnresolvedType::Parameter(param) => Some(*param),
                _ => None,
            })
            .collect()
    }
}

pub fn format_type(
    ty: impl Into<FormattableType>,
    type_names: impl Fn(TypeId) -> String,
    trait_names: impl Fn(TraitId) -> String,
    param_names: impl Fn(TypeParameterId) -> Option<String>,
    format: Format,
) -> String {
    format_type_with(ty, type_names, trait_names, param_names, format)
}

fn format_type_with(
    ty: impl Into<FormattableType>,
    type_names: impl Fn(TypeId) -> String,
    trait_names: impl Fn(TraitId) -> String,
    param_names: impl Fn(TypeParameterId) -> Option<String>,
    mut format: Format,
) -> String {
    let mut ty: FormattableType = ty.into();

    let ty_params = ty.params();
    let mut ty_param_names = ty.param_names(&param_names);
    if ty_param_names.is_empty() {
        format.type_function = TypeFunctionFormat::None;
    }

    let ty_var_names = ty.var_names();
    let ty_vars = mem::take(&mut ty.vars);
    if ty_var_names.is_empty() {
        format.type_variable = TypeVariableFormat::None;
    }
    let var_names = |var: TypeVariable| match format.type_variable {
        TypeVariableFormat::None => String::from("_"),
        TypeVariableFormat::Numeric => format!("{{{}}}", var.0),
        TypeVariableFormat::Description => ty_vars.get(&var).unwrap().clone(),
    };

    fn format_type(
        ty: FormattableType,
        type_names: &impl Fn(TypeId) -> String,
        trait_names: &impl Fn(TraitId) -> String,
        param_names: &impl Fn(TypeParameterId) -> Option<String>,
        var_names: &impl Fn(TypeVariable) -> String,
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
                                var_names,
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

        let formatted = match ty.kind {
            FormattableTypeKind::Type(UnresolvedType::Variable(var)) => var_names(var),
            FormattableTypeKind::Type(UnresolvedType::TerminatingVariable(_)) => {
                format_named_type!("()", Vec::new())
            }
            FormattableTypeKind::Type(UnresolvedType::NumericVariable(_)) => {
                format_named_type!("Number", Vec::new())
            }
            FormattableTypeKind::Type(UnresolvedType::Parameter(param)) => {
                param_names(param).unwrap_or_else(|| String::from("_"))
            }
            FormattableTypeKind::Type(UnresolvedType::Error) => String::from("_"),
            FormattableTypeKind::Type(UnresolvedType::Named(id, params, _)) => {
                format_named_type!(type_names(id), params)
            }
            FormattableTypeKind::Type(UnresolvedType::Builtin(ty)) => match ty {
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
            FormattableTypeKind::Type(UnresolvedType::Function(input, output)) => {
                let input = format_type(
                    (*input).into(),
                    type_names,
                    trait_names,
                    param_names,
                    var_names,
                    is_top_level,
                    false,
                    false,
                );

                let output = format_type(
                    (*output).into(),
                    type_names,
                    trait_names,
                    param_names,
                    var_names,
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
            FormattableTypeKind::Type(UnresolvedType::Tuple(mut tys)) => {
                let is_empty = tys.is_empty();

                let ty = match tys.len() {
                    0 => String::from("()"),
                    1 => {
                        format_type(
                            tys.pop().unwrap().into(),
                            type_names,
                            trait_names,
                            param_names,
                            var_names,
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
                                var_names,
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
            FormattableTypeKind::Trait(tr, params) => {
                format_named_type!(trait_names(tr), params)
            }
        };

        if surround_in_backticks {
            format!("`{}`", formatted)
        } else {
            formatted
        }
    }

    let show_params = matches!(format.type_function, TypeFunctionFormat::Arrow)
        && !matches!(ty.kind, FormattableTypeKind::Trait(_, _));

    let formatted = format_type(
        ty,
        &type_names,
        &trait_names,
        &param_names,
        &var_names,
        true,
        true,
        format.surround_in_backticks && show_params,
    );

    let formatted = if show_params {
        format!(
            "{}=> {}",
            ty_params
                .iter()
                .map(|param| format_type(
                    param.clone().into(),
                    &type_names,
                    &trait_names,
                    &param_names,
                    &var_names,
                    false,
                    false,
                    false
                ) + " ")
                .collect::<String>(),
            formatted
        )
    } else {
        formatted
    };

    let formatted = if format.surround_in_backticks {
        format!("`{}`", formatted)
    } else {
        formatted
    };

    let param_description = || match format.type_function {
        TypeFunctionFormat::None | TypeFunctionFormat::Arrow => unreachable!(),
        TypeFunctionFormat::Description => match ty_param_names.len() {
            0 => unreachable!(),
            1 => format!("for any type `{}`", ty_param_names.pop().unwrap()),
            _ => {
                let mut vars = ty_param_names
                    .into_iter()
                    .map(|param| format!("`{}`", param))
                    .collect::<Vec<_>>();

                let last = vars.pop().unwrap();

                format!("for any types {} and {}", vars.join(", "), last)
            }
        },
    };

    let var_description = || match format.type_variable {
        TypeVariableFormat::None | TypeVariableFormat::Numeric => unreachable!(),
        TypeVariableFormat::Description => match ty_vars.len() {
            0 => unreachable!(),
            1 => format!(
                "for some unknown type `{}`",
                ty_vars.iter().next().unwrap().1
            ),
            _ => {
                let mut vars = ty_var_names
                    .into_iter()
                    .map(|(_, s)| format!("`{}`", s))
                    .collect::<Vec<_>>();

                let last = vars.pop().unwrap();

                format!("for some unknown types {} and {}", vars.join(", "), last)
            }
        },
    };

    match (format.type_function, format.type_variable) {
        (
            TypeFunctionFormat::None | TypeFunctionFormat::Arrow,
            TypeVariableFormat::None | TypeVariableFormat::Numeric,
        ) => formatted,
        (TypeFunctionFormat::None, _) => format!("{} {}", formatted, var_description()),
        (
            TypeFunctionFormat::Description,
            TypeVariableFormat::None | TypeVariableFormat::Numeric,
        ) => {
            format!("{} {}", formatted, param_description())
        }
        (TypeFunctionFormat::Arrow, _) => format!("{} {}", formatted, var_description()),
        _ => format!(
            "{} {} and {}",
            formatted,
            param_description(),
            var_description()
        ),
    }
}

fn collect_vars(ty: &UnresolvedType, vars: &mut BTreeMap<TypeVariable, String>) {
    for var in ty.vars() {
        let index = vars.len();

        vars.entry(var).or_insert_with(|| {
            ('a'..='z')
                .nth(index)
                .map(|c| c.to_string())
                .unwrap_or_else(|| format!("{{{}}}", index))
        });
    }
}
