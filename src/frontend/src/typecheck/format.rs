use crate::typecheck::*;
use std::collections::BTreeMap;

pub fn format_type_scheme(ty: &Scheme) -> String {
    match ty {
        Scheme::Type(ty) => format_type(ty),
        Scheme::ForAll(forall) => {
            if forall.vars.is_empty() {
                return format_type(&forall.ty);
            }

            let mut names = BTreeMap::new();
            for variable in forall.vars.iter().collect::<Vec<_>>().into_iter().rev() {
                names.insert(variable.0, name_for_index(names.len()));
            }

            format!(
                "for {}-> {}",
                names.values().map(|t| t.clone() + " ").collect::<String>(),
                format_type_with(&forall.ty, &names)
            )
        }
    }
}

pub fn format_type(ty: &Type) -> String {
    format_type_with(ty, &BTreeMap::new())
}

pub fn format_type_with(ty: &Type, names: &BTreeMap<u32, String>) -> String {
    fn format_type(
        ty: &Type,
        is_top_level: bool,
        is_return: bool,
        names: &BTreeMap<u32, String>,
    ) -> String {
        match ty {
            Type::Variable(variable) => names
                .get(&variable.0)
                .cloned()
                .unwrap_or_else(|| String::from("_")),
            Type::Constructed { bottom: true, .. } => String::from("!"),
            Type::Constructed { id, params, .. } => {
                let ty_name = id
                    .name
                    .map(|name| name.to_string())
                    .unwrap_or_else(|| String::from("<unnamed>"));

                match id.format {
                    TypeNameFormat::Default => {
                        let params = params
                            .iter()
                            .map(|ty| String::from(" ") + &format_type(ty, false, true, names))
                            .collect::<String>();

                        if is_top_level {
                            format!("{}{}", ty_name, params)
                        } else {
                            format!("({}{})", ty_name, params)
                        }
                    }
                    TypeNameFormat::Function => {
                        let left = format_type(&params[0], true, false, names);

                        let right = format_type(&params[1], true, true, names);

                        if is_top_level && is_return {
                            format!("{} {} {}", left, ty_name, right)
                        } else {
                            format!("({} {} {})", left, ty_name, right)
                        }
                    }
                }
            }
        }
    }

    format_type(ty, true, true, names)
}

fn name_for_index(index: usize) -> String {
    const LETTERS: [char; 26] = [
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
        'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    ];

    LETTERS
        .get(index)
        .copied()
        .map(String::from)
        .unwrap_or_else(|| format!("T{}", index + 1))
}
