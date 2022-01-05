use crate::typecheck::*;
use std::collections::BTreeMap;

pub fn format_type_schema(ty: &TypeSchema) -> String {
    match ty {
        polytype::TypeSchema::Monotype(ty) => format_type(ty),
        polytype::TypeSchema::Polytype { .. } => {
            let mut ty = ty;
            let mut variables = Vec::new();
            while let TypeSchema::Polytype { variable, body } = ty {
                variables.push(*variable);
                ty = body
            }

            let ty = match ty {
                polytype::TypeSchema::Monotype(ty) => ty,
                polytype::TypeSchema::Polytype { .. } => unreachable!(),
            };

            let mut names = BTreeMap::new();
            for variable in variables.into_iter().rev() {
                names.insert(variable, name_for_index(names.len()));
            }

            format!(
                "for {}-> {}",
                names.values().map(|t| t.clone() + " ").collect::<String>(),
                format_type_with(ty, &names)
            )
        }
    }
}

pub fn format_type(ty: &Type) -> String {
    format_type_with(ty, &BTreeMap::new())
}

pub fn format_type_with(ty: &Type, names: &BTreeMap<usize, String>) -> String {
    fn format_type(
        ty: &Type,
        is_top_level: bool,
        is_return: bool,
        names: &BTreeMap<usize, String>,
    ) -> String {
        match ty {
            Type::Constructed(name, associated_types) => {
                let ty_name = name
                    .name
                    .map(|name| name.to_string())
                    .unwrap_or_else(|| String::from("<unnamed>"));

                match name.format {
                    TypeNameFormat::Default => {
                        let associated_types = associated_types
                            .iter()
                            .map(|ty| String::from(" ") + &format_type(ty, false, true, names))
                            .collect::<String>();

                        if is_top_level {
                            format!("{}{}", ty_name, associated_types)
                        } else {
                            format!("({}{})", ty_name, associated_types)
                        }
                    }
                    TypeNameFormat::Function => {
                        let left = format_type(&associated_types[0], true, false, names);

                        let right = format_type(&associated_types[1], true, true, names);

                        if is_top_level && is_return {
                            format!("{} {} {}", left, ty_name, right)
                        } else {
                            format!("({} {} {})", left, ty_name, right)
                        }
                    }
                }
            }
            Type::Variable(variable) => names
                .get(variable)
                .cloned()
                .unwrap_or_else(|| String::from("_")),
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
