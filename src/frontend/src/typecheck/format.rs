use super::{Type, TypeSchema};
use crate::TypeNameFormat;
use std::collections::BTreeMap;

pub fn format_type_schema(ty: &TypeSchema) -> String {
    match ty {
        polytype::TypeSchema::Monotype(ty) => format_type(ty),
        polytype::TypeSchema::Polytype { .. } => {
            let mut ty = ty;
            let mut names = BTreeMap::new();
            while let TypeSchema::Polytype { variable, body } = ty {
                names.insert(*variable, name_for_index(names.len()));
                ty = body
            }

            let ty = match ty {
                polytype::TypeSchema::Monotype(ty) => ty,
                polytype::TypeSchema::Polytype { .. } => unreachable!(),
            };

            format!(
                "for {}-> {}",
                names
                    .values()
                    .rev()
                    .map(|t| t.clone() + " ")
                    .collect::<String>(),
                format_type_with(ty, &names)
            )
        }
    }
}

macro_rules! format_type_fn {
    ($fn:ident($($param:ident: $ty:ty),* $(,)?), $body:expr) => {
        pub fn $fn(ty: &Type, $($param: $ty),*) -> String {
            match ty {
                Type::Constructed(name, associated_types) => {
                    // TODO: Parentheses
                    match name.format {
                        TypeNameFormat::Default => format!(
                            "{}{}",
                            name.name,
                            associated_types
                                .iter()
                                .map(|ty| String::from(" ") + &$fn(ty, $($param),*))
                                .collect::<String>()
                        ),
                        TypeNameFormat::Function => {
                            let left = $fn(&associated_types[0], $($param),*);
                            let right = $fn(&associated_types[1], $($param),*);

                            format!("{} {} {}", left, name.name, right)
                        }
                    }
                }
                Type::Variable(variable) => {
                    #[allow(clippy::redundant_closure_call)]
                    ($body)(variable)
                },
            }
        }
    };
}

format_type_fn!(format_type(), |_| String::from("_"));

format_type_fn!(
    format_type_with(names: &BTreeMap<usize, String>),
    |variable| names
        .get(variable)
        .cloned()
        .unwrap_or_else(|| String::from("_"))
);

fn name_for_index(index: usize) -> String {
    const LETTERS: [char; 26] = [
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
        'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    ];

    LETTERS
        .get(index)
        .copied()
        .map(String::from)
        .unwrap_or_else(|| format!("T{}", index))
}
