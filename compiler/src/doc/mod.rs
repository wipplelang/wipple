use crate::{compile, FilePath};
use serde::Serialize;
use std::{borrow::Cow, collections::HashMap};

const DOC_VERSION: u32 = 1;

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Documentation {
    pub version: u32,
    pub files: HashMap<String, File>,
}

#[derive(Debug, Clone, Default, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct File {
    // TODO: `[[doc "..."]]` attribute for file-level documentation
    pub types: Vec<Type>,
    pub traits: Vec<Trait>,
    pub constants: Vec<Constant>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Declaration {
    pub name: String,
    pub code: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Type {
    #[serde(flatten)]
    pub declaration: Declaration,
    pub kind: TypeKind,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
#[serde(untagged)]
pub enum TypeKind {
    Structure { fields: Vec<TypeField> },
    Enumeration { variants: Vec<TypeVariant> },
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TypeField {
    #[serde(flatten)]
    pub declaration: Declaration,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TypeVariant {
    #[serde(flatten)]
    pub declaration: Declaration,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Trait {
    #[serde(flatten)]
    pub declaration: Declaration,
    // TODO: instances
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Instance {
    pub code: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Constant {
    #[serde(flatten)]
    pub declaration: Declaration,
}

impl Documentation {
    pub fn new(program: compile::Program, codemap: HashMap<FilePath, Cow<str>>) -> Self {
        Documentation::with_filter(program, codemap, |_| true)
    }

    pub fn with_filter(
        program: compile::Program,
        codemap: HashMap<FilePath, Cow<str>>,
        mut filter: impl FnMut(FilePath) -> bool,
    ) -> Self {
        let mut files = HashMap::<String, File>::new();

        macro_rules! insert_decls {
            ($kind:ident, $map:expr) => {
                insert_decls!($kind => $kind, $map)
            };
            ($kind:ident$(($field:ident))? => $insert:ident, $map:expr) => {{
                let mut decls = program
                    .declarations
                    .$kind
                    .into_values()
                    .filter(|decl| filter(decl$(.$field)?.span.path))
                    .collect::<Vec<_>>();

                decls.sort_by_key(|decl| decl$(.$field)?.span.start);

                for decl in decls {
                    let code = {
                        let file = codemap.get(&decl$(.$field)?.span.path).unwrap();
                        file[decl$(.$field)?.span.start..decl$(.$field)?.span.end].to_string()
                    };

                    if let Some(result) = $map(&decl$(.$field)?, code) {
                        files
                            .entry(decl$(.$field)?.span.path.to_string())
                            .or_default()
                            .$insert
                            .push(result);
                    }
                }
            }};
        }

        insert_decls!(types, |decl: &compile::typecheck::Declaration<_>, code| {
            Some(Type {
                declaration: Declaration {
                    name: decl.name.expect("types always have names").to_string(),
                    code,
                },
                kind: TypeKind::Structure {
                    fields: Default::default(),
                },
            })
        });

        insert_decls!(traits, |decl: &compile::typecheck::Declaration<_>, code| {
            Some(Trait {
                declaration: Declaration {
                    name: decl.name.expect("types always have names").to_string(),
                    code,
                },
            })
        });

        insert_decls!(
            generic_constants(decl) => constants,
            |decl: &compile::typecheck::Declaration<_>, code| {
                // instances don't have names; hide them here
                decl.name.map(|name| Constant {
                    declaration: Declaration {
                        name: name.to_string(),
                        code,
                    },
                })
            }
        );

        Documentation {
            version: DOC_VERSION,
            files,
        }
    }
}
