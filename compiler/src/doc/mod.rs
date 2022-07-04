use crate::{compile, helpers::InternedString, FilePath};
use serde::Serialize;
use std::{collections::HashMap, path::Path, sync::Arc};

const DOC_VERSION: u32 = 1;

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Documentation {
    pub version: u32,
    pub name: String,
    pub types: Vec<Type>,
    pub traits: Vec<Trait>,
    pub constants: Vec<Constant>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Declaration {
    pub name: InternedString,
    pub code: String,
    pub help: Vec<InternedString>,
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
    pub fn new(
        program: compile::Program,
        codemap: HashMap<FilePath, Arc<str>>,
        root: &Path,
    ) -> Self {
        Documentation::with_filter(program, codemap, root, |_| true)
    }

    pub fn with_filter(
        program: compile::Program,
        codemap: HashMap<FilePath, Arc<str>>,
        root: &Path,
        mut filter: impl FnMut(FilePath) -> bool,
    ) -> Self {
        let name = root
            .with_extension("")
            .file_name()
            .unwrap()
            .to_string_lossy()
            .into_owned();

        let mut types = Vec::new();
        let mut traits = Vec::new();
        let mut constants = Vec::new();

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

                    if let Some(result) = $map(&decl, code) {
                        $insert.push(result);
                    }
                }
            }};
        }

        insert_decls!(types, |decl: &compile::typecheck::Declaration<
            compile::typecheck::TypeDeclaration,
        >,
                              code| {
            Some(Type {
                declaration: Declaration {
                    name: decl.name.expect("types always have names"),
                    code,
                    help: decl.value.attributes.help.clone(),
                },
                kind: TypeKind::Structure {
                    fields: Default::default(),
                },
            })
        });

        insert_decls!(traits, |decl: &compile::typecheck::Declaration<
            compile::typecheck::TraitDeclaration,
        >,
                               code| {
            Some(Trait {
                declaration: Declaration {
                    name: decl.name.expect("types always have names"),
                    code,
                    help: decl.value.attributes.decl_attributes.help.clone(),
                },
            })
        });

        insert_decls!(
            generic_constants(decl) => constants,
            |decl: &compile::typecheck::GenericConstantDeclaration<_, _>, code| {
                // instances don't have names; hide them here
                decl.decl.name.map(|name| Constant {
                    declaration: Declaration {
                        name,
                        code,
                        help: decl
                            .attributes
                            .as_ref()
                            .expect("generic constants always have attributes")
                            .help
                            .clone(),
                    },
                })
            }
        );

        Documentation {
            version: DOC_VERSION,
            name,
            types,
            traits,
            constants,
        }
    }
}
