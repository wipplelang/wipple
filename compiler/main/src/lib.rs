//! Coordinates the compiler passes.

mod convert;

use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, hash::Hash, rc::Rc};
use ts_rs::TS;
use wasm_bindgen::prelude::wasm_bindgen;

pub use wipple_codegen as codegen;
pub use wipple_linker as linker;
pub use wipple_lower as lower;
pub use wipple_syntax as syntax;
pub use wipple_typecheck as typecheck;
pub use wipple_util as util;

/// The default recursion limit.
// TODO: Make this configurable
pub const DEFAULT_RECURSION_LIMIT: u32 = 64;

fn initialize() {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}

fn serialize(value: &impl serde::Serialize) -> String {
    serde_json::to_string(value).unwrap()
}

fn deserialize<'de, T: serde::Deserialize<'de>>(value: &'de str) -> T {
    serde_json::from_str(value).expect("malformed input")
}

/// JavaScript entrypoint to the compiler.
#[wasm_bindgen]
pub fn compile(files: &str, dependencies: &str) -> String {
    initialize();

    let files: Vec<File> = deserialize(files);
    let dependencies: Option<Interface> = deserialize(dependencies);

    let driver = Driver::new();
    let result = driver.compile(files, dependencies);

    serialize(&result)
}

/// JavaScript entrypoint to the linker.
#[wasm_bindgen]
pub fn link(libraries: &str) -> Option<String> {
    initialize();

    let libraries: Vec<Library> = deserialize(libraries);

    let executable = linker::link(libraries).ok()?;
    Some(serialize(&executable))
}

/// JavaScript entrypoint to the formatter.
#[wasm_bindgen]
pub fn format(code: &str) -> String {
    initialize();

    let syntax_driver = SyntaxDriver::for_formatting();

    match syntax::tokenize::tokenize(&syntax_driver, code)
        .collect::<std::result::Result<Vec<_>, _>>()
    {
        Ok(tokens) => syntax::tokenize::format(tokens.iter().map(|token| &token.item)),
        Err(_) => code.to_string(),
    }
}

/// Parse a type.
#[wasm_bindgen]
pub fn parse_type(code: &str) -> Option<String> {
    initialize();

    let syntax_driver = SyntaxDriver::for_formatting();

    let tokens = syntax::tokenize::tokenize(&syntax_driver, code)
        .collect::<std::result::Result<Vec<_>, _>>()
        .ok()?;

    let logical_tokens = syntax::tokenize::to_logical_lines(&syntax_driver, tokens);
    let tree = syntax::tokenize::TokenTree::from_inline(&syntax_driver, logical_tokens)?;

    let parse_result = syntax::parse::parse_type(&syntax_driver, tree.as_ref());
    if !parse_result.diagnostics.is_empty() {
        return None;
    }

    Some(serialize(&parse_result.parsed))
}

/// Convert a compiled type into a parsed type.
#[wasm_bindgen]
pub fn parsed_type_from_compiled(type_: &str) -> String {
    initialize();

    let typecheck_type: util::WithInfo<Info, typecheck::Type<Driver>> = deserialize(type_);
    let lower_type = convert::typecheck::unconvert_type(typecheck_type);
    let syntax_type = convert::lower::unconvert_type::<SyntaxDriver>(lower_type);

    serialize(&syntax_type)
}

/// Check if two parsed types are equal, assuming names are unique.
#[wasm_bindgen]
pub fn parsed_types_are_equal(left: &str, right: &str) -> bool {
    initialize();

    let left: util::WithInfo<Info, syntax::Type<SyntaxDriver>> = deserialize(left);
    let right: util::WithInfo<Info, syntax::Type<SyntaxDriver>> = deserialize(right);

    fn check(left: syntax::Type<SyntaxDriver>, right: syntax::Type<SyntaxDriver>) -> bool {
        match (left, right) {
            (_, syntax::Type::Error | syntax::Type::Placeholder) => true,
            (
                syntax::Type::Declared { name, parameters },
                syntax::Type::Declared {
                    name: expected_name,
                    parameters: expected_parameters,
                },
            ) => {
                name.item == expected_name.item
                    && parameters.len() == expected_parameters.len()
                    && parameters
                        .into_iter()
                        .zip(expected_parameters)
                        .all(|(left, right)| check(left.item, right.item))
            }
            (
                syntax::Type::Function { inputs, output },
                syntax::Type::Function {
                    inputs: expected_inputs,
                    output: expected_output,
                },
            ) => {
                inputs.len() == expected_inputs.len()
                    && inputs
                        .into_iter()
                        .zip(expected_inputs)
                        .all(|(left, right)| check(left.item, right.item))
                    && check(*output.item, *expected_output.item)
            }
            (syntax::Type::Tuple(elements), syntax::Type::Tuple(expected_elements)) => {
                elements.len() == expected_elements.len()
                    && elements
                        .into_iter()
                        .zip(expected_elements)
                        .all(|(left, right)| check(left.item, right.item))
            }
            (syntax::Type::Block(r#type), syntax::Type::Block(expected_type)) => {
                check(*r#type.item, *expected_type.item)
            }
            (syntax::Type::Intrinsic, syntax::Type::Intrinsic) => true,
            _ => false,
        }
    }

    check(left.item, right.item)
}

/// The driver.
#[non_exhaustive]
#[derive(Debug)]
pub struct Driver {
    /// The recursion limit.
    pub recursion_limit: u32,

    /// Whether to include the source code in the compiled interface.
    pub hide_source: bool,

    interface: Interface,
    library: Library,
}

impl Driver {
    /// Create a new driver.
    pub fn new() -> Self {
        Driver {
            recursion_limit: DEFAULT_RECURSION_LIMIT,
            hide_source: false,
            interface: Default::default(),
            library: Default::default(),
        }
    }
}

/// The information contained within items produced by the compiler.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "Info")]
pub struct Info {
    /// Location information produced by the parser.
    pub location: syntax::Location,
}

impl From<syntax::Location> for Info {
    fn from(location: syntax::Location) -> Self {
        Info { location }
    }
}

/// A file provided to [`Driver::compile`].
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "File")]
pub struct File {
    /// The file's path.
    pub path: String,

    /// The path to be rendered in diagnostics.
    pub visible_path: String,

    /// The file's contents.
    pub code: String,
}

/// An interface generated by the compiler.
#[non_exhaustive]
#[derive(Debug, Default, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "Interface")]
pub struct Interface {
    /// The files used during compilation. Not all files may be available.
    pub files: Vec<File>,

    /// The names of top-level declarations in the program.
    pub top_level: HashMap<String, Vec<util::WithInfo<Info, lower::Path>>>,

    /// The type declarations in the program.
    pub type_declarations:
        HashMap<lower::Path, util::WithInfo<Info, typecheck::TypeDeclaration<Driver>>>,

    /// The trait declarations in the program.
    pub trait_declarations:
        HashMap<lower::Path, util::WithInfo<Info, typecheck::TraitDeclaration<Driver>>>,

    /// The type parameters in the program.
    pub type_parameter_declarations:
        HashMap<lower::Path, util::WithInfo<Info, typecheck::TypeParameterDeclaration<Driver>>>,

    /// The language declarations in the program.
    pub language_declarations: HashMap<String, lower::Path>,

    /// The constant declarations in the program.
    pub constant_declarations:
        HashMap<lower::Path, util::WithInfo<Info, typecheck::ConstantDeclaration<Driver>>>,

    /// The instance declarations in the program.
    pub instance_declarations:
        HashMap<lower::Path, util::WithInfo<Info, typecheck::InstanceDeclaration<Driver>>>,
}

/// A linked executable.
pub type Executable = wipple_linker::Executable<Driver>;

/// A library generated by the compiler.
pub type Library = wipple_linker::UnlinkedLibrary<Driver>;

/// An analyzed expression along with its compiled IR.
pub type Item = wipple_linker::UnlinkedItem<Driver>;

/// The result of [`Driver::compile`].
#[non_exhaustive]
#[derive(Debug, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "Result")]
pub struct Result {
    /// The generated interface.
    pub interface: Interface,

    /// The generated library.
    pub library: Library,

    /// Any diagnostics ocurring during compilation.
    pub diagnostics: Vec<util::WithInfo<Info, Diagnostic>>,
}

/// Diagnostics produced by the compiler.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase", tag = "type", content = "value")]
#[ts(export, rename = "Diagnostic")]
pub enum Diagnostic {
    Tokenize(syntax::tokenize::Diagnostic<SyntaxDriver>),
    Parse(syntax::parse::Diagnostic<SyntaxDriver>),
    Syntax(syntax::Diagnostic),
    Lower(lower::Diagnostic),
    Typecheck(typecheck::Diagnostic<Driver>),
}

impl Driver {
    /// Compile a set of source files into a [`Library`] and [`Interface`].
    pub fn compile(mut self, files: Vec<File>, dependencies: Option<Interface>) -> Result {
        let mut diagnostics = Vec::new();

        if let Some(dependencies) = &dependencies {
            self.interface.files.extend(dependencies.files.clone());
        }

        if !self.hide_source {
            self.interface.files.extend(files.clone());
        }

        let files = files.into_iter().map(|file| {
            let syntax_driver = SyntaxDriver {
                file_path: Rc::from(file.path.as_str()),
                visible_path: Rc::from(file.visible_path.as_str()),
                file_size: file.code.len() as u32,
            };

            let (tokens, tokenize_diagnostics): (Vec<_>, Vec<_>) =
                syntax::tokenize::tokenize(&syntax_driver, &file.code).partition_result();

            diagnostics.extend(
                tokenize_diagnostics
                    .into_iter()
                    .map(|diagnostic| diagnostic.map(Diagnostic::Tokenize)),
            );

            let logical_tokens = syntax::tokenize::to_logical_lines(&syntax_driver, tokens);

            let (tree, tokenize_diagnostics) =
                syntax::tokenize::TokenTree::from_top_level(&syntax_driver, logical_tokens);

            diagnostics.extend(
                tokenize_diagnostics
                    .into_iter()
                    .map(|diagnostic| diagnostic.map(Diagnostic::Tokenize)),
            );

            let parse_result = syntax::parse::parse_top_level(&syntax_driver, tree.as_ref());

            diagnostics.extend(
                parse_result
                    .diagnostics
                    .into_iter()
                    .map(|error| error.map(Diagnostic::Parse)),
            );

            let syntax_result = syntax::parse(&syntax_driver, parse_result.parsed);

            diagnostics.extend(
                syntax_result
                    .diagnostics
                    .into_iter()
                    .map(|diagnostic| diagnostic.map(Diagnostic::Syntax)),
            );

            convert::lower::convert(syntax_result.top_level)
        });

        let lower_result = wipple_lower::resolve(
            &self,
            files,
            dependencies
                .map(|interface| lower::Interface {
                    top_level: interface.top_level,
                    type_declarations: interface
                        .type_declarations
                        .into_iter()
                        .map(|(path, item)| {
                            (path, convert::interface::convert_type_declaration(item))
                        })
                        .collect(),
                    trait_declarations: interface
                        .trait_declarations
                        .into_iter()
                        .map(|(path, item)| {
                            (path, convert::interface::convert_trait_declaration(item))
                        })
                        .collect(),
                    type_parameter_declarations: interface
                        .type_parameter_declarations
                        .into_iter()
                        .map(|(path, item)| {
                            (
                                path,
                                convert::interface::convert_type_parameter_declaration(item),
                            )
                        })
                        .collect(),
                    language_declarations: interface.language_declarations,
                    constant_declarations: interface
                        .constant_declarations
                        .into_iter()
                        .map(|(path, item)| {
                            (path, convert::interface::convert_constant_declaration(item))
                        })
                        .collect(),
                    instance_declarations: interface
                        .instance_declarations
                        .into_iter()
                        .map(|(path, item)| {
                            (path, convert::interface::convert_instance_declaration(item))
                        })
                        .collect(),
                })
                .unwrap_or_default(),
        );

        diagnostics.extend(
            lower_result
                .diagnostics
                .into_iter()
                .map(|error| error.map(Diagnostic::Lower)),
        );

        self.interface.top_level = lower_result.interface.top_level;

        for (path, item) in lower_result.interface.type_declarations {
            let declaration = convert::typecheck::convert_type_declaration(item);
            self.interface.type_declarations.insert(path, declaration);
        }

        for (path, item) in lower_result.interface.trait_declarations {
            let declaration = convert::typecheck::convert_trait_declaration(item);
            self.interface.trait_declarations.insert(path, declaration);
        }

        for (path, item) in lower_result.interface.type_parameter_declarations {
            let declaration = convert::typecheck::convert_type_parameter_declaration(item);
            self.interface
                .type_parameter_declarations
                .insert(path, declaration);
        }

        self.interface.language_declarations = lower_result.interface.language_declarations;

        for (path, item) in &lower_result.interface.constant_declarations {
            let declaration = convert::typecheck::convert_constant_declaration(item.clone());

            self.interface
                .constant_declarations
                .insert(path.clone(), declaration);
        }

        for (path, item) in &lower_result.interface.instance_declarations {
            let declaration = convert::typecheck::convert_instance_declaration(item.clone());

            self.interface
                .instance_declarations
                .insert(path.clone(), declaration);
        }

        for (path, item) in lower_result.interface.constant_declarations {
            let body = match lower_result.library.items.get(&path) {
                Some(body) => body.clone(),
                None => continue,
            };

            let declaration = convert::typecheck::convert_constant_declaration(item);

            let typecheck_result = wipple_typecheck::resolve(
                &self,
                (
                    declaration.clone(),
                    convert::typecheck::convert_expression(body),
                ),
            );

            diagnostics.extend(
                typecheck_result
                    .diagnostics
                    .into_iter()
                    .map(|error| error.map(Diagnostic::Typecheck)),
            );

            let exhaustiveness_diagnostics =
                wipple_typecheck::check_exhaustiveness(&self, typecheck_result.item.as_ref());

            diagnostics.extend(
                exhaustiveness_diagnostics
                    .into_iter()
                    .map(|error| error.map(Diagnostic::Typecheck)),
            );

            let codegen_result =
                codegen::compile(&self, path.clone(), typecheck_result.item.as_ref());

            self.library.items.insert(
                path,
                Item {
                    parameters: declaration.item.parameters,
                    expression: typecheck_result.item,
                    ir: codegen_result.map(|result| result.labels),
                },
            );
        }

        for (path, item) in lower_result.interface.instance_declarations {
            let body = match lower_result.library.items.get(&path) {
                Some(body) => body.clone(),
                None => continue,
            };

            let declaration = convert::typecheck::convert_instance_declaration(item);

            let typecheck_result = wipple_typecheck::resolve(
                &self,
                (
                    declaration.clone(),
                    convert::typecheck::convert_expression(body),
                ),
            );

            diagnostics.extend(
                typecheck_result
                    .diagnostics
                    .into_iter()
                    .map(|error| error.map(Diagnostic::Typecheck)),
            );

            let exhaustiveness_diagnostics =
                wipple_typecheck::check_exhaustiveness(&self, typecheck_result.item.as_ref());

            diagnostics.extend(
                exhaustiveness_diagnostics
                    .into_iter()
                    .map(|error| error.map(Diagnostic::Typecheck)),
            );

            let codegen_result =
                codegen::compile(&self, path.clone(), typecheck_result.item.as_ref());

            self.library.items.insert(
                path,
                Item {
                    parameters: declaration.item.parameters,
                    expression: typecheck_result.item,
                    ir: codegen_result.map(|result| result.labels),
                },
            );
        }

        {
            let typecheck_result = wipple_typecheck::resolve(
                &self,
                wipple_util::WithInfo {
                    info: <Self as wipple_typecheck::Driver>::top_level_info(&self),
                    item: lower_result
                        .library
                        .code
                        .into_iter()
                        .map(convert::typecheck::convert_expression)
                        .collect(),
                },
            );

            diagnostics.extend(
                typecheck_result
                    .diagnostics
                    .into_iter()
                    .map(|error| error.map(Diagnostic::Typecheck)),
            );

            let exhaustiveness_diagnostics =
                wipple_typecheck::check_exhaustiveness(&self, typecheck_result.item.as_ref());

            diagnostics.extend(
                exhaustiveness_diagnostics
                    .into_iter()
                    .map(|error| error.map(Diagnostic::Typecheck)),
            );

            let codegen_result = codegen::compile(
                &self,
                lower::Path::top_level(),
                typecheck_result.item.as_ref(),
            );

            self.library.code.push(Item {
                parameters: Vec::new(),
                expression: typecheck_result.item,
                ir: codegen_result.map(|result| result.labels),
            });
        }

        let instances_by_trait = self
            .interface
            .instance_declarations
            .iter()
            .into_group_map_by(|(_, instance)| instance.item.instance.item.r#trait.clone());

        for (r#trait, instances) in instances_by_trait {
            let instances = instances
                .into_iter()
                .map(|(path, _)| path.clone())
                .collect::<Vec<_>>();

            let overlap_diagnostics =
                typecheck::instances_overlap(&self, &r#trait, instances.clone());

            diagnostics.extend(
                overlap_diagnostics
                    .into_iter()
                    .map(|error| error.map(Diagnostic::Typecheck)),
            );

            self.library.instances.insert(r#trait.clone(), instances);
        }

        macro_rules! insert_intrinsic_variant {
            ($name:literal) => {
                if let Some(value) = self.interface.language_declarations.get($name) {
                    self.library
                        .intrinsic_variants
                        .insert(String::from($name), variant_from_constructor(value.clone()));
                }
            };
            ($($name:literal),* $(,)?) => {
                $(insert_intrinsic_variant!($name);)*
            }
        }

        insert_intrinsic_variant!(
            "false",
            "true",
            "none",
            "some",
            "is-less-than",
            "is-equal-to",
            "is-greater-than",
        );

        Result {
            interface: self.interface,
            library: self.library,
            diagnostics: diagnostics.into_iter().unique().collect(),
        }
    }
}

#[doc(hidden)]
pub struct SyntaxDriver {
    file_path: Rc<str>,
    visible_path: Rc<str>,
    file_size: u32,
}

impl SyntaxDriver {
    fn for_formatting() -> Self {
        Self {
            file_path: Rc::from("format"),
            visible_path: Rc::from("format"),
            file_size: 0,
        }
    }
}

impl syntax::Driver for SyntaxDriver {
    type Info = Info;

    fn file_path(&self) -> Rc<str> {
        self.file_path.clone()
    }

    fn visible_path(&self) -> Rc<str> {
        self.visible_path.clone()
    }

    fn file_size(&self) -> u32 {
        self.file_size
    }

    fn merge_info(left: Self::Info, right: Self::Info) -> Self::Info {
        Info {
            location: syntax::Location {
                path: left.location.path,
                visible_path: left.location.visible_path,
                span: left.location.span.start..right.location.span.end,
            },
        }
    }
}

impl wipple_lower::Driver for Driver {
    type Info = Info;
}

impl wipple_typecheck::Driver for Driver {
    type Info = Info;
    type Path = wipple_lower::Path;

    fn recursion_limit(&self) -> u32 {
        self.recursion_limit
    }

    fn top_level_info(&self) -> Self::Info {
        syntax::Location {
            path: Rc::from("top-level"),
            visible_path: Rc::from("top-level"),
            span: 0..0,
        }
        .into()
    }

    fn path_for_language_type(&self, language_item: &'static str) -> Option<Self::Path> {
        Some(
            self.interface
                .language_declarations
                .get(language_item)?
                .clone(),
        )
    }

    fn path_for_language_trait(&self, language_item: &'static str) -> Option<Self::Path> {
        Some(
            self.interface
                .language_declarations
                .get(language_item)?
                .clone(),
        )
    }

    fn path_for_language_constructor(&self, language_item: &'static str) -> Option<Self::Path> {
        Some(
            self.interface
                .language_declarations
                .get(language_item)?
                .clone(),
        )
    }

    fn path_for_language_constant(&self, language_item: &'static str) -> Option<Self::Path> {
        Some(
            self.interface
                .language_declarations
                .get(language_item)?
                .clone(),
        )
    }

    fn paths_are_equal(&self, left: &Self::Path, right: &Self::Path) -> bool {
        left == right
    }

    fn get_type_declaration(
        &self,
        path: &Self::Path,
    ) -> util::WithInfo<Self::Info, wipple_typecheck::TypeDeclaration<Self>> {
        self.interface
            .type_declarations
            .get(path)
            .unwrap_or_else(|| panic!("missing type declaration {:?}", path))
            .clone()
    }

    fn get_trait_declaration(
        &self,
        path: &Self::Path,
    ) -> util::WithInfo<Self::Info, wipple_typecheck::TraitDeclaration<Self>> {
        self.interface
            .trait_declarations
            .get(path)
            .expect("missing trait declaration")
            .clone()
    }

    fn get_type_parameter_declaration(
        &self,
        path: &Self::Path,
    ) -> util::WithInfo<Self::Info, wipple_typecheck::TypeParameterDeclaration<Self>> {
        self.interface
            .type_parameter_declarations
            .get(path)
            .expect("missing type parameter declaration")
            .clone()
    }

    fn get_constant_declaration(
        &self,
        path: &Self::Path,
    ) -> util::WithInfo<Self::Info, wipple_typecheck::ConstantDeclaration<Self>> {
        self.interface
            .constant_declarations
            .get(path)
            .unwrap_or_else(|| panic!("missing constant declaration {path:#?}"))
            .clone()
    }

    fn get_instance_declaration(
        &self,
        path: &Self::Path,
    ) -> util::WithInfo<Self::Info, wipple_typecheck::InstanceDeclaration<Self>> {
        self.interface
            .instance_declarations
            .get(path)
            .expect("missing instance declaration")
            .clone()
    }

    fn get_instances_for_trait(&self, r#trait: &Self::Path) -> Vec<Self::Path> {
        self.interface
            .instance_declarations
            .iter()
            .filter(|(_, instance)| instance.item.instance.item.r#trait == *r#trait)
            .map(|(path, _)| path.clone())
            .collect()
    }

    fn get_enumeration_for_variant(&self, variant: &Self::Path) -> Self::Path {
        // The parent of a variant is its enumeration
        lower::Path(variant[0..variant.len() - 1].to_vec())
    }
}

impl wipple_codegen::Driver for Driver {
    fn number_type(&self) -> Option<Self::Path> {
        self.interface.language_declarations.get("number").cloned()
    }

    fn text_type(&self) -> Option<Self::Path> {
        self.interface.language_declarations.get("text").cloned()
    }

    fn boolean_type(&self) -> Option<Self::Path> {
        self.interface.language_declarations.get("boolean").cloned()
    }

    fn true_variant(&self) -> Option<Self::Path> {
        self.interface
            .language_declarations
            .get("true")
            .cloned()
            .map(variant_from_constructor)
    }

    fn number_equality_intrinsic(&self) -> Option<String> {
        Some(String::from("number-equality"))
    }

    fn text_equality_intrinsic(&self) -> Option<String> {
        Some(String::from("text-equality"))
    }
}

impl wipple_linker::Driver for Driver {}

fn variant_from_constructor(mut path: lower::Path) -> lower::Path {
    let name = match path.pop().unwrap() {
        lower::PathComponent::Constructor(name) => name,
        _ => panic!("expected constructor"),
    };

    path.push(lower::PathComponent::Variant(name));

    path
}
