//! Coordinates the compiler passes.

mod convert;
pub mod fix;
pub mod lint;
mod visit;

use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    sync::Arc,
};

pub use wipple_ir as ir;
pub use wipple_linker as linker;
pub use wipple_lower as lower;
pub use wipple_syntax as syntax;
pub use wipple_typecheck as typecheck;
pub use wipple_util as util;

/// The default recursion limit.
// TODO: Make this configurable
pub const DEFAULT_RECURSION_LIMIT: u32 = 64;

/// Entrypoint to the compiler.
pub fn compile(files: Vec<File>, dependencies: Vec<Interface>) -> Result {
    Driver::new().compile(files, dependencies)
}

/// Entrypoint to the linker.

pub fn link(libraries: Vec<Library>) -> Option<Executable> {
    linker::link(libraries).ok()
}

/// Entrypoint to the formatter.
pub fn format(code: &str) -> String {
    let syntax_driver = SyntaxDriver::for_formatting();

    match syntax::tokenize::tokenize(&syntax_driver, code)
        .collect::<std::result::Result<Vec<_>, _>>()
    {
        Ok(tokens) => syntax::tokenize::format(tokens.iter().map(|token| &token.item).collect()),
        Err(_) => code.to_string(),
    }
}

/// Resolve an attribute-like trait, where the first parameter is the provided
/// type and the remaining parameters are returned.
pub fn resolve_attribute_like_trait(
    name: &str,
    r#type: util::WithInfo<Info, typecheck::Type<Driver>>,
    number_of_parameters: u32,
    interface: Interface,
) -> Option<Vec<util::WithInfo<Info, typecheck::Type<Driver>>>> {
    let mut driver = Driver::new();
    driver.interface = interface;

    typecheck::resolve_attribute_like_trait(&driver, name, r#type.as_ref(), number_of_parameters)
}

/// Check if the provided declared type path represents a language item.
pub fn type_is_language_item(
    path: &wipple_lower::Path,
    language_item: &str,
    interface: Interface,
) -> bool {
    use wipple_typecheck::Driver as _;

    let mut driver = Driver::new();
    driver.interface = interface;

    driver
        .path_for_language_type(language_item)
        .is_some_and(|item| item == *path)
}

/// Attempt to fix the file mentioned in the provided diagnostic. See the
/// documentation for [`fix`](mod@fix) for more information.
pub fn fix_file(
    diagnostic: util::WithInfo<Info, Diagnostic>,
    files: Vec<File>,
    dependencies: Vec<Interface>,
) -> Option<(fix::Fix, String)> {
    let path = diagnostic.info.location.path.as_ref();

    let span = (diagnostic.info.location.span.start as usize)
        ..(diagnostic.info.location.span.end as usize);

    let result = Driver::new().compile(files.clone(), dependencies.clone());

    let fix = fix::fix(
        diagnostic.as_ref(),
        &result.interface,
        &result.library,
        move |fix| {
            let mut files = files.clone();
            let file = files.iter_mut().find(|file| file.path == path)?;

            let new_span = fix.apply_to(&mut file.code, span.clone());

            let new_info = Info {
                location: syntax::Location {
                    path: Arc::from(file.path.as_str()),
                    visible_path: Arc::from(file.visible_path.as_str()),
                    span: (new_span.start as u32)..(new_span.end as u32),
                },
            };

            let code = util::WithInfo {
                info: new_info,
                item: file.code.clone(),
            };

            let result = Driver::new().compile(files, dependencies.clone());

            Some((code, result.interface, result.library))
        },
    )?;

    Some(fix.item)
}

/// The driver.
#[non_exhaustive]
#[derive(Debug)]
pub struct Driver {
    recursion_limit: u32,
    hide_source: bool,
    lint: bool,
    dependencies: Vec<Interface>,
    interface: Interface,
    library: Library,
    ide: Ide,
}

impl Driver {
    fn new() -> Self {
        Driver {
            recursion_limit: DEFAULT_RECURSION_LIMIT,
            hide_source: false,
            lint: true,
            dependencies: Default::default(),
            interface: Default::default(),
            library: Default::default(),
            ide: Default::default(),
        }
    }
}

/// The information contained within items produced by the compiler.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
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
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
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
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Interface {
    /// The files used during compilation. Not all files may be available.
    pub files: Vec<File>,

    /// The names of top-level declarations in the program.
    pub top_level: HashMap<String, Vec<util::WithInfo<Info, lower::Path>>>,

    /// The syntax declarations in the program.
    pub syntax_declarations:
        HashMap<lower::Path, util::WithInfo<Info, typecheck::SyntaxDeclaration<Driver>>>,

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
    pub language_declarations: HashMap<String, Vec<lower::Path>>,

    /// The constant declarations in the program.
    pub constant_declarations:
        HashMap<lower::Path, util::WithInfo<Info, typecheck::ConstantDeclaration<Driver>>>,

    /// The instance declarations in the program.
    pub instance_declarations:
        HashMap<lower::Path, util::WithInfo<Info, typecheck::InstanceDeclaration<Driver>>>,
}

impl Extend<Self> for Interface {
    fn extend<T: IntoIterator<Item = Self>>(&mut self, iter: T) {
        for interface in iter {
            self.files.extend(interface.files);
            self.top_level.extend(interface.top_level);
            self.syntax_declarations
                .extend(interface.syntax_declarations);
            self.type_declarations.extend(interface.type_declarations);
            self.trait_declarations.extend(interface.trait_declarations);
            self.type_parameter_declarations
                .extend(interface.type_parameter_declarations);
            self.constant_declarations
                .extend(interface.constant_declarations);
            self.instance_declarations
                .extend(interface.instance_declarations);

            for (name, paths) in interface.language_declarations {
                self.language_declarations
                    .entry(name)
                    .or_default()
                    .extend(paths);
            }
        }
    }
}

/// A linked executable.
pub type Executable = wipple_linker::Executable<Driver>;

/// A library generated by the compiler.
pub type Library = wipple_linker::UnlinkedLibrary<Driver>;

/// Information collected for IDEs.
pub type Ide = wipple_lower::Ide<Driver>;

/// An analyzed expression along with its compiled IR.
pub type Item = wipple_linker::UnlinkedItem<Driver>;

/// An analyzed instance.
pub type Instance = wipple_linker::UnlinkedInstance<Driver>;

/// The result of [`Driver::compile`].
#[non_exhaustive]
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Result {
    /// The generated interface.
    pub interface: Interface,

    /// The generated library.
    pub library: Library,

    /// Information collected for IDEs.
    pub ide: Ide,

    /// Any diagnostics ocurring during compilation.
    pub diagnostics: Vec<util::WithInfo<Info, Diagnostic>>,
}

/// Diagnostics produced by the compiler.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", tag = "type", content = "value")]
pub enum Diagnostic {
    Tokenize(syntax::tokenize::Diagnostic<SyntaxDriver>),
    Parse(syntax::parse::Diagnostic<SyntaxDriver>),
    Syntax(syntax::Diagnostic),
    Lower(lower::Diagnostic),
    Typecheck(typecheck::Diagnostic<Driver>),
    Ir,
    Lint(lint::Lint),
}

impl Driver {
    /// Compile a set of source files into a [`Library`] and [`Interface`].
    fn compile(mut self, files: Vec<File>, dependencies: Vec<Interface>) -> Result {
        let mut diagnostics = Vec::new();

        for dependency in &dependencies {
            self.interface.files.extend(dependency.files.clone());
        }

        if !self.hide_source {
            self.interface.files.extend(files.clone());
        }

        let files = files.into_iter().map(|file| {
            let syntax_driver = SyntaxDriver {
                file_path: Arc::from(file.path.as_str()),
                visible_path: Arc::from(file.visible_path.as_str()),
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

            convert::lower::convert(file.visible_path, syntax_result.top_level)
        });

        let lower_result = wipple_lower::resolve(
            &self,
            files,
            dependencies
                .iter()
                .cloned()
                .map(|interface| lower::Interface {
                    top_level: interface.top_level,
                    syntax_declarations: interface
                        .syntax_declarations
                        .into_iter()
                        .map(|(name, item)| {
                            (name, convert::interface::convert_syntax_declaration(item))
                        })
                        .collect(),
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
                .collect(),
        );

        self.ide.merge(lower_result.ide);

        diagnostics.extend(
            lower_result
                .diagnostics
                .into_iter()
                .map(|error| error.map(Diagnostic::Lower)),
        );

        self.interface.top_level = lower_result.interface.top_level;

        for (path, item) in lower_result.interface.syntax_declarations {
            let declaration = convert::typecheck::convert_syntax_declaration(item);
            self.interface.syntax_declarations.insert(path, declaration);
        }

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

        self.dependencies = dependencies;

        for (path, declaration) in lower_result.interface.constant_declarations {
            let item = match lower_result
                .library
                .items
                .get(&path)
                // Constants always have values; skip ones that somehow don't...
                .and_then(|item| item.as_ref())
            {
                Some(item) => item.clone(),
                None => continue, // ...here
            };

            let declaration = convert::typecheck::convert_constant_declaration(declaration);

            let typecheck_result = wipple_typecheck::resolve(
                &self,
                (declaration.clone(), convert::typecheck::convert_item(item)),
            );

            let item = match typecheck_result.item {
                Some(item) => item,
                None => continue,
            };

            diagnostics.extend(
                typecheck_result
                    .diagnostics
                    .into_iter()
                    .map(|error| error.map(Diagnostic::Typecheck)),
            );

            let exhaustiveness_diagnostics =
                wipple_typecheck::check_exhaustiveness(&self, item.as_ref());

            diagnostics.extend(
                exhaustiveness_diagnostics
                    .into_iter()
                    .map(|error| error.map(Diagnostic::Typecheck)),
            );

            let ir_result = ir::compile(
                &self,
                path.clone(),
                &declaration.item.attributes,
                item.as_ref(),
                &typecheck_result.captures,
            );

            for (path, item) in ir_result.items {
                self.library.items.insert(
                    path,
                    Item {
                        parameters: declaration.item.parameters.clone(),
                        bounds: declaration
                            .item
                            .bounds
                            .clone()
                            .into_iter()
                            .filter_map(|bound| ir::instance_descriptor(&bound.item))
                            .collect(),
                        expression: item.expression,
                        ir: Some(item.instructions),
                        evaluate_once: item.evaluate_once,
                    },
                );
            }
        }

        for (path, declaration) in lower_result.interface.instance_declarations {
            let item = match lower_result.library.items.get(&path) {
                Some(item) => item.clone(),
                None => {
                    // `None` here means that the implementation is in a
                    // different library; skip it
                    continue;
                }
            };

            let declaration = convert::typecheck::convert_instance_declaration(declaration);

            let typecheck_result = wipple_typecheck::resolve(
                &self,
                (
                    declaration.clone(),
                    item.map(convert::typecheck::convert_item),
                ),
            );

            let item = match typecheck_result.item {
                Some(item) => item,
                None => continue,
            };

            diagnostics.extend(
                typecheck_result
                    .diagnostics
                    .into_iter()
                    .map(|error| error.map(Diagnostic::Typecheck)),
            );

            let exhaustiveness_diagnostics =
                wipple_typecheck::check_exhaustiveness(&self, item.as_ref());

            diagnostics.extend(
                exhaustiveness_diagnostics
                    .into_iter()
                    .map(|error| error.map(Diagnostic::Typecheck)),
            );

            let ir_result = ir::compile(
                &self,
                path.clone(),
                &[],
                item.as_ref(),
                &typecheck_result.captures,
            );

            for (path, item) in ir_result.items {
                self.library.items.insert(
                    path,
                    Item {
                        parameters: declaration.item.parameters.clone(),
                        bounds: declaration
                            .item
                            .bounds
                            .clone()
                            .into_iter()
                            .filter_map(|bound| ir::instance_descriptor(&bound.item))
                            .collect(),
                        expression: item.expression,
                        ir: Some(item.instructions),
                        evaluate_once: item.evaluate_once,
                    },
                );
            }
        }

        for (path, top_level_code) in lower_result.library.code {
            let typecheck_result = wipple_typecheck::resolve(
                &self,
                convert::typecheck::convert_top_level_code(top_level_code),
            );

            if let Some(item) = typecheck_result.item {
                diagnostics.extend(
                    typecheck_result
                        .diagnostics
                        .into_iter()
                        .map(|error| error.map(Diagnostic::Typecheck)),
                );

                let exhaustiveness_diagnostics =
                    wipple_typecheck::check_exhaustiveness(&self, item.as_ref());

                diagnostics.extend(
                    exhaustiveness_diagnostics
                        .into_iter()
                        .map(|error| error.map(Diagnostic::Typecheck)),
                );

                let ir_result = ir::compile(
                    &self,
                    path.clone(),
                    &[],
                    item.as_ref(),
                    &typecheck_result.captures,
                );

                for (path, item) in ir_result.items {
                    self.library.items.insert(
                        path,
                        Item {
                            parameters: Vec::new(),
                            bounds: Vec::new(),
                            expression: item.expression,
                            ir: Some(item.instructions),
                            evaluate_once: item.evaluate_once,
                        },
                    );
                }

                self.library.entrypoints.push(path);
            }
        }

        for (path, type_declaration) in &self.interface.type_declarations {
            if let Some(layout) = ir::layout_descriptor(&type_declaration.item) {
                self.library.layouts.insert(path.clone(), layout);
            }
        }

        let instances_by_trait = self
            .interface
            .instance_declarations
            .iter()
            .into_group_map_by(|(_, instance)| instance.item.instance.item.r#trait.clone());

        for (r#trait, instances) in instances_by_trait {
            let trait_declaration = typecheck::Driver::get_trait_declaration(&self, &r#trait);

            let mut default_instances = HashSet::new();
            let instances = instances
                .into_iter()
                .map(|(path, declaration)| {
                    if declaration.item.default {
                        default_instances.insert(path.clone());
                    }

                    Instance {
                        path: path.clone(),
                        trait_parameters: trait_declaration
                            .item
                            .parameters
                            .clone()
                            .into_iter()
                            .zip(declaration.item.instance.item.parameters.clone())
                            .collect(),
                    }
                })
                .collect::<Vec<_>>();

            let overlap_diagnostics = typecheck::instances_overlap(
                &self,
                &r#trait,
                instances
                    .iter()
                    .map(|instance| instance.path.clone())
                    .collect(),
            );

            diagnostics.extend(
                overlap_diagnostics
                    .into_iter()
                    .map(|error| error.map(Diagnostic::Typecheck)),
            );

            for instance in instances {
                let instances = if default_instances.contains(&instance.path) {
                    &mut self.library.default_instances
                } else {
                    &mut self.library.instances
                };

                instances.entry(r#trait.clone()).or_default().push(instance);
            }
        }

        if self.lint {
            let lints = lint::lint(&self.interface, &self.library);

            diagnostics.extend(lints.into_iter().map(|lint| lint.map(Diagnostic::Lint)));
        }

        Result {
            interface: self.interface,
            library: self.library,
            ide: self.ide,
            diagnostics: diagnostics.into_iter().unique().collect(),
        }
    }
}

#[doc(hidden)]
pub struct SyntaxDriver {
    file_path: Arc<str>,
    visible_path: Arc<str>,
    file_size: u32,
}

impl SyntaxDriver {
    fn for_formatting() -> Self {
        Self {
            file_path: Arc::from("format"),
            visible_path: Arc::from("format"),
            file_size: 0,
        }
    }
}

impl syntax::Driver for SyntaxDriver {
    type Info = Info;

    fn file_path(&self) -> Arc<str> {
        self.file_path.clone()
    }

    fn visible_path(&self) -> Arc<str> {
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

macro_rules! path_for_language {
    ($kind:ident, $self:expr, $language_item:expr) => {
        [&$self.interface]
            .into_iter()
            .chain(&$self.dependencies)
            .filter_map(|interface| interface.language_declarations.get($language_item))
            .flatten()
            .find(|path| matches!(path.last().unwrap(), lower::PathComponent::$kind(_)))
            .cloned()
    };
}

impl wipple_typecheck::Driver for Driver {
    type Info = Info;
    type Path = wipple_lower::Path;

    fn recursion_limit(&self) -> u32 {
        self.recursion_limit
    }

    fn top_level_info(&self) -> Self::Info {
        syntax::Location {
            path: Arc::from("top-level"),
            visible_path: Arc::from("top-level"),
            span: 0..0,
        }
        .into()
    }

    fn path_for_language_type(&self, language_item: &str) -> Option<Self::Path> {
        path_for_language!(Type, self, language_item)
    }

    fn path_for_language_trait(&self, language_item: &str) -> Option<Self::Path> {
        path_for_language!(Trait, self, language_item)
    }

    fn path_for_language_constructor(&self, language_item: &str) -> Option<Self::Path> {
        path_for_language!(Constructor, self, language_item)
    }

    fn path_for_language_constant(&self, language_item: &str) -> Option<Self::Path> {
        path_for_language!(Constant, self, language_item)
    }

    fn paths_are_equal(&self, left: &Self::Path, right: &Self::Path) -> bool {
        left == right
    }

    fn get_type_declaration(
        &self,
        path: &Self::Path,
    ) -> util::WithInfo<Self::Info, wipple_typecheck::TypeDeclaration<Self>> {
        [&self.interface]
            .into_iter()
            .chain(&self.dependencies)
            .find_map(|interface| interface.type_declarations.get(path).cloned())
            .unwrap_or_else(|| panic!("missing type declaration {:?}", path))
    }

    fn get_trait_declaration(
        &self,
        path: &Self::Path,
    ) -> util::WithInfo<Self::Info, wipple_typecheck::TraitDeclaration<Self>> {
        [&self.interface]
            .into_iter()
            .chain(&self.dependencies)
            .find_map(|interface| interface.trait_declarations.get(path).cloned())
            .unwrap_or_else(|| panic!("missing trait declaration {:?}", path))
    }

    fn get_type_parameter_declaration(
        &self,
        path: &Self::Path,
    ) -> util::WithInfo<Self::Info, wipple_typecheck::TypeParameterDeclaration<Self>> {
        [&self.interface]
            .into_iter()
            .chain(&self.dependencies)
            .find_map(|interface| interface.type_parameter_declarations.get(path).cloned())
            .unwrap_or_else(|| panic!("missing type parameter declaration {:?}", path))
    }

    fn get_constant_declaration(
        &self,
        path: &Self::Path,
    ) -> util::WithInfo<Self::Info, wipple_typecheck::ConstantDeclaration<Self>> {
        [&self.interface]
            .into_iter()
            .chain(&self.dependencies)
            .find_map(|interface| interface.constant_declarations.get(path).cloned())
            .unwrap_or_else(|| panic!("missing constant declaration {:?}", path))
    }

    fn get_instance_declaration(
        &self,
        path: &Self::Path,
    ) -> util::WithInfo<Self::Info, wipple_typecheck::InstanceDeclaration<Self>> {
        [&self.interface]
            .into_iter()
            .chain(&self.dependencies)
            .find_map(|interface| interface.instance_declarations.get(path).cloned())
            .unwrap_or_else(|| panic!("missing instance declaration {:?}", path))
    }

    fn get_instances_for_trait(&self, r#trait: &Self::Path) -> Vec<Self::Path> {
        [&self.interface]
            .into_iter()
            .chain(&self.dependencies)
            .flat_map(|interface| &interface.instance_declarations)
            .filter(|(_, instance)| instance.item.instance.item.r#trait == *r#trait)
            .map(|(path, _)| path.clone())
            .collect()
    }

    fn get_enumeration_for_variant(&self, variant: &Self::Path) -> Self::Path {
        // The parent of a variant is its enumeration
        lower::Path(variant[0..variant.len() - 1].to_vec())
    }
}

impl wipple_ir::Driver for Driver {
    fn number_type(&self) -> Option<Self::Path> {
        [&self.interface]
            .into_iter()
            .chain(&self.dependencies)
            .filter_map(|interface| interface.language_declarations.get("number"))
            .flatten()
            .find(|path| matches!(path.last().unwrap(), lower::PathComponent::Type(_)))
            .cloned()
    }

    fn text_type(&self) -> Option<Self::Path> {
        [&self.interface]
            .into_iter()
            .chain(&self.dependencies)
            .filter_map(|interface| interface.language_declarations.get("text"))
            .flatten()
            .find(|path| matches!(path.last().unwrap(), lower::PathComponent::Type(_)))
            .cloned()
    }

    fn boolean_type(&self) -> Option<Self::Path> {
        [&self.interface]
            .into_iter()
            .chain(&self.dependencies)
            .filter_map(|interface| interface.language_declarations.get("boolean"))
            .flatten()
            .find(|path| matches!(path.last().unwrap(), lower::PathComponent::Type(_)))
            .cloned()
    }

    fn true_variant(&self) -> Option<Self::Path> {
        [&self.interface]
            .into_iter()
            .chain(&self.dependencies)
            .filter_map(|interface| interface.language_declarations.get("true"))
            .flatten()
            .find(|path| matches!(path.last().unwrap(), lower::PathComponent::Constructor(_)))
            .cloned()
            .map(variant_from_constructor)
    }

    fn number_equality_intrinsic(&self) -> Option<String> {
        Some(String::from("number-equality"))
    }

    fn text_equality_intrinsic(&self) -> Option<String> {
        Some(String::from("text-equality"))
    }

    fn item_path_in(&self, path: &Self::Path, index: u32) -> Self::Path {
        path.join(lower::PathComponent::Item(index))
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
