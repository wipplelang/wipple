//! Compiler pass that resolves names.

mod resolve;

use derivative::Derivative;
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use std::{collections::HashMap, fmt::Debug};
use wipple_util::WithInfo;

/// Provides the lowerer with information about the program.
pub trait Driver: Sized {
    /// Additional information attached to every item.
    type Info: Debug + Clone + Serialize + DeserializeOwned;
}

/// Contains the definitions of items in a file.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""), Default(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct Interface<D: Driver> {
    /// The syntax declarations in the module.
    pub syntax_declarations: HashMap<Path, WithInfo<D::Info, SyntaxDeclaration<D>>>,

    /// The type declarations in the module.
    pub type_declarations: HashMap<Path, WithInfo<D::Info, TypeDeclaration<D>>>,

    /// The trait declarations in the module.
    pub trait_declarations: HashMap<Path, WithInfo<D::Info, TraitDeclaration<D>>>,

    /// The type parameters in the module.
    pub type_parameter_declarations: HashMap<Path, WithInfo<D::Info, TypeParameterDeclaration<D>>>,

    /// The language declarations in the module.
    pub language_declarations: HashMap<String, Vec<Path>>,

    /// The constant declarations in the module.
    pub constant_declarations: HashMap<Path, WithInfo<D::Info, ConstantDeclaration<D>>>,

    /// The instance declarations in the module.
    pub instance_declarations: HashMap<Path, WithInfo<D::Info, InstanceDeclaration<D>>>,

    /// The names of top-level declarations in the module.
    pub top_level: HashMap<String, Vec<WithInfo<D::Info, Path>>>,
}

/// Contains the implementations of items in a file.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""), Default(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct Library<D: Driver> {
    /// The implementations of constants and instances. A `None` value indicates
    /// that the implementation was omitted (eg. an instance with no value).
    pub items: HashMap<Path, Option<Item<D>>>,

    /// Any code to be run when the program starts.
    pub code: HashMap<Path, TopLevelCode<D>>,
}

/// Top-level code.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct TopLevelCode<D: Driver> {
    /// The code to run.
    pub statements: Vec<WithInfo<D::Info, crate::Expression<D>>>,
}

/// An implementation of a constant or instance.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct Item<D: Driver> {
    /// The item's body.
    pub body: WithInfo<D::Info, crate::Expression<D>>,

    /// The list of variables the item captures.
    pub captures: Vec<Path>,

    /// The top level relative to this item.
    pub top_level: Path,
}

/// Information collected for IDEs.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""), Default(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct Ide<D: Driver> {
    /// The list of resolved symbols in the module.
    pub symbols: Vec<WithInfo<D::Info, Path>>,
}

impl<D: Driver> Ide<D> {
    /// Merge two sets of IDE information.
    pub fn merge(&mut self, other: Self) {
        self.symbols.extend(other.symbols);
    }
}

/// Resolve a list of files into a module.
pub fn resolve<D: Driver>(
    _driver: &D,
    files: impl IntoIterator<Item = WithInfo<D::Info, UnresolvedFile<D>>>,
    dependencies: Vec<Interface<D>>,
) -> Result<D> {
    let mut errors = Vec::new();

    let result = resolve::resolve(files, dependencies);
    errors.extend(result.diagnostics);

    Result {
        interface: result.interface,
        library: result.library,
        ide: result.ide,
        diagnostics: errors,
    }
}

/// The result of [`resolve`].
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct Result<D: Driver> {
    /// The resolved [`Interface`].
    pub interface: Interface<D>,

    /// The resolved [`Library`].
    pub library: Library<D>,

    /// Information collected for IDEs.
    pub ide: Ide<D>,

    /// Any errors encountered while resolving the files.
    pub diagnostics: Vec<WithInfo<D::Info, Diagnostic>>,
}

/// An error occurring during lowering.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
#[serde(rename_all = "camelCase", tag = "type", content = "value")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum Diagnostic {
    /// The expression refers to a name that is not defined in the current
    /// scope.
    UnresolvedName(String),

    /// The expression refers to a type that is not defined in the current
    /// scope.
    UnresolvedType(String),

    /// The expression refers to a trait that is not defined in the current
    /// scope.
    UnresolvedTrait(String),

    /// The expression refers to a trait that is not defined in the current
    /// scope.
    UnresolvedVariant(String),

    /// The expression refers to a language feature that is not defined in the
    /// current scope.
    UnresolvedLanguageItem(String),

    /// A name could refer to multiple candidates.
    AmbiguousName {
        /// The name used.
        name: String,

        /// The candidates that the name could refer to.
        candidates: Vec<Path>,
    },

    /// This name is already defined in the current scope.
    AlreadyDefined(Path),

    /// Language declarations must be at the top level.
    NestedLanguageDeclaration,

    /// The type involved in the pattern is not a wrapper type.
    NotAWrapper,

    /// The pattern unwraps a type with more than one pattern.
    WrapperExpectsASinglePattern,

    /// Mutate patterns can only occur on the left-hand side of an assignment.
    InvalidMutatePattern,

    /// Missing types for a type or trait.
    MissingTypes(u32),

    /// Extra type provided to a type or trait.
    ExtraType,
}

/// An unresolved file.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct UnresolvedFile<D: Driver> {
    /// The name of the file.
    pub name: String,

    /// The top-level statements in the program.
    pub statements: Vec<WithInfo<D::Info, UnresolvedStatement<D>>>,
}

/// An unresolved statement.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum UnresolvedStatement<D: Driver> {
    /// A syntax declaration.
    #[serde(rename_all = "camelCase")]
    Syntax {
        /// The syntax's attributes.
        attributes: Vec<WithInfo<D::Info, Attribute<D>>>,

        /// The name of the syntax.
        name: WithInfo<D::Info, String>,
    },

    /// A type declaration.
    #[serde(rename_all = "camelCase")]
    Type {
        /// The type's attributes.
        attributes: Vec<WithInfo<D::Info, Attribute<D>>>,

        /// The name of the type.
        name: WithInfo<D::Info, String>,

        /// The type's parameters.
        parameters: Vec<WithInfo<D::Info, UnresolvedTypeParameter<D>>>,

        /// The type's representation.
        representation: WithInfo<D::Info, UnresolvedTypeRepresentation<D>>,
    },

    /// A trait declaration.
    #[serde(rename_all = "camelCase")]
    Trait {
        /// The trait's attributes.
        attributes: Vec<WithInfo<D::Info, Attribute<D>>>,

        /// The name of the trait.
        name: WithInfo<D::Info, String>,

        /// The trait's parameters.
        parameters: Vec<WithInfo<D::Info, UnresolvedTypeParameter<D>>>,

        /// The trait's type.
        r#type: Option<WithInfo<D::Info, UnresolvedType<D>>>,
    },

    /// A constant declaration.
    #[serde(rename_all = "camelCase")]
    Constant {
        /// The constant's attributes.
        attributes: Vec<WithInfo<D::Info, Attribute<D>>>,

        /// The name of the constant.
        name: WithInfo<D::Info, String>,

        /// The constant's parameters.
        parameters: Vec<WithInfo<D::Info, UnresolvedTypeParameter<D>>>,

        /// The constant's bounds.
        bounds: Vec<WithInfo<D::Info, UnresolvedInstance<D>>>,

        /// The constant's type.
        r#type: WithInfo<D::Info, UnresolvedType<D>>,

        /// The constant's body.
        body: WithInfo<D::Info, UnresolvedExpression<D>>,
    },

    /// An instance declaration.
    #[serde(rename_all = "camelCase")]
    Instance {
        /// The info belonging to the left-hand side of the assignment.
        pattern: WithInfo<D::Info, ()>,

        /// The instance's parameters.
        parameters: Vec<WithInfo<D::Info, UnresolvedTypeParameter<D>>>,

        /// The instance's bounds.
        bounds: Vec<WithInfo<D::Info, UnresolvedInstance<D>>>,

        /// The instance.
        instance: WithInfo<D::Info, UnresolvedInstance<D>>,

        /// The instance's body.
        body: Option<WithInfo<D::Info, UnresolvedExpression<D>>>,

        /// Whether the instance is the default instance.
        default: bool,
    },

    /// A variable assignment.
    #[serde(rename_all = "camelCase")]
    Assignment {
        /// The pattern to assign to.
        pattern: WithInfo<D::Info, UnresolvedPattern<D>>,

        /// The value.
        value: WithInfo<D::Info, UnresolvedExpression<D>>,
    },

    /// An expression.
    Expression(WithInfo<D::Info, UnresolvedExpression<D>>),
}

/// The kind of value a language declaration refers to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum LanguageDeclarationKind {
    /// A type.
    Type,

    /// A trait.
    Trait,

    /// A constant.
    Constant,
}

/// An unresolved expression.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum UnresolvedExpression<D: Driver> {
    /// An expression that could not be parsed.
    Error,

    /// Annotate an expression with an explicit type.
    #[serde(rename_all = "camelCase")]
    Annotate {
        /// The value to annotate.
        value: WithInfo<D::Info, Box<UnresolvedExpression<D>>>,

        /// The explicit type of the value.
        r#type: WithInfo<D::Info, UnresolvedType<D>>,
    },

    /// A name.
    Name(String),

    /// A number literal.
    Number(String),

    /// A text literal.
    Text(String),

    /// A formatted text literal.
    #[serde(rename_all = "camelCase")]
    Format {
        /// The segments of text that end in interpolated values.
        segments: Vec<FormatSegment<WithInfo<D::Info, UnresolvedExpression<D>>>>,

        /// Any trailing text after the segments.
        trailing: String,
    },

    /// A block.
    Block(Vec<WithInfo<D::Info, UnresolvedStatement<D>>>),

    /// Evaluate a block.
    Do(WithInfo<D::Info, Box<UnresolvedExpression<D>>>),

    /// A function.
    #[serde(rename_all = "camelCase")]
    Function {
        /// The function's inputs.
        inputs: Vec<WithInfo<D::Info, UnresolvedPattern<D>>>,

        /// The function's output.
        body: WithInfo<D::Info, Box<UnresolvedExpression<D>>>,
    },

    /// A function call.
    Call {
        /// The function to call.
        function: WithInfo<D::Info, Box<UnresolvedExpression<D>>>,

        /// The function's inputs.
        inputs: Vec<WithInfo<D::Info, UnresolvedExpression<D>>>,
    },

    /// Function application.
    Apply {
        /// The input.
        input: WithInfo<D::Info, Box<UnresolvedExpression<D>>>,

        /// The function.
        function: WithInfo<D::Info, Box<UnresolvedExpression<D>>>,
    },

    /// A binary operator expression.
    #[serde(rename_all = "camelCase")]
    BinaryOperator {
        /// The operator used.
        operator: WithInfo<D::Info, UnresolvedBinaryOperator>,

        /// The left-hand side.
        left: WithInfo<D::Info, Box<UnresolvedExpression<D>>>,

        /// The right-hand side.
        right: WithInfo<D::Info, Box<UnresolvedExpression<D>>>,
    },

    /// Convert a value of one type into a value of another type.
    #[serde(rename_all = "camelCase")]
    As {
        /// The value to convert.
        value: WithInfo<D::Info, Box<UnresolvedExpression<D>>>,

        /// The type to convert to.
        r#type: WithInfo<D::Info, UnresolvedType<D>>,
    },

    /// Check if a value matches a pattern.
    #[serde(rename_all = "camelCase")]
    Is {
        /// The value to match.
        value: WithInfo<D::Info, Box<UnresolvedExpression<D>>>,

        /// The pattern to match.
        pattern: WithInfo<D::Info, UnresolvedPattern<D>>,
    },

    /// A `when` expression.
    #[serde(rename_all = "camelCase")]
    When {
        /// The value to match.
        input: WithInfo<D::Info, Box<UnresolvedExpression<D>>>,

        /// The arms to execute.
        arms: Vec<WithInfo<D::Info, UnresolvedArm<D>>>,
    },

    /// An `intrinsic` expression.
    #[serde(rename_all = "camelCase")]
    Intrinsic {
        /// The name of the compiler intrinsic to call.
        name: WithInfo<D::Info, Option<String>>,

        /// The inputs to the intrinsic.
        inputs: Vec<WithInfo<D::Info, UnresolvedExpression<D>>>,
    },

    /// A tuple expression.
    Tuple(Vec<WithInfo<D::Info, UnresolvedExpression<D>>>),

    /// Collection construction.
    Collection(Vec<WithInfo<D::Info, UnresolvedExpression<D>>>),

    /// Structure construction.
    Structure(Vec<WithInfo<D::Info, UnresolvedFieldValue<D>>>),
}

/// An unresolved binary operator.
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, strum::Display)]
#[serde(rename_all = "camelCase")]
#[strum(serialize_all = "kebab-case")]
pub enum UnresolvedBinaryOperator {
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Power,
    And,
    Or,
    To,
    By,
}

impl UnresolvedBinaryOperator {
    fn short_circuits(&self) -> bool {
        matches!(
            self,
            UnresolvedBinaryOperator::And | UnresolvedBinaryOperator::Or
        )
    }
}

/// An unresolved type parameter.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct UnresolvedTypeParameter<D: Driver> {
    /// The name of the type parameter.
    pub name: WithInfo<D::Info, Option<String>>,

    /// Whether the type parameter was marked as `infer`.
    pub infer: Option<WithInfo<D::Info, ()>>,

    /// The type parameter's default type.
    pub default: Option<WithInfo<D::Info, UnresolvedType<D>>>,
}

/// An unresolved type representation.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum UnresolvedTypeRepresentation<D: Driver> {
    /// A marker type.
    Marker,

    /// A structure type.
    Structure(Vec<WithInfo<D::Info, UnresolvedField<D>>>),

    /// An enumeration type.
    Enumeration(Vec<WithInfo<D::Info, UnresolvedVariant<D>>>),

    /// A wrapper type.
    Wrapper(WithInfo<D::Info, UnresolvedType<D>>),
}

/// An unresolved structure field.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct UnresolvedField<D: Driver> {
    /// The index of the field.
    pub index: u32,

    /// The field's attributes.
    pub attributes: Vec<WithInfo<D::Info, Attribute<D>>>,

    /// The name of the field.
    pub name: WithInfo<D::Info, String>,

    /// The type of the field.
    pub r#type: WithInfo<D::Info, UnresolvedType<D>>,
}

/// An unresolved enumeration variant.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct UnresolvedVariant<D: Driver> {
    /// The index of the variant.
    pub index: u32,

    /// The variant's attributes.
    pub attributes: Vec<WithInfo<D::Info, Attribute<D>>>,

    /// The name of the variant.
    pub name: WithInfo<D::Info, String>,

    /// The types of the variant's associated values.
    pub types: Vec<WithInfo<D::Info, UnresolvedType<D>>>,
}

/// An unresolved type.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum UnresolvedType<D: Driver> {
    /// A type that could not be parsed.
    Error,

    /// A placeholder type.
    Placeholder,

    /// A declared type.
    #[serde(rename_all = "camelCase")]
    Declared {
        /// The name of the type.
        name: WithInfo<D::Info, Option<String>>,

        /// The parameters provided to the type.
        parameters: Vec<WithInfo<D::Info, UnresolvedType<D>>>,
    },

    /// A function type.
    #[serde(rename_all = "camelCase")]
    Function {
        /// The types of the function's inputs.
        inputs: Vec<WithInfo<D::Info, UnresolvedType<D>>>,

        /// The type of the function's output.
        output: WithInfo<D::Info, Box<UnresolvedType<D>>>,
    },

    /// A tuple type.
    Tuple(Vec<WithInfo<D::Info, UnresolvedType<D>>>),

    /// A type whose values are computed by a block.
    Block(WithInfo<D::Info, Box<UnresolvedType<D>>>),

    /// An intrinsic type provided by the runtime.
    Intrinsic,

    /// A type-level piece of text used to generate compiler errors.
    Message {
        /// The segments of text that end in interpolated types.
        segments: Vec<FormatSegment<WithInfo<D::Info, UnresolvedType<D>>>>,

        /// Any trailing text after the segments.
        trailing: String,
    },

    /// Use two types in the place of one. Useful for unifying a type parameter
    /// with a conrete type while preserving the resolved location of the type
    /// parameter.
    Equal {
        /// The left-hand side.
        left: WithInfo<D::Info, Box<UnresolvedType<D>>>,

        /// The right-hand side.
        right: WithInfo<D::Info, Box<UnresolvedType<D>>>,
    },
}

/// An unresolved instance.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct UnresolvedInstance<D: Driver> {
    /// The trait this instance refers to.
    pub r#trait: WithInfo<D::Info, Option<String>>,

    /// The parameters provided to the trait the instance refers to.
    pub parameters: Vec<WithInfo<D::Info, UnresolvedType<D>>>,
}

/// An unresolved pattern.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum UnresolvedPattern<D: Driver> {
    /// A pattern that could not be parsed.
    Error,

    /// A pattern that matches anything.
    Wildcard,

    /// A number pattern.
    Number(String),

    /// A text pattern.
    Text(String),

    /// A variable pattern.
    Name(String),

    /// A variant pattern, or a variable if a variant with this name doesn't
    /// exist.
    VariantOrName(Option<String>),

    /// A destructuring pattern.
    Destructure(Vec<WithInfo<D::Info, UnresolvedFieldPattern<D>>>),

    /// A variant pattern.
    #[serde(rename_all = "camelCase")]
    Variant {
        /// The variant this pattern matches.
        variant: WithInfo<D::Info, Option<String>>,

        /// The patterns matching each of the variant's associated values.
        value_patterns: Vec<WithInfo<D::Info, UnresolvedPattern<D>>>,
    },

    /// A tuple pattern.
    Tuple(Vec<WithInfo<D::Info, UnresolvedPattern<D>>>),

    /// Match either pattern.
    Or {
        /// The first pattern to match.
        left: WithInfo<D::Info, Box<UnresolvedPattern<D>>>,

        /// The pattern to match if matching [`PatternKind::Or::left`] fails.
        right: WithInfo<D::Info, Box<UnresolvedPattern<D>>>,
    },

    /// A pattern that changes the value of an existing variable.
    Mutate(WithInfo<D::Info, Option<String>>),

    /// Annotate a pattern with an explicit type.
    Annotate {
        /// The pattern to annotate.
        pattern: WithInfo<D::Info, Box<UnresolvedPattern<D>>>,

        /// The explicit type of the pattern.
        r#type: WithInfo<D::Info, UnresolvedType<D>>,
    },
}

/// A field in a destructuring pattern.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct UnresolvedFieldPattern<D: Driver> {
    /// The name of the field.
    pub name: WithInfo<D::Info, Option<String>>,

    /// The pattern matching the field's value.
    pub pattern: WithInfo<D::Info, UnresolvedPattern<D>>,
}

/// An arm in a `when` expression.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct UnresolvedArm<D: Driver> {
    /// The pattern to match on the input.
    pub pattern: WithInfo<D::Info, UnresolvedPattern<D>>,

    /// The arm's body.
    pub body: WithInfo<D::Info, UnresolvedExpression<D>>,
}

/// A field-value pair in a structure construction expression.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct UnresolvedFieldValue<D: Driver> {
    /// The name of the field.
    pub name: WithInfo<D::Info, Option<String>>,

    /// The field's value.
    pub value: WithInfo<D::Info, UnresolvedExpression<D>>,
}

/// A path that uniquely identifies a declaration.
#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Path(pub Vec<PathComponent>);

impl Path {
    /// The path of the top level (ie. the empty path).
    pub fn top_level() -> Self {
        Path(Vec::new())
    }

    /// Append a [`PathComponent`] to the path.
    pub fn join(&self, component: PathComponent) -> Self {
        Path(self.0.iter().cloned().chain([component]).collect())
    }
}

impl std::ops::Deref for Path {
    type Target = Vec<PathComponent>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for Path {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(
            &self
                .0
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("  /  "),
        )
    }
}

impl Serialize for Path {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.to_string().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Path {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let path = String::deserialize(deserializer)?;
        if path.is_empty() {
            return Ok(Path(Vec::new()));
        }

        let components = path
            .split("  /  ")
            .map(|component| {
                component.parse().map_err(|_| {
                    serde::de::Error::custom(format!(
                        "invalid path component {component:?} in path {path:?}"
                    ))
                })
            })
            .collect::<std::result::Result<Vec<_>, _>>()?;

        Ok(Path(components))
    }
}

/// A component of a [`Path`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum PathComponent {
    /// A file.
    File(String),

    /// A syntax declaration.
    Syntax(String),

    /// A type declaration.
    Type(String),

    /// A trait declaration.
    Trait(String),

    /// A constant declaration.
    Constant(String),

    /// A constructor for a structure type.
    Constructor(String),

    /// A variant in an enumeration.
    Variant(String),

    /// An instance declaration.
    Instance(u32),

    /// A language declaration.
    Language(String),

    /// A type parameter.
    TypeParameter(String),

    /// A variable.
    Variable(u32),

    /// An item (used during IR generation).
    Item(u32),
}

impl PathComponent {
    /// Returns `true` if the path component refers to a local definition (eg. a
    /// variable or type parameter).
    pub fn is_local(&self) -> bool {
        matches!(
            self,
            PathComponent::Variable(_) | PathComponent::TypeParameter(_)
        )
    }

    /// Returns `true` if the path component refers to a variable.
    pub fn is_variable(&self) -> bool {
        matches!(self, PathComponent::Variable(_))
    }

    /// Returns the name of the declaration the path component refers to, or
    /// `None` if the declaration has no name.
    pub fn name(&self) -> Option<&str> {
        match self {
            PathComponent::File(name)
            | PathComponent::Syntax(name)
            | PathComponent::Type(name)
            | PathComponent::Trait(name)
            | PathComponent::Constant(name)
            | PathComponent::Constructor(name)
            | PathComponent::Variant(name)
            | PathComponent::Language(name)
            | PathComponent::TypeParameter(name) => Some(name),
            PathComponent::Instance(_) | PathComponent::Variable(_) | PathComponent::Item(_) => {
                None
            }
        }
    }
}

impl std::fmt::Display for PathComponent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PathComponent::File(name) => write!(f, "file {}", name),
            PathComponent::Syntax(name) => write!(f, "syntax {}", name),
            PathComponent::Type(name) => write!(f, "type {}", name),
            PathComponent::Trait(name) => write!(f, "trait {}", name),
            PathComponent::Constant(name) => write!(f, "constant {}", name),
            PathComponent::Constructor(name) => write!(f, "constructor {}", name),
            PathComponent::Variant(name) => write!(f, "variant {}", name),
            PathComponent::Instance(index) => write!(f, "instance {}", index),
            PathComponent::Language(name) => write!(f, "language {}", name),
            PathComponent::TypeParameter(name) => write!(f, "type-parameter {}", name),
            PathComponent::Variable(index) => write!(f, "variable {}", index),
            PathComponent::Item(index) => write!(f, "item {}", index),
        }
    }
}

impl std::str::FromStr for PathComponent {
    type Err = ();

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let (prefix, name) = s.split_once(' ').ok_or(())?;

        match prefix {
            "file" => Ok(PathComponent::File(name.to_string())),
            "syntax" => Ok(PathComponent::Syntax(name.to_string())),
            "type" => Ok(PathComponent::Type(name.to_string())),
            "trait" => Ok(PathComponent::Trait(name.to_string())),
            "constant" => Ok(PathComponent::Constant(name.to_string())),
            "constructor" => Ok(PathComponent::Constructor(name.to_string())),
            "variant" => Ok(PathComponent::Variant(name.to_string())),
            "instance" => Ok(PathComponent::Instance(name.parse().map_err(|_| ())?)),
            "language" => Ok(PathComponent::Language(name.to_string())),
            "type-parameter" => Ok(PathComponent::TypeParameter(name.to_string())),
            "variable" => Ok(PathComponent::Variable(name.parse().map_err(|_| ())?)),
            "item" => Ok(PathComponent::Item(name.parse().map_err(|_| ())?)),
            _ => Err(()),
        }
    }
}

/// An attribute.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", tag = "type", content = "value")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum Attribute<D: Driver> {
    /// An invalid attribute.
    Error,

    /// A name.
    Name(WithInfo<D::Info, String>),

    /// A value associated with a name.
    Valued {
        /// The name.
        name: WithInfo<D::Info, String>,

        /// The value.
        value: WithInfo<D::Info, AttributeValue<D>>,
    },
}

/// An attribute value.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", tag = "type", content = "value")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum AttributeValue<D: Driver> {
    /// An invalid attribute value.
    Error,

    /// A name.
    Name(WithInfo<D::Info, String>),

    /// A number.
    Number(WithInfo<D::Info, String>),

    /// A piece of text.
    Text(WithInfo<D::Info, String>),
}

/// A resolved syntax declaration.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct SyntaxDeclaration<D: Driver> {
    /// The syntax's attributes.
    pub attributes: Vec<WithInfo<D::Info, crate::Attribute<D>>>,
}

/// A resolved type declaration.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct TypeDeclaration<D: Driver> {
    /// The trait's attributes.
    pub attributes: Vec<WithInfo<D::Info, crate::Attribute<D>>>,

    /// The type's parameters.
    pub parameters: Vec<crate::Path>,

    /// The type's representation.
    pub representation: WithInfo<D::Info, TypeRepresentation<D>>,
}

/// A resolved trait declaration.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct TraitDeclaration<D: Driver> {
    /// The trait's attributes.
    pub attributes: Vec<WithInfo<D::Info, crate::Attribute<D>>>,

    /// The trait's parameters.
    pub parameters: Vec<crate::Path>,

    /// The trait's type.
    pub r#type: Option<WithInfo<D::Info, crate::Type<D>>>,
}

/// A resolved constant declaration.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct ConstantDeclaration<D: Driver> {
    /// The constant's attributes.
    pub attributes: Vec<WithInfo<D::Info, crate::Attribute<D>>>,

    /// The constant's parameters.
    pub parameters: Vec<crate::Path>,

    /// The constant's bounds.
    pub bounds: Vec<WithInfo<D::Info, crate::Instance<D>>>,

    /// The constant's type.
    pub r#type: WithInfo<D::Info, crate::Type<D>>,
}

/// A resolved instance declaration.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct InstanceDeclaration<D: Driver> {
    /// The instance's parameters.
    pub parameters: Vec<crate::Path>,

    /// The instance's bounds.
    pub bounds: Vec<WithInfo<D::Info, crate::Instance<D>>>,

    /// The trait and parameters that this instance satisfies.
    pub instance: WithInfo<D::Info, crate::Instance<D>>,

    /// Whether the instance is the default instance.
    pub default: bool,
}

/// A resolved type parameter.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct TypeParameterDeclaration<D: Driver> {
    /// Whether the type parameter was marked as `infer`.
    pub infer: Option<WithInfo<D::Info, ()>>,

    /// The type parameter's default type.
    pub default: Option<WithInfo<D::Info, Type<D>>>,
}

/// A resolved expression.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum Expression<D: Driver> {
    /// An expression that could not be parsed.
    Error,

    /// Assign a value to a pattern.
    Assign {
        /// The pattern to assign to.
        pattern: WithInfo<D::Info, Pattern<D>>,

        /// The value to assign to the pattern.
        value: WithInfo<D::Info, Box<Expression<D>>>,
    },

    /// Change the value of an existing variable.
    Mutate {
        /// The name of the variable.
        name: WithInfo<D::Info, String>,

        /// The path to the variable.
        path: WithInfo<D::Info, Path>,

        /// The value to assign to the variable.
        value: WithInfo<D::Info, Box<Expression<D>>>,
    },

    /// Annotate an expression with an explicit type.
    #[serde(rename_all = "camelCase")]
    Annotate {
        /// The value to annotate.
        value: WithInfo<D::Info, Box<Expression<D>>>,

        /// The explicit type of the value.
        r#type: WithInfo<D::Info, Type<D>>,
    },

    /// A variable.
    Variable(String, Path),

    /// A number literal.
    Number(String),

    /// A text literal.
    Text(String),

    /// A constant.
    Constant(Path),

    /// An instance of a trait.
    Trait(Path),

    /// A formatted text literal.
    #[serde(rename_all = "camelCase")]
    Format {
        /// The segments of text that end in interpolated values.
        segments: Vec<FormatSegment<WithInfo<D::Info, Expression<D>>>>,

        /// Any trailing text after the segments.
        trailing: String,
    },

    /// A block.
    Block {
        /// The block's statements.
        statements: Vec<WithInfo<D::Info, Expression<D>>>,

        /// The list of variables the block captures.
        captures: Vec<Path>,
    },

    /// Evaluate a block.
    Do(WithInfo<D::Info, Box<Expression<D>>>),

    /// A function.
    #[serde(rename_all = "camelCase")]
    Function {
        /// The function's inputs.
        inputs: Vec<WithInfo<D::Info, Pattern<D>>>,

        /// The function's output.
        body: WithInfo<D::Info, Box<Expression<D>>>,

        /// The list of variables the function captures.
        captures: Vec<Path>,
    },

    /// A function call.
    Call {
        /// The function to call.
        function: WithInfo<D::Info, Box<Expression<D>>>,

        /// The function's inputs.
        inputs: Vec<WithInfo<D::Info, Expression<D>>>,
    },

    /// A `when` expression.
    #[serde(rename_all = "camelCase")]
    When {
        /// The value to match.
        input: WithInfo<D::Info, Box<Expression<D>>>,

        /// The arms to execute.
        arms: Vec<WithInfo<D::Info, Arm<D>>>,
    },

    /// An `intrinsic` expression.
    #[serde(rename_all = "camelCase")]
    Intrinsic {
        /// The name of the compiler intrinsic to call.
        name: WithInfo<D::Info, String>,

        /// The inputs to the intrinsic.
        inputs: Vec<WithInfo<D::Info, Expression<D>>>,
    },

    /// A tuple expression.
    Tuple(Vec<WithInfo<D::Info, Expression<D>>>),

    /// Collection construction.
    Collection(Vec<WithInfo<D::Info, Expression<D>>>),

    /// A value of a marker type.
    Marker(Path),

    /// Structure construction.
    Structure(Vec<WithInfo<D::Info, FieldValue<D>>>),

    /// Variant construction.
    Variant {
        /// The variant to construct.
        variant: WithInfo<D::Info, Path>,

        /// The variant's associated values.
        values: Vec<WithInfo<D::Info, Expression<D>>>,
    },

    /// Wrapper type construction.
    Wrapper {
        /// The type to construct.
        r#type: Path,

        /// The type's value.
        value: WithInfo<D::Info, Box<Expression<D>>>,
    },
}

/// A resolved type representation.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum TypeRepresentation<D: Driver> {
    /// A marker type.
    Marker,

    /// A structure type.
    Structure(Vec<WithInfo<D::Info, Field<D>>>),

    /// An enumeration type.
    Enumeration(Vec<WithInfo<D::Info, Variant<D>>>),

    /// A wrapper type.
    Wrapper(WithInfo<D::Info, Type<D>>),
}

/// A resolved structure field.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct Field<D: Driver> {
    /// The index of the field.
    pub index: u32,

    /// The field's attributes.
    pub attributes: Vec<WithInfo<D::Info, Attribute<D>>>,

    /// The name of the field.
    pub name: WithInfo<D::Info, String>,

    /// The type of the field.
    pub r#type: WithInfo<D::Info, Type<D>>,
}

/// A resolved enumeration variant.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct Variant<D: Driver> {
    /// The index of the variant.
    pub index: u32,

    /// The variant's attributes.
    pub attributes: Vec<WithInfo<D::Info, Attribute<D>>>,

    /// The name of the variant.
    pub name: WithInfo<D::Info, Path>,

    /// The types of the variant's associated values.
    pub types: Vec<WithInfo<D::Info, Type<D>>>,
}

/// A resolved type.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum Type<D: Driver> {
    /// A type that could not be parsed.
    Error,

    /// A placeholder type.
    Placeholder,

    /// A declared type.
    #[serde(rename_all = "camelCase")]
    Declared {
        /// The path to the type.
        path: WithInfo<D::Info, Path>,

        /// The parameters provided to the type.
        parameters: Vec<WithInfo<D::Info, Type<D>>>,
    },

    /// A type parameter.
    Parameter(Path),

    /// A function type.
    #[serde(rename_all = "camelCase")]
    Function {
        /// The types of the function's inputs.
        inputs: Vec<WithInfo<D::Info, Type<D>>>,

        /// The type of the function's output.
        output: WithInfo<D::Info, Box<Type<D>>>,
    },

    /// A tuple type.
    Tuple(Vec<WithInfo<D::Info, Type<D>>>),

    /// A type whose values are computed by a block.
    Block(WithInfo<D::Info, Box<Type<D>>>),

    /// An intrinsic type provided by the runtime.
    Intrinsic,

    /// A type-level piece of text used to generate compiler errors.
    #[serde(rename_all = "camelCase")]
    Message {
        /// The segments of text that end in interpolated types.
        segments: Vec<FormatSegment<WithInfo<D::Info, Type<D>>>>,

        /// Any trailing text after the segments.
        trailing: String,
    },

    /// Use two types in the place of one. Useful for unifying a type parameter
    /// with a conrete type while preserving the resolved location of the type
    /// parameter.
    Equal {
        /// The left-hand side.
        left: WithInfo<D::Info, Box<Type<D>>>,

        /// The right-hand side.
        right: WithInfo<D::Info, Box<Type<D>>>,
    },
}

/// A resolved instance.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct Instance<D: Driver> {
    /// The trait this instance refers to.
    pub r#trait: WithInfo<D::Info, Path>,

    /// The parameters provided to the trait the instance refers to.
    pub parameters: Vec<WithInfo<D::Info, Type<D>>>,
}

/// A resolved format segment.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FormatSegment<T> {
    /// The text preceding the interpolated value.
    pub text: String,

    /// The interpolated value.
    pub value: T,
}

/// A resolved pattern.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum Pattern<D: Driver> {
    /// A pattern that could not be parsed.
    Error,

    /// A pattern that matches anything.
    Wildcard,

    /// A number pattern.
    Number(String),

    /// A text pattern.
    Text(String),

    /// A variable pattern.
    Variable(String, Path),

    /// A destructuring pattern.
    Destructure(Vec<WithInfo<D::Info, FieldPattern<D>>>),

    /// A variant pattern.
    #[serde(rename_all = "camelCase")]
    Variant {
        /// The variant this pattern matches.
        variant: WithInfo<D::Info, Path>,

        /// The patterns matching each of the variant's associated values.
        value_patterns: Vec<WithInfo<D::Info, Pattern<D>>>,
    },

    /// A wrapper pattern.
    Wrapper {
        /// The wrapper type this pattern matches.
        path: WithInfo<D::Info, Path>,

        /// The pattern matching the wrapped value.
        value_pattern: WithInfo<D::Info, Box<Pattern<D>>>,
    },

    /// A tuple pattern.
    Tuple(Vec<WithInfo<D::Info, Pattern<D>>>),

    /// Match either pattern.
    Or {
        /// The first pattern to match.
        left: WithInfo<D::Info, Box<Pattern<D>>>,

        /// The pattern to match if matching [`PatternKind::Or::left`] fails.
        right: WithInfo<D::Info, Box<Pattern<D>>>,
    },

    /// Annotate a pattern with an explicit type.
    Annotate {
        /// The pattern to annotate.
        pattern: WithInfo<D::Info, Box<Pattern<D>>>,

        /// The explicit type of the pattern.
        r#type: WithInfo<D::Info, Type<D>>,
    },
}

/// A field in a destructuring pattern.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct FieldPattern<D: Driver> {
    /// The name of the field.
    pub name: WithInfo<D::Info, Option<String>>,

    /// The pattern matching the field's value.
    pub pattern: WithInfo<D::Info, Pattern<D>>,
}

/// An arm in a `when` expression.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct Arm<D: Driver> {
    /// The pattern to match on the input.
    pub pattern: WithInfo<D::Info, Pattern<D>>,

    /// The arm's body.
    pub body: WithInfo<D::Info, Expression<D>>,
}

/// A field-value pair in a structure construction expression.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct FieldValue<D: Driver> {
    /// The name of the field.
    pub name: WithInfo<D::Info, Option<String>>,

    /// The field's value.
    pub value: WithInfo<D::Info, Expression<D>>,
}
