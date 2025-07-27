//! Compiler pass that resolves names.

mod attribute;
mod constructors;
mod expression;
mod language;
mod name;
mod operator;
mod pattern;
mod resolve;
mod scope;
mod statements;
mod r#type;
mod utils;

use crate::util::WithInfo;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, fmt::Debug};

/// Contains the definitions of items in a file.
#[derive(Debug, Default)]
pub struct Interface {
    /// The syntax declarations in the module.
    pub syntax_declarations: HashMap<Path, WithInfo<SyntaxDeclaration>>,

    /// The type declarations in the module.
    pub type_declarations: HashMap<Path, WithInfo<TypeDeclaration>>,

    /// The trait declarations in the module.
    pub trait_declarations: HashMap<Path, WithInfo<TraitDeclaration>>,

    /// The type parameters in the module.
    pub type_parameter_declarations: HashMap<Path, WithInfo<TypeParameterDeclaration>>,

    /// The language declarations in the module.
    pub language_declarations: HashMap<String, Vec<Path>>,

    /// The constant declarations in the module.
    pub constant_declarations: HashMap<Path, WithInfo<ConstantDeclaration>>,

    /// The instance declarations in the module.
    pub instance_declarations: HashMap<Path, WithInfo<InstanceDeclaration>>,

    /// The names of top-level declarations in the module.
    pub top_level: HashMap<String, Vec<WithInfo<Path>>>,
}

/// Contains the implementations of items in a file.
#[derive(Debug, Clone, Default)]
pub struct Library {
    /// The implementations of constants and instances. A `None` value indicates
    /// that the implementation was omitted (eg. an instance with no value).
    pub items: HashMap<Path, Option<Item>>,

    /// Any code to be run when the program starts.
    pub code: HashMap<Path, TopLevelCode>,
}

/// Top-level code.
#[derive(Debug, Clone)]
pub struct TopLevelCode {
    /// The code to run.
    pub statements: Vec<WithInfo<crate::lower::Expression>>,
}

/// An implementation of a constant or instance.
#[derive(Debug, Clone)]
pub struct Item {
    /// The item's body.
    pub body: WithInfo<crate::lower::Expression>,

    /// The list of variables the item captures.
    pub captures: Vec<Path>,

    /// The top level relative to this item.
    pub top_level: Path,
}

/// Resolve a list of files into a module.
pub fn resolve(
    files: impl IntoIterator<Item = WithInfo<UnresolvedFile>>,
    dependencies: Interface,
) -> Result {
    let mut errors = Vec::new();

    let result = resolve::resolve(files, dependencies);
    errors.extend(result.diagnostics);

    Result {
        interface: result.interface,
        library: result.library,
        diagnostics: errors,
    }
}

/// The result of [`resolve`].
#[derive(Debug)]
pub struct Result {
    /// The resolved [`Interface`].
    pub interface: Interface,

    /// The resolved [`Library`].
    pub library: Library,

    /// Any errors encountered while resolving the files.
    pub diagnostics: Vec<WithInfo<Diagnostic>>,
}

/// An error occurring during lowering.
#[derive(Debug, Clone, Hash)]
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
#[derive(Debug, Clone)]
pub struct UnresolvedFile {
    /// The name of the file.
    pub name: String,

    /// The top-level statements in the program.
    pub statements: Vec<WithInfo<UnresolvedStatement>>,
}

/// An unresolved statement.
#[derive(Debug, Clone)]
pub enum UnresolvedStatement {
    /// A syntax declaration.
    Syntax {
        /// The syntax's attributes.
        attributes: Vec<WithInfo<Attribute>>,

        /// The name of the syntax.
        name: WithInfo<String>,
    },

    /// A type declaration.
    Type {
        /// The type's attributes.
        attributes: Vec<WithInfo<Attribute>>,

        /// The name of the type.
        name: WithInfo<String>,

        /// The type's parameters.
        parameters: Vec<WithInfo<UnresolvedTypeParameter>>,

        /// The type's representation.
        representation: WithInfo<UnresolvedTypeRepresentation>,
    },

    /// A trait declaration.
    Trait {
        /// The trait's attributes.
        attributes: Vec<WithInfo<Attribute>>,

        /// The name of the trait.
        name: WithInfo<String>,

        /// The trait's parameters.
        parameters: Vec<WithInfo<UnresolvedTypeParameter>>,

        /// The trait's type.
        r#type: Option<WithInfo<UnresolvedType>>,
    },

    /// A constant declaration.
    Constant {
        /// The constant's attributes.
        attributes: Vec<WithInfo<Attribute>>,

        /// The name of the constant.
        name: WithInfo<String>,

        /// The constant's parameters.
        parameters: Vec<WithInfo<UnresolvedTypeParameter>>,

        /// The constant's bounds.
        bounds: Vec<WithInfo<UnresolvedInstance>>,

        /// The constant's type.
        r#type: WithInfo<UnresolvedType>,

        /// The constant's body.
        body: WithInfo<UnresolvedExpression>,
    },

    /// An instance declaration.
    Instance {
        /// The info belonging to the left-hand side of the assignment.
        pattern: WithInfo<()>,

        /// The instance's parameters.
        parameters: Vec<WithInfo<UnresolvedTypeParameter>>,

        /// The instance's bounds.
        bounds: Vec<WithInfo<UnresolvedInstance>>,

        /// The instance.
        instance: WithInfo<UnresolvedInstance>,

        /// The instance's body.
        body: Option<WithInfo<UnresolvedExpression>>,

        /// Whether the instance is the default instance.
        default: bool,
    },

    /// A variable assignment.
    Assignment {
        /// The pattern to assign to.
        pattern: WithInfo<UnresolvedPattern>,

        /// The value.
        value: WithInfo<UnresolvedExpression>,
    },

    /// An expression.
    Expression(WithInfo<UnresolvedExpression>),
}

/// The kind of value a language declaration refers to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LanguageDeclarationKind {
    /// A type.
    Type,

    /// A trait.
    Trait,

    /// A constant.
    Constant,
}

/// An unresolved expression.
#[derive(Debug, Clone)]
pub enum UnresolvedExpression {
    /// An expression that could not be parsed.
    Error,

    /// Annotate an expression with an explicit type.
    Annotate {
        /// The value to annotate.
        value: WithInfo<Box<UnresolvedExpression>>,

        /// The explicit type of the value.
        r#type: WithInfo<UnresolvedType>,
    },

    /// A name.
    Name(String),

    /// A number literal.
    Number(String),

    /// A text literal.
    Text(String),

    /// A formatted text literal.
    Format {
        /// The segments of text that end in interpolated values.
        segments: Vec<FormatSegment<WithInfo<UnresolvedExpression>>>,

        /// Any trailing text after the segments.
        trailing: String,
    },

    /// A block.
    Block(Vec<WithInfo<UnresolvedStatement>>),

    /// Evaluate a block.
    Do(WithInfo<Box<UnresolvedExpression>>),

    /// A function.
    Function {
        /// The function's inputs.
        inputs: Vec<WithInfo<UnresolvedPattern>>,

        /// The function's output.
        body: WithInfo<Box<UnresolvedExpression>>,
    },

    /// A function call.
    Call {
        /// The function to call.
        function: WithInfo<Box<UnresolvedExpression>>,

        /// The function's inputs.
        inputs: Vec<WithInfo<UnresolvedExpression>>,
    },

    /// Function application.
    Apply {
        /// The input.
        input: WithInfo<Box<UnresolvedExpression>>,

        /// The function.
        function: WithInfo<Box<UnresolvedExpression>>,
    },

    /// A binary operator expression.
    BinaryOperator {
        /// The operator used.
        operator: WithInfo<UnresolvedBinaryOperator>,

        /// The left-hand side.
        left: WithInfo<Box<UnresolvedExpression>>,

        /// The right-hand side.
        right: WithInfo<Box<UnresolvedExpression>>,
    },

    /// Convert a value of one type into a value of another type.
    As {
        /// The value to convert.
        value: WithInfo<Box<UnresolvedExpression>>,

        /// The type to convert to.
        r#type: WithInfo<UnresolvedType>,
    },

    /// Check if a value matches a pattern.
    Is {
        /// The value to match.
        value: WithInfo<Box<UnresolvedExpression>>,

        /// The pattern to match.
        pattern: WithInfo<UnresolvedPattern>,
    },

    /// A `when` expression.
    When {
        /// The value to match.
        input: WithInfo<Box<UnresolvedExpression>>,

        /// The arms to execute.
        arms: Vec<WithInfo<UnresolvedArm>>,
    },

    /// An `intrinsic` expression.
    Intrinsic {
        /// The name of the compiler intrinsic to call.
        name: WithInfo<Option<String>>,

        /// The inputs to the intrinsic.
        inputs: Vec<WithInfo<UnresolvedExpression>>,
    },

    /// A tuple expression.
    Tuple(Vec<WithInfo<UnresolvedExpression>>),

    /// Collection construction.
    Collection(Vec<WithInfo<UnresolvedExpression>>),

    /// Structure construction.
    Structure(Vec<WithInfo<UnresolvedFieldValue>>),
}

/// An unresolved binary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::Display)]
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
#[derive(Debug, Clone)]
pub struct UnresolvedTypeParameter {
    /// The name of the type parameter.
    pub name: WithInfo<Option<String>>,

    /// Whether the type parameter was marked as `infer`.
    pub infer: Option<WithInfo<()>>,

    /// The type parameter's default type.
    pub default: Option<WithInfo<UnresolvedType>>,
}

/// An unresolved type representation.
#[derive(Debug, Clone)]
pub enum UnresolvedTypeRepresentation {
    /// A marker type.
    Marker,

    /// A structure type.
    Structure(Vec<WithInfo<UnresolvedField>>),

    /// An enumeration type.
    Enumeration(Vec<WithInfo<UnresolvedVariant>>),

    /// A wrapper type.
    Wrapper(WithInfo<UnresolvedType>),
}

/// An unresolved structure field.
#[derive(Debug, Clone)]
pub struct UnresolvedField {
    /// The index of the field.
    pub index: u32,

    /// The field's attributes.
    pub attributes: Vec<WithInfo<Attribute>>,

    /// The name of the field.
    pub name: WithInfo<String>,

    /// The type of the field.
    pub r#type: WithInfo<UnresolvedType>,
}

/// An unresolved enumeration variant.
#[derive(Debug, Clone)]
pub struct UnresolvedVariant {
    /// The index of the variant.
    pub index: u32,

    /// The variant's attributes.
    pub attributes: Vec<WithInfo<Attribute>>,

    /// The name of the variant.
    pub name: WithInfo<String>,

    /// The types of the variant's associated values.
    pub types: Vec<WithInfo<UnresolvedType>>,
}

/// An unresolved type.
#[derive(Debug, Clone)]
pub enum UnresolvedType {
    /// A type that could not be parsed.
    Error,

    /// A placeholder type.
    Placeholder,

    /// A declared type.
    Declared {
        /// The name of the type.
        name: WithInfo<Option<String>>,

        /// The parameters provided to the type.
        parameters: Vec<WithInfo<UnresolvedType>>,
    },

    /// A function type.
    Function {
        /// The types of the function's inputs.
        inputs: Vec<WithInfo<UnresolvedType>>,

        /// The type of the function's output.
        output: WithInfo<Box<UnresolvedType>>,
    },

    /// A tuple type.
    Tuple(Vec<WithInfo<UnresolvedType>>),

    /// A type whose values are computed by a block.
    Block(WithInfo<Box<UnresolvedType>>),

    /// An intrinsic type provided by the runtime.
    Intrinsic,

    /// A type-level piece of text used to generate compiler errors.
    Message {
        /// The segments of text that end in interpolated types.
        segments: Vec<FormatSegment<WithInfo<UnresolvedType>>>,

        /// Any trailing text after the segments.
        trailing: String,
    },

    /// Use two types in the place of one. Useful for unifying a type parameter
    /// with a conrete type while preserving the resolved location of the type
    /// parameter.
    Equal {
        /// The left-hand side.
        left: WithInfo<Box<UnresolvedType>>,

        /// The right-hand side.
        right: WithInfo<Box<UnresolvedType>>,
    },
}

/// An unresolved instance.
#[derive(Debug, Clone)]
pub struct UnresolvedInstance {
    /// The trait this instance refers to.
    pub r#trait: WithInfo<Option<String>>,

    /// The parameters provided to the trait the instance refers to.
    pub parameters: Vec<WithInfo<UnresolvedType>>,
}

/// An unresolved pattern.
#[derive(Debug, Clone)]
pub enum UnresolvedPattern {
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
    Destructure(Vec<WithInfo<UnresolvedFieldPattern>>),

    /// A variant pattern.
    Variant {
        /// The variant this pattern matches.
        variant: WithInfo<Option<String>>,

        /// The patterns matching each of the variant's associated values.
        value_patterns: Vec<WithInfo<UnresolvedPattern>>,
    },

    /// A tuple pattern.
    Tuple(Vec<WithInfo<UnresolvedPattern>>),

    /// Match either pattern.
    Or {
        /// The first pattern to match.
        left: WithInfo<Box<UnresolvedPattern>>,

        /// The pattern to match if matching [`PatternKind::Or::left`] fails.
        right: WithInfo<Box<UnresolvedPattern>>,
    },

    /// A pattern that changes the value of an existing variable.
    Mutate(WithInfo<Option<String>>),

    /// Annotate a pattern with an explicit type.
    Annotate {
        /// The pattern to annotate.
        pattern: WithInfo<Box<UnresolvedPattern>>,

        /// The explicit type of the pattern.
        r#type: WithInfo<UnresolvedType>,
    },
}

/// A field in a destructuring pattern.
#[derive(Debug, Clone)]
pub struct UnresolvedFieldPattern {
    /// The name of the field.
    pub name: WithInfo<Option<String>>,

    /// The pattern matching the field's value.
    pub pattern: WithInfo<UnresolvedPattern>,
}

/// An arm in a `when` expression.
#[derive(Debug, Clone)]
pub struct UnresolvedArm {
    /// The pattern to match on the input.
    pub pattern: WithInfo<UnresolvedPattern>,

    /// The arm's body.
    pub body: WithInfo<UnresolvedExpression>,
}

/// A field-value pair in a structure construction expression.
#[derive(Debug, Clone)]
pub struct UnresolvedFieldValue {
    /// The name of the field.
    pub name: WithInfo<Option<String>>,

    /// The field's value.
    pub value: WithInfo<UnresolvedExpression>,
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
#[derive(Debug, Clone)]
pub enum Attribute {
    /// An invalid attribute.
    Error,

    /// A name.
    Name(WithInfo<String>),

    /// A value associated with a name.
    Valued {
        /// The name.
        name: WithInfo<String>,

        /// The value.
        value: WithInfo<AttributeValue>,
    },
}

/// An attribute value.
#[derive(Debug, Clone)]
pub enum AttributeValue {
    /// An invalid attribute value.
    Error,

    /// A name.
    Name(WithInfo<String>),

    /// A number.
    Number(WithInfo<String>),

    /// A piece of text.
    Text(WithInfo<String>),
}

/// A resolved syntax declaration.
#[derive(Debug, Clone)]
pub struct SyntaxDeclaration {
    /// The syntax's attributes.
    pub attributes: Vec<WithInfo<crate::lower::Attribute>>,
}

/// A resolved type declaration.
#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    /// The trait's attributes.
    pub attributes: Vec<WithInfo<crate::lower::Attribute>>,

    /// The type's parameters.
    pub parameters: Vec<crate::lower::Path>,

    /// The type's representation.
    pub representation: WithInfo<TypeRepresentation>,
}

/// A resolved trait declaration.
#[derive(Debug, Clone)]
pub struct TraitDeclaration {
    /// The trait's attributes.
    pub attributes: Vec<WithInfo<crate::lower::Attribute>>,

    /// The trait's parameters.
    pub parameters: Vec<crate::lower::Path>,

    /// The trait's type.
    pub r#type: Option<WithInfo<crate::lower::Type>>,
}

/// A resolved constant declaration.
#[derive(Debug, Clone)]
pub struct ConstantDeclaration {
    /// The constant's attributes.
    pub attributes: Vec<WithInfo<crate::lower::Attribute>>,

    /// The constant's parameters.
    pub parameters: Vec<crate::lower::Path>,

    /// The constant's bounds.
    pub bounds: Vec<WithInfo<crate::lower::Instance>>,

    /// The constant's type.
    pub r#type: WithInfo<crate::lower::Type>,
}

/// A resolved instance declaration.
#[derive(Debug, Clone)]
pub struct InstanceDeclaration {
    /// The instance's parameters.
    pub parameters: Vec<crate::lower::Path>,

    /// The instance's bounds.
    pub bounds: Vec<WithInfo<crate::lower::Instance>>,

    /// The trait and parameters that this instance satisfies.
    pub instance: WithInfo<crate::lower::Instance>,

    /// Whether the instance is the default instance.
    pub default: bool,
}

/// A resolved type parameter.
#[derive(Debug, Clone)]
pub struct TypeParameterDeclaration {
    /// Whether the type parameter was marked as `infer`.
    pub infer: Option<WithInfo<()>>,

    /// The type parameter's default type.
    pub default: Option<WithInfo<Type>>,
}

/// A resolved expression.
#[derive(Debug, Clone)]
pub enum Expression {
    /// An expression that could not be parsed.
    Error,

    /// Assign a value to a pattern.
    Assign {
        /// The pattern to assign to.
        pattern: WithInfo<Pattern>,

        /// The value to assign to the pattern.
        value: WithInfo<Box<Expression>>,
    },

    /// Change the value of an existing variable.
    Mutate {
        /// The name of the variable.
        name: WithInfo<String>,

        /// The path to the variable.
        path: WithInfo<Path>,

        /// The value to assign to the variable.
        value: WithInfo<Box<Expression>>,
    },

    /// Annotate an expression with an explicit type.
    Annotate {
        /// The value to annotate.
        value: WithInfo<Box<Expression>>,

        /// The explicit type of the value.
        r#type: WithInfo<Type>,
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
    Format {
        /// The segments of text that end in interpolated values.
        segments: Vec<FormatSegment<WithInfo<Expression>>>,

        /// Any trailing text after the segments.
        trailing: String,
    },

    /// A block.
    Block {
        /// The block's statements.
        statements: Vec<WithInfo<Expression>>,

        /// The list of variables the block captures.
        captures: Vec<Path>,
    },

    /// Evaluate a block.
    Do(WithInfo<Box<Expression>>),

    /// A function.
    Function {
        /// The function's inputs.
        inputs: Vec<WithInfo<Pattern>>,

        /// The function's output.
        body: WithInfo<Box<Expression>>,

        /// The list of variables the function captures.
        captures: Vec<Path>,
    },

    /// A function call.
    Call {
        /// The function to call.
        function: WithInfo<Box<Expression>>,

        /// The function's inputs.
        inputs: Vec<WithInfo<Expression>>,
    },

    /// A `when` expression.
    When {
        /// The value to match.
        input: WithInfo<Box<Expression>>,

        /// The arms to execute.
        arms: Vec<WithInfo<Arm>>,
    },

    /// An `intrinsic` expression.
    Intrinsic {
        /// The name of the compiler intrinsic to call.
        name: WithInfo<String>,

        /// The inputs to the intrinsic.
        inputs: Vec<WithInfo<Expression>>,
    },

    /// A tuple expression.
    Tuple(Vec<WithInfo<Expression>>),

    /// Collection construction.
    Collection(Vec<WithInfo<Expression>>),

    /// A value of a marker type.
    Marker(Path),

    /// Structure construction.
    Structure(Vec<WithInfo<FieldValue>>),

    /// Variant construction.
    Variant {
        /// The variant to construct.
        variant: WithInfo<Path>,

        /// The variant's associated values.
        values: Vec<WithInfo<Expression>>,
    },

    /// Wrapper type construction.
    Wrapper {
        /// The type to construct.
        r#type: Path,

        /// The type's value.
        value: WithInfo<Box<Expression>>,
    },
}

/// A resolved type representation.
#[derive(Debug, Clone)]
pub enum TypeRepresentation {
    /// A marker type.
    Marker,

    /// A structure type.
    Structure(Vec<WithInfo<Field>>),

    /// An enumeration type.
    Enumeration(Vec<WithInfo<Variant>>),

    /// A wrapper type.
    Wrapper(WithInfo<Type>),
}

/// A resolved structure field.
#[derive(Debug, Clone)]
pub struct Field {
    /// The index of the field.
    pub index: u32,

    /// The field's attributes.
    pub attributes: Vec<WithInfo<Attribute>>,

    /// The name of the field.
    pub name: WithInfo<String>,

    /// The type of the field.
    pub r#type: WithInfo<Type>,
}

/// A resolved enumeration variant.
#[derive(Debug, Clone)]
pub struct Variant {
    /// The index of the variant.
    pub index: u32,

    /// The variant's attributes.
    pub attributes: Vec<WithInfo<Attribute>>,

    /// The name of the variant.
    pub name: WithInfo<Path>,

    /// The types of the variant's associated values.
    pub types: Vec<WithInfo<Type>>,
}

/// A resolved type.
#[derive(Debug, Clone)]
pub enum Type {
    /// A type that could not be parsed.
    Error,

    /// A placeholder type.
    Placeholder,

    /// A declared type.
    Declared {
        /// The path to the type.
        path: WithInfo<Path>,

        /// The parameters provided to the type.
        parameters: Vec<WithInfo<Type>>,
    },

    /// A type parameter.
    Parameter(Path),

    /// A function type.
    Function {
        /// The types of the function's inputs.
        inputs: Vec<WithInfo<Type>>,

        /// The type of the function's output.
        output: WithInfo<Box<Type>>,
    },

    /// A tuple type.
    Tuple(Vec<WithInfo<Type>>),

    /// A type whose values are computed by a block.
    Block(WithInfo<Box<Type>>),

    /// An intrinsic type provided by the runtime.
    Intrinsic,

    /// A type-level piece of text used to generate compiler errors.
    Message {
        /// The segments of text that end in interpolated types.
        segments: Vec<FormatSegment<WithInfo<Type>>>,

        /// Any trailing text after the segments.
        trailing: String,
    },

    /// Use two types in the place of one. Useful for unifying a type parameter
    /// with a conrete type while preserving the resolved location of the type
    /// parameter.
    Equal {
        /// The left-hand side.
        left: WithInfo<Box<Type>>,

        /// The right-hand side.
        right: WithInfo<Box<Type>>,
    },
}

/// A resolved instance.
#[derive(Debug, Clone)]
pub struct Instance {
    /// The trait this instance refers to.
    pub r#trait: WithInfo<Path>,

    /// The parameters provided to the trait the instance refers to.
    pub parameters: Vec<WithInfo<Type>>,
}

/// A resolved format segment.
#[derive(Debug, Clone)]
pub struct FormatSegment<T> {
    /// The text preceding the interpolated value.
    pub text: String,

    /// The interpolated value.
    pub value: T,
}

/// A resolved pattern.
#[derive(Debug, Clone)]
pub enum Pattern {
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
    Destructure(Vec<WithInfo<FieldPattern>>),

    /// A variant pattern.
    Variant {
        /// The variant this pattern matches.
        variant: WithInfo<Path>,

        /// The patterns matching each of the variant's associated values.
        value_patterns: Vec<WithInfo<Pattern>>,
    },

    /// A marker pattern.
    Marker(Path),

    /// A wrapper pattern.
    Wrapper {
        /// The wrapper type this pattern matches.
        path: WithInfo<Path>,

        /// The pattern matching the wrapped value.
        value_pattern: WithInfo<Box<Pattern>>,
    },

    /// A tuple pattern.
    Tuple(Vec<WithInfo<Pattern>>),

    /// Match either pattern.
    Or {
        /// The first pattern to match.
        left: WithInfo<Box<Pattern>>,

        /// The pattern to match if matching [`PatternKind::Or::left`] fails.
        right: WithInfo<Box<Pattern>>,
    },

    /// Annotate a pattern with an explicit type.
    Annotate {
        /// The pattern to annotate.
        pattern: WithInfo<Box<Pattern>>,

        /// The explicit type of the pattern.
        r#type: WithInfo<Type>,
    },
}

/// A field in a destructuring pattern.
#[derive(Debug, Clone)]
pub struct FieldPattern {
    /// The name of the field.
    pub name: WithInfo<Option<String>>,

    /// The pattern matching the field's value.
    pub pattern: WithInfo<Pattern>,
}

/// An arm in a `when` expression.
#[derive(Debug, Clone)]
pub struct Arm {
    /// The pattern to match on the input.
    pub pattern: WithInfo<Pattern>,

    /// The arm's body.
    pub body: WithInfo<Expression>,
}

/// A field-value pair in a structure construction expression.
#[derive(Debug, Clone)]
pub struct FieldValue {
    /// The name of the field.
    pub name: WithInfo<Option<String>>,

    /// The field's value.
    pub value: WithInfo<Expression>,
}
