//! Compiler pass that parses source code into a syntax tree.

pub mod ast;
pub mod parse;
pub mod tokenize;

mod text;

use derivative::Derivative;
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use std::{fmt::Debug, hash::Hash, ops::Range, sync::Arc};
use wipple_util::WithInfo;

/// Provides the parser with information about the program.
pub trait Driver: Sized + 'static {
    /// Additional information attached to every item.
    type Info: Debug + Clone + Serialize + DeserializeOwned + 'static;

    /// Retrieve the path of the file being parsed.
    fn file_path(&self) -> Arc<str>;

    /// Retrieve the path to be rendered in diagnostics.
    fn visible_path(&self) -> Arc<str>;

    /// Retrieve the size of the file being parsed in bytes.
    fn file_size(&self) -> u32;

    /// Merge two [`Info`](Driver::Info) values together.
    fn merge_info(left: Self::Info, right: Self::Info) -> Self::Info;
}

/// Location information about a parsed item.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Location {
    /// The path of the file this item belongs to.
    pub path: Arc<str>,

    /// The path to be displayed to the user.
    pub visible_path: Arc<str>,

    /// The location of the item in the source code.
    pub span: Range<u32>,
}

impl PartialOrd for Location {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Location {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.path.cmp(&other.path) {
            std::cmp::Ordering::Equal => {}
            ord => return ord,
        }

        match self.visible_path.cmp(&other.visible_path) {
            std::cmp::Ordering::Equal => {}
            ord => return ord,
        }

        match self.span.start.cmp(&other.span.start) {
            std::cmp::Ordering::Equal => {}
            ord => return ord,
        }

        match self.span.end.cmp(&other.span.end) {
            std::cmp::Ordering::Equal => {}
            ord => return ord,
        }

        std::cmp::Ordering::Equal
    }
}

impl Location {
    /// Check if `self` is contained within `other`.
    pub fn span_is_within(&self, other: &Self) -> bool {
        self.span.start >= other.span.start && self.span.end <= other.span.end
    }

    /// Merge two [`Info`]s together.
    pub fn merge(left: Self, right: Self) -> Self {
        Self {
            path: left.path,
            visible_path: left.visible_path,
            span: left.span.start..right.span.end,
        }
    }
}

/// Parse a syntax tree into a program.
pub fn parse<D: Driver>(_driver: &D, syntax: WithInfo<D::Info, parse::TopLevel<D>>) -> Result<D> {
    ast::top_level(syntax)
}

/// The result of [`parse`].
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct Result<D: Driver> {
    /// The top-level program.
    pub top_level: WithInfo<D::Info, TopLevel<D>>,

    /// Any errors encountered while parsing the source code.
    pub diagnostics: Vec<WithInfo<D::Info, Diagnostic>>,
}

/// The context in which an [`Error`] occurred.
#[allow(missing_docs)]
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum ErrorContext {
    Statement,
    Expression,
    Type,
    TypeParameter,
    Pattern,
    TypeFunction,
    Instance,
    Field,
    Variant,
}

/// An error occuring during parsing.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", tag = "type", content = "value")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum Diagnostic {
    /// The parser found a bound, but bounds aren't allowed here.
    UnexpectedBound,

    /// The parser expected a constant's initializer to immediately follow the
    /// constant's declaration.
    ExpectedConstantValue(String),

    /// A type representation must contain at least one field or variant.
    EmptyTypeRepresentation,

    /// A structure type must consist entirely of fields.
    ExpectedField,

    /// An enumeration type must consist entirely of variants.
    ExpectedVariant,

    /// A text literal contained an invalid character or escape sequence.
    InvalidTextLiteral(text::TextLiteralError),

    /// The parser did not receive the correct number of inputs for a
    /// placeholder text expression.
    InvalidPlaceholderText {
        /// The expected number of inputs.
        expected: u32,

        /// The provided number of inputs.
        found: u32,
    },
}

/// A binary operator.
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum BinaryOperator {
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

/// An attribute.
#[derive(Serialize, Derivative)]
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
#[derive(Serialize, Derivative)]
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

/// The parsed top-level program.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct TopLevel<D: Driver> {
    /// The top-level statements in the program.
    pub statements: Vec<WithInfo<D::Info, Statement<D>>>,
}

/// A parsed statement.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum Statement<D: Driver> {
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
        parameters: Vec<WithInfo<D::Info, TypeParameter<D>>>,

        /// The type's representation.
        representation: WithInfo<D::Info, TypeRepresentation<D>>,
    },

    /// A trait declaration.
    #[serde(rename_all = "camelCase")]
    Trait {
        /// The trait's attributes.
        attributes: Vec<WithInfo<D::Info, Attribute<D>>>,

        /// The name of the trait.
        name: WithInfo<D::Info, String>,

        /// The trait's parameters.
        parameters: Vec<WithInfo<D::Info, TypeParameter<D>>>,

        /// The trait's type.
        r#type: Option<WithInfo<D::Info, Type<D>>>,
    },

    /// A constant declaration.
    #[serde(rename_all = "camelCase")]
    Constant {
        /// The constant's attributes.
        attributes: Vec<WithInfo<D::Info, Attribute<D>>>,

        /// The name of the constant.
        name: WithInfo<D::Info, String>,

        /// The constant's parameters.
        parameters: Vec<WithInfo<D::Info, TypeParameter<D>>>,

        /// The constant's bounds.
        bounds: Vec<WithInfo<D::Info, Instance<D>>>,

        /// The constant's type.
        r#type: WithInfo<D::Info, Type<D>>,

        /// The constant's body.
        body: WithInfo<D::Info, Expression<D>>,
    },

    /// An instance declaration.
    #[serde(rename_all = "camelCase")]
    Instance {
        /// The info belonging to the left-hand side of the assignment.
        pattern: WithInfo<D::Info, ()>,

        /// The instance's parameters.
        parameters: Vec<WithInfo<D::Info, TypeParameter<D>>>,

        /// The instance's bounds.
        bounds: Vec<WithInfo<D::Info, Instance<D>>>,

        /// The instance.
        instance: WithInfo<D::Info, Instance<D>>,

        /// The instance's body.
        body: Option<WithInfo<D::Info, Expression<D>>>,

        /// Whether the instance is the default instance.
        default: bool,
    },

    /// A variable assignment.
    #[serde(rename_all = "camelCase")]
    Assignment {
        /// The pattern to assign to.
        pattern: WithInfo<D::Info, Pattern<D>>,

        /// The value.
        value: WithInfo<D::Info, Expression<D>>,
    },

    /// An expression.
    Expression(WithInfo<D::Info, Expression<D>>),
}

/// A parsed expression.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum Expression<D: Driver> {
    /// An expression that could not be parsed.
    Error,

    /// Annotate an expression with an explicit type.
    #[serde(rename_all = "camelCase")]
    Annotate {
        /// The value to annotate.
        value: WithInfo<D::Info, Box<Expression<D>>>,

        /// The explicit type of the value.
        r#type: WithInfo<D::Info, Type<D>>,
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
        segments: Vec<FormatSegment<WithInfo<D::Info, Expression<D>>>>,

        /// Any trailing text after the segments.
        trailing: String,
    },

    /// A block.
    Block(Vec<WithInfo<D::Info, Statement<D>>>),

    /// Evaluate a block.
    Do(WithInfo<D::Info, Box<Expression<D>>>),

    /// A function.
    #[serde(rename_all = "camelCase")]
    Function {
        /// The function's inputs.
        inputs: Vec<WithInfo<D::Info, Pattern<D>>>,

        /// The function's output.
        body: WithInfo<D::Info, Box<Expression<D>>>,
    },

    /// A function call.
    Call {
        /// The function to call.
        function: WithInfo<D::Info, Box<Expression<D>>>,

        /// The function's inputs.
        inputs: Vec<WithInfo<D::Info, Expression<D>>>,
    },

    /// Function application.
    Apply {
        /// The input.
        input: WithInfo<D::Info, Box<Expression<D>>>,

        /// The function.
        function: WithInfo<D::Info, Box<Expression<D>>>,
    },

    /// A binary operator expression.
    #[serde(rename_all = "camelCase")]
    BinaryOperator {
        /// The operator used.
        operator: WithInfo<D::Info, BinaryOperator>,

        /// The left-hand side.
        left: WithInfo<D::Info, Box<Expression<D>>>,

        /// The right-hand side.
        right: WithInfo<D::Info, Box<Expression<D>>>,
    },

    /// Convert a value of one type into a value of another type.
    #[serde(rename_all = "camelCase")]
    As {
        /// The value to convert.
        value: WithInfo<D::Info, Box<Expression<D>>>,

        /// The type to convert to.
        r#type: WithInfo<D::Info, Type<D>>,
    },

    /// Check if a value matches a pattern.
    #[serde(rename_all = "camelCase")]
    Is {
        /// The value to match.
        value: WithInfo<D::Info, Box<Expression<D>>>,

        /// The pattern to match.
        pattern: WithInfo<D::Info, Pattern<D>>,
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
        name: WithInfo<D::Info, Option<String>>,

        /// The inputs to the intrinsic.
        inputs: Vec<WithInfo<D::Info, Expression<D>>>,
    },

    /// A tuple expression.
    Tuple(Vec<WithInfo<D::Info, Expression<D>>>),

    /// Collection construction.
    Collection(Vec<WithInfo<D::Info, Expression<D>>>),

    /// Structure construction.
    Structure(Vec<WithInfo<D::Info, FieldValue<D>>>),
}

/// A parsed type parameter.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct TypeParameter<D: Driver> {
    /// The name of the type parameter.
    pub name: WithInfo<D::Info, Option<String>>,

    /// Whether the type parameter was marked as `infer`.
    pub infer: Option<WithInfo<D::Info, ()>>,

    /// The type parameter's default type.
    pub default: Option<WithInfo<D::Info, Type<D>>>,
}

/// A parsed type representation.
#[derive(Serialize, Derivative)]
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

/// A parsed structure field.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct Field<D: Driver> {
    /// The field's attributes.
    pub attributes: Vec<WithInfo<D::Info, Attribute<D>>>,

    /// The name of the field.
    pub name: WithInfo<D::Info, String>,

    /// The type of the field.
    pub r#type: WithInfo<D::Info, Type<D>>,
}

/// A parsed enumeration variant.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct Variant<D: Driver> {
    /// The variant's attributes.
    pub attributes: Vec<WithInfo<D::Info, Attribute<D>>>,

    /// The name of the variant.
    pub name: WithInfo<D::Info, String>,

    /// The types of the variant's associated values.
    pub types: Vec<WithInfo<D::Info, Type<D>>>,
}

/// A parsed type.
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
        /// The name of the type.
        name: WithInfo<D::Info, Option<String>>,

        /// The parameters provided to the type.
        parameters: Vec<WithInfo<D::Info, Type<D>>>,
    },

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

/// A parsed instance.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct Instance<D: Driver> {
    /// The trait this instance refers to.
    pub r#trait: WithInfo<D::Info, Option<String>>,

    /// The parameters provided to the trait the instance refers to.
    pub parameters: Vec<WithInfo<D::Info, Type<D>>>,
}

/// A parsed format segment.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FormatSegment<T> {
    /// The text preceding the interpolated value.
    pub text: String,

    /// The interpolated value.
    pub value: T,
}

/// A parsed pattern.
#[derive(Serialize, Derivative)]
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
    Name(String),

    /// A variant pattern, or a variable if a variant with this name doesn't
    /// exist.
    VariantOrName(WithInfo<D::Info, Option<String>>),

    /// A destructuring pattern.
    Destructure(Vec<WithInfo<D::Info, FieldPattern<D>>>),

    /// A variant pattern.
    #[serde(rename_all = "camelCase")]
    Variant {
        /// The variant this pattern matches.
        variant: WithInfo<D::Info, Option<String>>,

        /// The patterns matching each of the variant's associated values.
        value_patterns: Vec<WithInfo<D::Info, Pattern<D>>>,
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

    /// A pattern that changes the value of an existing variable.
    Mutate(WithInfo<D::Info, Option<String>>),

    /// Annotate a pattern with an explicit type.
    Annotate {
        /// The pattern to annotate.
        pattern: WithInfo<D::Info, Box<Pattern<D>>>,

        /// The explicit type of the pattern.
        r#type: WithInfo<D::Info, Type<D>>,
    },
}

/// A field in a destructuring pattern.
#[derive(Serialize, Derivative)]
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
#[derive(Serialize, Derivative)]
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
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct FieldValue<D: Driver> {
    /// The name of the field.
    pub name: WithInfo<D::Info, Option<String>>,

    /// The field's value.
    pub value: WithInfo<D::Info, Expression<D>>,
}
