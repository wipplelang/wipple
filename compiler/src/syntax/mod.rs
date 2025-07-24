//! Compiler pass that parses source code into a syntax tree.

pub mod ast;
pub mod parse;
pub mod tokenize;

mod text;

use crate::util::WithInfo;
use serde::Serialize;
use std::{fmt::Debug, hash::Hash, ops::Range, sync::Arc};

/// Location information about a parsed item.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct Location {
    /// The path of the file this item belongs to.
    pub path: Arc<str>,

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
            span: left.span.start..right.span.end,
        }
    }
}

/// Parse a syntax tree into a program.
pub fn parse(syntax: WithInfo<parse::TopLevel>) -> Result {
    ast::top_level(syntax)
}

/// The result of [`parse`].
#[derive(Debug)]
pub struct Result {
    /// The top-level program.
    pub top_level: WithInfo<TopLevel>,

    /// Any errors encountered while parsing the source code.
    pub diagnostics: Vec<WithInfo<Diagnostic>>,
}

/// The context in which an [`Error`] occurred.
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

/// The parsed top-level program.
#[derive(Debug, Clone)]
pub struct TopLevel {
    /// The top-level statements in the program.
    pub statements: Vec<WithInfo<Statement>>,
}

/// A parsed statement.
#[derive(Debug, Clone)]
pub enum Statement {
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
        parameters: Vec<WithInfo<TypeParameter>>,

        /// The type's representation.
        representation: WithInfo<TypeRepresentation>,
    },

    /// A trait declaration.
    Trait {
        /// The trait's attributes.
        attributes: Vec<WithInfo<Attribute>>,

        /// The name of the trait.
        name: WithInfo<String>,

        /// The trait's parameters.
        parameters: Vec<WithInfo<TypeParameter>>,

        /// The trait's type.
        r#type: Option<WithInfo<Type>>,
    },

    /// A constant declaration.
    Constant {
        /// The constant's attributes.
        attributes: Vec<WithInfo<Attribute>>,

        /// The name of the constant.
        name: WithInfo<String>,

        /// The constant's parameters.
        parameters: Vec<WithInfo<TypeParameter>>,

        /// The constant's bounds.
        bounds: Vec<WithInfo<Instance>>,

        /// The constant's type.
        r#type: WithInfo<Type>,

        /// The constant's body.
        body: WithInfo<Expression>,
    },

    /// An instance declaration.
    Instance {
        /// The info belonging to the left-hand side of the assignment.
        pattern: WithInfo<()>,

        /// The instance's parameters.
        parameters: Vec<WithInfo<TypeParameter>>,

        /// The instance's bounds.
        bounds: Vec<WithInfo<Instance>>,

        /// The instance.
        instance: WithInfo<Instance>,

        /// The instance's body.
        body: Option<WithInfo<Expression>>,

        /// Whether the instance is the default instance.
        default: bool,
    },

    /// A variable assignment.
    Assignment {
        /// The pattern to assign to.
        pattern: WithInfo<Pattern>,

        /// The value.
        value: WithInfo<Expression>,
    },

    /// An expression.
    Expression(WithInfo<Expression>),
}

/// A parsed expression.
#[derive(Debug, Clone)]
pub enum Expression {
    /// An expression that could not be parsed.
    Error,

    /// Annotate an expression with an explicit type.
    Annotate {
        /// The value to annotate.
        value: WithInfo<Box<Expression>>,

        /// The explicit type of the value.
        r#type: WithInfo<Type>,
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
        segments: Vec<FormatSegment<WithInfo<Expression>>>,

        /// Any trailing text after the segments.
        trailing: String,
    },

    /// A block.
    Block(Vec<WithInfo<Statement>>),

    /// Evaluate a block.
    Do(WithInfo<Box<Expression>>),

    /// A function.
    Function {
        /// The function's inputs.
        inputs: Vec<WithInfo<Pattern>>,

        /// The function's output.
        body: WithInfo<Box<Expression>>,
    },

    /// A function call.
    Call {
        /// The function to call.
        function: WithInfo<Box<Expression>>,

        /// The function's inputs.
        inputs: Vec<WithInfo<Expression>>,
    },

    /// Function application.
    Apply {
        /// The input.
        input: WithInfo<Box<Expression>>,

        /// The function.
        function: WithInfo<Box<Expression>>,
    },

    /// A binary operator expression.
    BinaryOperator {
        /// The operator used.
        operator: WithInfo<BinaryOperator>,

        /// The left-hand side.
        left: WithInfo<Box<Expression>>,

        /// The right-hand side.
        right: WithInfo<Box<Expression>>,
    },

    /// Convert a value of one type into a value of another type.
    As {
        /// The value to convert.
        value: WithInfo<Box<Expression>>,

        /// The type to convert to.
        r#type: WithInfo<Type>,
    },

    /// Check if a value matches a pattern.
    Is {
        /// The value to match.
        value: WithInfo<Box<Expression>>,

        /// The pattern to match.
        pattern: WithInfo<Pattern>,
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
        name: WithInfo<Option<String>>,

        /// The inputs to the intrinsic.
        inputs: Vec<WithInfo<Expression>>,
    },

    /// A tuple expression.
    Tuple(Vec<WithInfo<Expression>>),

    /// Collection construction.
    Collection(Vec<WithInfo<Expression>>),

    /// Structure construction.
    Structure(Vec<WithInfo<FieldValue>>),
}

/// A parsed type parameter.
#[derive(Debug, Clone)]
pub struct TypeParameter {
    /// The name of the type parameter.
    pub name: WithInfo<Option<String>>,

    /// Whether the type parameter was marked as `infer`.
    pub infer: Option<WithInfo<()>>,

    /// The type parameter's default type.
    pub default: Option<WithInfo<Type>>,
}

/// A parsed type representation.
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

/// A parsed structure field.
#[derive(Debug, Clone)]
pub struct Field {
    /// The field's attributes.
    pub attributes: Vec<WithInfo<Attribute>>,

    /// The name of the field.
    pub name: WithInfo<String>,

    /// The type of the field.
    pub r#type: WithInfo<Type>,
}

/// A parsed enumeration variant.
#[derive(Debug, Clone)]
pub struct Variant {
    /// The variant's attributes.
    pub attributes: Vec<WithInfo<Attribute>>,

    /// The name of the variant.
    pub name: WithInfo<String>,

    /// The types of the variant's associated values.
    pub types: Vec<WithInfo<Type>>,
}

/// A parsed type.
#[derive(Debug, Clone)]
pub enum Type {
    /// A type that could not be parsed.
    Error,

    /// A placeholder type.
    Placeholder,

    /// A declared type.
    Declared {
        /// The name of the type.
        name: WithInfo<Option<String>>,

        /// The parameters provided to the type.
        parameters: Vec<WithInfo<Type>>,
    },

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

/// A parsed instance.
#[derive(Debug, Clone)]
pub struct Instance {
    /// The trait this instance refers to.
    pub r#trait: WithInfo<Option<String>>,

    /// The parameters provided to the trait the instance refers to.
    pub parameters: Vec<WithInfo<Type>>,
}

/// A parsed format segment.
#[derive(Debug, Clone)]
pub struct FormatSegment<T> {
    /// The text preceding the interpolated value.
    pub text: String,

    /// The interpolated value.
    pub value: T,
}

/// A parsed pattern.
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
    Name(String),

    /// A variant pattern, or a variable if a variant with this name doesn't
    /// exist.
    VariantOrName(WithInfo<Option<String>>),

    /// A destructuring pattern.
    Destructure(Vec<WithInfo<FieldPattern>>),

    /// A variant pattern.
    Variant {
        /// The variant this pattern matches.
        variant: WithInfo<Option<String>>,

        /// The patterns matching each of the variant's associated values.
        value_patterns: Vec<WithInfo<Pattern>>,
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

    /// A pattern that changes the value of an existing variable.
    Mutate(WithInfo<Option<String>>),

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
