//! Compiler pass that parses source code into a syntax tree.

mod ast;
mod text;

use derivative::Derivative;
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use std::fmt::Debug;
use wipple_util::WithInfo;

/// Provides the parser with information about the program.
pub trait Driver: Sized {
    /// Additional information attached to every item.
    type Info: Debug + Clone + Serialize + DeserializeOwned;

    /// Retrieve the path of the file being parsed.
    fn file_path(&self) -> String;

    /// Merge two [`Info`](Driver::Info) values together.
    fn merge_info(left: Self::Info, right: Self::Info) -> Self::Info;
}

/// Parse a syntax tree into a program.
pub fn parse<D: Driver>(_driver: &D, syntax: WithInfo<D::Info, syntax::TopLevel<D>>) -> Result<D> {
    ast::top_level(syntax)
}

/// The result of [`parse`].
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct Result<D: Driver> {
    /// The top-level program.
    pub top_level: WithInfo<D::Info, TopLevel<D>>,

    /// Any errors encountered while parsing the source code.
    pub errors: Vec<WithInfo<D::Info, Error>>,
}

/// The context in which an [`Error`] occurred.
#[allow(missing_docs)]
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
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
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub enum Error {
    /// The parser found a bound, but bounds aren't allowed here.
    UnexpectedBound,

    /// The parser expected a constant's initializer to immediately follow the
    /// constant's declaration.
    ExpectedConstantValue,

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
    Compose,
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
}

/// Contains the types for nodes in a concrete syntax tree.
pub mod syntax {
    use super::*;

    /// The concrete top-level program.
    #[derive(Deserialize, Derivative)]
    #[derivative(
        Debug(bound = "D::Info: Debug"),
        Clone(bound = ""),
        PartialEq(bound = "D::Info: PartialEq"),
        Eq(bound = "D::Info: Eq")
    )]
    #[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
    pub struct TopLevel<D: Driver> {
        /// The top-level statements in the program.
        pub statements: Vec<WithInfo<D::Info, Statement<D>>>,
    }

    /// A concrete statement.
    #[derive(Deserialize, Derivative)]
    #[derivative(
        Debug(bound = "D::Info: Debug"),
        Clone(bound = ""),
        PartialEq(bound = "D::Info: PartialEq"),
        Eq(bound = "D::Info: Eq")
    )]
    #[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
    pub enum Statement<D: Driver> {
        /// A statement that could not be parsed.
        Error,

        /// A type declaration.
        #[serde(rename_all = "camelCase")]
        TypeDeclaration {
            /// The name of the type.
            name: WithInfo<D::Info, String>,

            /// The type's parameters.
            parameters: WithInfo<D::Info, TypeFunction<D>>,

            /// The type's representation.
            representation: WithInfo<D::Info, TypeRepresentation<D>>,
        },

        /// A trait declaration.
        #[serde(rename_all = "camelCase")]
        TraitDeclaration {
            /// The name of the trait.
            name: WithInfo<D::Info, String>,

            /// The trait's parameters.
            parameters: WithInfo<D::Info, TypeFunction<D>>,

            /// The trait's type.
            r#type: WithInfo<D::Info, Type<D>>,
        },

        /// An instance declaration.
        #[serde(rename_all = "camelCase")]
        InstanceDeclaration {
            /// The instance's parameters.
            parameters: WithInfo<D::Info, TypeFunction<D>>,

            /// The instance.
            instance: WithInfo<D::Info, Instance<D>>,

            /// The instance's value.
            body: WithInfo<D::Info, Expression<D>>,
        },

        /// A constant declaration.
        #[serde(rename_all = "camelCase")]
        ConstantDeclaration {
            /// The name of the constant.
            name: WithInfo<D::Info, String>,

            /// The constant's parameters.
            parameters: WithInfo<D::Info, TypeFunction<D>>,

            /// The constant's type.
            r#type: WithInfo<D::Info, Type<D>>,
        },

        /// A language declaration.
        #[serde(rename_all = "camelCase")]
        LanguageDeclaration {
            /// The name of the language feature.
            name: WithInfo<D::Info, String>,

            /// The item this language declaration corresponds to.
            item: WithInfo<D::Info, String>,
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

    /// A concrete type function.
    #[derive(Deserialize, Derivative)]
    #[derivative(
        Debug(bound = "D::Info: Debug"),
        Clone(bound = ""),
        PartialEq(bound = "D::Info: PartialEq"),
        Eq(bound = "D::Info: Eq")
    )]
    #[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
    pub struct TypeFunction<D: Driver> {
        /// The parameters in the type function.
        pub parameters: Vec<WithInfo<D::Info, TypeParameter<D>>>,

        /// The bounds on the parameters.
        pub bounds: Vec<WithInfo<D::Info, Instance<D>>>,
    }

    /// A concrete type parameter.
    #[derive(Deserialize, Derivative)]
    #[derivative(
        Debug(bound = "D::Info: Debug"),
        Clone(bound = ""),
        PartialEq(bound = "D::Info: PartialEq"),
        Eq(bound = "D::Info: Eq")
    )]
    #[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
    pub struct TypeParameter<D: Driver> {
        /// The name of the type parameter.
        pub name: WithInfo<D::Info, String>,

        /// Whether the type parameter was marked as `infer`.
        pub infer: Option<WithInfo<D::Info, ()>>,

        /// The type parameter's default type.
        pub default: Option<WithInfo<D::Info, Type<D>>>,
    }

    /// A concrete instance.
    #[derive(Deserialize, Derivative)]
    #[derivative(
        Debug(bound = "D::Info: Debug"),
        Clone(bound = ""),
        PartialEq(bound = "D::Info: PartialEq"),
        Eq(bound = "D::Info: Eq")
    )]
    #[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
    pub struct Instance<D: Driver> {
        /// The trait this instance refers to.
        pub r#trait: WithInfo<D::Info, String>,

        /// The parameters provided to the trait the instance refers to.
        pub parameters: Vec<WithInfo<D::Info, Type<D>>>,
    }

    /// A concrete type representation.
    #[derive(Deserialize, Derivative)]
    #[derivative(
        Debug(bound = "D::Info: Debug"),
        Clone(bound = ""),
        PartialEq(bound = "D::Info: PartialEq"),
        Eq(bound = "D::Info: Eq")
    )]
    #[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
    pub enum TypeRepresentation<D: Driver> {
        /// A marker type.
        Marker,

        /// An enumeration type with all of its variants on a single line.
        SimpleEnumeration(Vec<WithInfo<D::Info, String>>),

        /// A type with members across multiple lines.
        Compound(Vec<WithInfo<D::Info, TypeMember<D>>>),
    }

    /// A concrete type member.
    #[derive(Deserialize, Derivative)]
    #[derivative(
        Debug(bound = "D::Info: Debug"),
        Clone(bound = ""),
        PartialEq(bound = "D::Info: PartialEq"),
        Eq(bound = "D::Info: Eq")
    )]
    #[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
    pub struct TypeMember<D: Driver> {
        /// The name of the member.
        pub name: WithInfo<D::Info, String>,

        /// The kind of member.
        pub kind: TypeMemberKind<D>,
    }

    /// The kind of [`TypeMember`].
    #[derive(Deserialize, Derivative)]
    #[derivative(
        Debug(bound = "D::Info: Debug"),
        Clone(bound = ""),
        PartialEq(bound = "D::Info: PartialEq"),
        Eq(bound = "D::Info: Eq")
    )]
    #[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
    pub enum TypeMemberKind<D: Driver> {
        /// A field in a structure.
        Field(WithInfo<D::Info, Type<D>>),

        /// A variant in an enumeration.
        Variant(Vec<WithInfo<D::Info, Type<D>>>),
    }

    /// A concrete expression.
    #[derive(Deserialize, Derivative)]
    #[derivative(
        Debug(bound = "D::Info: Debug"),
        Clone(bound = ""),
        PartialEq(bound = "D::Info: PartialEq"),
        Eq(bound = "D::Info: Eq")
    )]
    #[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
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

        /// The unit expression.
        Unit,

        /// A formatted text literal.
        #[serde(rename_all = "camelCase")]
        Format {
            /// The text containing placeholders.
            text: WithInfo<D::Info, String>,

            /// The inputs to substitute the placeholders with.
            inputs: Vec<WithInfo<D::Info, Expression<D>>>,
        },

        /// A block.
        Block(Vec<WithInfo<D::Info, Statement<D>>>),

        /// A function.
        #[serde(rename_all = "camelCase")]
        Function {
            /// The function's input.
            pattern: WithInfo<D::Info, Pattern<D>>,

            /// The function's output.
            body: WithInfo<D::Info, Box<Expression<D>>>,
        },

        /// A function call.
        Call {
            /// The function to call.
            function: WithInfo<D::Info, Box<Expression<D>>>,

            /// The function's input.
            input: WithInfo<D::Info, Box<Expression<D>>>,
        },

        /// Function application.
        Apply {
            /// The input.
            input: Option<WithInfo<D::Info, Box<Expression<D>>>>,

            /// The function.
            function: Option<WithInfo<D::Info, Box<Expression<D>>>>,
        },

        /// A binary operator expression.
        #[serde(rename_all = "camelCase")]
        BinaryOperator {
            /// The operator used.
            operator: WithInfo<D::Info, BinaryOperator>,

            /// The left-hand side.
            left: Option<WithInfo<D::Info, Box<Expression<D>>>>,

            /// The right-hand side.
            right: Option<WithInfo<D::Info, Box<Expression<D>>>>,
        },

        /// Convert a value of one type into a value of another type.
        #[serde(rename_all = "camelCase")]
        As {
            /// The value to convert.
            value: Option<WithInfo<D::Info, Box<Expression<D>>>>,

            /// The type to convert to.
            r#type: WithInfo<D::Info, Type<D>>,
        },

        /// Check if a value matches a pattern.
        #[serde(rename_all = "camelCase")]
        Is {
            /// The value to match.
            value: Option<WithInfo<D::Info, Box<Expression<D>>>>,

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
            name: WithInfo<D::Info, String>,

            /// The inputs to the intrinsic.
            inputs: Vec<WithInfo<D::Info, Expression<D>>>,
        },

        /// A tuple expression.
        Tuple(Vec<WithInfo<D::Info, Expression<D>>>),

        /// Collection construction.
        Collection(Vec<WithInfo<D::Info, Expression<D>>>),

        /// Structure construction.
        Structure(Vec<WithInfo<D::Info, FieldValue<D>>>),

        /// Provide additional information about the semantics of an expression to
        /// the compiler.
        #[serde(rename_all = "camelCase")]
        Semantics {
            /// The name of the semantic.
            name: WithInfo<D::Info, String>,

            /// The expression that has the defined semantics.
            body: WithInfo<D::Info, Box<Expression<D>>>,
        },
    }

    /// A concrete type.
    #[derive(Deserialize, Derivative)]
    #[derivative(
        Debug(bound = "D::Info: Debug"),
        Clone(bound = ""),
        PartialEq(bound = "D::Info: PartialEq"),
        Eq(bound = "D::Info: Eq")
    )]
    #[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
    pub enum Type<D: Driver> {
        /// A type that could not be parsed.
        Error,

        /// A placeholder type.
        Placeholder,

        /// The unit type.
        Unit,

        /// A declared type.
        #[serde(rename_all = "camelCase")]
        Declared {
            /// The name of the type.
            name: WithInfo<D::Info, String>,

            /// The parameters provided to the type.
            parameters: Vec<WithInfo<D::Info, Type<D>>>,
        },

        /// A function type.
        #[serde(rename_all = "camelCase")]
        Function {
            /// The type of the function's input.
            input: WithInfo<D::Info, Box<Type<D>>>,

            /// The type of the function's output.
            output: WithInfo<D::Info, Box<Type<D>>>,
        },

        /// A tuple type.
        Tuple(Vec<WithInfo<D::Info, Type<D>>>),

        /// A type whose values are computed lazily.
        Lazy(WithInfo<D::Info, Box<Type<D>>>),
    }

    /// A concrete patterns.
    #[derive(Deserialize, Derivative)]
    #[derivative(
        Debug(bound = "D::Info: Debug"),
        Clone(bound = ""),
        PartialEq(bound = "D::Info: PartialEq"),
        Eq(bound = "D::Info: Eq")
    )]
    #[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
    pub enum Pattern<D: Driver> {
        /// A pattern that could not be parsed.
        Error,

        /// A pattern that matches anything.
        Wildcard,

        /// The unit pattern.
        Unit,

        /// A number pattern.
        Number(String),

        /// A text pattern.
        Text(String),

        /// A variable pattern.
        Name(String),

        /// A variant pattern, or a variable if a variant with this name doesn't
        /// exist.
        VariantOrName(String),

        /// A destructuring pattern.
        Destructure(Vec<WithInfo<D::Info, FieldPattern<D>>>),

        /// A variant pattern.
        #[serde(rename_all = "camelCase")]
        Variant {
            /// The variant this pattern matches.
            variant: WithInfo<D::Info, String>,

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
    }

    /// A field in a destructuring pattern.
    #[derive(Deserialize, Derivative)]
    #[derivative(
        Debug(bound = "D::Info: Debug"),
        Clone(bound = ""),
        PartialEq(bound = "D::Info: PartialEq"),
        Eq(bound = "D::Info: Eq")
    )]
    #[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
    pub struct FieldPattern<D: Driver> {
        /// The name of the field.
        pub name: WithInfo<D::Info, String>,

        /// The pattern matching the field's value.
        pub pattern: WithInfo<D::Info, Pattern<D>>,
    }

    /// An arm in a `when` expression.
    #[derive(Deserialize, Derivative)]
    #[derivative(
        Debug(bound = "D::Info: Debug"),
        Clone(bound = ""),
        PartialEq(bound = "D::Info: PartialEq"),
        Eq(bound = "D::Info: Eq")
    )]
    #[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
    pub struct Arm<D: Driver> {
        /// The pattern to match on the input.
        pub pattern: WithInfo<D::Info, Pattern<D>>,

        /// The condition to check if the input matches.
        pub condition: Option<WithInfo<D::Info, Expression<D>>>,

        /// The arm's body.
        pub body: WithInfo<D::Info, Expression<D>>,
    }

    /// A field-value pair in a structure construction expression.
    #[derive(Deserialize, Derivative)]
    #[derivative(
        Debug(bound = "D::Info: Debug"),
        Clone(bound = ""),
        PartialEq(bound = "D::Info: PartialEq"),
        Eq(bound = "D::Info: Eq")
    )]
    #[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
    pub struct FieldValue<D: Driver> {
        /// The name of the field.
        pub name: WithInfo<D::Info, String>,

        /// The field's value.
        pub value: WithInfo<D::Info, Expression<D>>,
    }
}

/// The parsed top-level program.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct TopLevel<D: Driver> {
    /// The top-level statements in the program.
    pub statements: Vec<WithInfo<D::Info, Statement<D>>>,
}

/// A parsed statement.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub enum Statement<D: Driver> {
    /// A type declaration.
    #[serde(rename_all = "camelCase")]
    Type {
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
        /// The name of the trait.
        name: WithInfo<D::Info, String>,

        /// The trait's parameters.
        parameters: Vec<WithInfo<D::Info, TypeParameter<D>>>,

        /// The trait's type.
        r#type: WithInfo<D::Info, Type<D>>,
    },

    /// A constant declaration.
    #[serde(rename_all = "camelCase")]
    Constant {
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
        /// The instance's parameters.
        parameters: Vec<WithInfo<D::Info, TypeParameter<D>>>,

        /// The instance's bounds.
        bounds: Vec<WithInfo<D::Info, Instance<D>>>,

        /// The instance.
        instance: WithInfo<D::Info, Instance<D>>,

        /// The instance's body.
        body: WithInfo<D::Info, Expression<D>>,
    },

    /// A language declaration.
    #[serde(rename_all = "camelCase")]
    Language {
        /// The name of the language feature.
        name: WithInfo<D::Info, String>,

        /// The item this language declaration corresponds to.
        item: WithInfo<D::Info, String>,
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
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
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

    /// A function.
    #[serde(rename_all = "camelCase")]
    Function {
        /// The function's input.
        pattern: WithInfo<D::Info, Pattern<D>>,

        /// The function's output.
        body: WithInfo<D::Info, Box<Expression<D>>>,
    },

    /// A function call.
    Call {
        /// The function to call.
        function: WithInfo<D::Info, Box<Expression<D>>>,

        /// The function's input.
        input: WithInfo<D::Info, Box<Expression<D>>>,
    },

    /// Function application.
    Apply {
        /// The input.
        input: Option<WithInfo<D::Info, Box<Expression<D>>>>,

        /// The function.
        function: Option<WithInfo<D::Info, Box<Expression<D>>>>,
    },

    /// A binary operator expression.
    #[serde(rename_all = "camelCase")]
    BinaryOperator {
        /// The operator used.
        operator: WithInfo<D::Info, BinaryOperator>,

        /// The left-hand side.
        left: Option<WithInfo<D::Info, Box<Expression<D>>>>,

        /// The right-hand side.
        right: Option<WithInfo<D::Info, Box<Expression<D>>>>,
    },

    /// Convert a value of one type into a value of another type.
    #[serde(rename_all = "camelCase")]
    As {
        /// The value to convert.
        value: Option<WithInfo<D::Info, Box<Expression<D>>>>,

        /// The type to convert to.
        r#type: WithInfo<D::Info, Type<D>>,
    },

    /// Check if a value matches a pattern.
    #[serde(rename_all = "camelCase")]
    Is {
        /// The value to match.
        value: Option<WithInfo<D::Info, Box<Expression<D>>>>,

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
        name: WithInfo<D::Info, String>,

        /// The inputs to the intrinsic.
        inputs: Vec<WithInfo<D::Info, Expression<D>>>,
    },

    /// A tuple expression.
    Tuple(Vec<WithInfo<D::Info, Expression<D>>>),

    /// Collection construction.
    Collection(Vec<WithInfo<D::Info, Expression<D>>>),

    /// Structure construction.
    Structure(Vec<WithInfo<D::Info, FieldValue<D>>>),

    /// Provide additional information about the semantics of an expression to
    /// the compiler.
    #[serde(rename_all = "camelCase")]
    Semantics {
        /// The name of the semantic.
        name: WithInfo<D::Info, String>,

        /// The expression that has the defined semantics.
        body: WithInfo<D::Info, Box<Expression<D>>>,
    },
}

/// A parsed type parameter.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct TypeParameter<D: Driver> {
    /// The name of the type parameter.
    pub name: WithInfo<D::Info, String>,

    /// Whether the type parameter was marked as `infer`.
    pub infer: Option<WithInfo<D::Info, ()>>,

    /// The type parameter's default type.
    pub default: Option<WithInfo<D::Info, Type<D>>>,
}

/// A parsed type representation.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub enum TypeRepresentation<D: Driver> {
    /// A marker type.
    Marker,

    /// A structure type.
    Structure(Vec<WithInfo<D::Info, Field<D>>>),

    /// An enumeration type.
    Enumeration(Vec<WithInfo<D::Info, Variant<D>>>),
}

/// A parsed structure field.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct Field<D: Driver> {
    /// The name of the field.
    pub name: WithInfo<D::Info, String>,

    /// The type of the field.
    pub r#type: WithInfo<D::Info, Type<D>>,
}

/// A parsed enumeration variant.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct Variant<D: Driver> {
    /// The name of the variant.
    pub name: WithInfo<D::Info, String>,

    /// The types of the variant's associated values.
    pub types: Vec<WithInfo<D::Info, Type<D>>>,
}

/// A parsed type.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub enum Type<D: Driver> {
    /// A type that could not be parsed.
    Error,

    /// A placeholder type.
    Placeholder,

    /// A declared type.
    #[serde(rename_all = "camelCase")]
    Declared {
        /// The name of the type.
        name: WithInfo<D::Info, String>,

        /// The parameters provided to the type.
        parameters: Vec<WithInfo<D::Info, Type<D>>>,
    },

    /// A function type.
    #[serde(rename_all = "camelCase")]
    Function {
        /// The type of the function's input.
        input: WithInfo<D::Info, Box<Type<D>>>,

        /// The type of the function's output.
        output: WithInfo<D::Info, Box<Type<D>>>,
    },

    /// A tuple type.
    Tuple(Vec<WithInfo<D::Info, Type<D>>>),

    /// A type whose values are computed lazily.
    Lazy(WithInfo<D::Info, Box<Type<D>>>),
}

/// A parsed instance.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct Instance<D: Driver> {
    /// The trait this instance refers to.
    pub r#trait: WithInfo<D::Info, String>,

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
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
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
    VariantOrName(String),

    /// A destructuring pattern.
    Destructure(Vec<WithInfo<D::Info, FieldPattern<D>>>),

    /// A variant pattern.
    #[serde(rename_all = "camelCase")]
    Variant {
        /// The variant this pattern matches.
        variant: WithInfo<D::Info, String>,

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
}

/// A field in a destructuring pattern.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct FieldPattern<D: Driver> {
    /// The name of the field.
    pub name: WithInfo<D::Info, String>,

    /// The pattern matching the field's value.
    pub pattern: WithInfo<D::Info, Pattern<D>>,
}

/// An arm in a `when` expression.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct Arm<D: Driver> {
    /// The pattern to match on the input.
    pub pattern: WithInfo<D::Info, Pattern<D>>,

    /// The condition to check if the input matches.
    pub condition: Option<WithInfo<D::Info, Expression<D>>>,

    /// The arm's body.
    pub body: WithInfo<D::Info, Expression<D>>,
}

/// A field-value pair in a structure construction expression.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct FieldValue<D: Driver> {
    /// The name of the field.
    pub name: WithInfo<D::Info, String>,

    /// The field's value.
    pub value: WithInfo<D::Info, Expression<D>>,
}