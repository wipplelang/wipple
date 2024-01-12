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

    /// Represents a number value.
    type Number: Debug + Clone + Serialize + DeserializeOwned;
}

/// Contains the definitions of items in a file.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""), Default(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct Interface<D: Driver> {
    /// The type declarations in the module.
    pub type_declarations: HashMap<Path, WithInfo<D::Info, TypeDeclaration<D>>>,

    /// The trait declarations in the module.
    pub trait_declarations: HashMap<Path, WithInfo<D::Info, TraitDeclaration<D>>>,

    /// The type parameters in the module.
    pub type_parameter_declarations: HashMap<Path, WithInfo<D::Info, TypeParameterDeclaration<D>>>,

    /// The language declarations in the module.
    pub language_declarations: HashMap<String, Path>,

    /// The constant declarations in the module.
    pub constant_declarations: HashMap<Path, WithInfo<D::Info, ConstantDeclaration<D>>>,

    /// The instance declarations in the module.
    pub instance_declarations: HashMap<Path, WithInfo<D::Info, InstanceDeclaration<D>>>,
}

impl<D: Driver> Interface<D> {
    fn merge(&mut self, other: Self, errors: &mut Vec<WithInfo<D::Info, Error>>) {
        // TODO: Check for conflicts

        self.type_declarations.extend(other.type_declarations);
        self.trait_declarations.extend(other.trait_declarations);
        self.type_parameter_declarations
            .extend(other.type_parameter_declarations);
        self.language_declarations
            .extend(other.language_declarations);
        self.constant_declarations
            .extend(other.constant_declarations);
        self.instance_declarations
            .extend(other.instance_declarations);
    }
}

/// Contains the implementations of items in a file.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""), Default(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct Library<D: Driver> {
    /// The implementations of constants and instances.
    pub items: HashMap<Path, WithInfo<D::Info, crate::Expression<D>>>,

    /// Any code to be run when the program starts.
    pub code: Vec<WithInfo<D::Info, Expression<D>>>,
}

/// Resolve a list of files into a module.
pub fn resolve<D: Driver>(
    _driver: &D,
    files: impl IntoIterator<Item = WithInfo<D::Info, UnresolvedFile<D>>>,
    dependencies: impl IntoIterator<Item = Interface<D>>,
) -> Result<D> {
    let mut errors = Vec::new();

    let dependencies = dependencies
        .into_iter()
        .fold(Interface::default(), |mut current, next| {
            current.merge(next, &mut errors);
            current
        });

    let result = resolve::resolve(files, dependencies);
    errors.extend(result.errors);

    Result {
        interface: result.interface,
        library: result.library,
        errors,
    }
}

/// The result of [`resolve`].
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct Result<D: Driver> {
    /// The resolved [`Interface`].
    pub interface: Interface<D>,

    /// The resolved [`Library`].
    pub library: Library<D>,

    /// Any errors encountered while resolving the files.
    pub errors: Vec<WithInfo<D::Info, Error>>,
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
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub enum Error {
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
}

/// An unresolved file.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct UnresolvedFile<D: Driver> {
    /// The top-level statements in the program.
    pub statements: Vec<WithInfo<D::Info, UnresolvedStatement<D>>>,
}

/// An unresolved statement.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub enum UnresolvedStatement<D: Driver> {
    /// A type declaration.
    #[serde(rename_all = "camelCase")]
    Type {
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
        /// The name of the trait.
        name: WithInfo<D::Info, String>,

        /// The trait's parameters.
        parameters: Vec<WithInfo<D::Info, UnresolvedTypeParameter<D>>>,

        /// The trait's type.
        r#type: WithInfo<D::Info, UnresolvedType<D>>,
    },

    /// A constant declaration.
    #[serde(rename_all = "camelCase")]
    Constant {
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
        /// The instance's parameters.
        parameters: Vec<WithInfo<D::Info, UnresolvedTypeParameter<D>>>,

        /// The instance's bounds.
        bounds: Vec<WithInfo<D::Info, UnresolvedInstance<D>>>,

        /// The instance.
        instance: WithInfo<D::Info, UnresolvedInstance<D>>,

        /// The instance's body.
        body: WithInfo<D::Info, UnresolvedExpression<D>>,
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
        pattern: WithInfo<D::Info, UnresolvedPattern<D>>,

        /// The value.
        value: WithInfo<D::Info, UnresolvedExpression<D>>,
    },

    /// An expression.
    Expression(WithInfo<D::Info, UnresolvedExpression<D>>),
}

/// An unresolved expression.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
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
    Number(D::Number),

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

    /// A function.
    #[serde(rename_all = "camelCase")]
    Function {
        /// The function's input.
        pattern: WithInfo<D::Info, UnresolvedPattern<D>>,

        /// The function's output.
        body: WithInfo<D::Info, Box<UnresolvedExpression<D>>>,
    },

    /// A function call.
    Call {
        /// The function to call.
        function: WithInfo<D::Info, Box<UnresolvedExpression<D>>>,

        /// The function's input.
        input: WithInfo<D::Info, Box<UnresolvedExpression<D>>>,
    },

    /// Function application.
    Apply {
        /// The input.
        input: Option<WithInfo<D::Info, Box<UnresolvedExpression<D>>>>,

        /// The function.
        function: Option<WithInfo<D::Info, Box<UnresolvedExpression<D>>>>,
    },

    /// A binary operator expression.
    #[serde(rename_all = "camelCase")]
    BinaryOperator {
        /// The operator used.
        operator: WithInfo<D::Info, UnresolvedBinaryOperator>,

        /// The left-hand side.
        left: Option<WithInfo<D::Info, Box<UnresolvedExpression<D>>>>,

        /// The right-hand side.
        right: Option<WithInfo<D::Info, Box<UnresolvedExpression<D>>>>,
    },

    /// Convert a value of one type into a value of another type.
    #[serde(rename_all = "camelCase")]
    As {
        /// The value to convert.
        value: Option<WithInfo<D::Info, Box<UnresolvedExpression<D>>>>,

        /// The type to convert to.
        r#type: WithInfo<D::Info, UnresolvedType<D>>,
    },

    /// Check if a value matches a pattern.
    #[serde(rename_all = "camelCase")]
    Is {
        /// The value to match.
        value: Option<WithInfo<D::Info, Box<UnresolvedExpression<D>>>>,

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
        name: WithInfo<D::Info, String>,

        /// The inputs to the intrinsic.
        inputs: Vec<WithInfo<D::Info, UnresolvedExpression<D>>>,
    },

    /// A tuple expression.
    Tuple(Vec<WithInfo<D::Info, UnresolvedExpression<D>>>),

    /// Collection construction.
    Collection(Vec<WithInfo<D::Info, UnresolvedExpression<D>>>),

    /// Structure construction.
    Structure(Vec<WithInfo<D::Info, UnresolvedFieldValue<D>>>),

    /// Provide additional information about the semantics of an expression to
    /// the compiler.
    #[serde(rename_all = "camelCase")]
    Semantics {
        /// The name of the semantic.
        name: WithInfo<D::Info, String>,

        /// The expression that has the defined semantics.
        body: WithInfo<D::Info, Box<UnresolvedExpression<D>>>,
    },
}

/// An unresolved binary operator.
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, strum::Display)]
#[serde(rename_all = "camelCase")]
#[strum(serialize_all = "kebab-case")]
pub enum UnresolvedBinaryOperator {
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

/// An unresolved type parameter.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct UnresolvedTypeParameter<D: Driver> {
    /// The name of the type parameter.
    pub name: WithInfo<D::Info, String>,

    /// Whether the type parameter was marked as `infer`.
    pub infer: Option<WithInfo<D::Info, ()>>,

    /// The type parameter's default type.
    pub default: Option<WithInfo<D::Info, UnresolvedType<D>>>,
}

/// An unresolved type representation.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub enum UnresolvedTypeRepresentation<D: Driver> {
    /// A marker type.
    Marker,

    /// A structure type.
    Structure(Vec<WithInfo<D::Info, UnresolvedField<D>>>),

    /// An enumeration type.
    Enumeration(Vec<WithInfo<D::Info, UnresolvedVariant<D>>>),
}

/// An unresolved structure field.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct UnresolvedField<D: Driver> {
    /// The name of the field.
    pub name: WithInfo<D::Info, String>,

    /// The type of the field.
    pub r#type: WithInfo<D::Info, UnresolvedType<D>>,
}

/// An unresolved enumeration variant.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct UnresolvedVariant<D: Driver> {
    /// The name of the variant.
    pub name: WithInfo<D::Info, String>,

    /// The types of the variant's associated values.
    pub types: Vec<WithInfo<D::Info, UnresolvedType<D>>>,
}

/// An unresolved type.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub enum UnresolvedType<D: Driver> {
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
        parameters: Vec<WithInfo<D::Info, UnresolvedType<D>>>,
    },

    /// A function type.
    #[serde(rename_all = "camelCase")]
    Function {
        /// The type of the function's input.
        input: WithInfo<D::Info, Box<UnresolvedType<D>>>,

        /// The type of the function's output.
        output: WithInfo<D::Info, Box<UnresolvedType<D>>>,
    },

    /// A tuple type.
    Tuple(Vec<WithInfo<D::Info, UnresolvedType<D>>>),

    /// A type whose values are computed lazily.
    Lazy(WithInfo<D::Info, Box<UnresolvedType<D>>>),
}

/// An unresolved instance.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct UnresolvedInstance<D: Driver> {
    /// The trait this instance refers to.
    pub r#trait: WithInfo<D::Info, String>,

    /// The parameters provided to the trait the instance refers to.
    pub parameters: Vec<WithInfo<D::Info, UnresolvedType<D>>>,
}

/// An unresolved pattern.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub enum UnresolvedPattern<D: Driver> {
    /// A pattern that could not be parsed.
    Error,

    /// A pattern that matches anything.
    Wildcard,

    /// A number pattern.
    Number(D::Number),

    /// A text pattern.
    Text(String),

    /// A variable pattern.
    Name(String),

    /// A variant pattern, or a variable if a variant with this name doesn't
    /// exist.
    VariantOrName(String),

    /// A destructuring pattern.
    Destructure(Vec<WithInfo<D::Info, UnresolvedFieldPattern<D>>>),

    /// A variant pattern.
    #[serde(rename_all = "camelCase")]
    Variant {
        /// The variant this pattern matches.
        variant: WithInfo<D::Info, String>,

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
}

/// A field in a destructuring pattern.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct UnresolvedFieldPattern<D: Driver> {
    /// The name of the field.
    pub name: WithInfo<D::Info, String>,

    /// The pattern matching the field's value.
    pub pattern: WithInfo<D::Info, UnresolvedPattern<D>>,
}

/// An arm in a `when` expression.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct UnresolvedArm<D: Driver> {
    /// The pattern to match on the input.
    pub pattern: WithInfo<D::Info, UnresolvedPattern<D>>,

    /// The condition to check if the input matches.
    pub condition: Option<WithInfo<D::Info, UnresolvedExpression<D>>>,

    /// The arm's body.
    pub body: WithInfo<D::Info, UnresolvedExpression<D>>,
}

/// A field-value pair in a structure construction expression.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct UnresolvedFieldValue<D: Driver> {
    /// The name of the field.
    pub name: WithInfo<D::Info, String>,

    /// The field's value.
    pub value: WithInfo<D::Info, UnresolvedExpression<D>>,
}

/// A path that uniquely identifies a declaration.
pub type Path = Vec<PathComponent>;

/// A component of a [`Path`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub enum PathComponent {
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

    /// Returns the name of the declaration the path component refers to, or
    /// `None` if the declaration has no name.
    pub fn name(&self) -> Option<&str> {
        match self {
            PathComponent::Type(name)
            | PathComponent::Trait(name)
            | PathComponent::Constant(name)
            | PathComponent::Constructor(name)
            | PathComponent::Variant(name)
            | PathComponent::Language(name)
            | PathComponent::TypeParameter(name) => Some(name),
            PathComponent::Instance(_) | PathComponent::Variable(_) => None,
        }
    }
}

/// A resolved type declaration.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct TypeDeclaration<D: Driver> {
    /// The type's parameters.
    pub parameters: Vec<crate::Path>,

    /// The type's representation.
    pub representation: WithInfo<D::Info, TypeRepresentation<D>>,
}

/// A resolved trait declaration.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct TraitDeclaration<D: Driver> {
    /// The trait's parameters.
    pub parameters: Vec<crate::Path>,

    /// The trait's type.
    pub r#type: WithInfo<D::Info, crate::Type<D>>,
}

/// A resolved constant declaration.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct ConstantDeclaration<D: Driver> {
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
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct InstanceDeclaration<D: Driver> {
    /// The instance's parameters.
    pub parameters: Vec<crate::Path>,

    /// The instance's bounds.
    pub bounds: Vec<WithInfo<D::Info, crate::Instance<D>>>,

    /// The trait and parameters that this instance satisfies.
    pub instance: WithInfo<D::Info, crate::Instance<D>>,
}

/// A resolved type parameter.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct TypeParameterDeclaration<D: Driver> {
    /// Whether the type parameter was marked as `infer`.
    pub infer: Option<WithInfo<D::Info, ()>>,

    /// The type parameter's default type.
    pub default: Option<WithInfo<D::Info, Type<D>>>,
}

/// A resolved expression.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
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

    /// Annotate an expression with an explicit type.
    #[serde(rename_all = "camelCase")]
    Annotate {
        /// The value to annotate.
        value: WithInfo<D::Info, Box<Expression<D>>>,

        /// The explicit type of the value.
        r#type: WithInfo<D::Info, Type<D>>,
    },

    /// A variable.
    Variable(Path),

    /// A number literal.
    Number(D::Number),

    /// A text literal.
    Text(String),

    /// The value of a marker type.
    Marker(Path),

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
    Block(Vec<WithInfo<D::Info, Expression<D>>>),

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

/// A resolved type representation.
#[derive(Serialize, Deserialize, Derivative)]
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

/// A resolved structure field.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct Field<D: Driver> {
    /// The name of the field.
    pub name: WithInfo<D::Info, String>,

    /// The type of the field.
    pub r#type: WithInfo<D::Info, Type<D>>,
}

/// A resolved enumeration variant.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct Variant<D: Driver> {
    /// The name of the variant.
    pub name: WithInfo<D::Info, String>,

    /// The types of the variant's associated values.
    pub types: Vec<WithInfo<D::Info, Type<D>>>,
}

/// A resolved type.
#[derive(Serialize, Deserialize, Derivative)]
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

/// A resolved instance.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
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
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub enum Pattern<D: Driver> {
    /// A pattern that could not be parsed.
    Error,

    /// A pattern that matches anything.
    Wildcard,

    /// A number pattern.
    Number(D::Number),

    /// A text pattern.
    Text(String),

    /// A variable pattern.
    Variable(Path),

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
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct FieldPattern<D: Driver> {
    /// The name of the field.
    pub name: WithInfo<D::Info, String>,

    /// The pattern matching the field's value.
    pub pattern: WithInfo<D::Info, Pattern<D>>,
}

/// An arm in a `when` expression.
#[derive(Serialize, Deserialize, Derivative)]
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
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct FieldValue<D: Driver> {
    /// The name of the field.
    pub name: WithInfo<D::Info, String>,

    /// The field's value.
    pub value: WithInfo<D::Info, Expression<D>>,
}
