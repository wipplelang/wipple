//! Compiler pass that determines the type of every expression in the program
//! and resolves constants and traits to concrete items.

mod resolve;

use derivative::Derivative;
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use std::{collections::HashMap, fmt::Debug, hash::Hash};
use wipple_util::WithInfo;

/// Provides the typechecker with information about the program.
pub trait Driver: Sized {
    /// Additional information attached to every item.
    type Info: Debug + Clone + Eq + Hash + Serialize + DeserializeOwned;

    /// Represents a path used to resolve declarations.
    type Path: Debug + Clone + Eq + Hash + Serialize + DeserializeOwned;

    /// The recursion limit.
    fn recursion_limit(&self) -> u32;

    /// The information attached to top-level code.
    fn top_level_info(&self) -> Self::Info;

    /// Retrieve the path of a type annotated with `[language]`.
    fn path_for_language_type(&self, language_item: &'static str) -> Option<Self::Path>;

    /// Retrieve the path of a trait annotated with `[language]`.
    fn path_for_language_trait(&self, language_item: &'static str) -> Option<Self::Path>;

    /// Check if two paths are equal.
    fn paths_are_equal(&self, left: &Self::Path, right: &Self::Path) -> bool;

    /// Retrieve the type declaration at the given path.
    fn get_type_declaration(
        &self,
        path: &Self::Path,
    ) -> WithInfo<Self::Info, TypeDeclaration<Self>>;

    /// Retrieve the trait declaration at the given path.
    fn get_trait_declaration(
        &self,
        path: &Self::Path,
    ) -> WithInfo<Self::Info, TraitDeclaration<Self>>;

    /// Retrieve the type parameter declaration at the given path.
    fn get_type_parameter_declaration(
        &self,
        path: &Self::Path,
    ) -> WithInfo<Self::Info, TypeParameterDeclaration<Self>>;

    /// Retrieve the constant declaration at the given path.
    fn get_constant_declaration(
        &self,
        path: &Self::Path,
    ) -> WithInfo<Self::Info, ConstantDeclaration<Self>>;

    /// Retrieve the instance declaration at the given path.
    fn get_instance_declaration(
        &self,
        path: &Self::Path,
    ) -> WithInfo<Self::Info, InstanceDeclaration<Self>>;

    /// Retrieve the instances available for the trait at the given path.
    fn get_instances_for_trait(&self, r#trait: &Self::Path) -> Vec<Self::Path>;

    /// Retrieve the enumeration for the variant at the given path.
    fn get_enumeration_for_variant(&self, variant: &Self::Path) -> Self::Path;
}

/// Internal representation of constants, instances, and top-level code for
/// typechecking.
pub struct ItemDeclaration<D: Driver>(resolve::ItemDeclarationInner<D>);

/// Internal trait for converting constants, instances, and top-level code into
/// the same representation for typechecking.
pub trait IntoItemDeclaration<D: Driver> {
    /// Convert the value into the representation used by the typechecker.
    fn into_item_declaration(self, driver: &D) -> WithInfo<D::Info, ItemDeclaration<D>>;
}

/// Resolve a constant, instance, or the top level.
pub fn resolve<D: Driver>(
    driver: &D,
    item_declaration: impl IntoItemDeclaration<D>,
) -> crate::Result<D> {
    resolve::resolve(driver, item_declaration)
}

/// The result of [`resolve`].
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct Result<D: Driver> {
    /// The resolved item.
    pub item: WithInfo<D::Info, TypedExpression<D>>,

    /// Any errors encountered while resolving the item.
    pub errors: Vec<WithInfo<D::Info, Error<D>>>,
}

/// An error occurring during typechecking.
#[derive(Serialize, Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub enum Error<D: Driver> {
    /// The typechecker hit the recursion limit while attempting to resolve the
    /// expression.
    RecursionLimit,

    /// The driver failed to provide a path for a required language item.
    MissingLanguageItem(&'static str),

    /// The type of the expression could not be determined.
    UnknownType(Type<D>),

    /// The expression refers to a type parameter not declared in the parameter
    /// list.
    UndeclaredTypeParameter(D::Path),

    /// There was a mismatch between the inferred type of the expression and the
    /// type required in its place.
    #[serde(rename_all = "camelCase")]
    Mismatch {
        /// The roles of the actual type.
        actual_roles: Vec<WithInfo<D::Info, Role>>,

        /// The inferred type of the expression.
        actual: Type<D>,

        /// The roles of the expected type.
        expected_roles: Vec<WithInfo<D::Info, Role>>,

        /// The expected type of the expression.
        expected: Type<D>,
    },

    /// A coercion would be required for a contravariant type. Currently, the
    /// only coercion is `A` to `lazy A`, so this error effectively means that
    /// the following program is invalid because `x` cannot be implicitly
    /// `evaluate`d:
    ///
    /// ```wipple
    /// x :: () -> lazy Number
    /// x : () -> 42
    ///
    /// y : () -> Number
    /// y : x ()
    /// ```
    #[serde(rename_all = "camelCase")]
    DisallowedCoercion(Type<D>),

    /// No instance satisfying the provided parameters could be resolved.
    #[serde(rename_all = "camelCase")]
    UnresolvedInstance {
        /// The trait for which no instance could be resolved.
        r#trait: D::Path,

        /// The parameters used to search for an instance.
        parameters: Vec<Type<D>>,

        /// If the instance could not be resolved because multiple candidates
        /// applied, they will be listed here.
        candidates: Vec<D::Info>,

        /// Contains the list of instances evaluated before failing to resolve
        /// [`ErrorKind::UnresolvedInstance::trait`].
        stack: Vec<D::Info>,
    },

    /// A structure expression was missing fields.
    MissingFields(Vec<String>),

    /// A structure expression contained an extra field.
    ExtraField,
}

/// The type of an expression.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub enum Type<D: Driver> {
    /// A type to be inferred or that could not be resolved.
    Unknown,

    /// A type parameter.
    Parameter(D::Path),

    /// A declared type.
    #[serde(rename_all = "camelCase")]
    Declared {
        /// The path to the type declaration.
        path: D::Path,

        /// The type's parameters.
        parameters: Vec<Type<D>>,
    },

    /// A function type.
    #[serde(rename_all = "camelCase")]
    Function {
        /// The type of the function's input.
        input: Box<Type<D>>,

        /// The type of the function's output.
        output: Box<Type<D>>,
    },

    /// A tuple type.
    Tuple(Vec<Type<D>>),

    /// A type whose values are computed lazily.
    Lazy(Box<Type<D>>),
}

/// An instance or bound.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct Instance<D: Driver> {
    /// The trait for which an instance must exist.
    pub r#trait: D::Path,

    /// The parameters used to search for an instance.
    pub parameters: Vec<Type<D>>,
}

/// A type declaration.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct TypeDeclaration<D: Driver> {
    /// The type's parameters.
    pub parameters: Vec<D::Path>,

    /// The type's representation (marker, structure or enumeration).
    pub representation: WithInfo<D::Info, TypeRepresentation<D>>,
}

/// A trait declaration.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct TraitDeclaration<D: Driver> {
    /// The trait's parameters.
    pub parameters: Vec<D::Path>,

    /// The trait's type.
    pub r#type: WithInfo<D::Info, Type<D>>,
}

/// A type parameter declaration.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct TypeParameterDeclaration<D: Driver> {
    /// Whether the parameter is marked `infer`.
    pub infer: Option<WithInfo<D::Info, ()>>,

    /// The parameter's default type.
    pub default: Option<WithInfo<D::Info, Type<D>>>,
}

/// The representation of a [`TypeDeclaration`].
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub enum TypeRepresentation<D: Driver> {
    /// The type contains no data.
    Marker,

    /// The type contains an ordered list of fields.
    Structure(HashMap<String, WithInfo<D::Info, StructureField<D>>>),

    /// The type can be one of several variants.
    Enumeration(HashMap<D::Path, WithInfo<D::Info, EnumerationVariant<D>>>),
}

/// A single field in a type represented as a structure.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct StructureField<D: Driver> {
    /// The type of the field's value.
    pub r#type: WithInfo<D::Info, Type<D>>,
}

/// A single variant in a type represented as an enumeration.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct EnumerationVariant<D: Driver> {
    /// The types of the variant's associated values.
    pub value_types: Vec<WithInfo<D::Info, Type<D>>>,
}

/// A constant declaration.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct ConstantDeclaration<D: Driver> {
    /// The constant's parameters.
    pub parameters: Vec<D::Path>,

    /// The constant's bounds.
    pub bounds: Vec<WithInfo<D::Info, Instance<D>>>,

    /// The constant's declared type.
    pub r#type: WithInfo<D::Info, Type<D>>,
}

/// An instance declaration.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct InstanceDeclaration<D: Driver> {
    /// The instance's parameters.
    pub parameters: Vec<D::Path>,

    /// The instance's bounds.
    pub bounds: Vec<WithInfo<D::Info, Instance<D>>>,

    /// The parameters corresponding to the instance's trait.
    pub instance: WithInfo<D::Info, Instance<D>>,
}

/// An untyped expression.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub enum UntypedExpression<D: Driver> {
    /// An expression that could not be resolved prior to typechecking.
    Unknown,

    /// An expression explicitly annotated with type.
    Annotate {
        /// The inner value.
        value: WithInfo<D::Info, Box<UntypedExpression<D>>>,

        /// The type of the inner value.
        r#type: Type<D>,
    },

    /// A value of a marker type.
    Marker(D::Path),

    /// The value of a variable.
    Variable(D::Path),

    /// The value of a constant.
    Constant(D::Path),

    /// An instance of a trait.
    Trait(D::Path),

    /// A number literal.
    Number(String),

    /// A text literal.
    Text(String),

    /// A block.
    Block(Vec<WithInfo<D::Info, UntypedExpression<D>>>),

    /// A function.
    #[serde(rename_all = "camelCase")]
    Function {
        /// The function's input.
        pattern: WithInfo<D::Info, Pattern<D>>,

        /// The function's output.
        body: WithInfo<D::Info, Box<UntypedExpression<D>>>,
    },

    /// A function call.
    #[serde(rename_all = "camelCase")]
    Call {
        /// The function to call.
        function: WithInfo<D::Info, Box<UntypedExpression<D>>>,

        /// The input to the function.
        input: WithInfo<D::Info, Box<UntypedExpression<D>>>,
    },

    /// A `when` expression.
    #[serde(rename_all = "camelCase")]
    When {
        /// The value to match.
        input: WithInfo<D::Info, Box<UntypedExpression<D>>>,

        /// The arms to execute.
        arms: Vec<WithInfo<D::Info, UntypedArm<D>>>,
    },

    /// An `intrinsic` expression.
    #[serde(rename_all = "camelCase")]
    Intrinsic {
        /// The name of the compiler intrinsic to call.
        name: String,

        /// The inputs to the intrinsic.
        inputs: Vec<WithInfo<D::Info, UntypedExpression<D>>>,
    },

    /// Bind a value to a pattern.
    #[serde(rename_all = "camelCase")]
    Initialize {
        /// The pattern to match.
        pattern: WithInfo<D::Info, Pattern<D>>,

        /// The value to match.
        value: WithInfo<D::Info, Box<UntypedExpression<D>>>,
    },

    /// Create a structure value.
    Structure(Vec<WithInfo<D::Info, UntypedStructureFieldValue<D>>>),

    /// Create a variant of an enumeration.
    #[serde(rename_all = "camelCase")]
    Variant {
        /// The variant this value creates.
        variant: WithInfo<D::Info, D::Path>,

        /// The variant's associated values.
        values: Vec<WithInfo<D::Info, UntypedExpression<D>>>,
    },

    /// A tuple expression.
    Tuple(Vec<WithInfo<D::Info, UntypedExpression<D>>>),

    /// Collection construction.
    Collection(Vec<WithInfo<D::Info, UntypedExpression<D>>>),

    /// String interpolation.
    #[serde(rename_all = "camelCase")]
    Format {
        /// The segments of text that end in interpolated values.
        segments: Vec<UntypedFormatSegment<D>>,

        /// Any trailing text after the segments.
        trailing: String,
    },

    /// Provide additional information about the semantics of an expression to
    /// the compiler.
    #[serde(rename_all = "camelCase")]
    Semantics {
        /// The name of the semantic.
        name: String,

        /// The expression that has the defined semantics.
        body: WithInfo<D::Info, Box<UntypedExpression<D>>>,
    },
}

/// A segment in a string interpolation expression.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct UntypedFormatSegment<D: Driver> {
    /// The text before the interpolated value.
    pub text: String,

    /// The value to insert after the text.
    pub value: WithInfo<D::Info, UntypedExpression<D>>,
}

/// The value of a structure field.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct UntypedStructureFieldValue<D: Driver> {
    /// The name of the field.
    pub name: String,

    /// The field's value.
    pub value: WithInfo<D::Info, UntypedExpression<D>>,
}

/// An arm in a `when` expression.
#[derive(Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct UntypedArm<D: Driver> {
    /// The pattern to match on the input.
    pub pattern: WithInfo<D::Info, Pattern<D>>,

    /// A condition to evaluate after the pattern matches.
    pub condition: Option<WithInfo<D::Info, UntypedExpression<D>>>,

    /// The arm's body.
    pub body: WithInfo<D::Info, UntypedExpression<D>>,
}

/// A typed expression.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct TypedExpression<D: Driver> {
    /// The type of the expression.
    pub r#type: Type<D>,

    /// The kind of expression.
    pub kind: TypedExpressionKind<D>,
}

/// The kind of [`TypedExpression`].
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub enum TypedExpressionKind<D: Driver> {
    /// An expression that could not be resolved.
    Unknown(Option<D::Path>),

    /// A value of a marker type.
    Marker(D::Path),

    /// The value of a variable.
    Variable(D::Path),

    /// A constant.
    Constant(D::Path),

    /// A trait.
    Trait(D::Path),

    /// A number literal.
    Number(String),

    /// A text literal.
    Text(String),

    /// A block.
    Block(Vec<WithInfo<D::Info, TypedExpression<D>>>),

    /// A function.
    #[serde(rename_all = "camelCase")]
    Function {
        /// The function's input.
        pattern: WithInfo<D::Info, Pattern<D>>,

        /// The function's output.
        body: WithInfo<D::Info, Box<TypedExpression<D>>>,
    },

    /// A function call.
    #[serde(rename_all = "camelCase")]
    Call {
        /// The function to call.
        function: WithInfo<D::Info, Box<TypedExpression<D>>>,

        /// The input to the function.
        input: WithInfo<D::Info, Box<TypedExpression<D>>>,
    },

    /// A `when` expression.
    #[serde(rename_all = "camelCase")]
    When {
        /// The value to match.
        input: WithInfo<D::Info, Box<TypedExpression<D>>>,

        /// The arms to execute.
        arms: Vec<WithInfo<D::Info, TypedArm<D>>>,
    },

    /// An `intrinsic` expression.
    #[serde(rename_all = "camelCase")]
    Intrinsic {
        /// The name of the compiler intrinsic to call.
        name: String,

        /// The inputs to the intrinsic.
        inputs: Vec<WithInfo<D::Info, TypedExpression<D>>>,
    },

    /// Bind a value to a pattern.
    #[serde(rename_all = "camelCase")]
    Initialize {
        /// The pattern to match.
        pattern: WithInfo<D::Info, Pattern<D>>,

        /// The value to match.
        value: WithInfo<D::Info, Box<TypedExpression<D>>>,
    },

    /// Create a structure value.
    Structure {
        /// The structure this value refers to.
        structure: D::Path,

        /// The values of the structure's fields.
        fields: Vec<WithInfo<D::Info, TypedStructureFieldValue<D>>>,
    },

    /// Create a variant of an enumeration.
    #[serde(rename_all = "camelCase")]
    Variant {
        /// The variant this value creates.
        variant: WithInfo<D::Info, D::Path>,

        /// The variant's associated values.
        values: Vec<WithInfo<D::Info, TypedExpression<D>>>,
    },

    /// A tuple expression.
    Tuple(Vec<WithInfo<D::Info, TypedExpression<D>>>),

    /// String interpolation.
    #[serde(rename_all = "camelCase")]
    Format {
        /// The segments of text that end in interpolated values.
        segments: Vec<TypedFormatSegment<D>>,

        /// Any trailing text after the segments.
        trailing: String,
    },

    /// Provide additional information about the semantics of an expression to
    /// the compiler.
    #[serde(rename_all = "camelCase")]
    Semantics {
        /// The name of the semantic.
        name: String,

        /// The expression that has the defined semantics.
        body: WithInfo<D::Info, Box<TypedExpression<D>>>,
    },

    /// A lazily-computed value.
    Lazy(WithInfo<D::Info, Box<TypedExpression<D>>>),
}

/// A segment in a string interpolation expression.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct TypedFormatSegment<D: Driver> {
    /// The text before the interpolated value.
    pub text: String,

    /// The value to insert after the text.
    pub value: WithInfo<D::Info, TypedExpression<D>>,
}

/// The value of a structure field.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct TypedStructureFieldValue<D: Driver> {
    /// The name of the field.
    pub name: String,

    /// The field's value.
    pub value: WithInfo<D::Info, TypedExpression<D>>,
}

/// An arm in a `when` expression.
#[derive(Serialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct TypedArm<D: Driver> {
    /// The pattern to match on the input.
    pub pattern: WithInfo<D::Info, Pattern<D>>,

    /// A condition to evaluate after the pattern matches.
    pub condition: Option<WithInfo<D::Info, TypedExpression<D>>>,

    /// The arm's body.
    pub body: WithInfo<D::Info, TypedExpression<D>>,
}

/// A pattern.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub enum Pattern<D: Driver> {
    /// A pattern that could not be resolved prior to or after typechecking.
    Unknown,

    /// A pattern that matches anything.
    Wildcard,

    /// A number pattern.
    Number(String),

    /// A text pattern.
    Text(String),

    /// A variable pattern.
    Variable(D::Path),

    /// A destructuring pattern.
    Destructure(Vec<WithInfo<D::Info, FieldPattern<D>>>),

    /// A variant pattern.
    #[serde(rename_all = "camelCase")]
    Variant {
        /// The variant this pattern matches.
        variant: WithInfo<D::Info, D::Path>,

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
    pub name: String,

    /// The pattern matching the field's value.
    pub pattern: WithInfo<D::Info, Pattern<D>>,
}

/// The role of a type.
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum Role {
    Pattern,
    Annotation,
    Trait,
    Instance,
    StructureField,
    VariantElement,
    FunctionInput,
    FunctionOutput,
    Bound,
    DefaultType,
    Variable,
    TypeParameter,
    EmptyBlock,
    WhenArm,
    CollectionElement,
}
