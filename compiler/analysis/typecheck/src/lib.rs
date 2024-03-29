//! Compiler pass that determines the type of every expression in the program
//! and resolves constants and traits to concrete items.

pub mod exhaustiveness;
mod resolve;

use derivative::Derivative;
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use std::{collections::HashMap, fmt::Debug, hash::Hash};
use ts_rs::TS;
use wipple_util::WithInfo;

/// Provides the typechecker with information about the program.
pub trait Driver: Sized {
    /// Additional information attached to every item.
    type Info: Debug + Clone + Eq + Hash + Serialize + DeserializeOwned + TS;

    /// Represents a path used to resolve declarations.
    type Path: Debug + Clone + Eq + Ord + Hash + Serialize + DeserializeOwned + TS;

    /// The recursion limit.
    fn recursion_limit(&self) -> u32;

    /// The information attached to top-level code.
    fn top_level_info(&self) -> Self::Info;

    /// Retrieve the path of a type annotated with `[language]`.
    fn path_for_language_type(&self, language_item: &'static str) -> Option<Self::Path>;

    /// Retrieve the path of a trait annotated with `[language]`.
    fn path_for_language_trait(&self, language_item: &'static str) -> Option<Self::Path>;

    /// Retrieve the path of a constructor annotated with `[language]`.
    fn path_for_language_constructor(&self, language_item: &'static str) -> Option<Self::Path>;

    /// Retrieve the path of a constant annotated with `[language]`.
    fn path_for_language_constant(&self, language_item: &'static str) -> Option<Self::Path>;

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

impl Driver for wipple_util::TsAny {
    type Info = wipple_util::TsAny;

    type Path = wipple_util::TsAny;

    fn recursion_limit(&self) -> u32 {
        unimplemented!()
    }

    fn top_level_info(&self) -> Self::Info {
        unimplemented!()
    }

    fn path_for_language_type(&self, _language_item: &'static str) -> Option<Self::Path> {
        unimplemented!()
    }

    fn path_for_language_trait(&self, _language_item: &'static str) -> Option<Self::Path> {
        unimplemented!()
    }

    fn path_for_language_constructor(&self, _language_item: &'static str) -> Option<Self::Path> {
        unimplemented!()
    }

    fn path_for_language_constant(&self, _language_item: &'static str) -> Option<Self::Path> {
        unimplemented!()
    }

    fn paths_are_equal(&self, _left: &Self::Path, _right: &Self::Path) -> bool {
        unimplemented!()
    }

    fn get_type_declaration(
        &self,
        _path: &Self::Path,
    ) -> WithInfo<Self::Info, TypeDeclaration<Self>> {
        unimplemented!()
    }

    fn get_trait_declaration(
        &self,
        _path: &Self::Path,
    ) -> WithInfo<Self::Info, TraitDeclaration<Self>> {
        unimplemented!()
    }

    fn get_type_parameter_declaration(
        &self,
        _path: &Self::Path,
    ) -> WithInfo<Self::Info, TypeParameterDeclaration<Self>> {
        unimplemented!()
    }

    fn get_constant_declaration(
        &self,
        _path: &Self::Path,
    ) -> WithInfo<Self::Info, ConstantDeclaration<Self>> {
        unimplemented!()
    }

    fn get_instance_declaration(
        &self,
        _path: &Self::Path,
    ) -> WithInfo<Self::Info, InstanceDeclaration<Self>> {
        unimplemented!()
    }

    fn get_instances_for_trait(&self, _trait: &Self::Path) -> Vec<Self::Path> {
        unimplemented!()
    }

    fn get_enumeration_for_variant(&self, _variant: &Self::Path) -> Self::Path {
        unimplemented!()
    }
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
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct Result<D: Driver> {
    /// The resolved item.
    pub item: WithInfo<D::Info, TypedExpression<D>>,

    /// Any errors encountered while resolving the item.
    pub diagnostics: Vec<WithInfo<D::Info, Diagnostic<D>>>,
}

/// Check that none of the provided instances overlap.
pub fn instances_overlap<D: Driver>(
    driver: &D,
    r#trait: &D::Path,
    instances: Vec<D::Path>,
) -> Vec<WithInfo<D::Info, Diagnostic<D>>> {
    resolve::instances_overlap(driver, r#trait, instances)
}

/// Check for inexhaustive bindings and `when` expressions.
pub fn check_exhaustiveness<D: Driver>(
    driver: &D,
    expression: WithInfo<D::Info, &TypedExpression<D>>,
) -> Vec<WithInfo<D::Info, Diagnostic<D>>> {
    exhaustiveness::check_exhaustiveness(driver, expression)
}

/// Obtain the type of a trait expression matching the provided instance. For
/// example:
///
/// ```wipple
/// Show : A => trait (A -> Text)
/// instance (Show Number) : ...
/// ```
///
/// If this function is called with the above instance, it will return
/// `Number -> Text`.
///
/// This function assumes that all of the trait's parameters are referenced in
/// its type.
// FIXME: Enforce the above
pub fn resolve_trait_type_from_instance<D: Driver>(
    driver: &D,
    instance: WithInfo<D::Info, &crate::Instance<D>>,
) -> WithInfo<D::Info, crate::Type<D>> {
    resolve::resolve_trait_type_from_instance(driver, instance)
}

/// Substitute the default types for type parameters mentioned in `r#type`.
pub fn substitute_defaults<D: Driver>(driver: &D, r#type: WithInfo<D::Info, &mut crate::Type<D>>) {
    resolve::substitute_defaults_in_parameters(driver, r#type)
}

/// List the type parameters mentioned in `r#type`.
pub fn parameters_in<D: Driver>(r#type: WithInfo<D::Info, &crate::Type<D>>) -> Vec<D::Path> {
    resolve::parameters_in(r#type)
}

/// An error occurring during typechecking.
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
#[serde(rename_all = "camelCase", tag = "type", content = "value")]
#[serde(bound = "")]
#[ts(export, rename = "typecheck_Diagnostic", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub enum Diagnostic<D: Driver> {
    /// The typechecker hit the recursion limit while attempting to resolve the
    /// expression.
    RecursionLimit,

    /// The driver failed to provide a path for a required language item.
    MissingLanguageItem(String),

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
        actual: WithInfo<D::Info, Type<D>>,

        /// The roles of the expected type.
        expected_roles: Vec<WithInfo<D::Info, Role>>,

        /// The expected type of the expression.
        expected: WithInfo<D::Info, Type<D>>,
    },

    /// The wrong number of inputs were provided to a function.
    WrongNumberOfInputs {
        /// The number of inputs provided to the function.
        actual: u32,

        /// The number of inputs expected by the function.
        expected: u32,
    },

    /// No instance satisfying the provided parameters could be resolved.
    #[serde(rename_all = "camelCase")]
    UnresolvedInstance {
        /// The instance that couldn't be resolved.
        instance: Instance<D>,

        /// If the instance could not be resolved because multiple candidates
        /// applied, they will be listed here.
        candidates: Vec<D::Info>,

        /// Contains the list of instances evaluated before failing to resolve
        /// [`ErrorKind::UnresolvedInstance::trait`].
        stack: Vec<WithInfo<D::Info, Instance<D>>>,
    },

    /// A structure expression was used, but the expression does not have a
    /// structure type.
    NotAStructure(WithInfo<D::Info, Type<D>>),

    /// A structure expression was missing fields.
    MissingFields(Vec<String>),

    /// A structure expression contained an extra field.
    ExtraField,

    /// Two instances overlap.
    #[serde(rename_all = "camelCase")]
    OverlappingInstances {
        /// The instance that overlaps with [`Error::OverlappingInstances::other`].
        instance: D::Path,

        /// The instance that overlaps with [`Error::OverlappingInstances::instance`].
        other: D::Path,
    },

    /// There are multiple default instances for a single trait.
    #[serde(rename_all = "camelCase")]
    MultipleDefaultInstances {
        /// The disallowed instance.
        instance: D::Path,
    },

    /// The binding or `when` expression does not exhaustively match its input.
    MissingPatterns(Vec<exhaustiveness::Pattern<D>>),

    /// This pattern is already handled by a previous pattern.
    ExtraPattern,

    /// A custom error message.
    Custom {
        /// The segments of text that end in interpolated types.
        segments: Vec<MessageTypeFormatSegment<D>>,

        /// Any trailing text after the segments.
        trailing: String,
    },
}

/// The type of an expression.
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
#[serde(rename_all = "camelCase", tag = "type", content = "value")]
#[serde(bound = "")]
#[ts(export, rename = "typecheck_Type", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub enum Type<D: Driver> {
    /// A type to be inferred or that could not be resolved.
    Unknown(#[serde(skip)] UnknownTypeId),

    /// A type parameter.
    Parameter(D::Path),

    /// A declared type.
    #[serde(rename_all = "camelCase")]
    Declared {
        /// The path to the type declaration.
        path: D::Path,

        /// The type's parameters.
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
        segments: Vec<MessageTypeFormatSegment<D>>,

        /// Any trailing text after the segments.
        trailing: String,
    },
}

/// A segment in a message type.
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
#[ts(
    export,
    rename = "typecheck_MessageTypeFormatSegment",
    concrete(D = wipple_util::TsAny),
    bound = "D::Info: TS"
)]
pub struct MessageTypeFormatSegment<D: Driver> {
    /// The text before the interpolated type.
    pub text: String,

    /// The type to insert after the text.
    pub r#type: WithInfo<D::Info, Type<D>>,
}

/// Used to disambiguate between unknown types by recording the internal type
/// variable the [`Unknown`](Type::Unknown) type replaced.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct UnknownTypeId(Option<u32>);

impl UnknownTypeId {
    /// No additional information is available about the type.
    pub fn none() -> Self {
        UnknownTypeId(None)
    }

    /// Returns `true` if `self` originated from the same type as `other`.
    pub fn is_from_same_type_as(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl PartialEq for UnknownTypeId {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl Eq for UnknownTypeId {}

impl Hash for UnknownTypeId {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {}
}

/// An instance or bound.
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
#[ts(export, rename = "typecheck_Instance", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub struct Instance<D: Driver> {
    /// The trait for which an instance must exist.
    pub r#trait: D::Path,

    /// The parameters used to search for an instance.
    pub parameters: Vec<WithInfo<D::Info, Type<D>>>,
}

/// A type declaration.
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
#[ts(export, rename = "typecheck_TypeDeclaration", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub struct TypeDeclaration<D: Driver> {
    /// The type's parameters.
    pub parameters: Vec<D::Path>,

    /// The type's representation (opaque, structure or enumeration).
    pub representation: WithInfo<D::Info, TypeRepresentation<D>>,
}

/// A trait declaration.
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
#[ts(export, rename = "typecheck_TraitDeclaration", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub struct TraitDeclaration<D: Driver> {
    /// The trait's parameters.
    pub parameters: Vec<D::Path>,

    /// The trait's type.
    pub r#type: WithInfo<D::Info, Type<D>>,
}

/// A type parameter declaration.
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
#[ts(export, rename = "typecheck_TypeParameterDeclaration", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub struct TypeParameterDeclaration<D: Driver> {
    /// Whether the parameter is marked `infer`.
    pub infer: Option<WithInfo<D::Info, ()>>,

    /// The parameter's default type.
    pub default: Option<WithInfo<D::Info, Type<D>>>,
}

/// The representation of a [`TypeDeclaration`].
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", tag = "type", content = "value")]
#[serde(bound = "")]
#[ts(export, rename = "typecheck_TypeRepresentation", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub enum TypeRepresentation<D: Driver> {
    /// The type has a single value.
    Marker,

    /// The type contains an ordered list of fields.
    Structure(HashMap<String, WithInfo<D::Info, StructureField<D>>>),

    /// The type can be one of several variants.
    Enumeration(HashMap<D::Path, WithInfo<D::Info, EnumerationVariant<D>>>),

    /// The type wraps another type.
    Wrapper(WithInfo<D::Info, Type<D>>),
}

/// A single field in a type represented as a structure.
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
#[ts(export, rename = "typecheck_StructureField", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub struct StructureField<D: Driver> {
    /// The type of the field's value.
    pub r#type: WithInfo<D::Info, Type<D>>,
}

/// A single variant in a type represented as an enumeration.
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
#[ts(export, rename = "typecheck_EnumerationVariant", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub struct EnumerationVariant<D: Driver> {
    /// The types of the variant's associated values.
    pub value_types: Vec<WithInfo<D::Info, Type<D>>>,
}

/// A constant declaration.
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
#[ts(export, rename = "typecheck_ConstantDeclaration", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub struct ConstantDeclaration<D: Driver> {
    /// The constant's parameters.
    pub parameters: Vec<D::Path>,

    /// The constant's bounds.
    pub bounds: Vec<WithInfo<D::Info, Instance<D>>>,

    /// The constant's declared type.
    pub r#type: WithInfo<D::Info, Type<D>>,

    /// The constant's simplified type.
    pub simplified_type: WithInfo<D::Info, Type<D>>,
}

/// An instance declaration.
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
#[ts(export, rename = "typecheck_InstanceDeclaration", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub struct InstanceDeclaration<D: Driver> {
    /// The instance's parameters.
    pub parameters: Vec<D::Path>,

    /// The instance's bounds.
    pub bounds: Vec<WithInfo<D::Info, Instance<D>>>,

    /// The parameters corresponding to the instance's trait.
    pub instance: WithInfo<D::Info, Instance<D>>,

    /// Whether the instance is the default instance.
    pub default: bool,
}

/// An untyped expression.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum UntypedExpression<D: Driver> {
    /// An expression that could not be resolved prior to typechecking.
    Unknown,

    /// An expression explicitly annotated with type.
    Annotate {
        /// The inner value.
        value: WithInfo<D::Info, Box<UntypedExpression<D>>>,

        /// The type of the inner value.
        r#type: WithInfo<D::Info, Type<D>>,
    },

    /// The value of a variable.
    Variable(String, D::Path),

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

    /// Evaluate a block.
    Do(WithInfo<D::Info, Box<UntypedExpression<D>>>),

    /// A function.
    #[serde(rename_all = "camelCase")]
    Function {
        /// The function's inputs.
        inputs: Vec<WithInfo<D::Info, Pattern<D>>>,

        /// The function's output.
        body: WithInfo<D::Info, Box<UntypedExpression<D>>>,
    },

    /// A function call.
    #[serde(rename_all = "camelCase")]
    Call {
        /// The function to call.
        function: WithInfo<D::Info, Box<UntypedExpression<D>>>,

        /// The inputs to the function.
        inputs: Vec<WithInfo<D::Info, UntypedExpression<D>>>,
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

    /// Change the value of an existing variable.
    Mutate {
        /// The name of the variable.
        name: String,

        /// The path to the variable.
        path: WithInfo<D::Info, D::Path>,

        /// The value to assign to the variable.
        value: WithInfo<D::Info, Box<UntypedExpression<D>>>,
    },

    /// Create a marker value.
    Marker(D::Path),

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

    /// Create a wrapper value.
    Wrapper {
        /// The wrapper type.
        r#type: D::Path,

        /// The type's value.
        value: WithInfo<D::Info, Box<UntypedExpression<D>>>,
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
}

/// A segment in a string interpolation expression.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct UntypedFormatSegment<D: Driver> {
    /// The text before the interpolated value.
    pub text: String,

    /// The value to insert after the text.
    pub value: WithInfo<D::Info, UntypedExpression<D>>,
}

/// The value of a structure field.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct UntypedStructureFieldValue<D: Driver> {
    /// The name of the field.
    pub name: String,

    /// The field's value.
    pub value: WithInfo<D::Info, UntypedExpression<D>>,
}

/// An arm in a `when` expression.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct UntypedArm<D: Driver> {
    /// The pattern to match on the input.
    pub pattern: WithInfo<D::Info, Pattern<D>>,

    /// The arm's body.
    pub body: WithInfo<D::Info, UntypedExpression<D>>,
}

/// A typed expression.
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
#[ts(export, rename = "typecheck_TypedExpression", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub struct TypedExpression<D: Driver> {
    /// The type of the expression.
    pub r#type: Type<D>,

    /// The kind of expression.
    pub kind: TypedExpressionKind<D>,
}

/// The kind of [`TypedExpression`].
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase", tag = "type", content = "value")]
#[serde(bound = "")]
#[ts(export, rename = "typecheck_TypedExpressionKind", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub enum TypedExpressionKind<D: Driver> {
    /// An expression that could not be resolved.
    Unknown(Option<D::Path>),

    /// The value of a variable.
    Variable(String, D::Path),

    /// A constant.
    Constant {
        /// The path to the constant declaration.
        path: D::Path,

        /// The types of the constant's parameters. This is used in case the
        /// constant's type doesn't reference all type parameters.
        parameters: Vec<Type<D>>,
    },

    /// A trait.
    Trait(D::Path),

    /// A number literal.
    Number(String),

    /// A text literal.
    Text(String),

    /// A block.
    Block(Vec<WithInfo<D::Info, TypedExpression<D>>>),

    /// Evaluate a block.
    Do(WithInfo<D::Info, Box<TypedExpression<D>>>),

    /// A function.
    #[serde(rename_all = "camelCase")]
    Function {
        /// The function's inputs,
        inputs: Vec<WithInfo<D::Info, Pattern<D>>>,

        /// The function's output.
        body: WithInfo<D::Info, Box<TypedExpression<D>>>,
    },

    /// A function call.
    #[serde(rename_all = "camelCase")]
    Call {
        /// The function to call.
        function: WithInfo<D::Info, Box<TypedExpression<D>>>,

        /// The inputs to the function.
        inputs: Vec<WithInfo<D::Info, TypedExpression<D>>>,
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

    /// Change the value of an existing variable.
    Mutate {
        /// The name of the variable.
        name: String,

        /// The path to the variable.
        path: WithInfo<D::Info, D::Path>,

        /// The value to assign to the variable.
        value: WithInfo<D::Info, Box<TypedExpression<D>>>,
    },

    /// Create a marker value.
    Marker(D::Path),

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

    /// Create a wrapper value.
    Wrapper(WithInfo<D::Info, Box<TypedExpression<D>>>),

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
}

/// A segment in a string interpolation expression.
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
#[ts(export, rename = "typecheck_TypedFormatSegment", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub struct TypedFormatSegment<D: Driver> {
    /// The text before the interpolated value.
    pub text: String,

    /// The value to insert after the text.
    pub value: WithInfo<D::Info, TypedExpression<D>>,
}

/// The value of a structure field.
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
#[ts(export, rename = "typecheck_TypedStructureFieldValue", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub struct TypedStructureFieldValue<D: Driver> {
    /// The name of the field.
    pub name: String,

    /// The field's value.
    pub value: WithInfo<D::Info, TypedExpression<D>>,
}

/// An arm in a `when` expression.
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
#[ts(export, rename = "typecheck_TypedArm", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub struct TypedArm<D: Driver> {
    /// The pattern to match on the input.
    pub pattern: WithInfo<D::Info, Pattern<D>>,

    /// The arm's body.
    pub body: WithInfo<D::Info, TypedExpression<D>>,
}

/// A pattern.
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
#[serde(rename_all = "camelCase", tag = "type", content = "value")]
#[serde(bound = "")]
#[ts(export, rename = "typecheck_Pattern", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
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
    Variable(String, D::Path),

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

    /// A wrapper pattern.
    Wrapper {
        /// The wrapper type this pattern matches.
        path: WithInfo<D::Info, D::Path>,

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
}

/// A field in a destructuring pattern.
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
#[ts(export, rename = "typecheck_FieldPattern", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub struct FieldPattern<D: Driver> {
    /// The name of the field.
    pub name: String,

    /// The pattern matching the field's value.
    pub pattern: WithInfo<D::Info, Pattern<D>>,
}

/// The role of a type.
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, TS)]
#[ts(export, rename = "typecheck_Role")]
#[serde(rename_all = "camelCase")]
pub enum Role {
    Pattern,
    Annotation,
    Trait,
    Instance,
    StructureField,
    VariantElement,
    WrappedType,
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

/// Traverse an expression.
pub trait Traverse<'a, I> {
    /// The expression contained within the [`WithInfo`].
    type Inner;

    /// Call `f` for every subexpression in `self`.
    fn traverse(&self, f: &mut dyn FnMut(WithInfo<I, &'a Self::Inner>));
}

impl<'a, D: Driver> Traverse<'a, D::Info> for WithInfo<D::Info, &'a TypedExpression<D>> {
    type Inner = TypedExpression<D>;

    fn traverse(&self, f: &mut dyn FnMut(WithInfo<D::Info, &'a Self::Inner>)) {
        f(self.clone());

        match &self.item.kind {
            TypedExpressionKind::Block(statements) => {
                for statement in statements {
                    statement.as_ref().traverse(f);
                }
            }
            TypedExpressionKind::Do(block) => {
                block.as_deref().traverse(f);
            }
            TypedExpressionKind::Function { body, .. } => {
                body.as_deref().traverse(f);
            }
            TypedExpressionKind::Call { function, inputs } => {
                function.as_deref().traverse(f);

                for input in inputs {
                    input.as_ref().traverse(f);
                }
            }
            TypedExpressionKind::When { input, arms } => {
                input.as_deref().traverse(f);

                for arm in arms {
                    arm.item.body.as_ref().traverse(f);
                }
            }
            TypedExpressionKind::Intrinsic { inputs, .. } => {
                for input in inputs {
                    input.as_ref().traverse(f);
                }
            }
            TypedExpressionKind::Initialize { value, .. } => {
                value.as_deref().traverse(f);
            }
            TypedExpressionKind::Mutate { value, .. } => {
                value.as_deref().traverse(f);
            }
            TypedExpressionKind::Structure { fields, .. } => {
                for field in fields {
                    field.item.value.as_ref().traverse(f);
                }
            }
            TypedExpressionKind::Variant { values, .. } => {
                for value in values {
                    value.as_ref().traverse(f);
                }
            }
            TypedExpressionKind::Wrapper(value) => {
                value.as_deref().traverse(f);
            }
            TypedExpressionKind::Tuple(elements) => {
                for element in elements {
                    element.as_ref().traverse(f);
                }
            }
            TypedExpressionKind::Format { segments, .. } => {
                for segment in segments {
                    segment.value.as_ref().traverse(f);
                }
            }
            TypedExpressionKind::Unknown(_)
            | TypedExpressionKind::Variable(_, _)
            | TypedExpressionKind::Constant { .. }
            | TypedExpressionKind::Trait(_)
            | TypedExpressionKind::Marker(_)
            | TypedExpressionKind::Number(_)
            | TypedExpressionKind::Text(_) => {}
        }
    }
}

impl<'a, D: Driver> Traverse<'a, D::Info> for WithInfo<D::Info, &'a Pattern<D>> {
    type Inner = Pattern<D>;

    fn traverse(&self, f: &mut dyn FnMut(WithInfo<D::Info, &'a Self::Inner>)) {
        f(self.clone());

        match &self.item {
            Pattern::Destructure(fields) => {
                for field in fields {
                    field.item.pattern.as_ref().traverse(f);
                }
            }
            Pattern::Variant { value_patterns, .. } => {
                for value_pattern in value_patterns {
                    value_pattern.as_ref().traverse(f);
                }
            }
            Pattern::Wrapper { value_pattern, .. } => {
                value_pattern.as_deref().traverse(f);
            }
            Pattern::Tuple(elements) => {
                for element in elements {
                    element.as_ref().traverse(f);
                }
            }
            Pattern::Or { left, right } => {
                left.as_deref().traverse(f);
                right.as_deref().traverse(f);
            }
            Pattern::Unknown
            | Pattern::Wildcard
            | Pattern::Number(_)
            | Pattern::Text(_)
            | Pattern::Variable(_, _) => {}
        }
    }
}
