//! Compiler pass that determines the type of every expression in the program
//! and resolves constants and traits to concrete items.

mod debug;
pub mod exhaustiveness;
mod infer;
mod items;
mod utils;

use crate::{lower::Path, syntax::Location, util::WithInfo};
use serde::Serialize;
use std::{collections::HashMap, fmt::Debug, hash::Hash};

/// Provides the typechecker with information about the program.
pub trait Driver {
    /// The recursion limit.
    fn recursion_limit(&self) -> u32;

    /// The information attached to top-level code.
    fn top_level_info(&self) -> Location;

    /// Retrieve the path of a type annotated with `[language]`.
    fn path_for_language_type(&self, language_item: &str) -> Option<Path>;

    /// Retrieve the path of a trait annotated with `[language]`.
    fn path_for_language_trait(&self, language_item: &str) -> Option<Path>;

    /// Retrieve the path of a constructor annotated with `[language]`.
    fn path_for_language_constructor(&self, language_item: &str) -> Option<Path>;

    /// Retrieve the path of a constant annotated with `[language]`.
    fn path_for_language_constant(&self, language_item: &str) -> Option<Path>;

    /// Check if two paths are equal.
    fn paths_are_equal(&self, left: &Path, right: &Path) -> bool;

    /// Retrieve the type declaration at the given path.
    fn get_type_declaration(&self, path: &Path) -> WithInfo<TypeDeclaration>;

    /// Retrieve the trait declaration at the given path.
    fn get_trait_declaration(&self, path: &Path) -> WithInfo<TraitDeclaration>;

    /// Retrieve the type parameter declaration at the given path.
    fn get_type_parameter_declaration(&self, path: &Path) -> WithInfo<TypeParameterDeclaration>;

    /// Retrieve the constant declaration at the given path.
    fn get_constant_declaration(&self, path: &Path) -> WithInfo<ConstantDeclaration>;

    /// Retrieve the instance declaration at the given path.
    fn get_instance_declaration(&self, path: &Path) -> WithInfo<InstanceDeclaration>;

    /// Retrieve the instances available for the trait at the given path.
    fn get_instances_for_trait(&self, r#trait: &Path) -> Vec<Path>;

    /// Retrieve the enumeration for the variant at the given path.
    fn get_enumeration_for_variant(&self, variant: &Path) -> Path;
}

/// Internal representation of constants, instances, and top-level code for
/// typechecking.
pub struct ItemDeclaration(items::ItemDeclarationInner);

/// Internal trait for converting constants, instances, and top-level code into
/// the same representation for typechecking.
pub trait IntoItemDeclaration {
    /// Convert the value into the representation used by the typechecker.
    fn into_item_declaration(
        self,
        driver: &dyn Driver,
        errors: &mut Vec<WithInfo<crate::typecheck::Diagnostic>>,
    ) -> WithInfo<Option<ItemDeclaration>>;
}

/// Resolve a constant, instance, or the top level.
pub fn resolve(
    driver: &dyn Driver,
    item_declaration: impl IntoItemDeclaration,
) -> crate::typecheck::Result {
    infer::resolve(driver, item_declaration)
}

/// The result of [`resolve`].
#[derive(Debug)]
pub struct Result {
    /// The resolved item.
    pub item: Option<WithInfo<TypedExpression>>,

    /// The list of variables the item captures.
    pub captures: Vec<Path>,

    /// Any errors encountered while resolving the item.
    pub diagnostics: Vec<WithInfo<Diagnostic>>,
}

/// Check that none of the provided instances overlap.
pub fn instances_overlap(
    driver: &dyn Driver,
    r#trait: &Path,
    instances: Vec<Path>,
) -> Vec<WithInfo<Diagnostic>> {
    utils::instances_overlap(driver, r#trait, instances)
}

/// Check for inexhaustive bindings and `when` expressions.
pub fn check_exhaustiveness(
    driver: &dyn Driver,
    expression: WithInfo<&TypedExpression>,
) -> Vec<WithInfo<Diagnostic>> {
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
pub fn resolve_trait_type_from_instance(
    driver: &dyn Driver,
    instance: WithInfo<&crate::typecheck::Instance>,
) -> Option<WithInfo<crate::typecheck::Type>> {
    utils::resolve_trait_type_from_instance(driver, instance)
}

/// Substitute the default types for type parameters mentioned in `r#type`.
pub fn substitute_defaults(driver: &dyn Driver, r#type: WithInfo<&mut crate::typecheck::Type>) {
    utils::substitute_defaults_in_parameters(driver, r#type)
}

/// Resolve an attribute-like trait, where the first parameter is the provided
/// type and the remaining parameters are returned.
pub fn resolve_attribute_like_trait(
    driver: &dyn Driver,
    language_item: &str,
    r#type: WithInfo<&Type>,
    number_of_parameters: u32,
) -> Option<Vec<WithInfo<Type>>> {
    utils::resolve_attribute_like_trait(driver, language_item, r#type, number_of_parameters)
}

/// An error occurring during typechecking.
#[derive(Debug, Hash)]
pub enum Diagnostic {
    /// The typechecker hit the recursion limit while attempting to resolve the
    /// expression.
    RecursionLimit,

    /// The driver failed to provide a path for a required language item.
    MissingLanguageItem(String),

    /// The type of the expression could not be determined.
    UnknownType(Type),

    /// The expression refers to a type parameter not declared in the parameter
    /// list.
    UndeclaredTypeParameter(Path),

    /// There was a mismatch between the inferred type of the expression and the
    /// type required in its place.
    Mismatch {
        /// The inferred type of the expression.
        actual: WithInfo<Type>,

        /// The expected type of the expression.
        expected: WithInfo<Type>,
    },

    /// A function is missing an input.
    MissingInputs(Vec<WithInfo<Type>>),

    /// An extra input was provided to a function.
    ExtraInput,

    /// No instance satisfying the provided parameters could be resolved.
    UnresolvedInstance {
        /// The instance that couldn't be resolved.
        instance: Instance,

        /// If the instance could not be resolved because multiple candidates
        /// applied, they will be listed here.
        candidates: Vec<Location>,

        /// Contains the list of instances evaluated before failing to resolve
        /// [`ErrorKind::UnresolvedInstance::trait`].
        stack: Vec<WithInfo<Instance>>,
    },

    /// A trait that doesn't have a value was used in expression position.
    TraitHasNoValue(Path),

    /// An instance is missing a value.
    ExpectedInstanceValue,

    /// An instance has a value, but the trait doesn't declare one.
    UnexpectedInstanceValue,

    /// A structure expression was used, but the expression does not have a
    /// structure type.
    NotAStructure(WithInfo<Type>),

    /// A structure expression was missing fields.
    MissingFields(Vec<String>),

    /// A structure expression contained an extra field.
    ExtraField,

    /// Two instances overlap.
    OverlappingInstances {
        /// The instance that overlaps with [`Error::OverlappingInstances::other`].
        instance: Path,

        /// The instance that overlaps with [`Error::OverlappingInstances::instance`].
        other: Path,
    },

    /// The binding or `when` expression does not exhaustively match its input.
    MissingPatterns(Vec<exhaustiveness::Pattern>),

    /// This pattern is already handled by a previous pattern.
    ExtraPattern,

    /// A custom error message.
    Custom {
        /// The error message.
        message: CustomMessage,

        /// A description of the error.
        description: Option<CustomMessage>,
    },
}

/// The type-level text for an error message or fix.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CustomMessage {
    /// The segments of text that end in interpolated types.
    pub segments: Vec<MessageTypeFormatSegment>,

    /// Any trailing text after the segments.
    pub trailing: String,
}

/// The type of an expression.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum Type {
    /// A type to be inferred or that could not be resolved.
    Unknown,

    /// A type parameter.
    Parameter(Path),

    /// A declared type.
    Declared {
        /// The path to the type declaration.
        path: Path,

        /// The type's parameters.
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
        segments: Vec<MessageTypeFormatSegment>,

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

/// A segment in a message type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct MessageTypeFormatSegment {
    /// The text before the interpolated type.
    pub text: String,

    /// The type to insert after the text.
    pub r#type: WithInfo<Type>,
}

/// An instance or bound.
#[derive(Debug, Clone, Hash, Serialize)]
pub struct Instance {
    /// The trait for which an instance must exist.
    pub r#trait: Path,

    /// The parameters used to search for an instance.
    pub parameters: Vec<WithInfo<Type>>,
}

/// An attribute.
#[derive(Debug, Clone, Serialize)]
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
#[derive(Debug, Clone, Serialize)]
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

/// A syntax declaration.
#[derive(Debug, Clone, Serialize)]
pub struct SyntaxDeclaration {
    /// The syntax's attributes.
    pub attributes: Vec<WithInfo<Attribute>>,
}

/// A type declaration.
#[derive(Debug, Clone, Serialize)]
pub struct TypeDeclaration {
    /// The type's attributes.
    pub attributes: Vec<WithInfo<Attribute>>,

    /// The type's parameters.
    pub parameters: Vec<Path>,

    /// The type's representation (opaque, structure or enumeration).
    pub representation: WithInfo<TypeRepresentation>,
}

/// A trait declaration.
#[derive(Debug, Clone, Serialize)]
pub struct TraitDeclaration {
    /// The trait's attributes.
    pub attributes: Vec<WithInfo<Attribute>>,

    /// The trait's parameters.
    pub parameters: Vec<Path>,

    /// The trait's type.
    pub r#type: Option<WithInfo<Type>>,
}

/// A type parameter declaration.
#[derive(Debug, Clone, Serialize)]
pub struct TypeParameterDeclaration {
    /// Whether the parameter is marked `infer`.
    pub infer: Option<WithInfo<()>>,

    /// The parameter's default type.
    pub default: Option<WithInfo<Type>>,
}

/// The representation of a [`TypeDeclaration`].
#[derive(Debug, Clone, Serialize)]
pub enum TypeRepresentation {
    /// The type has a single value.
    Marker,

    /// The type contains an ordered list of fields.
    Structure(HashMap<String, WithInfo<StructureField>>),

    /// The type can be one of several variants.
    Enumeration(HashMap<Path, WithInfo<EnumerationVariant>>),

    /// The type wraps another type.
    Wrapper(WithInfo<Type>),
}

/// A single field in a type represented as a structure.
#[derive(Debug, Clone, Serialize)]
pub struct StructureField {
    /// The index of the field.
    pub index: u32,

    /// The field's attributes.
    pub attributes: Vec<WithInfo<Attribute>>,

    /// The type of the field's value.
    pub r#type: WithInfo<Type>,
}

/// A single variant in a type represented as an enumeration.
#[derive(Debug, Clone, Serialize)]
pub struct EnumerationVariant {
    /// The index of the variant.
    pub index: u32,

    /// The variant's attributes.
    pub attributes: Vec<WithInfo<Attribute>>,

    /// The types of the variant's associated values.
    pub value_types: Vec<WithInfo<Type>>,
}

/// A constant declaration.
#[derive(Debug, Clone, Serialize)]
pub struct ConstantDeclaration {
    /// The constant's attributes.
    pub attributes: Vec<WithInfo<Attribute>>,

    /// The constant's parameters.
    pub parameters: Vec<Path>,

    /// The constant's bounds.
    pub bounds: Vec<WithInfo<Instance>>,

    /// The constant's declared type.
    pub r#type: WithInfo<Type>,
}

/// An instance declaration.
#[derive(Debug, Clone, Serialize)]
pub struct InstanceDeclaration {
    /// The instance's parameters.
    pub parameters: Vec<Path>,

    /// The instance's bounds.
    pub bounds: Vec<WithInfo<Instance>>,

    /// The parameters corresponding to the instance's trait.
    pub instance: WithInfo<Instance>,

    /// Whether the instance is the default instance.
    pub default: bool,
}

/// An untyped item.
#[derive(Debug, Clone)]
pub struct UntypedItem {
    /// The item's body.
    pub body: WithInfo<UntypedExpression>,

    /// The list of variables the item captures.
    pub captures: Vec<Path>,
}

/// Untyped top-level code.
#[derive(Debug, Clone)]
pub struct UntypedTopLevelCode {
    /// The code to run.
    pub statements: Vec<WithInfo<UntypedExpression>>,
}

/// An untyped expression.
#[derive(Debug, Clone)]
pub enum UntypedExpression {
    /// An expression that could not be resolved prior to typechecking.
    Unknown,

    /// An expression explicitly annotated with type.
    Annotate {
        /// The inner value.
        value: WithInfo<Box<UntypedExpression>>,

        /// The type of the inner value.
        r#type: WithInfo<Type>,
    },

    /// The value of a variable.
    Variable(String, Path),

    /// The value of a constant.
    Constant(Path),

    /// An instance of a trait.
    Trait(Path),

    /// A number literal.
    Number(String),

    /// A text literal.
    Text(String),

    /// A block.
    Block {
        /// The block's statements.
        statements: Vec<WithInfo<UntypedExpression>>,

        /// Whether the block represents a top level.
        top_level: bool,

        /// The list of variables the block captures.
        captures: Vec<Path>,
    },

    /// Evaluate a block.
    Do(WithInfo<Box<UntypedExpression>>),

    /// A function.
    Function {
        /// The function's inputs.
        inputs: Vec<WithInfo<Pattern>>,

        /// The function's output.
        body: WithInfo<Box<UntypedExpression>>,

        /// The list of variables the function captures.
        captures: Vec<Path>,
    },

    /// A function call.
    Call {
        /// The function to call.
        function: WithInfo<Box<UntypedExpression>>,

        /// The inputs to the function.
        inputs: Vec<WithInfo<UntypedExpression>>,
    },

    /// A `when` expression.
    When {
        /// The value to match.
        input: WithInfo<Box<UntypedExpression>>,

        /// The arms to execute.
        arms: Vec<WithInfo<UntypedArm>>,
    },

    /// An `intrinsic` expression.
    Intrinsic {
        /// The name of the compiler intrinsic to call.
        name: String,

        /// The inputs to the intrinsic.
        inputs: Vec<WithInfo<UntypedExpression>>,
    },

    /// Bind a value to a pattern.
    Initialize {
        /// The pattern to match.
        pattern: WithInfo<Pattern>,

        /// The value to match.
        value: WithInfo<Box<UntypedExpression>>,
    },

    /// Change the value of an existing variable.
    Mutate {
        /// The name of the variable.
        name: String,

        /// The path to the variable.
        path: WithInfo<Path>,

        /// The value to assign to the variable.
        value: WithInfo<Box<UntypedExpression>>,
    },

    /// Create a marker value.
    Marker(Path),

    /// Create a structure value.
    Structure(Vec<WithInfo<UntypedStructureFieldValue>>),

    /// Create a variant of an enumeration.
    Variant {
        /// The variant this value creates.
        variant: WithInfo<Path>,

        /// The variant's associated values.
        values: Vec<WithInfo<UntypedExpression>>,
    },

    /// Create a wrapper value.
    Wrapper {
        /// The wrapper type.
        r#type: Path,

        /// The type's value.
        value: WithInfo<Box<UntypedExpression>>,
    },

    /// A tuple expression.
    Tuple(Vec<WithInfo<UntypedExpression>>),

    /// Collection construction.
    Collection(Vec<WithInfo<UntypedExpression>>),

    /// String interpolation.
    Format {
        /// The segments of text that end in interpolated values.
        segments: Vec<UntypedFormatSegment>,

        /// Any trailing text after the segments.
        trailing: String,
    },
}

/// A segment in a string interpolation expression.
#[derive(Debug, Clone)]
pub struct UntypedFormatSegment {
    /// The text before the interpolated value.
    pub text: String,

    /// The value to insert after the text.
    pub value: WithInfo<UntypedExpression>,
}

/// The value of a structure field.
#[derive(Debug, Clone)]
pub struct UntypedStructureFieldValue {
    /// The name of the field.
    pub name: String,

    /// The field's value.
    pub value: WithInfo<UntypedExpression>,
}

/// An arm in a `when` expression.
#[derive(Debug, Clone)]
pub struct UntypedArm {
    /// The pattern to match on the input.
    pub pattern: WithInfo<Pattern>,

    /// The arm's body.
    pub body: WithInfo<UntypedExpression>,
}

/// A typed expression.
#[derive(Debug, Clone)]
pub struct TypedExpression {
    /// The type of the expression.
    pub r#type: Type,

    /// The kind of expression.
    pub kind: TypedExpressionKind,
}

/// The kind of [`TypedExpression`].
#[derive(Debug, Clone)]
pub enum TypedExpressionKind {
    /// An expression that could not be resolved, along with the original
    /// expression if available.
    Unknown(Option<Box<TypedExpressionKind>>),

    /// The value of a variable.
    Variable(String, Path),

    /// A constant.
    Constant {
        /// The path to the constant declaration.
        path: Path,

        /// The types of the constant's parameters. This is used in case the
        /// constant's type doesn't reference all type parameters.
        substitutions: HashMap<Path, Type>,

        /// The resolved bounds.
        bounds: Vec<WithInfo<std::result::Result<Path, Instance>>>,
    },

    /// A trait.
    Trait {
        /// The path to the trait declaration.
        path: Path,

        /// The types of the instance's parameters. This is used in case the
        /// instance's type doesn't reference all type parameters.
        substitutions: HashMap<Path, Type>,

        /// The path to the resolved instance, or a bound.
        instance: WithInfo<std::result::Result<Path, Instance>>,
    },

    /// A number literal.
    Number(String),

    /// A text literal.
    Text(String),

    /// A block.
    Block {
        /// The block's statements.
        statements: Vec<WithInfo<TypedExpression>>,

        /// The list of variables the block captures.
        captures: Vec<Path>,
    },

    /// Evaluate a block.
    Do(WithInfo<Box<TypedExpression>>),

    /// A function.
    Function {
        /// The function's inputs,
        inputs: Vec<WithInfo<Pattern>>,

        /// The function's output.
        body: WithInfo<Box<TypedExpression>>,

        /// The list of variables the function captures.
        captures: Vec<Path>,
    },

    /// A function call.
    Call {
        /// The function to call.
        function: WithInfo<Box<TypedExpression>>,

        /// The inputs to the function.
        inputs: Vec<WithInfo<TypedExpression>>,
    },

    /// A `when` expression.
    When {
        /// The value to match.
        input: WithInfo<Box<TypedExpression>>,

        /// The arms to execute.
        arms: Vec<WithInfo<TypedArm>>,
    },

    /// An `intrinsic` expression.
    Intrinsic {
        /// The name of the compiler intrinsic to call.
        name: String,

        /// The inputs to the intrinsic.
        inputs: Vec<WithInfo<TypedExpression>>,
    },

    /// Bind a value to a pattern.
    Initialize {
        /// The pattern to match.
        pattern: WithInfo<Pattern>,

        /// The value to match.
        value: WithInfo<Box<TypedExpression>>,
    },

    /// Change the value of an existing variable.
    Mutate {
        /// The name of the variable.
        name: String,

        /// The path to the variable.
        path: WithInfo<Path>,

        /// The value to assign to the variable.
        value: WithInfo<Box<TypedExpression>>,
    },

    /// Create a marker value.
    Marker(Path),

    /// Create a structure value.
    Structure {
        /// The structure this value refers to.
        structure: Path,

        /// The values of the structure's fields.
        fields: Vec<WithInfo<TypedStructureFieldValue>>,
    },

    /// Create a variant of an enumeration.
    Variant {
        /// The variant this value creates.
        variant: WithInfo<Path>,

        /// The variant's associated values.
        values: Vec<WithInfo<TypedExpression>>,
    },

    /// Create a wrapper value.
    Wrapper(WithInfo<Box<TypedExpression>>),

    /// A tuple expression.
    Tuple(Vec<WithInfo<TypedExpression>>),

    /// String interpolation.
    Format {
        /// The segments of text that end in interpolated values.
        segments: Vec<TypedFormatSegment>,

        /// Any trailing text after the segments.
        trailing: String,
    },
}

/// A segment in a string interpolation expression.
#[derive(Debug, Clone)]
pub struct TypedFormatSegment {
    /// The text before the interpolated value.
    pub text: String,

    /// The value to insert after the text.
    pub value: WithInfo<TypedExpression>,
}

/// The value of a structure field.
#[derive(Debug, Clone)]
pub struct TypedStructureFieldValue {
    /// The name of the field.
    pub name: String,

    /// The index of the field.
    pub index: Option<u32>,

    /// The field's value.
    pub value: WithInfo<TypedExpression>,
}

/// An arm in a `when` expression.
#[derive(Debug, Clone)]
pub struct TypedArm {
    /// The pattern to match on the input.
    pub pattern: WithInfo<Pattern>,

    /// The arm's body.
    pub body: WithInfo<TypedExpression>,
}

/// A pattern.
#[derive(Debug, Clone, Hash)]
pub enum Pattern {
    /// A pattern that could not be resolved prior to or after typechecking.
    Unknown,

    /// A pattern that matches anything.
    Wildcard,

    /// A number pattern.
    Number(String),

    /// A text pattern.
    Text(String),

    /// A variable pattern.
    Variable(String, Path),

    /// A destructuring pattern.
    Destructure {
        /// The structure this pattern matches, or `None` if the structure is
        /// not known.
        structure: Option<WithInfo<Path>>,

        /// The patterns matching each of the structure's fields.
        field_patterns: Vec<WithInfo<FieldPattern>>,
    },

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
#[derive(Debug, Clone, Hash)]
pub struct FieldPattern {
    /// The name of the field.
    pub name: String,

    /// The pattern matching the field's value.
    pub pattern: WithInfo<Pattern>,
}

/// Traverse an expression.
pub trait Traverse<'a> {
    /// The expression contained within the [`WithInfo`].
    type Inner;

    /// Call `f` for every subexpression in `self`.
    fn traverse(&self, f: &mut dyn FnMut(WithInfo<&'a Self::Inner>));
}

impl<'a> Traverse<'a> for WithInfo<&'a TypedExpression> {
    type Inner = TypedExpression;

    fn traverse(&self, f: &mut dyn FnMut(WithInfo<&'a Self::Inner>)) {
        f(self.clone());

        match &self.item.kind {
            TypedExpressionKind::Block { statements, .. } => {
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
            | TypedExpressionKind::Trait { .. }
            | TypedExpressionKind::Marker(_)
            | TypedExpressionKind::Number(_)
            | TypedExpressionKind::Text(_) => {}
        }
    }
}

impl<'a> Traverse<'a> for WithInfo<&'a Pattern> {
    type Inner = Pattern;

    fn traverse(&self, f: &mut dyn FnMut(WithInfo<&'a Self::Inner>)) {
        f(self.clone());

        match &self.item {
            Pattern::Destructure { field_patterns, .. } => {
                for field in field_patterns {
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
            Pattern::Annotate { pattern, .. } => {
                pattern.as_deref().traverse(f);
            }
            Pattern::Unknown
            | Pattern::Wildcard
            | Pattern::Number(_)
            | Pattern::Text(_)
            | Pattern::Variable(_, _)
            | Pattern::Marker(_) => {}
        }
    }
}

impl Type {
    /// Check if `self` could potentially unify with `other`. This doesn't
    /// guarantee the two types unify (because of trait bounds), but it's useful
    /// for diagnostics.
    pub fn could_unify_with(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            (Type::Parameter(left), Type::Parameter(right)) => left == right,
            (
                Type::Declared {
                    path: left,
                    parameters: left_parameters,
                },
                Type::Declared {
                    path: right,
                    parameters: right_parameters,
                },
            ) => {
                left == right
                    && left_parameters.len() == right_parameters.len()
                    && left_parameters
                        .iter()
                        .zip(right_parameters)
                        .all(|(left, right)| left.item.could_unify_with(&right.item))
            }
            (
                Type::Function {
                    inputs: left_inputs,
                    output: left_output,
                },
                Type::Function {
                    inputs: right_inputs,
                    output: right_output,
                },
            ) => {
                left_inputs.len() == right_inputs.len()
                    && left_inputs
                        .iter()
                        .zip(right_inputs)
                        .all(|(left, right)| left.item.could_unify_with(&right.item))
                    && left_output.item.could_unify_with(&right_output.item)
            }
            (Type::Tuple(left), Type::Tuple(right)) => {
                left.len() == right.len()
                    && left
                        .iter()
                        .zip(right)
                        .all(|(left, right)| left.item.could_unify_with(&right.item))
            }
            (Type::Block(left), Type::Block(right)) => left.item.could_unify_with(&right.item),
            (Type::Intrinsic, Type::Intrinsic) => true,
            (Type::Message { .. }, Type::Message { .. }) => false,
            (Type::Equal { left, right }, other) | (other, Type::Equal { left, right }) => {
                left.item.could_unify_with(other) && right.item.could_unify_with(other)
            }
            _ => false,
        }
    }
}
