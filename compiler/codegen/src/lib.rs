//! Compile a typechecked item to IR/bytecode.

mod compile;
mod tail_call;

use derivative::Derivative;
use serde::{Deserialize, Serialize};
use wipple_util::WithInfo;

/// Provides the code generation with information about the program.
pub trait Driver: wipple_typecheck::Driver {
    /// The name of the variant representing `True`.
    fn true_variant(&self) -> Option<Self::Path>;

    /// The name of the intrinsic that compares numbers.
    fn number_equality_intrinsic(&self) -> Option<String>;

    /// The name of the intrinsic that compares text.
    fn text_equality_intrinsic(&self) -> Option<String>;
}

/// Generate IR from an expression. This function must only be called if
/// typechecking the item produced no errors.
pub fn compile<D: Driver>(
    driver: &D,
    expression: WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>,
) -> Option<Result<D>> {
    let mut labels = compile::compile(driver, expression)?;

    for instructions in &mut labels {
        tail_call::apply(instructions);
    }

    Some(Result { labels })
}

/// The result of [`compile`].
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct Result<D: Driver> {
    /// The compiled instructions, grouped by label.
    pub labels: Vec<Vec<Instruction<D>>>,
}

/// A label to jump to (ie. an index of [`Result::labels`]).
pub type Label = usize;

/// An instruction.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub enum Instruction<D: Driver> {
    /// (Stack management) Discard the top of the stack.
    Drop,

    /// (Pattern matching) Initialize the top of the stack to the next variable,
    /// keeping the value on the top of the stack.
    Initialize(u32),

    /// (Pattern matching) Retrieve a field of the structure value on the top of
    /// the stack, keeping the value on the top of the stack.
    Field(String),

    /// (Pattern matching) Retrieve the _n_th element of the tuple or variant
    /// value on the top of the stack, keeping the value on the top of the stack.
    Element(u32),

    /// A variable.
    Variable(u32),

    /// A constant.
    Constant(D::Path),

    /// An instance of a trait.
    Instance(D::Path),

    /// (Consuming) Call the function on the top of the stack with the input on
    /// the top of the stack.
    Call,

    /// (Consuming) Evaluate the lazy value on the top of the stack.
    Evaluate,

    /// (Consuming) An intrinsic provided by the runtime with _n_ inputs.
    Intrinsic(String, u32),

    /// (Consuming) String interpolation.
    Format(Vec<String>, String),

    /// (Values) A text value.
    Text(String),

    /// (Values) A number value.
    Number(String),

    /// (Values) A tuple.
    Tuple(u32),

    /// (Values) Produce a new value with a runtime type.
    Typed(TypeDescriptor<D>, TypedInstruction<D>),

    /// (Control flow) Go to another label if the variant on the top of the
    /// stack does not match the variant in the condition.
    JumpIfNot(D::Path, Label),

    /// (Control flow) Return the top of the stack as the result of this
    /// function.
    Return,

    /// (Control flow) Go to another label.
    Jump(Label),

    /// (Control flow) Call the function on the top of the stack, replacing the
    /// current stack.
    TailCall,

    /// (Control flow) The program should never reach this instruction; if it
    /// does, there is a compiler bug.
    Unreachable,
}

/// An instruction that produces a value with a runtime type.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub enum TypedInstruction<D: Driver> {
    /// A value of a marker type.
    Marker,

    /// Create a structure, mapping each field to the next value on the stack.
    Structure(Vec<String>),

    /// Create a variant from the top _n_ values on the stack.
    Variant(D::Path, u32),

    /// A function that can potentially capture variables.
    Function(Vec<u32>, Label),

    /// A lazily-evaluated expression.
    Lazy(Vec<u32>, Label),
}

/// Used when finding a suitable instance for a trait at runtime.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub enum TypeDescriptor<D: Driver> {
    /// A type parameter. This will only occur in the type descriptor for a
    /// generic item, never in a value.
    Parameter(D::Path),

    /// A named marker type, structure, or enumeration.
    Named(D::Path, Vec<TypeDescriptor<D>>),

    /// A function type.
    Function(Box<TypeDescriptor<D>>, Box<TypeDescriptor<D>>),

    /// A tuple type.
    Tuple(Vec<TypeDescriptor<D>>),

    /// The type of a lazy value.
    Lazy(Box<TypeDescriptor<D>>),
}

impl<D: Driver> std::fmt::Display for Instruction<D>
where
    D::Path: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Drop => write!(f, "drop"),
            Instruction::Initialize(variable) => write!(f, "initialize {variable}"),
            Instruction::Field(field) => write!(f, "field {field}"),
            Instruction::Element(element) => write!(f, "element {element}"),
            Instruction::Variable(variable) => write!(f, "variable {variable}"),
            Instruction::Constant(path) => write!(f, "constant {path}"),
            Instruction::Instance(path) => write!(f, "instance {path}"),
            Instruction::Call => write!(f, "call"),
            Instruction::Evaluate => write!(f, "evaluate"),
            Instruction::Intrinsic(name, inputs) => write!(f, "intrinsic {name} {inputs}"),
            Instruction::Format(segments, trailing) => write!(
                f,
                "format{} {trailing:?}",
                segments.iter().fold(String::new(), |mut result, segment| {
                    use std::fmt::Write;
                    write!(&mut result, " {segment:?}").unwrap();
                    result
                }),
            ),
            Instruction::Text(text) => write!(f, "text {text:?}"),
            Instruction::Number(number) => write!(f, "number {number:?}"),
            Instruction::Tuple(elements) => write!(f, "tuple {elements}"),
            Instruction::Typed(descriptor, instruction) => {
                write!(f, "{instruction} :: {descriptor}")
            }
            Instruction::JumpIfNot(variant, label) => {
                write!(f, "jump if not {variant:?} {label}")
            }
            Instruction::Return => write!(f, "return"),
            Instruction::Jump(label) => write!(f, "jump {label}"),
            Instruction::TailCall => write!(f, "tail call"),
            Instruction::Unreachable => write!(f, "unreachable"),
        }
    }
}

impl<D: Driver> std::fmt::Display for TypedInstruction<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedInstruction::Marker => write!(f, "marker"),
            TypedInstruction::Structure(fields) => write!(f, "structure {fields:?}"),
            TypedInstruction::Variant(variant, elements) => {
                write!(f, "variant {variant:?} {elements}")
            }
            TypedInstruction::Function(captures, label) => {
                write!(f, "function {captures:?} {label}")
            }
            TypedInstruction::Lazy(captures, label) => write!(f, "lazy {captures:?} {label}"),
        }
    }
}

impl<D: Driver> std::fmt::Display for TypeDescriptor<D>
where
    D::Path: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeDescriptor::Parameter(parameter) => write!(f, "{parameter}"),
            TypeDescriptor::Named(path, parameters) => write!(
                f,
                "({path} {})",
                parameters
                    .iter()
                    .fold(String::new(), |mut result, parameter| {
                        use std::fmt::Write;
                        write!(&mut result, " {parameter:?}").unwrap();
                        result
                    }),
            ),
            TypeDescriptor::Function(input, output) => write!(f, "({input} -> {output})"),
            TypeDescriptor::Tuple(elements) => write!(
                f,
                "({})",
                elements
                    .iter()
                    .map(|element| element.to_string())
                    .collect::<Vec<_>>()
                    .join(" , "),
            ),
            TypeDescriptor::Lazy(r#type) => write!(f, "(lazy {type})"),
        }
    }
}
