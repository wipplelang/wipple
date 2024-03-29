//! Compile a typechecked item to IR/bytecode.

mod compile;
mod tail_call;

use derivative::Derivative;
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use wipple_util::WithInfo;

/// Provides the code generation with information about the program.
pub trait Driver: wipple_typecheck::Driver {
    /// The path of the type representing `Number`.
    fn number_type(&self) -> Option<Self::Path>;

    /// The path of the type representing `Text`.
    fn text_type(&self) -> Option<Self::Path>;

    /// The path of the type representing `Boolean`.
    fn boolean_type(&self) -> Option<Self::Path>;

    /// The path of the variant representing `True`.
    fn true_variant(&self) -> Option<Self::Path>;

    /// The name of the intrinsic that compares numbers.
    fn number_equality_intrinsic(&self) -> Option<String>;

    /// The name of the intrinsic that compares text.
    fn text_equality_intrinsic(&self) -> Option<String>;
}

impl Driver for wipple_util::TsAny {
    fn number_type(&self) -> Option<Self::Path> {
        unimplemented!()
    }

    fn text_type(&self) -> Option<Self::Path> {
        unimplemented!()
    }

    fn boolean_type(&self) -> Option<Self::Path> {
        unimplemented!()
    }

    fn true_variant(&self) -> Option<Self::Path> {
        unimplemented!()
    }

    fn number_equality_intrinsic(&self) -> Option<String> {
        unimplemented!()
    }

    fn text_equality_intrinsic(&self) -> Option<String> {
        unimplemented!()
    }
}

/// Generate IR from an expression. This function must only be called if
/// typechecking the item produced no errors.
pub fn compile<D: Driver>(
    driver: &D,
    path: D::Path,
    expression: WithInfo<D::Info, &wipple_typecheck::TypedExpression<D>>,
) -> Option<Result<D>> {
    let mut labels = compile::compile(driver, path, expression)?;

    for instructions in &mut labels {
        tail_call::apply(instructions);
    }

    Some(Result { labels })
}

/// Generate a type descriptor from a type.
pub fn type_descriptor<D: crate::Driver>(
    r#type: &wipple_typecheck::Type<D>,
) -> Option<crate::TypeDescriptor<D>> {
    compile::type_descriptor(r#type)
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
#[ts(export, rename = "codegen_Instruction", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub enum Instruction<D: Driver> {
    /// (Stack management) Duplicate the top of the stack.
    Copy,

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

    /// (Pattern matching) Unwrap the wrapper type on the top of the stack,
    /// keeping the value on the top of the stack.
    Unwrap,

    /// A variable.
    Variable(u32),

    /// (Consuming) Call the function on the top of the stack with _n_ inputs.
    Call(u32),

    /// (Consuming) Evaluate the block on the top of the stack.
    Do,

    /// (Consuming) Set a variable to the value on the top of the stack.
    Mutate(u32),

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
    TailCall(u32),

    /// (Control flow) Evaluate the block on the top of the stack, replacing the
    /// current stack.
    TailDo,

    /// (Control flow) The program should never reach this instruction; if it
    /// does, there is a compiler bug.
    Unreachable,
}

/// An instruction that produces a value with a runtime type.
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
#[ts(export, rename = "codegen_TypedInstruction", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub enum TypedInstruction<D: Driver> {
    /// (Consuming) An intrinsic provided by the runtime with _n_ inputs.
    Intrinsic(String, u32),

    /// A text value.
    Text(String),

    /// A number value.
    Number(String),

    /// (Consuming) String interpolation.
    Format(Vec<String>, String),

    /// Create a marker value.
    Marker,

    /// Create a structure, mapping each field to the next value on the stack.
    Structure(Vec<String>),

    /// Create a variant from the top _n_ values on the stack.
    Variant(D::Path, u32),

    /// Create a wrapper value from the top value on the stack.
    Wrapper,

    /// A function that can potentially capture variables.
    Function(Vec<u32>, D::Path, Label),

    /// A block expression.
    Block(Vec<u32>, D::Path, Label),

    /// A constant.
    Constant(D::Path, Vec<TypeDescriptor<D>>),

    /// An instance of a trait.
    Instance(D::Path),
}

/// Used when finding a suitable instance for a trait at runtime.
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
#[ts(export, rename = "codegen_TypeDescriptor", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub enum TypeDescriptor<D: Driver> {
    /// A type parameter. This will only occur in the type descriptor for a
    /// generic item, never in a value.
    Parameter(D::Path),

    /// A named opaque type, structure, or enumeration.
    Named(D::Path, Vec<TypeDescriptor<D>>),

    /// A function type.
    Function(Vec<TypeDescriptor<D>>, Box<TypeDescriptor<D>>),

    /// A tuple type.
    Tuple(Vec<TypeDescriptor<D>>),

    /// The type of a block value.
    Block(Box<TypeDescriptor<D>>),

    /// An intrinsic type provided by the runtime.
    Intrinsic,
}

impl<D: Driver> std::fmt::Display for Instruction<D>
where
    D::Path: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Copy => write!(f, "copy"),
            Instruction::Drop => write!(f, "drop"),
            Instruction::Initialize(variable) => write!(f, "initialize {variable}"),
            Instruction::Field(field) => write!(f, "field {field}"),
            Instruction::Element(element) => write!(f, "element {element}"),
            Instruction::Unwrap => write!(f, "unwrap"),
            Instruction::Variable(variable) => write!(f, "variable {variable}"),
            Instruction::Call(inputs) => write!(f, "call {inputs}"),
            Instruction::Do => write!(f, "do"),
            Instruction::Mutate(variable) => write!(f, "mutate {variable}"),
            Instruction::Tuple(elements) => write!(f, "tuple {elements}"),
            Instruction::Typed(descriptor, instruction) => {
                write!(f, "{instruction} :: {descriptor}")
            }
            Instruction::JumpIfNot(variant, label) => {
                write!(f, "jump if not {variant:?} {label}")
            }
            Instruction::Return => write!(f, "return"),
            Instruction::Jump(label) => write!(f, "jump {label}"),
            Instruction::TailCall(inputs) => write!(f, "tail call {inputs}"),
            Instruction::TailDo => write!(f, "tail do"),
            Instruction::Unreachable => write!(f, "unreachable"),
        }
    }
}

impl<D: Driver> std::fmt::Display for TypedInstruction<D>
where
    D::Path: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedInstruction::Intrinsic(name, inputs) => write!(f, "intrinsic {name} {inputs}"),
            TypedInstruction::Text(text) => write!(f, "text {text:?}"),
            TypedInstruction::Number(number) => write!(f, "number {number:?}"),
            TypedInstruction::Format(segments, trailing) => write!(
                f,
                "format{} {trailing:?}",
                segments.iter().fold(String::new(), |mut result, segment| {
                    use std::fmt::Write;
                    write!(&mut result, " {segment:?}").unwrap();
                    result
                }),
            ),
            TypedInstruction::Marker => write!(f, "marker"),
            TypedInstruction::Structure(fields) => write!(f, "structure {fields:?}"),
            TypedInstruction::Variant(variant, elements) => {
                write!(f, "variant {variant:?} {elements}")
            }
            TypedInstruction::Wrapper => write!(f, "wrapper"),
            TypedInstruction::Function(captures, path, label) => {
                write!(f, "function {captures:?} ({path}) {label}")
            }
            TypedInstruction::Block(captures, path, label) => {
                write!(f, "block {captures:?} ({path}) {label}")
            }
            TypedInstruction::Constant(path, _) => write!(f, "constant {path}"),
            TypedInstruction::Instance(path) => write!(f, "instance {path}"),
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
            TypeDescriptor::Function(inputs, output) => write!(
                f,
                "({} -> {})",
                inputs
                    .iter()
                    .map(|input| input.to_string())
                    .collect::<Vec<_>>()
                    .join(" "),
                output
            ),
            TypeDescriptor::Tuple(elements) => write!(
                f,
                "({})",
                elements
                    .iter()
                    .map(|element| element.to_string())
                    .collect::<Vec<_>>()
                    .join(" , "),
            ),
            TypeDescriptor::Block(r#type) => write!(f, "{{{type}}}"),
            TypeDescriptor::Intrinsic => write!(f, "intrinsic"),
        }
    }
}
