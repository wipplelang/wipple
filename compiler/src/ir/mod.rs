//! Compile a typechecked item to IR/bytecode.

mod compile;

use crate::{lower::Path, util::WithInfo};
use serde::Serialize;
use std::collections::HashMap;
use typeshare::typeshare;

/// Provides the code generation with information about the program.
pub trait Driver: crate::typecheck::Driver {
    /// The path of the type representing `Number`.
    fn number_type(&self) -> Option<Path>;

    /// The path of the type representing `Text`.
    fn text_type(&self) -> Option<Path>;

    /// The path of the type representing `Boolean`.
    fn boolean_type(&self) -> Option<Path>;

    /// The path of the variant representing `True`.
    fn true_variant(&self) -> Option<Path>;

    /// The name of the intrinsic that compares numbers.
    fn number_equality_intrinsic(&self) -> Option<String>;

    /// The name of the intrinsic that compares text.
    fn text_equality_intrinsic(&self) -> Option<String>;

    /// The path of an item contained within the item at the provided path.
    fn item_path_in(&self, path: &Path, index: u32) -> Path;
}

/// Generate IR from an expression. This function must only be called if
/// typechecking the item produced no errors.
pub fn compile<'a>(
    driver: &dyn Driver,
    path: Path,
    attributes: &'a [WithInfo<crate::typecheck::Attribute>],
    expression: WithInfo<&'a crate::typecheck::TypedExpression>,
    captures: &[Path],
) -> Result {
    let items = compile::compile(driver, path, attributes, expression, captures);
    Result { items }
}

/// Generate a type descriptor from a type.
pub fn type_descriptor(r#type: &crate::typecheck::Type) -> Option<crate::ir::TypeDescriptor> {
    compile::type_descriptor(r#type)
}

/// Generate a layout descriptor from a type declaration.
pub fn layout_descriptor(
    type_declaration: &crate::typecheck::TypeDeclaration,
) -> Option<crate::ir::LayoutDescriptor> {
    compile::layout_descriptor(type_declaration)
}

/// Generate a bound descriptor from a bound.
pub fn instance_descriptor(
    bound: &crate::typecheck::Instance,
) -> Option<crate::ir::InstanceDescriptor> {
    compile::instance_descriptor(bound)
}

/// The result of [`compile`].
#[derive(Debug)]
pub struct Result {
    /// The compiled items.
    pub items: HashMap<Path, Item>,
}

/// A compiled item.
#[derive(Debug)]
pub struct Item {
    /// The number of variables this item captures.
    pub captures: u32,

    /// The expression from which this item was compiled.
    pub expression: WithInfo<crate::typecheck::TypedExpression>,

    /// The compiled instructions.
    pub instructions: Vec<crate::ir::Instruction>,
}

/// An instruction.
#[typeshare]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "camelCase", tag = "type", content = "value")]
pub enum Instruction {
    /// (Stack management) Duplicate the top of the stack.
    Copy,

    /// (Stack management) Discard the top of the stack.
    Drop,

    /// (Pattern matching) Initialize the top of the stack to the next variable,
    /// keeping the value on the top of the stack.
    Initialize(u32),

    /// (Pattern matching) Retrieve a field of the structure value on the top of
    /// the stack, keeping the value on the top of the stack.
    Field(u32),

    /// (Pattern matching) Retrieve the _n_th element of the variant on the top
    /// of the stack, keeping the value on the top of the stack.
    VariantElement(u32),

    /// (Pattern matching) Retrieve the _n_th element of the tuple on the top of
    /// the stack, keeping the value on the top of the stack.
    TupleElement(u32),

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

    /// (Consuming) An intrinsic provided by the runtime with _n_ inputs.
    #[serde(rename_all = "camelCase")]
    Intrinsic { name: String, inputs: u32 },

    /// A text value.
    Text(String),

    /// A number value.
    Number(String),

    /// (Consuming) String interpolation.
    #[serde(rename_all = "camelCase")]
    Format {
        segments: Vec<String>,
        trailing: String,
    },

    /// Create a marker value.
    Marker,

    /// Create a structure, mapping each field to the next value on the stack.
    Structure(Vec<u32>),

    /// Create a variant from the top _n_ values on the stack.
    #[serde(rename_all = "camelCase")]
    Variant { variant: u32, elements: u32 },

    /// Create a wrapper value from the top value on the stack.
    Wrapper,

    /// A function (or block) expression that can capture variables.
    #[serde(rename_all = "camelCase")]
    Function { captures: Vec<u32>, path: Path },

    /// A constant with resolved parameters and bounds.
    #[serde(rename_all = "camelCase")]
    Constant {
        path: Path,
        parameters: Vec<TypeDescriptor>,
    },

    /// An instance of a trait.
    #[serde(rename_all = "camelCase")]
    Instance {
        trait_path: Path,
        parameters: Vec<TypeDescriptor>,
    },

    /// (Control flow) Begin a block.
    Block(Vec<Instruction>),

    /// (Control flow) Break out of _n_ blocks.
    Break(u32),

    /// (Control flow) Break out of _n_ blocks if the variant on the top of the
    /// stack does not match the variant in the condition.
    #[serde(rename_all = "camelCase")]
    BreakIfNot { variant: u32, count: u32 },

    /// (Control flow) Return the top of the stack as the result of this
    /// function.
    Return,

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

/// Used when finding a suitable instance for a trait at runtime.
#[typeshare]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "camelCase", tag = "type", content = "value")]
pub enum TypeDescriptor {
    /// A type parameter. This will only occur in the type descriptor for a
    /// generic item, never in a value.
    Parameter(Path),

    /// A named opaque type, structure, or enumeration.
    #[serde(rename_all = "camelCase")]
    Named {
        path: Path,
        parameters: Vec<TypeDescriptor>,
    },

    /// A function type.
    #[serde(rename_all = "camelCase")]
    Function {
        inputs: Vec<TypeDescriptor>,
        output: Box<TypeDescriptor>,
    },

    /// A tuple type.
    Tuple(Vec<TypeDescriptor>),

    /// The type of a block value.
    Block(Box<TypeDescriptor>),

    /// An intrinsic type provided by the runtime.
    Intrinsic,

    /// Two equal types.
    #[serde(rename_all = "camelCase")]
    Equal {
        left: Box<TypeDescriptor>,
        right: Box<TypeDescriptor>,
    },
}

/// Contains layout information for a named type.
#[typeshare]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "camelCase", tag = "type", content = "value")]
pub enum LayoutDescriptor {
    /// A marker.
    Marker,

    /// A wrapper.
    Wrapper(Box<TypeDescriptor>),

    /// A structure.
    Structure(Vec<TypeDescriptor>),

    /// An enumeration.
    Enumeration(Vec<Vec<TypeDescriptor>>),

    /// An intrinsic.
    Intrinsic, // TODO: Remove 'intrinsic' and specify layout with attribute
}

/// Used to store the available traits resolved from bounds at runtime.
#[typeshare]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InstanceDescriptor {
    /// The path to the trait.
    pub trait_path: Path,

    /// The type parameters provided to the trait.
    pub parameters: Vec<TypeDescriptor>,
}

impl std::fmt::Display for Instruction
where
    Path: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display(f, 0)
    }
}

impl Instruction
where
    Path: std::fmt::Display,
{
    fn display(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{}", "  ".repeat(indent))?;

        match self {
            Instruction::Copy => write!(f, "copy"),
            Instruction::Drop => write!(f, "drop"),
            Instruction::Initialize(variable) => write!(f, "initialize {variable}"),
            Instruction::Field(field) => write!(f, "field {field}"),
            Instruction::VariantElement(element) => write!(f, "variant element {element}"),
            Instruction::TupleElement(element) => write!(f, "tuple element {element}"),
            Instruction::Unwrap => write!(f, "unwrap"),
            Instruction::Variable(variable) => write!(f, "variable {variable}"),
            Instruction::Call(inputs) => write!(f, "call {inputs}"),
            Instruction::Do => write!(f, "do"),
            Instruction::Mutate(variable) => write!(f, "mutate {variable}"),
            Instruction::Tuple(elements) => write!(f, "tuple {elements}"),
            Instruction::Intrinsic { name, inputs } => write!(f, "intrinsic {name} {inputs}"),
            Instruction::Text(text) => write!(f, "text {text:?}"),
            Instruction::Number(number) => write!(f, "number {number:?}"),
            Instruction::Format { segments, trailing } => write!(
                f,
                "format{} {trailing:?}",
                segments.iter().fold(String::new(), |mut result, segment| {
                    use std::fmt::Write;
                    write!(&mut result, " {segment:?}").unwrap();
                    result
                }),
            ),
            Instruction::Marker => write!(f, "marker"),
            Instruction::Structure(fields) => write!(f, "structure {fields:?}"),
            Instruction::Variant { variant, elements } => {
                write!(f, "variant {variant:?} {elements}")
            }
            Instruction::Wrapper => write!(f, "wrapper"),
            Instruction::Function { captures, path } => {
                write!(f, "function {captures:?} ({path})")
            }
            Instruction::Constant { path, .. } => write!(f, "constant {path}"),
            Instruction::Instance { trait_path, .. } => write!(f, "instance {trait_path}"),
            Instruction::Block(instructions) => {
                writeln!(f, "block")?;

                for instruction in instructions {
                    instruction.display(f, indent + 1)?;
                    writeln!(f)?;
                }

                write!(f, "{}end", "  ".repeat(indent))
            }
            Instruction::Break(label) => write!(f, "break {label}"),
            Instruction::BreakIfNot {
                variant,
                count: label,
            } => {
                write!(f, "break if not {variant} {label}")
            }
            Instruction::Return => write!(f, "return"),
            Instruction::TailCall(inputs) => write!(f, "tail call {inputs}"),
            Instruction::TailDo => write!(f, "tail do"),
            Instruction::Unreachable => write!(f, "unreachable"),
        }
    }
}

impl std::fmt::Display for TypeDescriptor
where
    Path: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeDescriptor::Parameter(parameter) => write!(f, "{parameter}"),
            TypeDescriptor::Named { path, parameters } => write!(
                f,
                "({path}{})",
                parameters
                    .iter()
                    .fold(String::new(), |mut result, parameter| {
                        use std::fmt::Write;
                        write!(&mut result, " {parameter:?}").unwrap();
                        result
                    }),
            ),
            TypeDescriptor::Function { inputs, output } => write!(
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
            TypeDescriptor::Equal { left, right } => write!(f, "({} = {})", left, right),
        }
    }
}
