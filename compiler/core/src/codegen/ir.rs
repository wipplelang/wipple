use crate::{
    codegen::CodegenError,
    db::Node,
    span::{Span, Str},
};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Default)]
pub struct Program {
    pub source_files: Vec<Node>,
    pub definitions: BTreeMap<DefinitionKey, Function>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DefinitionKey {
    TopLevel,
    Constant(ConstantDefinitionKey),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ConstantDefinitionKey {
    pub node: Node,
    pub bounds: BTreeMap<Vec<Node>, Instance>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Named {
        definition: Node,
        parameters: Vec<Type>,
    },
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
    Parameter(Node),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeRepresentation {
    Intrinsic { representation: Option<Str> },
    Marker,
    Structure(Vec<Type>),
    Enumeration(Vec<Vec<Type>>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Instance {
    Bound(Vec<Node>),
    Definition(ConstantDefinitionKey),
}

#[derive(Debug, Clone)]
pub enum Instruction {
    If {
        node: Option<Node>,
        branches: Vec<(Vec<Condition>, Vec<Instruction>, Option<Node>)>,
        else_branch: Option<(Vec<Instruction>, Option<Node>)>,
    },
    Loop {
        node: Node,
        body: Vec<Instruction>,
        result: Node,
    },
    Return {
        value: Node,
    },
    Trace {
        span: Span,
    },
    Value {
        node: Node,
        value: Value,
    },
}

#[derive(Debug, Clone)]
pub enum Value {
    Bound(Vec<Node>),
    Call {
        function: Node,
        inputs: Vec<Node>,
    },
    Constant(DefinitionKey),
    Function(Function),
    Field {
        input: Node,
        field_name: String,
        field_index: usize,
    },
    Tuple(Vec<Node>),
    Marker,
    MutableVariable(Node),
    Number(String),
    Runtime {
        name: String,
        inputs: Vec<Node>,
    },
    String(String),
    Structure(Vec<(String, Node)>),
    TupleElement {
        input: Node,
        index: usize,
    },
    Variable(Node),
    Variant {
        name: String,
        index: usize,
        elements: Vec<Node>,
    },
    VariantElement {
        input: Node,
        variant_name: String,
        variant_index: usize,
        element: usize,
    },
}

#[derive(Debug, Clone, Default)]
pub struct Function {
    pub inputs: Vec<Node>,
    pub instructions: Vec<Instruction>,
    pub closure: Option<(Node, Vec<Node>)>,
}

#[derive(Debug, Clone)]
pub enum Condition {
    Or(Vec<Vec<Condition>>),
    EqualToNumber {
        input: Node,
        value: String,
    },
    EqualToString {
        input: Node,
        value: String,
    },
    EqualToVariant {
        input: Node,
        variant_name: String,
        variant_index: usize,
    },
    Initialize {
        variable: Node,
        node: Option<Node>,
        value: Value,
        mutable: bool,
    },
    Mutate {
        input: Node,
        variable: Node,
    },
}

impl Instruction {
    pub fn primary_node(&self) -> Option<Node> {
        match *self {
            Instruction::If { node, .. } => node,
            Instruction::Return { value } => Some(value),
            Instruction::Loop { node, .. } => Some(node),
            Instruction::Trace { .. } => None,
            Instruction::Value { node, .. } => Some(node),
        }
    }

    pub fn for_each_node(
        &mut self,
        traverse_functions: bool,
        f: &mut dyn FnMut(&mut Node) -> Result<(), CodegenError>,
    ) -> Result<(), CodegenError> {
        match self {
            Instruction::If {
                node,
                branches,
                else_branch,
            } => {
                if let Some(node) = node {
                    f(node)?;
                }

                for (conditions, instructions, then_node) in branches {
                    for condition in conditions {
                        for node in condition.nodes_mut() {
                            f(node)?;
                        }
                    }

                    for instruction in instructions {
                        instruction.for_each_node(traverse_functions, f)?;
                    }

                    if let Some(node) = then_node {
                        f(node)?;
                    }
                }

                if let Some((instructions, else_node)) = else_branch {
                    for instruction in instructions {
                        instruction.for_each_node(traverse_functions, f)?;
                    }

                    if let Some(node) = else_node {
                        f(node)?;
                    }
                }
            }
            Instruction::Return { value } => f(value)?,
            Instruction::Loop { node, body, result } => {
                f(node)?;
                for instruction in body {
                    instruction.for_each_node(traverse_functions, f)?;
                }
                f(result)?;
            }
            Instruction::Trace { .. } => {}
            Instruction::Value { node, value } => {
                f(node)?;

                if traverse_functions && let Value::Function(function) = value {
                    for input in &mut function.inputs {
                        f(input)?;
                    }

                    for instruction in &mut function.instructions {
                        instruction.for_each_node(true, f)?;
                    }
                }
            }
        }

        Ok(())
    }

    pub fn traverse_mut(
        &mut self,
        f: &mut dyn FnMut(&mut Self) -> Result<(), CodegenError>,
    ) -> Result<(), CodegenError> {
        f(self)?;

        match self {
            Instruction::If {
                branches,
                else_branch,
                ..
            } => {
                for (_, instructions, _) in branches {
                    for instruction in instructions {
                        instruction.traverse_mut(f)?;
                    }
                }

                if let Some((instructions, _)) = else_branch {
                    for instruction in instructions {
                        instruction.traverse_mut(f)?;
                    }
                }
            }
            Instruction::Return { .. } => {}
            Instruction::Loop { body, .. } => {
                for instruction in body {
                    instruction.traverse_mut(f)?;
                }
            }
            Instruction::Trace { .. } => {}
            Instruction::Value { value, .. } => {
                if let Value::Function(function) = value {
                    for instruction in &mut function.instructions {
                        instruction.traverse_mut(f)?;
                    }
                }
            }
        }

        Ok(())
    }
}

pub fn traverse_instructions(
    instructions: &mut Vec<Instruction>,
    f: &mut dyn FnMut(&mut Vec<Instruction>) -> Result<(), CodegenError>,
) -> Result<(), CodegenError> {
    f(instructions)?;

    for instruction in instructions {
        match instruction {
            Instruction::If {
                branches,
                else_branch,
                ..
            } => {
                for (_, instructions, _) in branches {
                    traverse_instructions(instructions, f)?;
                }

                if let Some((instructions, _)) = else_branch {
                    traverse_instructions(instructions, f)?;
                }
            }
            Instruction::Return { .. } => {}
            Instruction::Loop { body, .. } => {
                traverse_instructions(body, f)?;
            }
            Instruction::Trace { .. } => {}
            Instruction::Value { value, .. } => {
                if let Value::Function(function) = value {
                    traverse_instructions(&mut function.instructions, f)?;
                }
            }
        }
    }

    Ok(())
}

impl Condition {
    pub fn primary_node(&self) -> Option<Node> {
        match *self {
            Condition::Or(_) => None,
            Condition::EqualToNumber { input, .. } => Some(input),
            Condition::EqualToString { input, .. } => Some(input),
            Condition::EqualToVariant { input, .. } => Some(input),
            Condition::Initialize { variable, .. } => Some(variable),
            Condition::Mutate { input, .. } => Some(input),
        }
    }

    pub fn nodes_mut(&mut self) -> Vec<&mut Node> {
        match self {
            Condition::Or(conditions) => conditions
                .iter_mut()
                .flatten()
                .flat_map(|condition| condition.nodes_mut())
                .collect(),
            Condition::EqualToNumber { input, .. } => vec![input],
            Condition::EqualToString { input, .. } => vec![input],
            Condition::EqualToVariant { input, .. } => vec![input],
            Condition::Initialize { variable, .. } => vec![variable],
            Condition::Mutate { input, variable } => vec![input, variable],
        }
    }
}
