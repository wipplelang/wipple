use crate::{
    codegen::CodegenResult,
    database::{NodeRef, Span},
};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Default)]
pub struct Program {
    pub files: Vec<Span>,
    pub top_level: Definition,
    pub definitions: BTreeMap<DefinitionKey, Definition>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct DefinitionKey {
    pub node: NodeRef,
    pub substitutions: BTreeMap<NodeRef, Type>,
    pub bounds: BTreeMap<NodeRef, Instance>,
}

#[derive(Debug, Clone, Default)]
pub struct Definition {
    pub ty: Option<Type>,
    pub instructions: Vec<Instruction>,
    pub types: BTreeMap<NodeRef, Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Named(NodeRef, Vec<Type>, TypeFlags),
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
    Parameter(NodeRef),
}

#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeFlags {
    pub intrinsic: bool,
    pub flat: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeRepresentation {
    Intrinsic,
    Marker,
    Structure(Vec<Type>),
    Enumeration(Vec<Vec<Type>>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Instance {
    Bound(NodeRef),
    Definition(DefinitionKey),
}

#[derive(Debug, Clone)]
pub enum Instruction {
    If {
        node: Option<NodeRef>,
        branches: Vec<(Vec<Condition>, Vec<Instruction>, Option<NodeRef>)>,
        else_branch: Option<(Vec<Instruction>, Option<NodeRef>)>,
    },
    Return {
        value: NodeRef,
    },
    Trace {
        location: NodeRef,
    },
    Value {
        node: NodeRef,
        value: Value,
    },
}

#[derive(Debug, Clone)]
pub enum Value {
    Bound(NodeRef),
    Call {
        function: NodeRef,
        inputs: Vec<NodeRef>,
    },
    Concat {
        segments: Vec<(String, NodeRef)>,
        trailing: String,
    },
    Constant(DefinitionKey),
    Function {
        inputs: Vec<NodeRef>,
        captures: Vec<NodeRef>,
        instructions: Vec<Instruction>,
    },
    Field {
        input: NodeRef,
        field_name: String,
        field_index: usize,
    },
    Tuple(Vec<NodeRef>),
    Marker,
    MutableVariable(NodeRef),
    Number(String),
    Runtime {
        name: String,
        inputs: Vec<NodeRef>,
    },
    String(String),
    Structure(Vec<(String, NodeRef)>),
    TupleElement {
        input: NodeRef,
        index: usize,
    },
    Unreachable,
    Variable(NodeRef),
    Variant {
        index: usize,
        elements: Vec<NodeRef>,
    },
    VariantElement {
        input: NodeRef,
        variant: usize,
        index: usize,
    },
}

#[derive(Debug, Clone)]
pub enum Condition {
    Or(Vec<Vec<Condition>>),
    EqualToNumber {
        input: NodeRef,
        value: String,
    },
    EqualToString {
        input: NodeRef,
        value: String,
    },
    EqualToVariant {
        input: NodeRef,
        variant: usize,
    },
    Initialize {
        variable: NodeRef,
        node: Option<NodeRef>,
        value: Value,
        mutable: bool,
    },
    Mutate {
        input: NodeRef,
        variable: NodeRef,
    },
}

impl Instruction {
    pub fn nodes(&self, traverse_functions: bool) -> Vec<&NodeRef> {
        match self {
            Instruction::If {
                node,
                branches,
                else_branch,
            } => {
                node.iter()
                    .chain(
                        branches
                            .iter()
                            .flat_map(|(conditions, instructions, then_node)| {
                                conditions
                                    .iter()
                                    .flat_map(|condition| condition.nodes())
                                    .chain(instructions.iter().flat_map(|instruction| {
                                        instruction.nodes(traverse_functions)
                                    }))
                                    .chain(then_node)
                            })
                            .chain(else_branch.iter().flat_map(|(instructions, else_node)| {
                                instructions
                                    .iter()
                                    .flat_map(|instruction| instruction.nodes(traverse_functions))
                                    .chain(else_node)
                            })),
                    )
                    .collect()
            }
            Instruction::Return { .. } => Vec::new(),
            Instruction::Trace { .. } => Vec::new(),
            Instruction::Value { node, value } => {
                let mut nodes = vec![node];

                if traverse_functions
                    && let Value::Function {
                        inputs,
                        instructions,
                        ..
                    } = value
                {
                    nodes.extend(inputs);

                    for instruction in instructions {
                        nodes.extend(instruction.nodes(true));
                    }
                }

                nodes
            }
        }
    }

    pub fn traverse_mut(&mut self, f: &mut dyn FnMut(&mut Self) -> CodegenResult) -> CodegenResult {
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
            Instruction::Trace { .. } => {}
            Instruction::Value { value, .. } => {
                if let Value::Function { instructions, .. } = value {
                    for instruction in instructions {
                        instruction.traverse_mut(f)?;
                    }
                }
            }
        }

        Ok(())
    }
}

impl Condition {
    pub fn nodes(&self) -> Vec<&NodeRef> {
        match self {
            Condition::Or(conditions) => conditions
                .iter()
                .flatten()
                .flat_map(|condition| condition.nodes())
                .collect(),
            Condition::EqualToNumber { input, .. } => vec![input],
            Condition::EqualToString { input, .. } => vec![input],
            Condition::EqualToVariant { input, .. } => vec![input],
            Condition::Initialize { variable, .. } => vec![variable],
            Condition::Mutate { input, variable } => vec![input, variable],
        }
    }
}
