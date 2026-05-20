use crate::{codegen::CodegenError, db::Node, span::Span};
use arcstr::Substr;
use std::collections::BTreeMap;

#[derive(Debug, Clone, Default)]
pub struct Program {
    pub top_level: Definition,
    pub definitions: BTreeMap<DefinitionKey, Definition>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct DefinitionKey {
    pub node: Node,
    pub substitutions: BTreeMap<Node, Type>,
    pub bounds: BTreeMap<Node, Instance>,
}

#[derive(Debug, Clone, Default)]
pub struct Definition {
    pub ty: Option<Type>,
    pub instructions: Vec<Instruction>,
    pub types: BTreeMap<Node, Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Named {
        definition: Node,
        parameters: Vec<Type>,
        intrinsic: bool,
        representation: Option<Substr>,
        abi: Option<Substr>,
    },
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
    Parameter(Node),
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
    Bound(Node),
    Definition(DefinitionKey),
}

#[derive(Debug, Clone)]
pub enum Instruction {
    If {
        node: Option<Node>,
        branches: Vec<(Vec<Condition>, Vec<Instruction>, Option<Node>)>,
        else_branch: Option<(Vec<Instruction>, Option<Node>)>,
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
    Bound(Node),
    Call {
        function: Node,
        inputs: Vec<Node>,
    },
    Concat {
        segments: Vec<(String, Node)>,
        trailing: String,
    },
    Constant(DefinitionKey),
    Function {
        inputs: Vec<Node>,
        captures: Vec<Node>,
        instructions: Vec<Instruction>,
    },
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
    Unreachable,
    Variable(Node),
    Variant {
        index: usize,
        elements: Vec<Node>,
    },
    VariantElement {
        input: Node,
        variant: usize,
        index: usize,
    },
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
        variant: usize,
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
    pub fn nodes(&self, traverse_functions: bool) -> Vec<Node> {
        match self {
            Instruction::If {
                node,
                branches,
                else_branch,
            } => {
                node.iter()
                    .copied()
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
                                    .chain(*then_node)
                            })
                            .chain(else_branch.iter().flat_map(|(instructions, else_node)| {
                                instructions
                                    .iter()
                                    .flat_map(|instruction| instruction.nodes(traverse_functions))
                                    .chain(*else_node)
                            })),
                    )
                    .collect()
            }
            Instruction::Return { value } => vec![*value],
            Instruction::Trace { .. } => Vec::new(),
            Instruction::Value { node, value } => {
                let mut nodes = vec![*node];

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
    pub fn nodes(&self) -> Vec<Node> {
        match *self {
            Condition::Or(ref conditions) => conditions
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
