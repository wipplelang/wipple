mod program;
mod types;

pub use program::*;
pub use types::*;

use crate::{
    codegen::CodegenError,
    db::{Db, Node},
    facts::Codegen,
    typecheck::bounds::ResolvedBounds,
    visit::{
        Bounds, TypeParameters,
        definitions::{ConstantDefinition, ConstantValue, Defined, InstanceDefinition},
    },
};
use dyn_clone::DynClone;
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Debug,
    ops::ControlFlow,
};

#[typetag::serde]
pub trait Write: Debug + DynClone + Send + Sync + 'static {
    fn write(&self, db: &Db, ctx: &mut Ctx) -> Result<(), CodegenError>;
}

dyn_clone::clone_trait_object!(Write);

pub struct Ctx {
    reachable_definitions: BTreeSet<DefinitionKey>,
    instructions: Vec<Vec<Instruction>>,
    conditions: Vec<Vec<Condition>>,
}

impl Ctx {
    fn new() -> Self {
        Ctx {
            reachable_definitions: Default::default(),
            instructions: vec![Vec::new()],
            conditions: Vec::new(),
        }
    }

    pub fn write(&mut self, db: &Db, node: Node) -> Result<(), CodegenError> {
        let Some(value) = db.get(node).map(|Codegen(value)| value.clone()) else {
            return Ok(());
        };

        value.write(db, self)
    }

    pub fn push_instructions(&mut self) {
        self.instructions.push(Vec::new());
    }

    pub fn pop_instructions(&mut self) -> Vec<Instruction> {
        self.instructions.pop().unwrap()
    }

    pub fn instruction(&mut self, instruction: Instruction) {
        self.instructions.last_mut().unwrap().push(instruction);
    }

    pub fn push_conditions(&mut self) {
        self.conditions.push(Vec::new());
    }

    pub fn pop_conditions(&mut self) -> Vec<Condition> {
        self.conditions.pop().unwrap()
    }

    pub fn condition(&mut self, condition: Condition) {
        self.conditions.last_mut().unwrap().push(condition);
    }

    pub fn mark_reachable(&mut self, definition: DefinitionKey) {
        self.reachable_definitions.insert(definition);
    }

    pub fn bounds_for_constant(
        &mut self,
        definition: Node,
        bound_path: &[Node],
        bounds: &ResolvedBounds,
    ) -> Result<BTreeMap<Vec<Node>, Instance>, CodegenError> {
        self.reachable_definitions
            .insert(DefinitionKey::Constant(definition));

        bounds
            .0
            .keys()
            .filter(|other| other.starts_with(bound_path) && other.len() == bound_path.len() + 1)
            .map(|other| {
                self.bound_for_instance(other, bounds)
                    .map(|instance| (other.strip_prefix(bound_path).unwrap().to_vec(), instance))
            })
            .collect()
    }

    pub fn bound_for_instance(
        &mut self,
        bound_path: &[Node],
        bounds: &ResolvedBounds,
    ) -> Result<Instance, CodegenError> {
        let bound = bounds
            .0
            .get(bound_path)
            .ok_or_else(|| anyhow::format_err!("missing bound path {bound_path:?}"))?
            .as_ref()
            .map_err(|_| anyhow::format_err!("unresolved bound at {bound_path:?}"))?;

        if bound.instance.is_from_bound {
            // This is relative to the enclosing definition (see `codegen_constant`)
            return Ok(Instance::Bound(vec![bound.instance.node]));
        }

        self.reachable_definitions
            .insert(DefinitionKey::Constant(bound.instance.node));

        Ok(Instance::Instance {
            definition: DefinitionKey::Constant(bound.instance.node),
            bounds: self.bounds_for_constant(bound.instance.node, bound_path, bounds)?,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IncludeDefinitions {
    pub defined: bool,
    pub referenced: bool,
}

impl Default for IncludeDefinitions {
    fn default() -> Self {
        IncludeDefinitions {
            defined: false,
            referenced: true,
        }
    }
}

impl IncludeDefinitions {
    pub fn for_library() -> Self {
        IncludeDefinitions {
            defined: true,
            referenced: false,
        }
    }

    pub fn for_repl() -> Self {
        IncludeDefinitions {
            defined: true,
            referenced: true,
        }
    }
}

impl Program {
    pub fn from_statements(
        db: &Db,
        source_files: &[Node],
        statements: &[Node],
        lib_statements: &[Node],
        include_definitions: IncludeDefinitions,
    ) -> Result<Program, CodegenError> {
        let mut program = Program {
            layer: db.layer(),
            source_files: source_files.to_vec(),
            definitions: Default::default(),
        };

        let mut ctx = Ctx::new();
        ctx.reachable_definitions.insert(DefinitionKey::TopLevel);

        if include_definitions.defined {
            db.for_each_fact::<_, ()>(&mut |db, node, Defined(definition)| {
                if include_definitions.referenced && !db.owns(node) {
                    return ControlFlow::Continue(());
                }

                if definition.downcast_ref::<ConstantDefinition>().is_some()
                    || definition
                        .downcast_ref::<InstanceDefinition>()
                        .is_some_and(|definition| !definition.error)
                {
                    ctx.reachable_definitions
                        .insert(DefinitionKey::Constant(node));
                }

                ControlFlow::Continue(())
            });
        }

        let mut visited = BTreeSet::new();
        loop {
            let mut progress = false;

            for key in ctx.reachable_definitions.clone() {
                if !visited.insert(key) {
                    continue;
                }

                match key {
                    DefinitionKey::Constant(node) => {
                        let Defined(definition) = db
                            .get(node)
                            .ok_or_else(|| anyhow::format_err!("no definition for {node:?}"))?;

                        let body = if definition.downcast_ref::<ConstantDefinition>().is_some() {
                            db.get(node).map(|ConstantValue(value)| *value)
                        } else if let Some(definition) =
                            definition.downcast_ref::<InstanceDefinition>()
                        {
                            definition.value
                        } else {
                            None
                        }
                        .ok_or_else(|| anyhow::format_err!("definition {node:?} has no value"))?;

                        let TypeParameters(type_parameters) =
                            db.get(node).cloned().unwrap_or_default();

                        let Bounds(bounds) = db.get(node).cloned().unwrap_or_default();

                        let mut definition_ctx = Ctx::new();
                        definition_ctx.write(db, body)?;
                        definition_ctx.instruction(Instruction::Return { value: body });

                        let function = Function {
                            type_parameters,
                            bounds: Some(Vec::from_iter(bounds)),
                            instructions: definition_ctx.pop_instructions(),
                            ..Default::default()
                        };

                        program
                            .definitions
                            .insert(DefinitionKey::Constant(node), function);

                        ctx.reachable_definitions
                            .extend(definition_ctx.reachable_definitions);
                    }
                    DefinitionKey::TopLevel => {
                        for &statement in statements.iter().chain(lib_statements) {
                            ctx.write(db, statement)?;
                        }

                        program.definitions.insert(
                            DefinitionKey::TopLevel,
                            Function {
                                instructions: ctx.pop_instructions(),
                                ..Default::default()
                            },
                        );
                    }
                }

                progress = true;
            }

            if !progress {
                break;
            }
        }

        if include_definitions.defined && include_definitions.referenced {
            program.definitions.retain(|&key, _| match key {
                DefinitionKey::Constant(node) => db.owns(node),
                DefinitionKey::TopLevel => true,
            });
        }

        Ok(program)
    }
}
