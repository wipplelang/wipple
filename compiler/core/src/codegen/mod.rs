pub mod ir;
pub mod js;

use crate::{
    db::{Db, Node},
    facts::Codegen,
    typecheck::bounds::ResolvedBounds,
    visit::{
        Bounds,
        definitions::{ConstantDefinition, ConstantValue, Defined, InstanceDefinition},
    },
};
use dyn_clone::DynClone;
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Debug,
    ops::ControlFlow,
};

#[derive(Debug, Clone, Copy)]
pub struct Options<'a> {
    pub file_name: Option<&'a str>,
    pub source_root: &'a str,
    pub trace: TraceOptions<'a>,
    pub incremental: bool,
}

#[derive(Debug, Clone, Copy, Default)]
pub enum TraceOptions<'a> {
    #[default]
    None,
    All,
    Files(&'a [&'a str]),
}

#[typetag::serde]
pub trait CodegenValue: Debug + DynClone + Send + Sync + 'static {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError>;
}

dyn_clone::clone_trait_object!(CodegenValue);

pub type CodegenError = anyhow::Error;

pub struct CodegenCtx {
    reachable_definitions: BTreeSet<ir::DefinitionKey>,
    instructions: Vec<Vec<ir::Instruction>>,
    conditions: Vec<Vec<ir::Condition>>,
}

impl CodegenCtx {
    fn new() -> Self {
        CodegenCtx {
            reachable_definitions: Default::default(),
            instructions: vec![Vec::new()],
            conditions: Vec::new(),
        }
    }

    pub fn codegen(&mut self, db: &Db, node: Node) -> Result<(), CodegenError> {
        let Some(value) = db.get(node).map(|Codegen(value)| value.clone()) else {
            return Ok(());
        };

        value.codegen(db, self)
    }

    pub fn push_instructions(&mut self) {
        self.instructions.push(Vec::new());
    }

    pub fn pop_instructions(&mut self) -> Vec<ir::Instruction> {
        self.instructions.pop().unwrap()
    }

    pub fn instruction(&mut self, instruction: ir::Instruction) {
        self.instructions.last_mut().unwrap().push(instruction);
    }

    pub fn push_conditions(&mut self) {
        self.conditions.push(Vec::new());
    }

    pub fn pop_conditions(&mut self) -> Vec<ir::Condition> {
        self.conditions.pop().unwrap()
    }

    pub fn condition(&mut self, condition: ir::Condition) {
        self.conditions.last_mut().unwrap().push(condition);
    }

    pub fn mark_reachable(&mut self, definition: ir::DefinitionKey) {
        self.reachable_definitions.insert(definition);
    }

    pub fn bounds_for_constant(
        &mut self,
        definition: Node,
        bound_path: &[Node],
        bounds: &ResolvedBounds,
    ) -> Result<BTreeMap<Vec<Node>, ir::Instance>, CodegenError> {
        self.reachable_definitions
            .insert(ir::DefinitionKey::Constant(definition));

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
    ) -> Result<ir::Instance, CodegenError> {
        let bound = bounds
            .0
            .get(bound_path)
            .ok_or_else(|| anyhow::format_err!("missing bound path {bound_path:?}"))?
            .as_ref()
            .map_err(|_| anyhow::format_err!("unresolved bound at {bound_path:?}"))?;

        if bound.instance.is_from_bound {
            // This is relative to the enclosing definition (see `codegen_constant`)
            return Ok(ir::Instance::Bound(vec![bound.instance.node]));
        }

        self.reachable_definitions
            .insert(ir::DefinitionKey::Constant(bound.instance.node));

        Ok(ir::Instance::Instance {
            definition: ir::DefinitionKey::Constant(bound.instance.node),
            bounds: self.bounds_for_constant(bound.instance.node, bound_path, bounds)?,
        })
    }
}

pub fn codegen(
    db: &Db,
    source_files: &[Node],
    statements: &[Node],
    lib_statements: &[Node],
    include_all_definitions: bool,
) -> Result<ir::Program, CodegenError> {
    let mut program = ir::Program::default();
    program.source_files.extend(source_files);

    let mut ctx = CodegenCtx::new();

    ctx.reachable_definitions
        .insert(ir::DefinitionKey::TopLevel);

    if include_all_definitions {
        db.for_each_fact::<_, ()>(&mut |_, node, Defined(definition)| {
            if definition.downcast_ref::<ConstantDefinition>().is_some()
                || definition
                    .downcast_ref::<InstanceDefinition>()
                    .is_some_and(|definition| !definition.error)
            {
                ctx.reachable_definitions
                    .insert(ir::DefinitionKey::Constant(node));
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
                ir::DefinitionKey::Constant(node) => {
                    let Defined(definition) = db
                        .get(node)
                        .ok_or_else(|| anyhow::format_err!("no definition for {node:?}"))?;

                    let body = if definition.downcast_ref::<ConstantDefinition>().is_some() {
                        db.get(node).map(|ConstantValue(value)| *value)
                    } else if let Some(definition) = definition.downcast_ref::<InstanceDefinition>()
                    {
                        definition.value
                    } else {
                        None
                    }
                    .ok_or_else(|| anyhow::format_err!("definition {node:?} has no value"))?;

                    let Bounds(bounds) = db.get(node).cloned().unwrap_or_default();

                    let mut definition_ctx = CodegenCtx::new();
                    definition_ctx.codegen(db, body)?;
                    definition_ctx.instruction(ir::Instruction::Return { value: body });

                    let function = ir::Function {
                        bounds: Some(bounds),
                        instructions: definition_ctx.pop_instructions(),
                        ..Default::default()
                    };

                    program
                        .definitions
                        .insert(ir::DefinitionKey::Constant(node), function);

                    ctx.reachable_definitions
                        .extend(definition_ctx.reachable_definitions);
                }
                ir::DefinitionKey::TopLevel => {
                    for &statement in statements.iter().chain(lib_statements) {
                        ctx.codegen(db, statement)?;
                    }

                    program.definitions.insert(
                        ir::DefinitionKey::TopLevel,
                        ir::Function {
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

    Ok(program)
}
