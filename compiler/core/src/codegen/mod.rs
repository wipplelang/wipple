mod imports;
pub mod ir;
pub mod mangle;
pub mod monomorphize;
mod optimize;
pub mod types;
pub mod wasm;

use crate::{
    codegen::{
        imports::collect_imports, monomorphize::MonomorphizeCtx, optimize::optimize, types::ir_type,
    },
    db::{Db, Node},
    facts::Codegen,
    typecheck::{
        bounds::{Bounds, ResolvedBound, UnresolvedBound},
        ty::Ty,
    },
};
use dyn_clone::DynClone;
use std::{collections::BTreeMap, fmt::Debug};

#[derive(Debug, Clone, Copy, Default)]
pub struct Options<'a> {
    pub sourcemap: bool,
    pub trace: TraceOptions<'a>,
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
    monomorphize_ctx: MonomorphizeCtx,
    instructions: Vec<Vec<ir::Instruction>>,
    conditions: Vec<Vec<ir::Condition>>,
}

impl CodegenCtx {
    fn new() -> Self {
        CodegenCtx {
            monomorphize_ctx: Default::default(),
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

    pub fn definition_key(
        &mut self,
        db: &Db,
        definition: Node,
        parameters: &BTreeMap<Node, Ty>,
        bounds: BTreeMap<Node, ir::Instance>,
        generic: bool,
    ) -> Result<ir::DefinitionKey, CodegenError> {
        self.monomorphize_ctx
            .get_or_insert(db, definition, parameters, bounds, generic)
    }

    pub fn codegen_constant(
        &mut self,
        db: &Db,
        definition: Node,
        parameters: &BTreeMap<Node, Ty>,
        bounds: BTreeMap<Node, Result<ResolvedBound, UnresolvedBound>>,
        generic: bool,
    ) -> Result<ir::DefinitionKey, CodegenError> {
        let bounds = bounds
            .into_iter()
            .map(|(node, bound)| {
                let bound =
                    bound.map_err(|_| anyhow::format_err!("unresolved bound for {node:?}"))?;

                let instance = self.codegen_instance(db, bound, true)?;

                Ok((node, instance))
            })
            .collect::<Result<BTreeMap<_, _>, CodegenError>>()?;

        self.definition_key(db, definition, parameters, bounds, generic)
    }

    pub fn codegen_instance(
        &mut self,
        db: &Db,
        bound: ResolvedBound,
        generic: bool,
    ) -> Result<ir::Instance, CodegenError> {
        if bound.instance.is_from_bound {
            return Ok(ir::Instance::Bound(bound.instance.node));
        }

        let Bounds(bounds) = db.get(bound.resolved_node).cloned().unwrap_or_default();

        Ok(ir::Instance::Definition(self.codegen_constant(
            db,
            bound.instance.node,
            &bound.instance_parameters,
            bounds,
            generic,
        )?))
    }
}

pub fn codegen(db: &Db, statements: &[Node], lib: &[Node]) -> Result<ir::Program, CodegenError> {
    let mut ctx = CodegenCtx::new();

    let mut program = ir::Program::default();

    for &statement in statements.iter().chain(lib) {
        ctx.codegen(db, statement)?;
    }

    program.top_level.instructions = ctx.pop_instructions();

    for instruction in &mut program.top_level.instructions {
        instruction.for_each_node(true, &mut |node| {
            if let Some(ty) = ir_type(db, Ty::Node(*node)) {
                program.top_level.types.insert(*node, ty);
            }
        });
    }

    program.definitions = ctx.monomorphize_ctx.monomorphize_definitions(db)?.collect();

    for definition in [&mut program.top_level]
        .into_iter()
        .chain(program.definitions.values_mut())
    {
        collect_imports(definition)?;
    }

    optimize(&mut program);

    Ok(program)
}
