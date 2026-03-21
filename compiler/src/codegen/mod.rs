pub mod ir;
pub mod mangle;
pub mod monomorphize;
pub mod types;
pub mod wasm;

use crate::{
    codegen::monomorphize::MonomorphizeCtx,
    database::{Db, NodeRef},
    typecheck::Substitutions,
};
use std::{
    collections::BTreeMap,
    ops::{Deref, DerefMut},
};

#[derive(Debug, Clone, Copy)]
pub struct Options<'a> {
    pub sourcemap: bool,
    pub trace: &'a [&'a str],
}

pub trait Codegen {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> CodegenResult {
        let _ = ctx;
        Err(anyhow::format_err!("cannot codegen {node:?}"))
    }

    fn type_representation(
        &self,
        node: &NodeRef,
        ctx: &mut CodegenCtx<'_>,
    ) -> Option<ir::TypeRepresentation> {
        let _ = node;
        let _ = ctx;
        None
    }

    fn identifier(&self) -> Option<String> {
        None
    }
}

pub type CodegenResult<T = ()> = anyhow::Result<T>;

pub struct CodegenCtx<'a> {
    pub db: &'a mut Db,
    monomorphize_ctx: MonomorphizeCtx,
    instructions: Vec<Vec<ir::Instruction>>,
    conditions: Vec<Vec<ir::Condition>>,
}

impl Deref for CodegenCtx<'_> {
    type Target = Db;

    fn deref(&self) -> &Self::Target {
        self.db
    }
}

impl DerefMut for CodegenCtx<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.db
    }
}

impl<'a> CodegenCtx<'a> {
    fn new(db: &'a mut Db) -> Self {
        CodegenCtx {
            db,
            monomorphize_ctx: Default::default(),
            instructions: vec![Vec::new()],
            conditions: Vec::new(),
        }
    }

    pub fn codegen(&mut self, node: &NodeRef) -> CodegenResult {
        node.codegen(node, self)
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
        definition: &NodeRef,
        substitutions: &Substitutions,
        bounds: BTreeMap<NodeRef, ir::Instance>,
        generic: bool,
    ) -> CodegenResult<ir::DefinitionKey> {
        self.monomorphize_ctx
            .get_or_insert(definition, substitutions, bounds, generic, self.db)
    }
}

pub fn codegen(
    db: &mut Db,
    files: &[NodeRef],
    lib_files: &[NodeRef],
) -> CodegenResult<ir::Program> {
    let mut ctx = CodegenCtx::new(db);

    let mut program = ir::Program::default();

    for file in files.iter().chain(lib_files) {
        program.files.push(ctx.span(file));
        ctx.codegen(file)?;
    }

    program.top_level.instructions = ctx.pop_instructions();

    for instruction in &program.top_level.instructions {
        for node in instruction.nodes(true) {
            if let Some(ty) = ctx.db.ir_type(node.clone()) {
                program.top_level.types.insert(node.clone(), ty);
            }
        }
    }

    program.definitions = ctx
        .monomorphize_ctx
        .monomorphize_definitions(ctx.db)?
        .collect();

    Ok(program)
}
