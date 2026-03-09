pub mod ir;
pub mod js;
pub mod mangle;
pub mod monomorphize;
pub mod types;

use crate::{
    codegen::monomorphize::MonomorphizeCtx,
    database::{Db, NodeRef},
    typecheck::Substitutions,
};
use std::{
    collections::{BTreeMap, HashSet},
    ops::{Deref, DerefMut},
};

pub trait Codegen {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> Option<ir::SpannedExpression>;

    fn identifier(&self) -> Option<String> {
        None
    }
}

pub struct CodegenCtx<'a> {
    pub db: &'a mut Db,
    monomorphize_ctx: MonomorphizeCtx,
    reachable_intrinsics: HashSet<String>,
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
            reachable_intrinsics: Default::default(),
        }
    }

    pub fn codegen(&mut self, node: &NodeRef) -> Option<ir::SpannedExpression> {
        node.codegen(node, self)
    }

    pub fn definition_key(
        &mut self,
        definition: &NodeRef,
        substitutions: &Substitutions,
        bounds: BTreeMap<NodeRef, ir::Instance>,
        generic: bool,
    ) -> Option<ir::DefinitionKey> {
        self.monomorphize_ctx
            .get_or_insert(definition, substitutions, bounds, generic, self.db)
    }

    pub fn mark_reachable_intrinsic(&mut self, name: &str) {
        self.reachable_intrinsics.insert(name.to_string());
    }
}

pub fn codegen(db: &mut Db, files: &[NodeRef], lib_files: &[NodeRef]) -> Option<ir::Program> {
    let mut ctx = CodegenCtx::new(db);

    let mut program = ir::Program::default();

    for file in files.iter().chain(lib_files) {
        program.files.push(ctx.codegen(file)?);
    }

    program.definitions = ctx.monomorphize_definitions()?.collect();

    program.intrinsics = ctx.reachable_intrinsics;

    Some(program)
}
