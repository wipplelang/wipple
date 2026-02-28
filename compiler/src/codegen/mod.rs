pub mod ir;
pub mod js;

use crate::{
    database::{Db, NodeRef},
    typecheck::Bounds,
    visit::{Defined, Definition},
};
use std::{
    collections::{BTreeSet, HashSet},
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
    reachable: BTreeSet<NodeRef>,
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
            reachable: Default::default(),
            reachable_intrinsics: Default::default(),
        }
    }

    pub fn codegen(&mut self, node: &NodeRef) -> Option<ir::SpannedExpression> {
        node.codegen(node, self)
    }

    pub fn mark_reachable(&mut self, source: &NodeRef, definition: &NodeRef) {
        self.reachable.insert(definition.clone());

        if let Some(Bounds(items)) = self.db.get(source) {
            for (_, item) in items {
                self.reachable.insert(item.bound.trait_node);

                if let Some(instance) = item.instance {
                    self.reachable.insert(instance.instance_node);
                }
            }
        }
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

    let definitions = ctx
        .db
        .iter()
        .map(|(_, Defined(definition))| definition)
        .collect::<Vec<_>>();

    loop {
        let mut progress = false;

        for definition in &definitions {
            let node = definition.node();

            if program.definitions.contains_key(&node) || !ctx.reachable.contains(&node) {
                continue;
            }

            let body = match definition {
                Definition::Constant(definition) => definition.value.as_ref(),
                Definition::Instance(definition) => definition.value.as_ref(),
                _ => continue,
            };

            let Some(body) = body else {
                continue;
            };

            program.definitions.insert(node.clone(), ctx.codegen(body)?);

            progress = true;
        }

        if !progress {
            break;
        }
    }

    program.intrinsics = ctx.reachable_intrinsics;

    Some(program)
}
