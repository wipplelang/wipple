pub mod ir;
pub mod js;
pub mod monomorphize;

use crate::{
    codegen::monomorphize::MonomorphizeCtx,
    db::{Db, Node},
    facts::Codegen,
    typecheck::bounds::Bounds,
};
use dyn_clone::DynClone;
use std::{collections::BTreeMap, fmt::Debug};

#[derive(Debug, Clone, Copy)]
pub struct Options<'a> {
    pub file_name: &'a str,
    pub source_root: &'a str,
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
        definition: Node,
        bounds: BTreeMap<Vec<Node>, ir::Instance>,
        generic: bool,
    ) -> Result<ir::ConstantDefinitionKey, CodegenError> {
        self.monomorphize_ctx
            .get_or_insert(definition, bounds, generic)
    }

    pub fn codegen_constant(
        &mut self,
        db: &Db,
        definition: Node,
        bound_path: &[Node],
        bounds: &Bounds,
        generic: bool,
    ) -> Result<ir::ConstantDefinitionKey, CodegenError> {
        let bounds = bounds
            .0
            .keys()
            .filter(|other| other.starts_with(bound_path) && other.len() == bound_path.len() + 1)
            .map(|other| {
                self.codegen_instance(db, other, bounds, true)
                    .map(|instance| (other.strip_prefix(bound_path).unwrap().to_vec(), instance))
            })
            .collect::<Result<BTreeMap<_, _>, CodegenError>>()?;

        self.definition_key(definition, bounds, generic)
    }

    pub fn codegen_instance(
        &mut self,
        db: &Db,
        bound_path: &[Node],
        bounds: &Bounds,
        generic: bool,
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

        Ok(ir::Instance::Definition(self.codegen_constant(
            db,
            bound.instance.node,
            bound_path,
            bounds,
            generic,
        )?))
    }
}

pub fn codegen(
    db: &Db,
    source_files: &[Node],
    statements: &[Node],
    lib_statements: &[Node],
) -> Result<ir::Program, CodegenError> {
    let mut ctx = CodegenCtx::new();

    let mut program = ir::Program::default();
    program.source_files.extend(source_files);

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

    for (key, definition) in ctx.monomorphize_ctx.monomorphize_definitions(db)? {
        program
            .definitions
            .insert(ir::DefinitionKey::Constant(key), definition);
    }

    Ok(program)
}
