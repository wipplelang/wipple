#![allow(
    clippy::too_many_arguments,
    clippy::type_complexity,
    clippy::new_without_default
)]

mod format;
mod optimize;
mod ssa;

use crate::{
    analysis::typecheck, helpers::InternedString, Compiler, ConstantId, EnumerationId, FieldIndex,
    ItemId, StructureId, VariableId, VariantIndex,
};
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use serde_with::serde_as;
use std::{
    collections::BTreeMap,
    os::raw::{c_int, c_uint},
};

pub use ssa::Type;
pub use typecheck::Intrinsic;

#[serde_as]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Program {
    pub labels: Vec<(LabelKind, usize, Vec<BasicBlock>)>,

    #[serde_as(as = "Vec<(_, _)>")]
    pub structures: BTreeMap<StructureId, Vec<Type>>,

    #[serde_as(as = "Vec<(_, _)>")]
    pub enumerations: BTreeMap<EnumerationId, Vec<Vec<Type>>>,

    pub entrypoint: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LabelKind {
    Entrypoint,
    Constant(Type),
    Function(Type, Type),
    Closure(CaptureList, Type, Type),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BasicBlock {
    pub description: String,
    pub statements: Vec<Statement>,
    pub terminator: Option<Terminator>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Statement {
    Copy,
    Drop,
    Initialize(usize),
    Free(usize),
    WithContext(usize),
    ResetContext,
    Unpack(CaptureList),
    Expression(Type, Expression),
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Terminator {
    Unreachable,
    Return,
    Jump(usize),
    If(VariantIndex, usize, usize),
    TailCall,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expression {
    Marker,
    Text(InternedString),
    Number(rust_decimal::Decimal),
    Integer(i64),
    Natural(u64),
    Byte(u8),
    Signed(c_int),
    Unsigned(c_uint),
    Float(f32),
    Double(f64),
    Variable(usize),
    Constant(usize),
    Function(usize),
    Closure(CaptureList, usize),
    Call,
    External(InternedString, InternedString, usize),
    Runtime(Intrinsic, usize),
    Tuple(usize),
    Format(Vec<InternedString>, Option<InternedString>),
    Structure(usize),
    Variant(VariantIndex, usize),
    TupleElement(usize),
    StructureElement(FieldIndex),
    VariantElement(VariantIndex, usize),
    Reference,
    Dereference,
    Context(usize),
}

#[serde_as]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CaptureList(#[serde_as(as = "Vec<(_, _)>")] pub BTreeMap<usize, usize>);

impl Compiler {
    pub fn ir_from(&self, program: &typecheck::Program) -> Program {
        assert!(
            !self.has_errors(),
            "cannot generate IR for program containing errors"
        );

        let program = self.convert_to_ssa(program);

        let contexts = program.contexts.clone();
        let structures = program.structures.clone();
        let mut enumerations = program.enumerations.clone();

        let bool_type = self.new_enumeration_id();
        enumerations.insert(bool_type, vec![Vec::new(), Vec::new()]);

        let mut gen = IrGen {
            items: Default::default(),
            labels: Default::default(),
            scopes: Default::default(),
            contexts,
            structures,
            enumerations,
            bool_type,
        };

        for (id, item) in &program.items {
            let label = gen.new_label(|_, _| {
                if *id == program.entrypoint {
                    LabelKind::Entrypoint
                } else {
                    LabelKind::Constant(item.ty.clone())
                }
            });

            gen.items.insert(*id, label);
        }

        for (id, constant) in program.items {
            let label = *gen.items.get(&id).unwrap();
            let mut pos = gen.new_basic_block(label, "item entrypoint");
            gen.gen_expr(constant, label, &mut pos);
            *gen.terminator_for(label, pos) = Some(Terminator::Return);
        }

        Program {
            labels: gen
                .labels
                .into_iter()
                .map(|(kind, vars, blocks)| (kind.unwrap(), vars.len(), blocks))
                .collect(),
            structures: gen.structures,
            enumerations: gen.enumerations,
            entrypoint: *gen.items.get(&program.entrypoint).unwrap(),
        }
    }
}

struct IrGen {
    items: BTreeMap<ItemId, usize>,
    labels: Vec<(
        Option<LabelKind>,
        BTreeMap<VariableId, usize>,
        Vec<BasicBlock>,
    )>,
    scopes: Vec<Vec<VariableId>>,
    contexts: BTreeMap<ConstantId, ItemId>,
    structures: BTreeMap<StructureId, Vec<Type>>,
    enumerations: BTreeMap<EnumerationId, Vec<Vec<Type>>>,
    bool_type: EnumerationId,
}

impl IrGen {
    fn new_label(&mut self, kind: impl FnOnce(&mut Self, usize) -> LabelKind) -> usize {
        let label = self.labels.len();
        self.labels.insert(label, Default::default());
        let kind = kind(self, label);
        self.labels[label].0 = Some(kind);
        label
    }

    fn basic_blocks_for(&mut self, label: usize) -> &mut Vec<BasicBlock> {
        &mut self.labels[label].2
    }

    fn new_basic_block(&mut self, label: usize, description: impl ToString) -> usize {
        let basic_blocks = self.basic_blocks_for(label);
        let pos = basic_blocks.len();

        basic_blocks.push(BasicBlock {
            description: description.to_string(),
            statements: Default::default(),
            terminator: None,
        });

        pos
    }

    fn basic_block_for(&mut self, label: usize, pos: usize) -> &mut BasicBlock {
        &mut self.basic_blocks_for(label)[pos]
    }

    fn statements_for(&mut self, label: usize, pos: usize) -> &mut Vec<Statement> {
        &mut self.basic_block_for(label, pos).statements
    }

    fn terminator_for(&mut self, label: usize, pos: usize) -> &mut Option<Terminator> {
        &mut self.basic_block_for(label, pos).terminator
    }

    fn variable_for(&mut self, label: usize, variable: VariableId) -> usize {
        let vars = &mut self.labels[label].1;
        let next = vars.len();
        *vars.entry(variable).or_insert(next)
    }
}

impl IrGen {
    fn gen_expr(&mut self, expr: ssa::Expression, label: usize, pos: &mut usize) {
        match expr.kind {
            ssa::ExpressionKind::Marker => {
                self.statements_for(label, *pos)
                    .push(Statement::Expression(expr.ty, Expression::Marker));
            }
            ssa::ExpressionKind::Variable(var) => {
                let var = self.variable_for(label, var);
                self.statements_for(label, *pos)
                    .push(Statement::Expression(expr.ty, Expression::Variable(var)));
            }
            ssa::ExpressionKind::Text(text) => {
                self.statements_for(label, *pos)
                    .push(Statement::Expression(expr.ty, Expression::Text(text)));
            }
            ssa::ExpressionKind::Number(number) => {
                self.statements_for(label, *pos)
                    .push(Statement::Expression(expr.ty, Expression::Number(number)));
            }
            ssa::ExpressionKind::Integer(integer) => {
                self.statements_for(label, *pos)
                    .push(Statement::Expression(expr.ty, Expression::Integer(integer)));
            }
            ssa::ExpressionKind::Natural(natural) => {
                self.statements_for(label, *pos)
                    .push(Statement::Expression(expr.ty, Expression::Natural(natural)));
            }
            ssa::ExpressionKind::Byte(byte) => {
                self.statements_for(label, *pos)
                    .push(Statement::Expression(expr.ty, Expression::Byte(byte)));
            }
            ssa::ExpressionKind::Signed(signed) => {
                self.statements_for(label, *pos)
                    .push(Statement::Expression(expr.ty, Expression::Signed(signed)));
            }
            ssa::ExpressionKind::Unsigned(unsigned) => {
                self.statements_for(label, *pos).push(Statement::Expression(
                    expr.ty,
                    Expression::Unsigned(unsigned),
                ));
            }
            ssa::ExpressionKind::Float(float) => {
                self.statements_for(label, *pos)
                    .push(Statement::Expression(expr.ty, Expression::Float(float)));
            }
            ssa::ExpressionKind::Double(double) => {
                self.statements_for(label, *pos)
                    .push(Statement::Expression(expr.ty, Expression::Double(double)));
            }
            ssa::ExpressionKind::Block(exprs) => {
                if exprs.is_empty() {
                    self.statements_for(label, *pos).push(Statement::Expression(
                        Type::Tuple(Vec::new()),
                        Expression::Tuple(0),
                    ));
                } else {
                    self.scopes.push(Vec::new());

                    let mut drop_statement = None::<Statement>;
                    for (index, expr) in exprs.into_iter().enumerate() {
                        if index != 0 {
                            self.statements_for(label, *pos)
                                .push(drop_statement.as_ref().unwrap().clone());
                        }

                        drop_statement = Some(Statement::Drop);

                        self.gen_expr(expr, label, pos);
                    }

                    self.gen_end(label, pos);
                    self.scopes.pop().unwrap();
                }
            }
            ssa::ExpressionKind::Call(function, input) => {
                self.gen_expr(*function, label, pos);
                self.gen_expr(*input, label, pos);

                if expr.tail {
                    *self.terminator_for(label, *pos) = Some(Terminator::TailCall);
                    *pos = self.new_basic_block(label, "following tail call");
                } else {
                    self.statements_for(label, *pos)
                        .push(Statement::Expression(expr.ty, Expression::Call));
                }
            }
            ssa::ExpressionKind::Function(pattern, body, captures) => {
                let (input_ty, output_ty) = match &expr.ty {
                    Type::FunctionReference(input, output) => (input.as_ref(), output.as_ref()),
                    _ => unreachable!(),
                };

                let mut capture_list = None;
                let function_label = self.new_label(|gen, function_label| {
                    if captures.is_empty() {
                        LabelKind::Function(input_ty.clone(), output_ty.clone())
                    } else {
                        let captures = CaptureList(
                            captures
                                .into_iter()
                                .map(|(var, _)| {
                                    (
                                        gen.variable_for(label, var),
                                        gen.variable_for(function_label, var),
                                    )
                                })
                                .unique()
                                .collect(),
                        );

                        capture_list = Some(captures.clone());

                        LabelKind::Closure(captures, input_ty.clone(), output_ty.clone())
                    }
                });

                let mut function_pos = self.new_basic_block(function_label, "function entrypoint");

                if let Some(capture_list) = capture_list.as_ref() {
                    self.statements_for(function_label, function_pos)
                        .push(Statement::Unpack(capture_list.clone()));
                }

                {
                    let mut body_pos = self.new_basic_block(function_label, "function body");

                    self.scopes.push(Vec::new());
                    self.gen_pattern(
                        pattern,
                        input_ty,
                        body_pos,
                        function_label,
                        &mut function_pos,
                    );
                    self.gen_expr(*body, function_label, &mut body_pos);
                    *self.terminator_for(function_label, function_pos) =
                        Some(Terminator::Unreachable);
                    *self.terminator_for(function_label, body_pos) = Some(Terminator::Return);
                    self.scopes.pop().unwrap();

                    if let Some(capture_list) = capture_list.as_ref() {
                        for var in capture_list.0.values() {
                            self.statements_for(function_label, body_pos)
                                .push(Statement::Free(*var));
                        }
                    }
                }

                self.statements_for(label, *pos).push(Statement::Expression(
                    expr.ty,
                    if let Some(capture_list) = capture_list {
                        Expression::Closure(capture_list, function_label)
                    } else {
                        Expression::Function(function_label)
                    },
                ));
            }
            ssa::ExpressionKind::When(input, arms) => {
                let ty = input.ty.clone();
                self.gen_expr(*input, label, pos);
                self.gen_when(arms, &ty, label, pos);
            }
            ssa::ExpressionKind::External(lib, identifier, exprs) => {
                let count = exprs.len();

                for expr in exprs {
                    self.gen_expr(expr, label, pos);
                }

                self.statements_for(label, *pos).push(Statement::Expression(
                    expr.ty,
                    Expression::External(lib, identifier, count),
                ));
            }
            ssa::ExpressionKind::Intrinsic(func, exprs) => {
                let count = exprs.len();

                for expr in exprs {
                    self.gen_expr(expr, label, pos);
                }

                self.statements_for(label, *pos).push(Statement::Expression(
                    expr.ty,
                    Expression::Runtime(func, count),
                ));
            }
            ssa::ExpressionKind::Structure(exprs) => {
                let count = exprs.len();

                for expr in exprs {
                    self.gen_expr(expr, label, pos);
                }

                self.statements_for(label, *pos)
                    .push(Statement::Expression(expr.ty, Expression::Structure(count)));
            }
            ssa::ExpressionKind::Tuple(exprs) => {
                let count = exprs.len();

                for expr in exprs {
                    self.gen_expr(expr, label, pos);
                }

                self.statements_for(label, *pos)
                    .push(Statement::Expression(expr.ty, Expression::Tuple(count)));
            }
            ssa::ExpressionKind::Format(segments, trailing_segment) => {
                let segments = segments
                    .into_iter()
                    .map(|(text, expr)| {
                        self.gen_expr(expr, label, pos);
                        text
                    })
                    .collect();

                self.statements_for(label, *pos).push(Statement::Expression(
                    expr.ty,
                    Expression::Format(segments, trailing_segment),
                ));
            }
            ssa::ExpressionKind::Variant(discriminant, exprs) => {
                let count = exprs.len();

                for expr in exprs {
                    self.gen_expr(expr, label, pos);
                }

                self.statements_for(label, *pos).push(Statement::Expression(
                    expr.ty,
                    Expression::Variant(discriminant, count),
                ));
            }
            ssa::ExpressionKind::Constant(id) => {
                let id = *self
                    .items
                    .get(&id)
                    .unwrap_or_else(|| panic!("cannot find {id:?}"));

                self.statements_for(label, *pos)
                    .push(Statement::Expression(expr.ty, Expression::Constant(id)));
            }
            ssa::ExpressionKind::With((id, value), body) => {
                self.gen_expr(*value, label, pos);

                if let Some(&id) = self.contexts.get(&id) {
                    let id = *self
                        .items
                        .get(&id)
                        .unwrap_or_else(|| panic!("cannot find {id:?}"));

                    self.statements_for(label, *pos)
                        .push(Statement::WithContext(id));

                    self.gen_expr(*body, label, pos);

                    self.statements_for(label, *pos)
                        .push(Statement::ResetContext);
                } else {
                    // This branch occurs when the constant is never actually used anywhere in the
                    // program, so we can just generate the body without worrying about setting the
                    // context
                    self.statements_for(label, *pos).push(Statement::Drop);
                    self.gen_expr(*body, label, pos);
                }
            }
            ssa::ExpressionKind::ContextualConstant(id) => {
                let id = *self
                    .contexts
                    .get(&id)
                    .unwrap_or_else(|| panic!("cannot find {id:?}"));

                let id = *self
                    .items
                    .get(&id)
                    .unwrap_or_else(|| panic!("cannot find {id:?}"));

                self.statements_for(label, *pos)
                    .push(Statement::Expression(expr.ty, Expression::Context(id)));
            }
            ssa::ExpressionKind::End(value) => {
                self.gen_expr(*value, label, pos);
                self.gen_end(label, pos);
                *self.terminator_for(label, *pos) = Some(Terminator::Return);
                *pos = self.new_basic_block(label, "following function end");
            }
        }
    }

    fn gen_end(&mut self, label: usize, pos: &mut usize) {
        for var in self.scopes.last().unwrap().clone().into_iter().rev() {
            let var = self.variable_for(label, var);
            self.statements_for(label, *pos).push(Statement::Free(var));
        }
    }

    fn gen_when(
        &mut self,
        mut arms: Vec<ssa::Arm>,
        input_ty: &Type,
        label: usize,
        pos: &mut usize,
    ) {
        if arms.is_empty() {
            self.statements_for(label, *pos).push(Statement::Drop);
            return;
        }

        // HACK: Optimization for the common case of assigning to a single variable.
        // In the future, we'll implement better optimizations that actually remove
        // unnecessary copies
        if arms.len() == 1 {
            let arm = arms.first().unwrap();

            if arm.guard.is_none() {
                if let ssa::PatternKind::Variable(var) = arm.pattern.kind {
                    let var = self.variable_for(label, var);
                    let arm = arms.pop().unwrap();

                    self.statements_for(label, *pos)
                        .push(Statement::Initialize(var));

                    self.gen_expr(arm.body, label, pos);

                    self.statements_for(label, *pos).push(Statement::Free(var));

                    return;
                }
            }
        }

        let continue_pos = self.new_basic_block(label, "following `when` expression");

        for arm in arms {
            self.statements_for(label, *pos).push(Statement::Copy);

            let mut body_pos = self.new_basic_block(label, "`when` arm body");
            self.scopes.push(Vec::new());

            if let Some(guard) = arm.guard {
                let condition_pos = self.new_basic_block(label, "`when` arm condition");
                let else_pos = self.new_basic_block(label, "`when` arm condition not satisfied");

                self.gen_pattern(arm.pattern, input_ty, condition_pos, label, pos);
                *self.terminator_for(label, *pos) = Some(Terminator::Jump(else_pos));

                *pos = condition_pos;
                self.gen_expr(guard, label, pos);
                *self.terminator_for(label, *pos) =
                    Some(Terminator::If(VariantIndex::new(1), body_pos, else_pos));

                *pos = else_pos;
            } else {
                self.gen_pattern(arm.pattern, input_ty, body_pos, label, pos);
            }

            *self.terminator_for(label, *pos) = Some(Terminator::Unreachable);

            self.statements_for(label, body_pos).push(Statement::Drop);
            self.gen_expr(arm.body, label, &mut body_pos);

            let free_pos = self.new_basic_block(label, "end of `when` expression");

            *self.terminator_for(label, body_pos) = Some(Terminator::Jump(free_pos));

            for var in self.scopes.pop().unwrap().into_iter().rev() {
                let var = self.variable_for(label, var);
                self.statements_for(label, free_pos)
                    .push(Statement::Free(var));
            }

            *self.terminator_for(label, free_pos) = Some(Terminator::Jump(continue_pos));
        }

        *pos = continue_pos;
    }

    fn gen_pattern(
        &mut self,
        pattern: ssa::Pattern,
        input_ty: &Type,
        body_pos: usize,
        label: usize,
        pos: &mut usize,
    ) {
        macro_rules! match_number {
            ($kind:ident($n:expr), $comparison:expr) => {{
                self.statements_for(label, *pos)
                    .push(Statement::Expression(Type::$kind, Expression::$kind($n)));

                let bool_type = Type::Enumeration(self.bool_type);

                self.statements_for(label, *pos).push(Statement::Expression(
                    bool_type,
                    Expression::Runtime($comparison, 2),
                ));

                let else_pos = self.new_basic_block(label, "following numeric pattern");
                *self.terminator_for(label, *pos) =
                    Some(Terminator::If(VariantIndex::new(1), body_pos, else_pos));
                *pos = else_pos;
            }};
        }

        match pattern.kind {
            ssa::PatternKind::Wildcard => {
                self.statements_for(label, *pos).push(Statement::Drop);
                *self.terminator_for(label, *pos) = Some(Terminator::Jump(body_pos));
                *pos = self.new_basic_block(label, "following `_` pattern");
            }
            ssa::PatternKind::Number(number) => {
                match_number!(Number(number), Intrinsic::NumberEquality);
            }
            ssa::PatternKind::Integer(integer) => {
                match_number!(Integer(integer), Intrinsic::IntegerEquality);
            }
            ssa::PatternKind::Natural(natural) => {
                match_number!(Natural(natural), Intrinsic::NaturalEquality);
            }
            ssa::PatternKind::Byte(byte) => {
                match_number!(Byte(byte), Intrinsic::ByteEquality);
            }
            ssa::PatternKind::Signed(signed) => {
                match_number!(Signed(signed), Intrinsic::SignedEquality);
            }
            ssa::PatternKind::Unsigned(unsigned) => {
                match_number!(Unsigned(unsigned), Intrinsic::UnsignedEquality);
            }
            ssa::PatternKind::Float(float) => {
                match_number!(Float(float), Intrinsic::FloatEquality);
            }
            ssa::PatternKind::Double(double) => {
                match_number!(Double(double), Intrinsic::DoubleEquality);
            }
            ssa::PatternKind::Text(text) => {
                self.statements_for(label, *pos).push(Statement::Expression(
                    Type::TextReference,
                    Expression::Text(text),
                ));

                let bool_type = Type::Enumeration(self.bool_type);

                self.statements_for(label, *pos).push(Statement::Expression(
                    bool_type,
                    Expression::Runtime(Intrinsic::TextEquality, 2),
                ));

                let else_pos = self.new_basic_block(label, "following text pattern");
                *self.terminator_for(label, *pos) =
                    Some(Terminator::If(VariantIndex::new(1), body_pos, else_pos));
                *pos = else_pos;
            }
            ssa::PatternKind::Variable(var) => {
                self.scopes.last_mut().unwrap().push(var);
                let var = self.variable_for(label, var);
                self.statements_for(label, *pos)
                    .push(Statement::Initialize(var));
                *self.terminator_for(label, *pos) = Some(Terminator::Jump(body_pos));
                *pos = self.new_basic_block(label, "following variable pattern");
            }
            ssa::PatternKind::Or(left, right) => {
                let continue_pos = self.new_basic_block(label, "`or` pattern satisfied");

                self.statements_for(label, *pos).push(Statement::Copy);
                self.gen_pattern(*left, input_ty, continue_pos, label, pos);

                self.statements_for(label, continue_pos)
                    .push(Statement::Drop);
                *self.terminator_for(label, continue_pos) = Some(Terminator::Jump(body_pos));

                self.gen_pattern(*right, input_ty, body_pos, label, pos);
            }
            ssa::PatternKind::Tuple(patterns) => {
                let tuple_tys = match input_ty {
                    Type::Tuple(tys) => tys,
                    _ => unreachable!(),
                };

                let else_pos = self.new_basic_block(label, "tuple pattern not satisfied");

                for (index, pattern) in patterns.into_iter().enumerate() {
                    let element_ty = &tuple_tys[index];

                    let next_pos = self.new_basic_block(label, "following tuple element");

                    self.statements_for(label, *pos).push(Statement::Copy);

                    self.statements_for(label, *pos).push(Statement::Expression(
                        element_ty.clone(),
                        Expression::TupleElement(index),
                    ));

                    self.gen_pattern(pattern, element_ty, next_pos, label, pos);

                    *self.terminator_for(label, *pos) = Some(Terminator::Jump(else_pos));

                    *pos = next_pos;
                }

                self.statements_for(label, *pos).push(Statement::Drop);

                *self.terminator_for(label, *pos) = Some(Terminator::Jump(body_pos));

                *pos = else_pos;
            }
            ssa::PatternKind::Destructure(fields) => {
                let (id, needs_deref) = match input_ty {
                    Type::Structure(id) => (id, false),
                    Type::StructureReference(id) => (id, true),
                    _ => unreachable!(),
                };

                let field_tys = self.structures.get(id).unwrap().clone();

                if needs_deref {
                    self.statements_for(label, *pos).push(Statement::Expression(
                        Type::Structure(*id),
                        Expression::Dereference,
                    ));
                }

                let else_pos = self.new_basic_block(label, "destructuring pattern not satisfied");

                for (index, field) in fields {
                    let field_ty = &field_tys[index.into_inner()];

                    let next_pos = self.new_basic_block(label, "following destructuring field");

                    self.statements_for(label, *pos).push(Statement::Copy);

                    self.statements_for(label, *pos).push(Statement::Expression(
                        field_ty.clone(),
                        Expression::StructureElement(index),
                    ));

                    self.gen_pattern(field, field_ty, next_pos, label, pos);

                    *self.terminator_for(label, *pos) = Some(Terminator::Jump(else_pos));

                    *pos = next_pos;
                }

                self.statements_for(label, *pos).push(Statement::Drop);

                *self.terminator_for(label, *pos) = Some(Terminator::Jump(body_pos));

                *pos = else_pos;
                self.statements_for(label, *pos).push(Statement::Drop);
            }
            ssa::PatternKind::Variant(discriminant, patterns) => {
                let (id, needs_deref) = match input_ty {
                    Type::Enumeration(id) => (id, false),
                    Type::EnumerationReference(id) => (id, true),
                    _ => unreachable!(),
                };

                let variant_tys =
                    self.enumerations.get(id).unwrap()[discriminant.into_inner()].clone();

                if needs_deref {
                    self.statements_for(label, *pos).push(Statement::Expression(
                        Type::Enumeration(*id),
                        Expression::Dereference,
                    ));
                }

                if patterns.is_empty() {
                    let else_pos = self.new_basic_block(label, "variant pattern not satisfied");

                    *self.terminator_for(label, *pos) =
                        Some(Terminator::If(discriminant, body_pos, else_pos));

                    *pos = else_pos;
                } else {
                    let element_pos = self.new_basic_block(label, "extracting variant element");
                    let else_pos = self.new_basic_block(label, "variant pattern not satisfied");

                    self.statements_for(label, *pos).push(Statement::Copy);

                    *self.terminator_for(label, *pos) =
                        Some(Terminator::If(discriminant, element_pos, else_pos));

                    *pos = element_pos;

                    for (index, pattern) in patterns.into_iter().enumerate() {
                        let element_ty = &variant_tys[index];

                        let next_pos = self.new_basic_block(label, "following variant element");

                        self.statements_for(label, *pos).push(Statement::Copy);

                        self.statements_for(label, *pos).push(Statement::Expression(
                            element_ty.clone(),
                            Expression::VariantElement(discriminant, index),
                        ));

                        self.gen_pattern(pattern, element_ty, next_pos, label, pos);

                        *self.terminator_for(label, *pos) = Some(Terminator::Jump(else_pos));

                        *pos = next_pos;
                    }

                    self.statements_for(label, *pos).push(Statement::Drop);

                    *self.terminator_for(label, *pos) = Some(Terminator::Jump(body_pos));

                    *pos = else_pos;
                    self.statements_for(label, *pos).push(Statement::Drop);
                }
            }
        }
    }
}
