#![allow(
    clippy::too_many_arguments,
    clippy::type_complexity,
    clippy::new_without_default
)]

mod format;
mod ssa;

use crate::{
    analysis::typecheck, helpers::InternedString, parse::Span, Compiler, Label, Loader,
    MonomorphizedConstantId, VariableId,
};
use std::{
    collections::BTreeMap,
    os::raw::{c_int, c_uint},
};

pub mod abi {
    pub const RUNTIME: &str = "runtime";
}

#[derive(Debug, Clone)]
pub struct Program {
    pub labels: BTreeMap<Label, (Option<Span>, Vec<Statement>)>,
    pub entrypoint: Label,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Comment(String),
    Begin,
    End,
    Copy,
    Drop,
    Jump(Label),
    If(usize, Label),
    Unreachable,
    Initialize(VariableId),
    Value(Value),
    Call,
    TailCall,
    External(InternedString, InternedString, usize),
    Tuple(usize),
    Structure(usize),
    Variant(usize, usize),
    TupleElement(usize),
    StructureElement(usize),
    VariantElement(usize, usize),
}

#[derive(Debug, Clone)]
pub enum Value {
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
    Variable(VariableId),
    Constant(Label),
    Function(Label),
    Closure(Label),
}

impl<L: Loader> Compiler<L> {
    pub fn ir_from(&mut self, program: &typecheck::Program) -> Program {
        let program = self.convert_to_ssa(program);

        let mut gen = IrGen {
            compiler: self,
            constants: Default::default(),
            labels: Default::default(),
        };

        for id in program.constants.keys() {
            let label = gen.compiler.new_label();
            gen.constants.insert(*id, label);
        }

        for (id, constant) in program.constants {
            let mut label = *gen.constants.get(&id).unwrap();
            gen.initialize_label(label, constant.span);
            gen.gen_expr(constant, &mut label);
        }

        let entrypoint_label = gen.new_label(None);
        gen.gen_block(program.body, &mut entrypoint_label.clone());

        Program {
            labels: gen.labels,
            entrypoint: entrypoint_label,
        }
    }
}

struct IrGen<'a, L: Loader> {
    compiler: &'a mut Compiler<L>,
    constants: BTreeMap<MonomorphizedConstantId, Label>,
    labels: BTreeMap<Label, (Option<Span>, Vec<Statement>)>,
}

impl<L: Loader> IrGen<'_, L> {
    fn new_label(&mut self, span: Option<Span>) -> Label {
        let label = self.compiler.new_label();
        self.initialize_label(label, span);
        label
    }

    fn initialize_label(&mut self, label: Label, span: Option<Span>) {
        self.labels.insert(label, (span, Vec::new()));
    }

    fn statements_for(&mut self, label: Label) -> &mut Vec<Statement> {
        &mut self.labels.get_mut(&label).unwrap().1
    }
}

impl<L: Loader> IrGen<'_, L> {
    fn gen_expr(&mut self, expr: ssa::Expression, label: &mut Label) {
        match expr.kind {
            ssa::ExpressionKind::Marker => {
                self.statements_for(*label)
                    .push(Statement::Value(Value::Marker));
            }
            ssa::ExpressionKind::Variable(var) => {
                self.statements_for(*label)
                    .push(Statement::Value(Value::Variable(var)));
            }
            ssa::ExpressionKind::Text(text) => {
                self.statements_for(*label)
                    .push(Statement::Value(Value::Text(text)));
            }
            ssa::ExpressionKind::Number(number) => {
                self.statements_for(*label)
                    .push(Statement::Value(Value::Number(number)));
            }
            ssa::ExpressionKind::Integer(integer) => {
                self.statements_for(*label)
                    .push(Statement::Value(Value::Integer(integer)));
            }
            ssa::ExpressionKind::Natural(natural) => {
                self.statements_for(*label)
                    .push(Statement::Value(Value::Natural(natural)));
            }
            ssa::ExpressionKind::Byte(byte) => {
                self.statements_for(*label)
                    .push(Statement::Value(Value::Byte(byte)));
            }
            ssa::ExpressionKind::Signed(signed) => {
                self.statements_for(*label)
                    .push(Statement::Value(Value::Signed(signed)));
            }
            ssa::ExpressionKind::Unsigned(unsigned) => {
                self.statements_for(*label)
                    .push(Statement::Value(Value::Unsigned(unsigned)));
            }
            ssa::ExpressionKind::Float(float) => {
                self.statements_for(*label)
                    .push(Statement::Value(Value::Float(float)));
            }
            ssa::ExpressionKind::Double(double) => {
                self.statements_for(*label)
                    .push(Statement::Value(Value::Double(double)));
            }
            ssa::ExpressionKind::Block(exprs) => self.gen_block(exprs, label),
            ssa::ExpressionKind::Call(function, input) => {
                self.statements_for(*label)
                    .push(Statement::Comment(String::from("evaluate function")));
                self.gen_expr(*function, label);

                self.statements_for(*label)
                    .push(Statement::Comment(String::from("evaluate input")));
                self.gen_expr(*input, label);

                self.statements_for(*label).push(if expr.tail {
                    Statement::TailCall
                } else {
                    Statement::Call
                });
            }
            ssa::ExpressionKind::Function(pattern, body, captures) => {
                let function_label = self.new_label(pattern.span);

                {
                    let mut function_label = function_label;
                    let mut body_label = self.new_label(body.span);

                    self.statements_for(function_label)
                        .push(Statement::Comment(String::from("function pattern")));

                    self.gen_pattern(pattern, body_label, &mut function_label);

                    self.statements_for(function_label)
                        .push(Statement::Unreachable);

                    self.statements_for(body_label)
                        .push(Statement::Comment(String::from("function body")));

                    self.gen_expr(*body, &mut body_label);
                }

                let value = if captures.is_empty() {
                    Value::Function(function_label)
                } else {
                    Value::Closure(function_label)
                };

                self.statements_for(*label).push(Statement::Value(value));
            }
            ssa::ExpressionKind::When(input, arms) => {
                self.statements_for(*label)
                    .push(Statement::Comment(String::from("evaluate `when` input")));
                self.gen_expr(*input, label);

                self.statements_for(*label)
                    .push(Statement::Comment(String::from("`when` arms")));
                self.gen_when(arms, label);
            }
            ssa::ExpressionKind::External(lib, identifier, exprs) => {
                let count = exprs.len();

                for (index, expr) in exprs.into_iter().enumerate() {
                    self.statements_for(*label)
                        .push(Statement::Comment(format!("input {} to `external`", index)));

                    self.gen_expr(expr, label);
                }

                self.statements_for(*label)
                    .push(Statement::External(lib, identifier, count));
            }
            ssa::ExpressionKind::Structure(exprs) => {
                let count = exprs.len();

                for (index, expr) in exprs.into_iter().enumerate() {
                    self.statements_for(*label)
                        .push(Statement::Comment(format!("input {} to structure", index)));

                    self.gen_expr(expr, label);
                }

                self.statements_for(*label)
                    .push(Statement::Structure(count));
            }
            ssa::ExpressionKind::Tuple(exprs) => {
                let count = exprs.len();

                for (index, expr) in exprs.into_iter().enumerate() {
                    self.statements_for(*label)
                        .push(Statement::Comment(format!("input {} to tuple", index)));

                    self.gen_expr(expr, label);
                }

                self.statements_for(*label).push(Statement::Tuple(count));
            }
            ssa::ExpressionKind::Variant(discriminant, exprs) => {
                let count = exprs.len();

                for (index, expr) in exprs.into_iter().enumerate() {
                    self.statements_for(*label)
                        .push(Statement::Comment(format!("input {} to variant", index)));

                    self.gen_expr(expr, label);
                }

                self.statements_for(*label)
                    .push(Statement::Variant(discriminant, count));
            }
            ssa::ExpressionKind::Constant(id) => {
                let id = *self.constants.get(&id).unwrap();
                self.statements_for(*label)
                    .push(Statement::Value(Value::Constant(id)));
            }
        }
    }

    fn gen_block(&mut self, exprs: Vec<ssa::Expression>, label: &mut Label) {
        if exprs.is_empty() {
            self.statements_for(*label).push(Statement::Tuple(0));
        } else {
            self.statements_for(*label).push(Statement::Begin);

            for (index, expr) in exprs.into_iter().enumerate() {
                if index != 0 {
                    self.statements_for(*label).push(Statement::Drop);
                }

                self.statements_for(*label)
                    .push(Statement::Comment(format!("statement {} in block", index)));

                self.gen_expr(expr, label);
            }

            self.statements_for(*label).push(Statement::End);
        }
    }

    fn gen_when(&mut self, arms: Vec<ssa::Arm>, label: &mut Label) {
        if arms.is_empty() {
            self.statements_for(*label).push(Statement::Drop);
            return;
        }

        let continue_label = self.new_label(None);

        for (index, arm) in arms.into_iter().enumerate() {
            let mut body_label = self.new_label(arm.body.span);

            self.statements_for(*label)
                .push(Statement::Comment(format!("arm {} in `when`", index)));
            self.statements_for(*label).push(Statement::Copy);
            self.gen_pattern(arm.pattern, body_label, label);

            self.statements_for(body_label).push(Statement::Drop);
            self.gen_expr(arm.body, &mut body_label);
            self.statements_for(body_label)
                .push(Statement::Jump(continue_label));
        }

        self.statements_for(*label)
            .push(Statement::Comment(String::from("end of `when`")));

        self.statements_for(*label).push(Statement::Unreachable);

        *label = continue_label;
    }

    fn gen_pattern(&mut self, pattern: ssa::Pattern, body_label: Label, label: &mut Label) {
        macro_rules! match_number {
            ($kind:ident($n:expr), $comparison:literal) => {{
                self.statements_for(*label)
                    .push(Statement::Value(Value::$kind($n)));

                self.statements_for(*label).push(Statement::External(
                    InternedString::new(abi::RUNTIME),
                    InternedString::new($comparison),
                    2,
                ));

                self.statements_for(*label)
                    .push(Statement::If(1, body_label));
            }};
        }

        match pattern.kind {
            ssa::PatternKind::Wildcard => {
                self.statements_for(*label).push(Statement::Drop);
                self.statements_for(*label)
                    .push(Statement::Jump(body_label));
            }
            ssa::PatternKind::Number(number) => {
                match_number!(Number(number), "number-equality");
            }
            ssa::PatternKind::Integer(integer) => {
                match_number!(Integer(integer), "integer-equality");
            }
            ssa::PatternKind::Natural(natural) => {
                match_number!(Natural(natural), "natural-equality");
            }
            ssa::PatternKind::Byte(byte) => {
                match_number!(Byte(byte), "byte-equality");
            }
            ssa::PatternKind::Signed(signed) => {
                match_number!(Signed(signed), "signed-equality");
            }
            ssa::PatternKind::Unsigned(unsigned) => {
                match_number!(Unsigned(unsigned), "unsigned-equality");
            }
            ssa::PatternKind::Float(float) => {
                match_number!(Float(float), "float-equality");
            }
            ssa::PatternKind::Double(double) => {
                match_number!(Double(double), "double-equality");
            }
            ssa::PatternKind::Text(text) => {
                self.statements_for(*label)
                    .push(Statement::Value(Value::Text(text)));

                self.statements_for(*label).push(Statement::External(
                    InternedString::new(abi::RUNTIME),
                    InternedString::new("text-equality"),
                    2,
                ));

                self.statements_for(*label)
                    .push(Statement::If(1, body_label));
            }
            ssa::PatternKind::Variable(var) => {
                self.statements_for(*label).push(Statement::Initialize(var));
                self.statements_for(*label)
                    .push(Statement::Jump(body_label));
            }
            ssa::PatternKind::Or(left, right) => {
                let continue_label = self.new_label(None);

                self.statements_for(*label).push(Statement::Copy);
                self.gen_pattern(*left, continue_label, label);

                self.statements_for(continue_label).push(Statement::Drop);
                self.statements_for(continue_label)
                    .push(Statement::Jump(body_label));

                self.gen_pattern(*right, body_label, label);
            }
            ssa::PatternKind::Where(pattern, condition) => {
                let condition_label = self.new_label(condition.span);
                let else_label = self.new_label(None);

                self.gen_pattern(*pattern, condition_label, label);
                self.statements_for(*label)
                    .push(Statement::Jump(else_label));

                *label = condition_label;
                self.gen_expr(*condition, label);
                self.statements_for(*label)
                    .push(Statement::If(1, body_label));
                self.statements_for(*label)
                    .push(Statement::Jump(else_label));

                *label = else_label;
            }
            ssa::PatternKind::Tuple(patterns) => {
                let else_label = self.new_label(None);

                let mut patterns = patterns.into_iter().enumerate().peekable();
                while let Some((index, pattern)) = patterns.next() {
                    let next_label =
                        self.new_label(patterns.peek().and_then(|(_, pattern)| pattern.span));

                    self.statements_for(*label)
                        .push(Statement::Comment(format!("pattern {} in tuple", index)));

                    self.statements_for(*label).push(Statement::Copy);

                    self.statements_for(*label)
                        .push(Statement::TupleElement(index));

                    self.gen_pattern(pattern, next_label, label);

                    self.statements_for(*label)
                        .push(Statement::Jump(else_label));

                    *label = next_label;
                }

                self.statements_for(*label)
                    .push(Statement::Comment(String::from("end of tuple pattern")));

                self.statements_for(*label).push(Statement::Drop);

                self.statements_for(*label)
                    .push(Statement::Jump(body_label));

                *label = else_label;
                self.statements_for(*label).push(Statement::Drop);
            }
            ssa::PatternKind::Destructure(fields) => {
                let else_label = self.new_label(None);

                let mut fields = fields.into_iter().peekable();
                while let Some((index, field)) = fields.next() {
                    let next_label =
                        self.new_label(fields.peek().and_then(|(_, field)| field.span));

                    self.statements_for(*label)
                        .push(Statement::Comment(format!("field {} in structure", index)));

                    self.statements_for(*label).push(Statement::Copy);

                    self.statements_for(*label)
                        .push(Statement::StructureElement(index));

                    self.gen_pattern(field, next_label, label);

                    self.statements_for(*label)
                        .push(Statement::Jump(else_label));

                    *label = next_label;
                }

                self.statements_for(*label)
                    .push(Statement::Comment(String::from("end of structure pattern")));

                self.statements_for(*label).push(Statement::Drop);

                self.statements_for(*label)
                    .push(Statement::Jump(body_label));

                *label = else_label;
                self.statements_for(*label).push(Statement::Drop);
            }
            ssa::PatternKind::Variant(discriminant, patterns) => {
                let element_label = self.new_label(None);
                let else_label = self.new_label(None);

                self.statements_for(*label).push(Statement::Copy);

                self.statements_for(*label)
                    .push(Statement::If(discriminant, element_label));

                self.statements_for(*label)
                    .push(Statement::Jump(else_label));

                *label = element_label;

                let mut patterns = patterns.into_iter().enumerate().peekable();
                while let Some((index, pattern)) = patterns.next() {
                    let next_label =
                        self.new_label(patterns.peek().and_then(|(_, pattern)| pattern.span));

                    self.statements_for(*label)
                        .push(Statement::Comment(format!("pattern {} in variant", index)));

                    self.statements_for(*label).push(Statement::Copy);

                    self.statements_for(*label)
                        .push(Statement::VariantElement(discriminant, index));

                    self.gen_pattern(pattern, next_label, label);

                    self.statements_for(*label)
                        .push(Statement::Jump(else_label));

                    *label = next_label;
                }

                self.statements_for(*label)
                    .push(Statement::Comment(String::from("end of variant pattern")));

                self.statements_for(*label).push(Statement::Drop);

                self.statements_for(*label)
                    .push(Statement::Jump(body_label));

                *label = else_label;
                self.statements_for(*label).push(Statement::Drop);
            }
        }
    }
}
