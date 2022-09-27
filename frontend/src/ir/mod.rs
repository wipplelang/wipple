#![allow(
    clippy::too_many_arguments,
    clippy::type_complexity,
    clippy::new_without_default
)]

mod format;
mod prepare;

use crate::{
    analysis::typecheck, helpers::InternedString, Compiler, Label, Loader, MonomorphizedConstantId,
    VariableId,
};
use itertools::Itertools;
use std::{
    collections::BTreeMap,
    os::raw::{c_int, c_uint},
};

pub mod abi {
    pub const RUNTIME: &str = "runtime";
}

#[derive(Debug, Clone)]
pub struct Program {
    pub labels: BTreeMap<Label, Vec<Statement>>,
    pub entrypoint: Label,
}

#[derive(Debug, Clone)]
pub enum Statement {
    /// Duplicate the top value on the stack.
    Copy,
    /// Remove the top value on the stack.
    Drop,
    /// Take the top value on the stack and assign it to a variable in the
    /// current scope.
    Initialize(VariableId),
    /// Go to the provided label, maintaining the current stack and variable
    /// scope. When the label finishes executing, return to the caller.
    Goto(Label, bool),
    /// Go to the provided label with the top of the stack as input, maintaining
    /// the current stack and variable scope. When the label finishes executing,
    /// return to the caller.
    Sub(Label, bool),
    /// Pop a value off the stack. If this value is equivalent to the value
    /// produced by [`Statement::Variant(1, 0)`], go to the first label.
    /// Otherwise, go to the second label.
    If(Label, Label),
    /// Push a marker value to the stack.
    Marker,
    /// Treat a label as a closure and push it to the stack.
    Closure(Label),
    /// Retrieve a variable from the current scope and push its value to the
    /// stack.
    Variable(VariableId),
    /// Treat a label as a constant, initialize it if needed, and push its value
    /// to the stack.
    Constant(Label),
    /// Push a number to the stack.
    Number(rust_decimal::Decimal),
    /// Push a number to the stack.
    Integer(i64),
    /// Push a number to the stack.
    Natural(u64),
    /// Push a number to the stack.
    Byte(u8),
    /// Push a number to the stack.
    Signed(c_int),
    /// Push a number to the stack.
    Unsigned(c_uint),
    /// Push a number to the stack.
    Float(f32),
    /// Push a number to the stack.
    Double(f64),
    /// Push text to the stack.
    Text(InternedString),
    /// Take the top two values from the stack, the first being a function and
    /// the second being its input. Then call the function with the input and
    /// push the result to the stack.
    Call(bool),
    /// Call an external function with the top *n* values on the stack and push
    /// the result to the stack.
    External(InternedString, InternedString, usize),
    /// Push a tuple to the stack with the top *n* values on the stack.
    Tuple(usize),
    /// Push a structure to the stack with the top *n* values on the stack.
    Structure(usize),
    /// Push a variant to the stack with the top *n* values on the stack.
    Variant(usize, usize),
    /// Retrieve the *n*th element from the tuple on the top of the stack.
    TupleElement(usize),
    /// Retrieve the *n*th element from the structure on the top of the stack.
    StructureElement(usize),
    /// Retrieve the *n*th element from the variant on the top of the stack.
    VariantElement(usize, usize),
    /// Take the top value from the stack and check if its discriminant is equal
    /// to the one provided. Executes [`Statement::Variant(1, 0)`] if they are
    /// equal, [`Statement::Variant(0, 0)`] otherwise.
    CompareDiscriminants(usize),
}

impl<L: Loader> Compiler<L> {
    pub fn ir_from(&mut self, program: &typecheck::Program) -> Program {
        let program = self.prepare(program);

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
            let statements = gen.gen_expr(constant, true);
            let label = gen.constants.get(&id).unwrap();
            gen.labels.insert(*label, statements);
        }

        let statements = gen.gen_expr(program.body, true);
        let entrypoint_label = gen.compiler.new_label();
        gen.labels.insert(entrypoint_label, statements);

        Program {
            labels: gen.labels,
            entrypoint: entrypoint_label,
        }
    }
}

struct IrGen<'a, L: Loader> {
    compiler: &'a mut Compiler<L>,
    constants: BTreeMap<MonomorphizedConstantId, Label>,
    labels: BTreeMap<Label, Vec<Statement>>,
}

impl<L: Loader> IrGen<'_, L> {
    #[must_use]
    fn gen_expr(&mut self, expr: prepare::Expression, is_tail: bool) -> Vec<Statement> {
        let mut statements = Vec::new();

        macro_rules! sub {
            ($sub:ident, $statements:expr, $is_tail:expr) => {{
                let label = self.compiler.new_label();
                {
                    let statements = $statements;
                    self.labels.insert(label, statements);
                }

                statements.push(Statement::$sub(label, $is_tail));
            }};
        }

        match expr {
            prepare::Expression::Marker => {
                statements.push(Statement::Marker);
            }
            prepare::Expression::Variable(var) => {
                statements.push(Statement::Variable(var));
            }
            prepare::Expression::Text(text) => {
                statements.push(Statement::Text(text));
            }
            prepare::Expression::Number(number) => {
                statements.push(Statement::Number(number));
            }
            prepare::Expression::Integer(integer) => {
                statements.push(Statement::Integer(integer));
            }
            prepare::Expression::Natural(natural) => {
                statements.push(Statement::Natural(natural));
            }
            prepare::Expression::Byte(byte) => {
                statements.push(Statement::Byte(byte));
            }
            prepare::Expression::Signed(signed) => {
                statements.push(Statement::Signed(signed));
            }
            prepare::Expression::Unsigned(unsigned) => {
                statements.push(Statement::Unsigned(unsigned));
            }
            prepare::Expression::Float(float) => {
                statements.push(Statement::Float(float));
            }
            prepare::Expression::Double(double) => {
                statements.push(Statement::Double(double));
            }
            prepare::Expression::Block(exprs) => {
                if exprs.is_empty() {
                    statements.push(Statement::Tuple(0));
                } else {
                    let block_label = self.compiler.new_label();
                    {
                        let mut statements = Vec::new();

                        let count = exprs.len();
                        for (index, expr) in exprs.into_iter().enumerate() {
                            statements.append(&mut self.gen_expr(expr, index + 1 < count));
                        }

                        self.labels.insert(block_label, statements);
                    }

                    statements.push(Statement::Goto(block_label, is_tail));
                }
            }
            prepare::Expression::Call(function, input) => {
                sub!(Goto, self.gen_expr(*function, false), false);
                sub!(Goto, self.gen_expr(*input, false), false);
                statements.push(Statement::Call(is_tail));
            }
            prepare::Expression::Function(pattern, body) => {
                let body_label = self.compiler.new_label();
                {
                    let statements = self.gen_expr(*body, true);
                    self.labels.insert(body_label, statements);
                }

                let function_label = self.compiler.new_label();
                {
                    let statements = self.gen_pattern(pattern, body_label, None, false);
                    self.labels.insert(function_label, statements);
                }

                statements.push(Statement::Closure(function_label));
            }
            prepare::Expression::When(input, arms) => {
                statements.append(&mut self.gen_expr(*input, false));
                sub!(Sub, self.gen_when(arms, is_tail), is_tail);
            }
            prepare::Expression::External(lib, identifier, exprs) => {
                let count = exprs.len();

                for expr in exprs {
                    statements.append(&mut self.gen_expr(expr, false));
                }

                statements.push(Statement::External(lib, identifier, count));
            }
            prepare::Expression::Structure(exprs) => {
                let count = exprs.len();

                for expr in exprs {
                    statements.append(&mut self.gen_expr(expr, false));
                }

                statements.push(Statement::Structure(count));
            }
            prepare::Expression::Tuple(exprs) => {
                let count = exprs.len();

                for expr in exprs {
                    statements.append(&mut self.gen_expr(expr, false));
                }

                statements.push(Statement::Tuple(count));
            }
            prepare::Expression::Variant(discriminant, exprs) => {
                let count = exprs.len();

                for expr in exprs {
                    statements.append(&mut self.gen_expr(expr, false));
                }

                statements.push(Statement::Variant(discriminant, count));
            }
            prepare::Expression::Constant(id) => {
                let id = self.constants.get(&id).unwrap();
                statements.push(Statement::Constant(*id));
            }
        }

        statements
    }

    #[must_use]
    fn gen_when(&mut self, arms: Vec<prepare::Arm>, is_tail: bool) -> Vec<Statement> {
        let mut statements = Vec::new();
        let first_arm_label = self.compiler.new_label();

        statements.push(Statement::Sub(first_arm_label, is_tail));

        #[allow(clippy::needless_collect)] // self is borrowed inside the for loop
        let arms = std::iter::once(first_arm_label)
            .chain(std::iter::repeat_with(|| self.compiler.new_label()))
            .zip(arms)
            .map(Some)
            .chain(std::iter::once(None))
            .collect::<Vec<_>>();

        for (current, next) in arms.into_iter().tuple_windows() {
            let (current_arm_label, arm) = current.unwrap();
            let next_arm_label = next.map(|(label, _)| label);

            let arm_body_label = self.compiler.new_label();
            {
                let statements = self.gen_expr(arm.body, is_tail);
                self.labels.insert(arm_body_label, statements);
            }

            let mut statements = Vec::new();

            let copy = next_arm_label.is_some();
            if copy {
                statements.push(Statement::Copy);
            }

            statements.append(&mut self.gen_pattern(
                arm.pattern,
                arm_body_label,
                next_arm_label,
                copy,
            ));

            self.labels.insert(current_arm_label, statements);
        }

        statements
    }

    #[must_use]
    fn gen_pattern(
        &mut self,
        pattern: prepare::Pattern,
        body_label: Label,
        else_label: Option<Label>,
        sub: bool,
    ) -> Vec<Statement> {
        let mut statements = Vec::new();

        macro_rules! gen_tail_if {
            () => {{
                if let Some(else_label) = else_label {
                    statements.push(Statement::If(body_label, else_label));
                } else {
                    statements.push(Statement::Sub(body_label, true));
                }
            }};
        }

        macro_rules! match_number {
            ($kind:ident($n:expr), $comparison:literal) => {{
                statements.push(Statement::$kind($n));

                statements.push(Statement::External(
                    InternedString::new(abi::RUNTIME),
                    InternedString::new($comparison),
                    2,
                ));

                gen_tail_if!();
            }};
        }

        macro_rules! gen_goto_or_sub {
            ($label:expr, $sub:expr) => {{
                if $sub {
                    statements.push(Statement::Sub($label, true));
                } else {
                    statements.push(Statement::Goto($label, true));
                }
            }};
        }

        match pattern {
            prepare::Pattern::Wildcard => gen_goto_or_sub!(body_label, sub),
            prepare::Pattern::Number(number) => {
                match_number!(Number(number), "number-equality");
            }
            prepare::Pattern::Integer(integer) => {
                match_number!(Integer(integer), "integer-equality");
            }
            prepare::Pattern::Natural(natural) => {
                match_number!(Natural(natural), "natural-equality");
            }
            prepare::Pattern::Byte(byte) => {
                match_number!(Byte(byte), "byte-equality");
            }
            prepare::Pattern::Signed(signed) => {
                match_number!(Signed(signed), "signed-equality");
            }
            prepare::Pattern::Unsigned(unsigned) => {
                match_number!(Unsigned(unsigned), "unsigned-equality");
            }
            prepare::Pattern::Float(float) => {
                match_number!(Float(float), "float-equality");
            }
            prepare::Pattern::Double(double) => {
                match_number!(Double(double), "double-equality");
            }
            prepare::Pattern::Text(text) => {
                statements.push(Statement::Text(text));

                statements.push(Statement::External(
                    InternedString::new(abi::RUNTIME),
                    InternedString::new("text-equality"),
                    2,
                ));

                gen_tail_if!();
            }
            prepare::Pattern::Variable(var) => {
                statements.push(Statement::Initialize(var));
                gen_goto_or_sub!(body_label, sub);
            }
            prepare::Pattern::Or(left, right) => {
                statements.push(Statement::Copy);

                let right_label = self.compiler.new_label();
                {
                    let mut statements = Vec::new();
                    statements.push(Statement::Copy);
                    statements.append(&mut self.gen_pattern(*right, body_label, else_label, true));
                    self.labels.insert(right_label, statements);
                }

                statements.append(&mut self.gen_pattern(
                    *left,
                    body_label,
                    Some(right_label),
                    true,
                ));
            }
            prepare::Pattern::Where(pattern, condition) => {
                let condition_label = self.compiler.new_label();
                {
                    let mut statements = Vec::new();
                    statements.push(Statement::Copy);
                    statements.append(&mut self.gen_expr(*condition, true));

                    statements.push(Statement::If(
                        body_label,
                        else_label.expect("`where` patterns are never exhaustive"),
                    ));

                    self.labels.insert(condition_label, statements);
                }

                statements.append(&mut self.gen_pattern(
                    *pattern,
                    condition_label,
                    else_label,
                    true,
                ));
            }
            prepare::Pattern::Tuple(patterns) => {
                let (first_pattern_label, sub) = if patterns.is_empty() {
                    (body_label, sub)
                } else {
                    (self.compiler.new_label(), true)
                };

                gen_goto_or_sub!(first_pattern_label, sub);

                #[allow(clippy::needless_collect)] // self is borrowed inside the for loop
                let patterns = std::iter::once(first_pattern_label)
                    .chain(std::iter::repeat_with(|| self.compiler.new_label()))
                    .zip(patterns)
                    .map(Some)
                    .chain(std::iter::once(None))
                    .collect::<Vec<_>>();

                for (index, (current, next)) in patterns.into_iter().tuple_windows().enumerate() {
                    let (current_pattern_label, pattern) = current.unwrap();
                    let next_pattern_label = next.map(|(label, _)| label);

                    let mut statements = Vec::new();
                    statements.push(Statement::Copy);
                    statements.push(Statement::TupleElement(index));

                    statements.append(&mut self.gen_pattern(
                        pattern,
                        next_pattern_label.unwrap_or(body_label),
                        else_label,
                        true,
                    ));

                    self.labels.insert(current_pattern_label, statements);
                }
            }
            prepare::Pattern::Destructure(fields) => {
                let (first_pattern_label, sub) = if fields.is_empty() {
                    (body_label, sub)
                } else {
                    (self.compiler.new_label(), true)
                };

                gen_goto_or_sub!(first_pattern_label, sub);

                #[allow(clippy::needless_collect)] // self is borrowed inside the for loop
                let fields = std::iter::once(first_pattern_label)
                    .chain(std::iter::repeat_with(|| self.compiler.new_label()))
                    .zip(fields)
                    .map(Some)
                    .chain(std::iter::once(None))
                    .collect::<Vec<_>>();

                for (current, next) in fields.into_iter().tuple_windows() {
                    let (current_pattern_label, (index, pattern)) = current.unwrap();
                    let next_pattern_label = next.map(|(label, _)| label);

                    let mut statements = Vec::new();
                    statements.push(Statement::Copy);
                    statements.push(Statement::StructureElement(index));

                    statements.append(&mut self.gen_pattern(
                        pattern,
                        next_pattern_label.unwrap_or(body_label),
                        else_label,
                        true,
                    ));

                    self.labels.insert(current_pattern_label, statements);
                }
            }
            prepare::Pattern::Variant(discriminant, patterns) => {
                let (first_pattern_label, sub) = if patterns.is_empty() {
                    statements.push(Statement::CompareDiscriminants(discriminant));
                    (body_label, sub)
                } else {
                    statements.push(Statement::Copy);
                    statements.push(Statement::CompareDiscriminants(discriminant));
                    (self.compiler.new_label(), true)
                };

                if let Some(else_label) = else_label {
                    statements.push(Statement::If(first_pattern_label, else_label));
                } else {
                    statements.push(Statement::Drop);
                    gen_goto_or_sub!(first_pattern_label, sub);
                }

                #[allow(clippy::needless_collect)] // self is borrowed inside the for loop
                let patterns = std::iter::once(first_pattern_label)
                    .chain(std::iter::repeat_with(|| self.compiler.new_label()))
                    .zip(patterns)
                    .map(Some)
                    .chain(std::iter::once(None))
                    .collect::<Vec<_>>();

                for (index, (current, next)) in patterns.into_iter().tuple_windows().enumerate() {
                    let (current_pattern_label, pattern) = current.unwrap();
                    let next_pattern_label = next.map(|(label, _)| label);

                    let mut statements = Vec::new();
                    statements.push(Statement::Copy);
                    statements.push(Statement::VariantElement(discriminant, index));

                    statements.append(&mut self.gen_pattern(
                        pattern,
                        next_pattern_label.unwrap_or(body_label),
                        else_label,
                        true,
                    ));

                    self.labels.insert(current_pattern_label, statements);
                }
            }
        }

        statements
    }
}
