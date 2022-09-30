use crate::{
    analysis, helpers::InternedString, Compiler, Loader, MonomorphizedConstantId, VariableId,
};
use std::{
    collections::BTreeMap,
    os::raw::{c_int, c_uint},
};

#[derive(Debug, Clone)]
pub struct Program {
    pub constants: BTreeMap<MonomorphizedConstantId, Expression>,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Marker,
    Variable(VariableId),
    Text(InternedString),
    Block(Vec<Expression>),
    Call(Box<Expression>, Box<Expression>),
    Function(Pattern, Box<Expression>),
    When(Box<Expression>, Vec<Arm>),
    External(InternedString, InternedString, Vec<Expression>),
    Structure(Vec<Expression>),
    Variant(usize, Vec<Expression>),
    Tuple(Vec<Expression>),
    Number(rust_decimal::Decimal),
    Integer(i64),
    Natural(u64),
    Byte(u8),
    Signed(c_int),
    Unsigned(c_uint),
    Float(f32),
    Double(f64),
    Constant(MonomorphizedConstantId),
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub pattern: Pattern,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Variable(VariableId),
    Text(InternedString),
    Number(rust_decimal::Decimal),
    Integer(i64),
    Natural(u64),
    Byte(u8),
    Signed(c_int),
    Unsigned(c_uint),
    Float(f32),
    Double(f64),
    Tuple(Vec<Pattern>),
    Destructure(BTreeMap<usize, Pattern>),
    Variant(usize, Vec<Pattern>),
    Or(Box<Pattern>, Box<Pattern>),
    Where(Box<Pattern>, Box<Expression>),
}

impl<L: Loader> Compiler<L> {
    pub(super) fn prepare(&mut self, program: &analysis::Program) -> Program {
        Program {
            constants: program
                .declarations
                .monomorphized_constants
                .iter()
                .map(|(id, (_, _, _, constant))| (*id, self.prepare_expr(&constant.value)))
                .collect(),
            body: self.prepare_block(&program.body),
        }
    }

    fn prepare_expr(&mut self, expr: &analysis::Expression) -> Expression {
        match &expr.kind {
            analysis::ExpressionKind::Marker => Expression::Marker,
            analysis::ExpressionKind::Variable(var) => Expression::Variable(*var),
            analysis::ExpressionKind::Text(text) => Expression::Text(*text),
            analysis::ExpressionKind::Block(exprs) => self.prepare_block(exprs),
            analysis::ExpressionKind::Call(func, input) => Expression::Call(
                Box::new(self.prepare_expr(func)),
                Box::new(self.prepare_expr(input)),
            ),
            analysis::ExpressionKind::Function(pattern, body) => Expression::Function(
                self.prepare_pattern(pattern),
                Box::new(self.prepare_expr(body)),
            ),
            analysis::ExpressionKind::When(input, arms) => Expression::When(
                Box::new(self.prepare_expr(input)),
                arms.iter().map(|arm| self.prepare_arm(arm)).collect(),
            ),
            analysis::ExpressionKind::External(abi, identifier, inputs) => Expression::External(
                *abi,
                *identifier,
                inputs.iter().map(|expr| self.prepare_expr(expr)).collect(),
            ),
            analysis::ExpressionKind::Initialize(_, _) => {
                unreachable!("variable initialization is handled specially by prepare_block")
            }
            analysis::ExpressionKind::Structure(exprs) => {
                Expression::Structure(exprs.iter().map(|expr| self.prepare_expr(expr)).collect())
            }
            analysis::ExpressionKind::Variant(discriminant, exprs) => Expression::Variant(
                *discriminant,
                exprs.iter().map(|expr| self.prepare_expr(expr)).collect(),
            ),
            analysis::ExpressionKind::Tuple(exprs) => {
                Expression::Tuple(exprs.iter().map(|expr| self.prepare_expr(expr)).collect())
            }
            analysis::ExpressionKind::Number(number) => Expression::Number(*number),
            analysis::ExpressionKind::Integer(integer) => Expression::Integer(*integer),
            analysis::ExpressionKind::Natural(natural) => Expression::Natural(*natural),
            analysis::ExpressionKind::Byte(byte) => Expression::Byte(*byte),
            analysis::ExpressionKind::Signed(signed) => Expression::Signed(*signed),
            analysis::ExpressionKind::Unsigned(unsigned) => Expression::Unsigned(*unsigned),
            analysis::ExpressionKind::Float(float) => Expression::Float(*float),
            analysis::ExpressionKind::Double(double) => Expression::Double(*double),
            analysis::ExpressionKind::Constant(constant) => Expression::Constant(*constant),
        }
    }

    fn prepare_block(&mut self, exprs: &[analysis::Expression]) -> Expression {
        let mut result = Vec::new();

        for (index, expr) in exprs.iter().enumerate() {
            match &expr.kind {
                analysis::ExpressionKind::Initialize(pattern, value) => {
                    let remaining = &exprs[(index + 1)..];

                    result.push(Expression::When(
                        Box::new(self.prepare_expr(value)),
                        vec![Arm {
                            pattern: self.prepare_pattern(pattern),
                            body: self.prepare_block(remaining),
                        }],
                    ));

                    break;
                }
                _ => result.push(self.prepare_expr(expr)),
            }
        }

        if result.len() == 1 {
            result.pop().unwrap()
        } else {
            Expression::Block(result)
        }
    }

    fn prepare_arm(&mut self, arm: &analysis::Arm) -> Arm {
        Arm {
            pattern: self.prepare_pattern(&arm.pattern),
            body: self.prepare_expr(&arm.body),
        }
    }

    fn prepare_pattern(&mut self, pattern: &analysis::Pattern) -> Pattern {
        match &pattern.kind {
            analysis::PatternKind::Wildcard => Pattern::Wildcard,
            analysis::PatternKind::Variable(var) => Pattern::Variable(*var),
            analysis::PatternKind::Text(text) => Pattern::Text(*text),
            analysis::PatternKind::Number(number) => Pattern::Number(*number),
            analysis::PatternKind::Integer(integer) => Pattern::Integer(*integer),
            analysis::PatternKind::Natural(natural) => Pattern::Natural(*natural),
            analysis::PatternKind::Byte(byte) => Pattern::Byte(*byte),
            analysis::PatternKind::Signed(signed) => Pattern::Signed(*signed),
            analysis::PatternKind::Unsigned(unsigned) => Pattern::Unsigned(*unsigned),
            analysis::PatternKind::Float(float) => Pattern::Float(*float),
            analysis::PatternKind::Double(double) => Pattern::Double(*double),
            analysis::PatternKind::Tuple(patterns) => Pattern::Tuple(
                patterns
                    .iter()
                    .map(|pattern| self.prepare_pattern(pattern))
                    .collect(),
            ),
            analysis::PatternKind::Destructure(fields) => Pattern::Destructure(
                fields
                    .iter()
                    .map(|(index, pattern)| (*index, self.prepare_pattern(pattern)))
                    .collect(),
            ),
            analysis::PatternKind::Variant(discriminant, values) => Pattern::Variant(
                *discriminant,
                values
                    .iter()
                    .map(|pattern| self.prepare_pattern(pattern))
                    .collect(),
            ),
            analysis::PatternKind::Or(left, right) => Pattern::Or(
                Box::new(self.prepare_pattern(left)),
                Box::new(self.prepare_pattern(right)),
            ),
            analysis::PatternKind::Where(pattern, condition) => Pattern::Where(
                Box::new(self.prepare_pattern(pattern)),
                Box::new(self.prepare_expr(condition)),
            ),
        }
    }
}
