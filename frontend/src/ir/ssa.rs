use crate::{
    analysis, helpers::InternedString, parse::Span, Compiler, Loader, MonomorphizedConstantId,
    VariableId,
};
use std::{
    collections::BTreeMap,
    os::raw::{c_int, c_uint},
};

#[derive(Debug, Clone)]
pub struct Program {
    pub constants: BTreeMap<MonomorphizedConstantId, Expression>,
    pub body: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub span: Option<Span>,
    pub tail: bool,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Marker,
    Variable(VariableId),
    Text(InternedString),
    Block(Vec<Expression>),
    Call(Box<Expression>, Box<Expression>),
    Function(Pattern, Box<Expression>, analysis::lower::CaptureList),
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
    pub span: Option<Span>,
    pub pattern: Pattern,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub span: Option<Span>,
    pub kind: PatternKind,
}

#[derive(Debug, Clone)]
pub enum PatternKind {
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
    pub(super) fn convert_to_ssa(&mut self, program: &analysis::Program) -> Program {
        Program {
            constants: program
                .declarations
                .monomorphized_constants
                .iter()
                .map(|(id, (_, _, _, constant))| {
                    (*id, self.convert_expr_to_ssa(&constant.value, true))
                })
                .collect(),
            body: self.convert_block_to_ssa(&program.body, true),
        }
    }

    fn convert_expr_to_ssa(&mut self, expr: &analysis::Expression, tail: bool) -> Expression {
        Expression {
            span: Some(expr.span),
            tail,
            kind: match &expr.kind {
                analysis::ExpressionKind::Marker => ExpressionKind::Marker,
                analysis::ExpressionKind::Variable(var) => ExpressionKind::Variable(*var),
                analysis::ExpressionKind::Text(text) => ExpressionKind::Text(*text),
                analysis::ExpressionKind::Block(exprs) => {
                    ExpressionKind::Block(self.convert_block_to_ssa(exprs, tail))
                }
                analysis::ExpressionKind::Call(func, input) => ExpressionKind::Call(
                    Box::new(self.convert_expr_to_ssa(func, false)),
                    Box::new(self.convert_expr_to_ssa(input, false)),
                ),
                analysis::ExpressionKind::Function(pattern, body, captures) => {
                    ExpressionKind::Function(
                        self.convert_pattern_to_ssa(pattern),
                        Box::new(self.convert_expr_to_ssa(body, tail)),
                        captures.clone(),
                    )
                }
                analysis::ExpressionKind::When(input, arms) => ExpressionKind::When(
                    Box::new(self.convert_expr_to_ssa(input, false)),
                    arms.iter()
                        .map(|arm| self.convert_arm_to_ssa(arm, tail))
                        .collect(),
                ),
                analysis::ExpressionKind::External(abi, identifier, inputs) => {
                    ExpressionKind::External(
                        *abi,
                        *identifier,
                        inputs
                            .iter()
                            .map(|expr| self.convert_expr_to_ssa(expr, false))
                            .collect(),
                    )
                }
                analysis::ExpressionKind::Initialize(_, _) => {
                    unreachable!(
                        "variable initialization is handled specially by convert_block_to_ssa"
                    )
                }
                analysis::ExpressionKind::Structure(exprs) => ExpressionKind::Structure(
                    exprs
                        .iter()
                        .map(|expr| self.convert_expr_to_ssa(expr, false))
                        .collect(),
                ),
                analysis::ExpressionKind::Variant(discriminant, exprs) => ExpressionKind::Variant(
                    *discriminant,
                    exprs
                        .iter()
                        .map(|expr| self.convert_expr_to_ssa(expr, false))
                        .collect(),
                ),
                analysis::ExpressionKind::Tuple(exprs) => ExpressionKind::Tuple(
                    exprs
                        .iter()
                        .map(|expr| self.convert_expr_to_ssa(expr, false))
                        .collect(),
                ),
                analysis::ExpressionKind::Number(number) => ExpressionKind::Number(*number),
                analysis::ExpressionKind::Integer(integer) => ExpressionKind::Integer(*integer),
                analysis::ExpressionKind::Natural(natural) => ExpressionKind::Natural(*natural),
                analysis::ExpressionKind::Byte(byte) => ExpressionKind::Byte(*byte),
                analysis::ExpressionKind::Signed(signed) => ExpressionKind::Signed(*signed),
                analysis::ExpressionKind::Unsigned(unsigned) => ExpressionKind::Unsigned(*unsigned),
                analysis::ExpressionKind::Float(float) => ExpressionKind::Float(*float),
                analysis::ExpressionKind::Double(double) => ExpressionKind::Double(*double),
                analysis::ExpressionKind::Constant(constant) => ExpressionKind::Constant(*constant),
            },
        }
    }

    fn convert_block_to_ssa(
        &mut self,
        exprs: &[analysis::Expression],
        tail: bool,
    ) -> Vec<Expression> {
        let mut result = Vec::new();
        let count = exprs.len();
        for (index, expr) in exprs.iter().enumerate() {
            let tail = tail && index + 1 == count;

            if let analysis::ExpressionKind::Initialize(pattern, value) = &expr.kind {
                let remaining = &exprs[(index + 1)..];

                result.push(Expression {
                    span: Some(expr.span),
                    tail,
                    kind: ExpressionKind::When(
                        Box::new(self.convert_expr_to_ssa(value, false)),
                        vec![Arm {
                            span: Some(pattern.span),
                            pattern: self.convert_pattern_to_ssa(pattern),
                            body: Expression {
                                tail,
                                span: remaining.first().map(|expr| {
                                    expr.span.with_end(remaining.last().unwrap().span.end)
                                }),
                                kind: ExpressionKind::Block(
                                    self.convert_block_to_ssa(remaining, tail),
                                ),
                            },
                        }],
                    ),
                });

                break;
            } else {
                result.push(self.convert_expr_to_ssa(expr, tail));
            }
        }

        result
    }

    fn convert_arm_to_ssa(&mut self, arm: &analysis::Arm, tail: bool) -> Arm {
        Arm {
            span: Some(arm.span),
            pattern: self.convert_pattern_to_ssa(&arm.pattern),
            body: self.convert_expr_to_ssa(&arm.body, tail),
        }
    }

    fn convert_pattern_to_ssa(&mut self, pattern: &analysis::Pattern) -> Pattern {
        Pattern {
            span: Some(pattern.span),
            kind: match &pattern.kind {
                analysis::PatternKind::Wildcard => PatternKind::Wildcard,
                analysis::PatternKind::Variable(var) => PatternKind::Variable(*var),
                analysis::PatternKind::Text(text) => PatternKind::Text(*text),
                analysis::PatternKind::Number(number) => PatternKind::Number(*number),
                analysis::PatternKind::Integer(integer) => PatternKind::Integer(*integer),
                analysis::PatternKind::Natural(natural) => PatternKind::Natural(*natural),
                analysis::PatternKind::Byte(byte) => PatternKind::Byte(*byte),
                analysis::PatternKind::Signed(signed) => PatternKind::Signed(*signed),
                analysis::PatternKind::Unsigned(unsigned) => PatternKind::Unsigned(*unsigned),
                analysis::PatternKind::Float(float) => PatternKind::Float(*float),
                analysis::PatternKind::Double(double) => PatternKind::Double(*double),
                analysis::PatternKind::Tuple(patterns) => PatternKind::Tuple(
                    patterns
                        .iter()
                        .map(|pattern| self.convert_pattern_to_ssa(pattern))
                        .collect(),
                ),
                analysis::PatternKind::Destructure(fields) => PatternKind::Destructure(
                    fields
                        .iter()
                        .map(|(index, pattern)| (*index, self.convert_pattern_to_ssa(pattern)))
                        .collect(),
                ),
                analysis::PatternKind::Variant(discriminant, values) => PatternKind::Variant(
                    *discriminant,
                    values
                        .iter()
                        .map(|pattern| self.convert_pattern_to_ssa(pattern))
                        .collect(),
                ),
                analysis::PatternKind::Or(left, right) => PatternKind::Or(
                    Box::new(self.convert_pattern_to_ssa(left)),
                    Box::new(self.convert_pattern_to_ssa(right)),
                ),
                analysis::PatternKind::Where(pattern, condition) => PatternKind::Where(
                    Box::new(self.convert_pattern_to_ssa(pattern)),
                    Box::new(self.convert_expr_to_ssa(condition, false)),
                ),
            },
        }
    }
}
