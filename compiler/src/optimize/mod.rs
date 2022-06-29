use crate::{
    compile::{self, typecheck},
    helpers::InternedString,
    parse::Span,
    Compiler, MonomorphizedConstantId, VariableId,
};
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

mod no_ops;
mod tail_calls;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Program {
    pub constants: BTreeMap<MonomorphizedConstantId, Expression>,
    pub body: Vec<Expression>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Expression {
    pub span: Option<Span>,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExpressionKind {
    Marker,
    Variable(VariableId),
    Constant(MonomorphizedConstantId),
    Text(InternedString),
    Number(Decimal),
    Block(Vec<Expression>),
    Call(Box<Expression>, Box<Expression>),
    Function(Pattern, Box<Expression>),
    When(Box<Expression>, Vec<Arm>),
    External(
        InternedString,
        InternedString,
        Vec<(Expression, typecheck::Type)>,
        typecheck::Type,
    ),
    Initialize(Pattern, Box<Expression>),
    Structure(Vec<Expression>),
    Variant(usize, Vec<Expression>),
    ListLiteral(Vec<Expression>),
    Return(Box<Expression>),
    Loop(Box<Expression>),
    Break(Box<Expression>),
    Continue,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Pattern {
    pub span: Option<Span>,
    pub kind: PatternKind,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PatternKind {
    Wildcard,
    Number(Decimal),
    Text(InternedString),
    Variable(VariableId),
    Destructure(BTreeMap<usize, Pattern>),
    Variant(usize, Vec<Pattern>),
    Or(Box<Pattern>, Box<Pattern>),
    Where(Box<Pattern>, Box<Expression>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Arm {
    pub span: Option<Span>,
    pub pattern: Pattern,
    pub body: Expression,
}

impl<L> Compiler<'_, L> {
    pub fn optimize(&mut self, program: compile::Program) -> Program {
        let mut program = Program::from(program);

        self.optimize_no_ops(&mut program);
        self.optimize_tail_calls(&mut program);

        program
    }
}

impl From<compile::Program> for Program {
    fn from(program: compile::Program) -> Self {
        Program {
            constants: program
                .declarations
                .monomorphized_constants
                .into_iter()
                .map(|(id, (_, decl))| (id, decl.value.into()))
                .collect(),
            body: program.body.into_iter().map(From::from).collect(),
        }
    }
}

impl From<compile::Expression> for Expression {
    fn from(expr: compile::Expression) -> Self {
        Expression {
            span: Some(expr.span),
            kind: match expr.kind {
                compile::ExpressionKind::Marker => ExpressionKind::Marker,
                compile::ExpressionKind::Variable(var) => ExpressionKind::Variable(var),
                compile::ExpressionKind::Text(text) => ExpressionKind::Text(text),
                compile::ExpressionKind::Number(number) => ExpressionKind::Number(number),
                compile::ExpressionKind::Block(statements, _) => {
                    ExpressionKind::Block(statements.into_iter().map(From::from).collect())
                }
                compile::ExpressionKind::Call(func, input) => {
                    ExpressionKind::Call(Box::new((*func).into()), Box::new((*input).into()))
                }
                compile::ExpressionKind::Function(pattern, body) => {
                    ExpressionKind::Function(pattern.into(), Box::new((*body).into()))
                }
                compile::ExpressionKind::When(input, arms) => ExpressionKind::When(
                    Box::new((*input).into()),
                    arms.into_iter().map(From::from).collect(),
                ),
                compile::ExpressionKind::External(namespace, identifier, inputs) => {
                    ExpressionKind::External(
                        namespace,
                        identifier,
                        inputs
                            .into_iter()
                            .map(|expr| {
                                let ty = expr.ty.clone();
                                (expr.into(), ty)
                            })
                            .collect(),
                        expr.ty,
                    )
                }
                compile::ExpressionKind::Initialize(pattern, value) => {
                    ExpressionKind::Initialize(pattern.into(), Box::new((*value).into()))
                }
                compile::ExpressionKind::Structure(fields) => {
                    ExpressionKind::Structure(fields.into_iter().map(From::from).collect())
                }
                compile::ExpressionKind::Variant(variant, values) => {
                    ExpressionKind::Variant(variant, values.into_iter().map(From::from).collect())
                }
                compile::ExpressionKind::ListLiteral(values) => {
                    ExpressionKind::ListLiteral(values.into_iter().map(From::from).collect())
                }
                compile::ExpressionKind::Constant(constant) => ExpressionKind::Constant(constant),
                compile::ExpressionKind::Return(value) => {
                    ExpressionKind::Return(Box::new((*value).into()))
                }
                compile::ExpressionKind::Loop(body) => {
                    ExpressionKind::Loop(Box::new((*body).into()))
                }
                compile::ExpressionKind::Break(value) => {
                    ExpressionKind::Break(Box::new((*value).into()))
                }
                compile::ExpressionKind::Continue => ExpressionKind::Continue,
            },
        }
    }
}

impl From<compile::Pattern> for Pattern {
    fn from(pattern: compile::Pattern) -> Self {
        Pattern {
            span: Some(pattern.span),
            kind: match pattern.kind {
                compile::PatternKind::Wildcard => PatternKind::Wildcard,
                compile::PatternKind::Number(number) => PatternKind::Number(number),
                compile::PatternKind::Text(text) => PatternKind::Text(text),
                compile::PatternKind::Variable(var) => PatternKind::Variable(var),
                compile::PatternKind::Destructure(fields) => PatternKind::Destructure(
                    fields
                        .into_iter()
                        .map(|(index, pattern)| (index, pattern.into()))
                        .collect(),
                ),
                compile::PatternKind::Variant(index, values) => {
                    PatternKind::Variant(index, values.into_iter().map(From::from).collect())
                }
                compile::PatternKind::Or(lhs, rhs) => {
                    PatternKind::Or(Box::new((*lhs).into()), Box::new((*rhs).into()))
                }
                compile::PatternKind::Where(pattern, condition) => {
                    PatternKind::Where(Box::new((*pattern).into()), Box::new((*condition).into()))
                }
            },
        }
    }
}

impl From<compile::Arm> for Arm {
    fn from(arm: compile::Arm) -> Self {
        Arm {
            span: Some(arm.span),
            pattern: arm.pattern.into(),
            body: arm.body.into(),
        }
    }
}

impl Program {
    pub fn traverse(&mut self, mut f: impl FnMut(&mut Expression)) {
        for constant in self.constants.values_mut() {
            constant.traverse_inner(&mut f);
        }

        for statement in &mut self.body {
            statement.traverse_inner(&mut f);
        }
    }
}

impl Expression {
    pub fn traverse(&mut self, mut f: impl FnMut(&mut Expression)) {
        self.traverse_inner(&mut f);
    }

    fn traverse_inner(&mut self, f: &mut impl FnMut(&mut Expression)) {
        match &mut self.kind {
            ExpressionKind::Block(statements) => {
                for statement in statements {
                    statement.traverse_inner(f);
                }
            }
            ExpressionKind::Call(func, input) => {
                func.traverse_inner(f);
                input.traverse_inner(f);
            }
            ExpressionKind::Function(_, body) => {
                body.traverse_inner(f);
            }
            ExpressionKind::When(input, arms) => {
                input.traverse_inner(f);

                for arm in arms {
                    arm.body.traverse_inner(f);
                }
            }
            ExpressionKind::External(_, _, inputs, _) => {
                for (input, _) in inputs {
                    input.traverse_inner(f);
                }
            }
            ExpressionKind::Initialize(_, value) => {
                value.traverse_inner(f);
            }
            ExpressionKind::Structure(fields) => {
                for field in fields {
                    field.traverse_inner(f);
                }
            }
            ExpressionKind::Variant(_, values) => {
                for value in values {
                    value.traverse_inner(f);
                }
            }
            ExpressionKind::ListLiteral(values) => {
                for value in values {
                    value.traverse_inner(f);
                }
            }
            ExpressionKind::Return(value) => {
                value.traverse_inner(f);
            }
            ExpressionKind::Loop(body) => {
                body.traverse_inner(f);
            }
            ExpressionKind::Break(value) => {
                value.traverse_inner(f);
            }
            _ => {}
        }
    }
}
