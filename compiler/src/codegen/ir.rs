use crate::{
    codegen::CodegenCtx,
    database::{NodeRef, Span},
    typecheck,
};
use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct DefinitionKey {
    pub node: NodeRef,
    pub substitutions: BTreeMap<NodeRef, Type>,
    pub bounds: BTreeMap<NodeRef, Instance>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Named(NodeRef, Vec<Type>),
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
    Parameter(NodeRef),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Instance {
    Bound(NodeRef),
    Definition(DefinitionKey),
}

#[derive(Debug, Clone, Default)]
pub struct Program {
    pub files: Vec<SpannedExpression>,
    pub definitions: BTreeMap<DefinitionKey, SpannedExpression>,
}

#[derive(Debug, Clone)]
pub struct SpannedExpression {
    pub span: Option<Span>,
    pub ty: Option<Type>,
    pub identifier: Option<String>,
    pub inner: Expression,
}

#[derive(Debug, Clone)]
pub enum Expression {
    And(Vec<SpannedExpression>),
    AssignTo(Box<SpannedExpression>, NodeRef),
    AssignToMutable(Box<SpannedExpression>, NodeRef),
    Bound(NodeRef),
    Call(Box<SpannedExpression>, Vec<SpannedExpression>),
    Concat(Vec<SpannedExpression>),
    Declare(NodeRef),
    Constant(DefinitionKey),
    EqualToNumber(Box<SpannedExpression>, String),
    EqualToString(Box<SpannedExpression>, String),
    EqualToVariant(Box<SpannedExpression>, usize),
    Field(Box<SpannedExpression>, String),
    Function(Vec<NodeRef>, Vec<SpannedExpression>, Vec<NodeRef>),
    If(
        Vec<(SpannedExpression, Option<Box<SpannedExpression>>)>,
        Option<Box<SpannedExpression>>,
    ),
    Index(Box<SpannedExpression>, usize),
    List(Vec<SpannedExpression>),
    Marker,
    Mutable(NodeRef),
    Mutate(NodeRef, NodeRef),
    NoOp,
    Number(String),
    Or(Vec<SpannedExpression>),
    Return(Box<SpannedExpression>),
    Runtime(String, Vec<SpannedExpression>),
    Scope(Vec<SpannedExpression>),
    Sequence(Vec<SpannedExpression>),
    String(String),
    Structure(Vec<(String, SpannedExpression)>),
    Trace,
    Variable(NodeRef),
    Variant(usize, Vec<SpannedExpression>),
}

impl Expression {
    pub fn at(self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> SpannedExpression {
        SpannedExpression {
            span: ctx.get::<Span>(node),
            ty: typecheck::Type::Node(node.clone()).key(ctx.db),
            identifier: None,
            inner: self,
        }
    }
}

impl SpannedExpression {
    pub fn with_identifier(mut self, identifier: impl Into<String>) -> Self {
        self.identifier = Some(identifier.into());
        self
    }
}

impl SpannedExpression {
    pub fn traverse_mut(&mut self, f: &mut dyn FnMut(&mut Self)) {
        f(self);

        match &mut self.inner {
            Expression::And(expressions)
            | Expression::Concat(expressions)
            | Expression::List(expressions)
            | Expression::Or(expressions)
            | Expression::Sequence(expressions) => {
                for expression in expressions {
                    expression.traverse_mut(f);
                }
            }
            Expression::AssignTo(value, _) | Expression::AssignToMutable(value, _) => {
                value.traverse_mut(f);
            }
            Expression::Call(function, inputs) => {
                function.traverse_mut(f);

                for expression in inputs {
                    expression.traverse_mut(f);
                }
            }
            Expression::If(conditions, else_value) => {
                for (condition, then_value) in conditions {
                    condition.traverse_mut(f);

                    if let Some(expression) = then_value {
                        expression.traverse_mut(f);
                    }
                }

                if let Some(expression) = else_value {
                    expression.traverse_mut(f);
                }
            }
            Expression::EqualToNumber(expression, _)
            | Expression::EqualToString(expression, _)
            | Expression::EqualToVariant(expression, _)
            | Expression::Field(expression, _)
            | Expression::Index(expression, _)
            | Expression::Return(expression) => {
                expression.traverse_mut(f);
            }
            Expression::Function(_, statements, _) | Expression::Scope(statements) => {
                for statement in statements {
                    statement.traverse_mut(f);
                }
            }
            Expression::Runtime(_, inputs) | Expression::Variant(_, inputs) => {
                for expression in inputs {
                    expression.traverse_mut(f);
                }
            }
            Expression::Structure(fields) => {
                for (_, value) in fields {
                    value.traverse_mut(f);
                }
            }
            Expression::Bound(_)
            | Expression::Constant(_)
            | Expression::Declare(_)
            | Expression::Marker
            | Expression::Mutable(_)
            | Expression::Mutate(_, _)
            | Expression::NoOp
            | Expression::Number(_)
            | Expression::String(_)
            | Expression::Trace
            | Expression::Variable(_) => {}
        }
    }
}
