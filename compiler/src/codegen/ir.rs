use crate::{
    codegen::CodegenCtx,
    database::{NodeRef, Span},
};
use std::collections::{BTreeMap, HashSet};

#[derive(Debug, Clone, Default)]
pub struct Program {
    pub files: Vec<SpannedExpression>,
    pub definitions: BTreeMap<NodeRef, SpannedExpression>,
    pub intrinsics: HashSet<String>,
}

#[derive(Debug, Clone)]
pub struct SpannedExpression {
    pub span: Option<Span>,
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
    Constant(NodeRef, Vec<(NodeRef, SpannedExpression)>),
    Declare(NodeRef),
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
    pub fn at(self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> Option<SpannedExpression> {
        let span = ctx.get::<Span>(node);
        Some(SpannedExpression {
            span,
            identifier: None,
            inner: self,
        })
    }
}

impl SpannedExpression {
    pub fn with_identifier(mut self, identifier: impl Into<String>) -> Self {
        self.identifier = Some(identifier.into());
        self
    }
}
