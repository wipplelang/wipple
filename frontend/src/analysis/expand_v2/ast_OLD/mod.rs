mod expression;
mod pattern;
mod type_annotation;

pub use expression::*;
pub use pattern::*;
pub use type_annotation::*;

use crate::{
    analysis::expand_v2::{syntax::SyntaxExpression, Scope},
    helpers::{Backtrace, InternedString},
    parse::Span,
};
use async_trait::async_trait;

#[derive(Debug, Clone)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Empty,
    Declaration(DeclarationStatementKind),
    Assign(Pattern, Expression),
    Use((Span, InternedString)),
    Expression(ExpressionKind),
}

#[derive(Debug, Clone)]
pub enum DeclarationStatementKind {
    Type((Span, InternedString), TypeDeclaration),
    Trait((Span, InternedString), TraitDeclaration),
    Constant((Span, InternedString), ConstantDeclaration),
    Instance(Instance),
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub parameters: Vec<TypeParameter>,
    pub bounds: Vec<Bound>,
    pub trait_span: Span,
    pub trait_name: InternedString,
    pub trait_parameters: Vec<TypeAnnotation>,
    pub value: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    pub parameters: Vec<TypeParameter>,
    pub kind: TypeKind,
}

#[derive(Debug, Clone)]
pub struct TraitDeclaration {
    pub parameters: Vec<TypeParameter>,
    pub ty: Option<TypeAnnotation>,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Marker,
    Structure(Vec<DataField>),
    Enumeration(Vec<DataVariant>),
}

#[derive(Debug, Clone)]
pub struct DataField {
    pub name: InternedString,
    pub attributes: DataFieldAttributes,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone, Default)]
pub struct DataFieldAttributes {}

#[derive(Debug, Clone)]
pub struct DataVariant {
    pub name: InternedString,
    pub span: Span,
    pub attributes: DataVariantAttributes,
    pub values: Vec<TypeAnnotation>,
}

#[derive(Debug, Clone, Default)]
pub struct DataVariantAttributes {}

#[derive(Debug, Clone)]
pub struct ConstantDeclaration {
    pub name: InternedString,
    pub parameters: Vec<TypeParameter>,
    pub bounds: Vec<Bound>,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct Bound {
    pub span: Span,
    pub trait_span: Span,
    pub trait_name: InternedString,
    pub parameters: Vec<TypeAnnotation>,
}

#[async_trait]
pub(crate) trait Visitor<'a>
where
    Self: Copy + Send + Sync,
{
    type Output: 'a;

    fn error(self, span: Span, trace: Backtrace) -> Self::Output;

    fn empty(self, span: Span) -> Self::Output;

    async fn expand(self, expr: SyntaxExpression, scope: &'a Scope<'a>) -> Self::Output;

    async fn reduce(
        self,
        span: Span,
        first: SyntaxExpression,
        rest: impl Iterator<Item = SyntaxExpression> + Send,
        scope: &'a Scope<'a>,
    ) -> Self::Output;
}
