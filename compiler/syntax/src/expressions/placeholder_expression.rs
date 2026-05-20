use crate::expressions::visit_expression;
use serde::{Deserialize, Serialize};
use wipple_core::{
    codegen::{CodegenCtx, CodegenError, CodegenValue},
    db::{Db, Fact, Node},
    render::Render,
    span::Span,
    visit::{Visit, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IsPlaceholder;

#[typetag::serde]
impl Fact for IsPlaceholder {}

impl Render for IsPlaceholder {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlaceholderExpression {
    pub span: Span,
}

pub fn parse_placeholder_expression(
    parser: &mut Parser,
) -> Result<PlaceholderExpression, ParseError> {
    let span = parser.spanned();
    parser.token(TokenKind::UnderscoreKeyword)?;
    parser.commit("in this placeholder expression");
    Ok(PlaceholderExpression { span: span(parser) })
}

#[typetag::serde]
impl Visit for PlaceholderExpression {
    fn span(&self) -> &Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);
        db.insert(node, IsPlaceholder);
        visitor.codegen(db, node, PlaceholderExpressionCodegen);
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct PlaceholderExpressionCodegen;

#[typetag::serde]
impl CodegenValue for PlaceholderExpressionCodegen {
    fn codegen(&self, _db: &Db, _ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        Ok(())
    }
}
