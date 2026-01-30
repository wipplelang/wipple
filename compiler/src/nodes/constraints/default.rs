use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Node, NodeRef},
    nodes::{parse_named_type_parameter, parse_type, visit_constraint},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::DefaultConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct DefaultConstraintNode {
    pub parameter: NodeRef,
    pub value: NodeRef,
}

impl Node for DefaultConstraintNode {}

pub fn parse_default_constraint(
    parser: &mut Parser<'_>,
) -> Result<DefaultConstraintNode, ParseError> {
    parser.token_with_reason(TokenKind::LeftParenthesis, "between these parentheses")?;
    let parameter = parser.node(parse_named_type_parameter)?;
    parser.token(TokenKind::AnnotateOperator)?;
    parser.commit("in this type annotation");
    parser.consume_line_breaks();
    let value = parse_type(parser)?;
    parser.token(TokenKind::RightParenthesis)?;

    Ok(DefaultConstraintNode { parameter, value })
}

impl Visit for DefaultConstraintNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_constraint(node, visitor);

        visitor.visit(&self.parameter);

        visitor.visit(&self.value);
        visitor.edge(&self.value, node, "value");

        visitor.constraint(DefaultConstraint::new(
            self.parameter.clone(),
            self.value.clone(),
        ));
    }
}

impl Codegen for DefaultConstraintNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        Err(ctx.error())
    }
}
