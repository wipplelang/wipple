use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Node, NodeRef},
    nodes::{InheritTemporaries, parse_pattern_element, parse_type_element, visit_pattern},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::GroupConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct AnnotatePatternNode {
    pub pattern: NodeRef,
    pub ty: NodeRef,
}

impl Node for AnnotatePatternNode {}

pub fn parse_annotate_pattern(parser: &mut Parser<'_>) -> Result<AnnotatePatternNode, ParseError> {
    let pattern = parse_pattern_element(parser)?;
    parser.token(TokenKind::AnnotateOperator)?;
    parser.commit("in this type annotation");
    parser.consume_line_breaks();
    let ty = parse_type_element(parser)?;

    Ok(AnnotatePatternNode { pattern, ty })
}

impl Visit for AnnotatePatternNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_pattern(node, visitor);

        visitor.visit(&self.pattern);
        visitor.edge(&self.pattern, node, "pattern");

        visitor.visit(&self.ty);
        visitor.edge(&self.ty, node, "type");

        visitor.constraint(GroupConstraint::new(self.pattern.clone(), self.ty.clone()));
        visitor.constraint(GroupConstraint::new(node.clone(), self.pattern.clone()));

        visitor.insert(node, InheritTemporaries(vec![self.pattern.clone()]));
    }
}

impl Codegen for AnnotatePatternNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        ctx.write(&self.pattern)?;
        Ok(())
    }
}
