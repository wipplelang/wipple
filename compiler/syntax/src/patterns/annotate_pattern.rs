use crate::{
    expressions::annotate_expression::AnnotateConstraintTrace,
    patterns::{parse_pattern_element, visit_pattern},
    types::parse_type_element,
};

use serde::{Deserialize, Serialize};
use wipple_core::{
    ast::AstKey,
    codegen::{CodegenCtx, CodegenError, CodegenValue},
    db::{Db, Node},
    span::Span,
    typecheck::{constraints::group_constraint::GroupConstraint, groups::Annotated},
    visit::{Visit, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnnotatePattern {
    pub span: Span,
    pub pattern: AstKey,
    pub ty: AstKey,
}

pub fn parse_annotate_pattern(parser: &mut Parser<'_>) -> Result<AnnotatePattern, ParseError> {
    let span = parser.spanned();
    let pattern = parse_pattern_element(parser)?;
    parser.token(TokenKind::AnnotateOperator)?;
    parser.commit("in this type annotation");
    parser.consume_line_breaks();
    let ty = parse_type_element(parser)?;
    Ok(AnnotatePattern {
        span: span(parser),
        pattern,
        ty,
    })
}

#[typetag::serde]
impl Visit for AnnotatePattern {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_pattern(db, node, visitor, None);

        db.hide(node);

        let pattern = visitor.visit(db, &self.pattern);
        let ty = visitor.visit(db, &self.ty);
        db.graph.edge(ty, pattern, "type");

        visitor.constraint(
            db,
            GroupConstraint::new(pattern, ty)
                .with_trace(AnnotateConstraintTrace { value: pattern, ty }),
        );

        visitor.constraint(db, GroupConstraint::new(node, pattern));

        db.insert(pattern, Annotated);

        visitor.codegen(db, node, AnnotatePatternCodegen { pattern });
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct AnnotatePatternCodegen {
    pattern: Node,
}

#[typetag::serde]
impl CodegenValue for AnnotatePatternCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        ctx.codegen(db, self.pattern)
    }
}
