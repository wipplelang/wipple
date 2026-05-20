use crate::{
    patterns::{Matching, visit_pattern},
    types::named_type::NamedType,
};

use serde::{Deserialize, Serialize};
use wipple_core::{
    anyhow,
    arcstr::Substr,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    span::Span,
    typecheck::constraints::group_constraint::GroupConstraint,
    visit::{Visit, Visitor, exhaustiveness::MatchPathSegment},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NumberPattern {
    pub span: Span,
    pub value: Substr,
}

pub fn parse_number_pattern(parser: &mut Parser) -> Result<NumberPattern, ParseError> {
    let span = parser.spanned();
    let value = parser.token(TokenKind::Number)?;
    Ok(NumberPattern {
        span: span(parser),
        value,
    })
}

#[typetag::serde]
impl Visit for NumberPattern {
    fn span(&self) -> &Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_pattern(db, node, visitor, Some(MatchPathSegment::NoMatch));

        let number_type = db.node();
        db.hide(number_type);
        let syntax = Box::new(NamedType {
            span: self.span.clone(),
            name: Substr::from("Number"),
            parameters: Vec::new(),
        }) as Box<dyn Visit>;
        visitor.visit_as(db, syntax, number_type);

        visitor.constraint(db, GroupConstraint::new(node, number_type));

        visitor.codegen(
            db,
            node,
            NumberPatternCodegen {
                node,
                value: self.value.clone(),
            },
        );
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct NumberPatternCodegen {
    node: Node,
    value: Substr,
}

#[typetag::serde]
impl CodegenValue for NumberPatternCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        let matching = db
            .get::<Matching>(self.node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?
            .0;

        ctx.condition(ir::Condition::EqualToNumber {
            input: matching,
            value: self.value.to_string(),
        });

        Ok(())
    }
}
