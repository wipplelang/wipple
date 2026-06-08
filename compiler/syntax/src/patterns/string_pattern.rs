use crate::{
    patterns::{Matching, visit_pattern},
    types::named_type::NamedType,
};

use serde::{Deserialize, Serialize};
use wipple_core::{
    anyhow,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    span::{Span, Str},
    typecheck::{constraints::group_constraint::GroupConstraint, groups::Annotated},
    visit::{Visit, Visitor, exhaustiveness::MatchPathSegment},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StringPattern {
    pub span: Span,
    pub value: Str,
}

pub fn parse_string_pattern(parser: &mut Parser<'_>) -> Result<StringPattern, ParseError> {
    let span = parser.spanned();
    let value = parser.token(TokenKind::String)?;
    Ok(StringPattern {
        span: span(parser),
        value,
    })
}

#[typetag::serde]
impl Visit for StringPattern {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_pattern(db, node, visitor, Some(MatchPathSegment::NoMatch));

        let string_type = db.node();
        db.hide(string_type);
        let syntax = visitor.in_ast(
            db,
            Box::new(NamedType {
                span: self.span.clone(),
                name: Str::from("String"),
                parameters: Vec::new(),
            }),
        );
        visitor.visit_as(db, &syntax, string_type);

        visitor.constraint(db, GroupConstraint::new(node, string_type));

        db.insert(node, Annotated);

        visitor.codegen(
            db,
            node,
            StringPatternCodegen {
                node,
                value: self.value.clone(),
            },
        );
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct StringPatternCodegen {
    node: Node,
    value: Str,
}

#[typetag::serde]
impl CodegenValue for StringPatternCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        let matching = db
            .get::<Matching>(self.node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?
            .0;

        ctx.condition(ir::Condition::EqualToString {
            input: matching,
            value: self.value.to_string(),
        });

        Ok(())
    }
}
