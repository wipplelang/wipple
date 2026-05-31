use crate::patterns::{Matching, visit_pattern};

use serde::{Deserialize, Serialize};
use wipple_core::{
    anyhow,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    facts::DebugInfo,
    span::{Span, Str},
    visit::{
        IsMutated, Visit, Visitor, definitions::VariableDefinition,
        exhaustiveness::MatchPathSegment,
    },
};
use wipple_parse::{
    names::parse_variable_name,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VariablePattern {
    pub span: Span,
    pub variable: Str,
}

pub fn parse_variable_pattern(parser: &mut Parser<'_>) -> Result<VariablePattern, ParseError> {
    let span = parser.spanned();
    let variable = parse_variable_name(parser)?;
    Ok(VariablePattern {
        span: span(parser),
        variable,
    })
}

#[typetag::serde]
impl Visit for VariablePattern {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_pattern(db, node, visitor, Some(MatchPathSegment::Match));

        db.get_mut_or_default::<DebugInfo>(node).variable = true;

        let current_match = visitor.current_match.as_ref().unwrap();
        let value = current_match.value;
        let is_mutable = current_match.mutable;

        visitor.define(
            db,
            node,
            Box::new(VariableDefinition {
                name: self.variable.clone(),
                value,
                is_mutable,
            }),
        );

        visitor.codegen(db, node, VariablePatternCodegen { node });
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct VariablePatternCodegen {
    node: Node,
}

#[typetag::serde]
impl CodegenValue for VariablePatternCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        let matching = db
            .get::<Matching>(self.node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?
            .0;

        ctx.condition(ir::Condition::Initialize {
            variable: self.node,
            node: Some(matching),
            value: ir::Value::Variable(matching),
            mutable: db.contains::<IsMutated>(self.node),
        });

        Ok(())
    }
}
