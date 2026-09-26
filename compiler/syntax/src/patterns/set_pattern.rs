use crate::patterns::{InvalidSetPattern, Matching, visit_pattern};
use serde::{Deserialize, Serialize};
use wipple_core::{
    anyhow,
    codegen::{CodegenError, hir},
    db::{Db, Node},
    facts::{Description, DescriptionEntry},
    render::Comments,
    span::{Span, Str},
    typecheck::{constraints::ty_constraint::TyConstraint, ty::Ty},
    util::Link,
    visit::{
        IsCaptured, IsMutated, Visit, Visitor, definitions::VariableDefinition,
        exhaustiveness::MatchPathSegment,
    },
};
use wipple_parse::{
    lexer::TokenKind,
    names::parse_variable_name,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SetPattern {
    pub span: Span,
    pub variable: Str,
}

pub fn parse_set_pattern(parser: &mut Parser<'_>) -> Result<SetPattern, ParseError> {
    parser.token(TokenKind::SetKeyword)?;
    parser.commit("in this `set` pattern");
    let span = parser.spanned();
    let variable = parse_variable_name(parser)?;
    Ok(SetPattern {
        span: span(parser),
        variable,
    })
}

#[typetag::serde]
impl Visit for SetPattern {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_pattern(db, node, visitor, Some(MatchPathSegment::Match));

        let current_match = visitor.current_match.as_ref().unwrap();
        let value = current_match.value;

        if !current_match.allow_set {
            db.insert(node, InvalidSetPattern::Nested);
        }

        let Some((variable_definition_node, variable_definition)) =
            visitor.resolve_as::<VariableDefinition>(db, &self.variable, node)
        else {
            return;
        };

        if !variable_definition.is_mutable {
            db.insert(node, InvalidSetPattern::Immutable(variable_definition_node));
        }

        visitor.constraint(
            db,
            TyConstraint::new(node, Ty::Node(variable_definition_node)),
        );

        if visitor.capture(variable_definition_node) {
            db.insert(variable_definition_node, IsCaptured);
        }

        db.graph.replace(node, variable_definition_node);

        db.insert(variable_definition_node, IsMutated);

        visitor.codegen(
            db,
            node,
            SetPatternCodegen {
                node,
                variable: variable_definition_node,
            },
        );

        db.get_mut_or_default::<Description>(node)
            .push(DescriptionEntry {
                comments: Comments::builtin(
                    node,
                    "[`variable`] is changed to [`value`] here.",
                    [
                        ("variable", Some(Link::node(node))),
                        ("value", Some(Link::node(value))),
                    ],
                ),
                is_primary: false,
            });
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct SetPatternCodegen {
    node: Node,
    variable: Node,
}

#[typetag::serde]
impl hir::Write for SetPatternCodegen {
    fn write(&self, db: &Db, ctx: &mut hir::Ctx) -> Result<(), CodegenError> {
        let matching = db
            .get::<Matching>(self.node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?
            .0;

        ctx.condition(hir::Condition::Mutate {
            input: matching,
            variable: self.variable,
        });
        Ok(())
    }
}
