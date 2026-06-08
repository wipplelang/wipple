use crate::{
    constraints::visit_constraint,
    types::{ExtraType, MissingTypes, parse_atomic_type},
};

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use wipple_core::{
    ast::AstKey,
    db::{Db, Node},
    render::{Render, RenderCtx},
    span::{Span, Str},
    typecheck::{
        bounds::{Bound, UnresolvedBound},
        constraints::{
            ConstraintTrace,
            bound_constraint::{BoundConstraint as TypecheckBoundConstraint, IsBound},
        },
        ty::Ty,
    },
    util::exact_for_each,
    visit::{Visit, Visitor, definitions::TraitDefinition},
};
use wipple_parse::{
    lexer::TokenKind,
    names::parse_type_name,
    parser::{ParseError, ParseToken, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BoundConstraint {
    pub span: Span,
    pub trait_name: Str,
    pub parameters: Vec<AstKey>,
}

pub fn parse_bound_constraint(parser: &mut Parser<'_>) -> Result<BoundConstraint, ParseError> {
    let span = parser.spanned();

    parser
        .token(ParseToken::from(TokenKind::LeftParenthesis).reason("between these parentheses"))?;

    let trait_name = parse_type_name(parser)?;

    let parameters = parser.parse_many(0, parse_atomic_type)?;

    parser.token(TokenKind::RightParenthesis)?;

    Ok(BoundConstraint {
        span: span(parser),
        trait_name,
        parameters,
    })
}

#[typetag::serde]
impl Visit for BoundConstraint {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_constraint(db, node, visitor);
        db.insert(node, IsBound);

        let Some((trait_node, trait_definition)) =
            visitor.resolve_as::<TraitDefinition>(db, &self.trait_name, node)
        else {
            return;
        };

        let parameters = self
            .parameters
            .iter()
            .map(|parameter| visitor.visit(db, parameter))
            .collect::<Vec<_>>();

        let mut bound_parameters = BTreeMap::new();
        let (missing, extra) = exact_for_each(
            &trait_definition.parameters,
            &parameters,
            |&parameter, &substitution| {
                bound_parameters.insert(parameter, substitution);
            },
        );

        if !missing.is_empty() {
            db.insert(node, MissingTypes(missing.to_vec()));
        }

        for &parameter in extra {
            db.insert(parameter, ExtraType);
        }

        let substitutions = visitor.substitutions(
            Default::default(),
            bound_parameters
                .iter()
                .map(|(&parameter, &substitution)| (parameter, Ty::Node(substitution)))
                .collect(),
        );

        visitor.constraint(
            db,
            TypecheckBoundConstraint::new(
                node,
                Bound {
                    source_node: node,
                    bound_node: node,
                    trait_node,
                    target_node: None,
                    substitutions,
                    is_optional: false,
                },
            )
            .with_trace(BoundConstraintTrace {
                source_node: node,
                trait_node,
                parameters: bound_parameters,
            }),
        );
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct BoundConstraintTrace {
    source_node: Node,
    trait_node: Node,
    parameters: BTreeMap<Node, Node>,
}

#[typetag::serde]
impl ConstraintTrace for BoundConstraintTrace {
    fn nodes_mut(&mut self) -> Vec<&mut Node> {
        [&mut self.source_node]
            .into_iter()
            .chain(self.parameters.values_mut())
            .collect()
    }

    fn nodes(&self, _db: &Db) -> Vec<Node> {
        [self.source_node]
            .into_iter()
            .chain(self.parameters.values().copied())
            .collect()
    }

    fn source_node_mut(&mut self) -> Option<&mut Node> {
        Some(&mut self.source_node)
    }

    fn allow_hidden_nodes(&self) -> bool {
        false
    }
}

impl Render for BoundConstraintTrace {
    fn render_into(&self, db: &Db, ctx: &mut RenderCtx) {
        let bound = UnresolvedBound {
            trait_node: self.trait_node,
            parameters: self
                .parameters
                .iter()
                .map(|(&parameter, &substitution)| (parameter, Ty::Node(substitution)))
                .collect(),
        };

        ctx.node(self.source_node);
        ctx.string(" requires the instance ");
        bound.render_into(db, ctx);
        ctx.string(".");
    }
}
