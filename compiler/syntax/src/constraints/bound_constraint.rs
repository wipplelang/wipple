use crate::{
    constraints::visit_constraint,
    types::{ExtraType, MissingTypes, parse_atomic_type},
};

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use wipple_core::{
    ast::AstKey,
    db::{Db, Node},
    span::{Span, Str},
    typecheck::{
        bounds::Bound,
        constraints::{
            bound_constraint::{BoundConstraint as TypecheckBoundConstraint, IsBound},
            instantiate_constraint::InstantiateConstraint,
        },
        groups::Typed,
        ty::Ty,
    },
    util::exact_for_each,
    visit::{Bounds, Visit, Visitor, definitions::TraitDefinition},
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
        db.insert(node, Typed::default());
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
            BTreeMap::from([(trait_node, node)]),
            bound_parameters
                .iter()
                .map(|(&parameter, &substitution)| (parameter, Ty::Node(substitution)))
                .collect(),
        );

        visitor.constraint(
            db,
            InstantiateConstraint::new(node, trait_node, substitutions),
        );

        visitor.constraint(
            db,
            TypecheckBoundConstraint::new(
                node,
                Bound {
                    source_node: node,
                    bound_path: vec![node],
                    bound_node: node,
                    trait_node,
                    substitutions,
                    is_optional: false,
                },
            ),
        );

        db.get_mut_or_default::<Bounds>(visitor.current_definition.as_ref().unwrap().node)
            .0
            .insert(node);
    }
}
