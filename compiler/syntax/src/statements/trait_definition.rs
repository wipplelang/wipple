use crate::{
    attributes::attribute::parse_attributes,
    constraints::parse_constraints,
    statements::{parse_comments, visit_statement},
    types::{parse_atomic_type, type_parameter::parse_type_parameters},
};

use serde::{Deserialize, Serialize};
use wipple_core::{
    ast::AstKey,
    db::{Db, Node},
    span::{Span, Str},
    typecheck::constraints::group_constraint::GroupConstraint,
    visit::{
        Visit, Visitor,
        definitions::{self, Definition, TraitDefinitionAttributes},
    },
};
use wipple_parse::{
    lexer::TokenKind,
    names::parse_type_name,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraitDefinition {
    pub span: Span,
    pub comments: Vec<Str>,
    pub attributes: Vec<AstKey>,
    pub name: Str,
    pub parameters: Vec<AstKey>,
    pub ty: AstKey,
    pub constraints: Vec<AstKey>,
}

pub fn parse_trait_definition_statement(
    parser: &mut Parser<'_>,
) -> Result<TraitDefinition, ParseError> {
    let comments = parse_comments(parser)?;
    let attributes = parse_attributes(parser)?;
    let span = parser.spanned();
    let name = parse_type_name(parser)?;
    parser.token(TokenKind::AssignOperator)?;
    parser.consume_line_breaks();
    let parameters = parse_type_parameters(parser)?;
    parser.token(TokenKind::TraitKeyword)?;
    parser.commit("in this trait definition");
    let (ty, constraints) = parse_trait_constraints(parser)?;
    Ok(TraitDefinition {
        span: span(parser),
        comments,
        attributes,
        name,
        parameters,
        ty,
        constraints,
    })
}

pub fn parse_trait_constraints(
    parser: &mut Parser<'_>,
) -> Result<(AstKey, Vec<AstKey>), ParseError> {
    let ty = parse_atomic_type(parser)?;
    let constraints = parser
        .parse_optional(parse_constraints)?
        .unwrap_or_default();
    Ok((ty, constraints))
}

#[typetag::serde]
impl Visit for TraitDefinition {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit_definitions(
        &self,
        db: &mut Db,
        node: Node,
        visitor: &mut Visitor,
    ) -> Vec<(Node, Box<dyn Definition>)> {
        let attributes = self
            .attributes
            .iter()
            .map(|attribute| visitor.visit(db, &attribute.clone()))
            .collect::<Vec<_>>();

        vec![(
            node,
            Box::new(definitions::TraitDefinition {
                name: self.name.clone(),
                comments: self.comments.clone(),
                attributes: TraitDefinitionAttributes::parse(db, &attributes),
                parameters: self.parameters.iter().map(|_| db.node()).collect(),
            }),
        )]
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_statement(db, node, visitor);

        visitor.within_definition::<definitions::TraitDefinition>(
            db,
            node,
            |db, visitor, definition| {
                visitor.push_scope(db, node);

                let parameters = self.parameters;
                visitor.with_definition_flag(
                    |d| &mut d.implicit_type_parameters,
                    |visitor| {
                        for (&parameter_node, parameter) in
                            definition.parameters.iter().zip(parameters)
                        {
                            visitor.visit_as(db, &parameter, parameter_node);
                        }
                    },
                );

                let ty = visitor.visit(db, &self.ty);
                db.graph.edge(ty, node, "type");

                visitor.constraint(db, GroupConstraint::new(node, ty));

                for constraint in self.constraints {
                    let constraint = visitor.visit(db, &constraint);
                    db.graph.edge(constraint, node, "constraint");
                }

                visitor.pop_scope(db);
            },
        );
    }
}
