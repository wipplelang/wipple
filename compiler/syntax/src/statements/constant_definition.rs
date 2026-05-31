use crate::{
    attributes::attribute::parse_attributes,
    constraints::parse_constraints,
    statements::{parse_comments, visit_statement},
    types::parse_type,
};

use serde::{Deserialize, Serialize};
use wipple_core::{
    ast::AstKey,
    db::{Db, Node},
    span::{Span, Str},
    typecheck::constraints::group_constraint::GroupConstraint,
    visit::{
        Visit, Visitor,
        definitions::{self, ConstantAttributes, Definition},
    },
};
use wipple_parse::{
    lexer::TokenKind,
    names::parse_variable_name,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstantDefinition {
    pub span: Span,
    pub comments: Vec<Str>,
    pub attributes: Vec<AstKey>,
    pub name: Str,
    pub ty: AstKey,
    pub constraints: Vec<AstKey>,
}

pub fn parse_constant_definition_statement(
    parser: &mut Parser<'_>,
) -> Result<ConstantDefinition, ParseError> {
    let comments = parse_comments(parser)?;
    let attributes = parse_attributes(parser)?;
    let span = parser.spanned();
    let name = parse_variable_name(parser)?;
    let (ty, constraints) = parse_constant_constraints(parser)?;
    Ok(ConstantDefinition {
        span: span(parser),
        comments,
        attributes,
        name,
        ty,
        constraints,
    })
}

pub fn parse_constant_constraints(
    parser: &mut Parser<'_>,
) -> Result<(AstKey, Vec<AstKey>), ParseError> {
    parser.token(TokenKind::AnnotateOperator)?;
    parser.commit("in this constant definition");
    parser.consume_line_breaks();
    let ty = parse_type(parser)?;
    let constraints = parser
        .parse_optional(parse_constraints)?
        .unwrap_or_default();
    Ok((ty, constraints))
}

#[typetag::serde]
impl Visit for ConstantDefinition {
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
            Box::new(definitions::ConstantDefinition {
                name: self.name.clone(),
                comments: self.comments.clone(),
                attributes: ConstantAttributes::parse(db, &attributes),
            }),
        )]
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_statement(db, node, visitor);

        visitor.within_definition::<definitions::ConstantDefinition>(db, node, |db, visitor, _| {
            visitor.push_scope(db, node);

            visitor.with_definition_flag(
                |d| &mut d.implicit_type_parameters,
                |visitor| {
                    let ty = visitor.visit(db, &self.ty);
                    db.graph.edge(ty, node, "type");

                    for constraint in self.constraints {
                        let constraint = visitor.visit(db, &constraint);
                        db.graph.edge(constraint, node, "constraint");
                    }

                    visitor.constraint(db, GroupConstraint::new(node, ty));
                },
            );

            visitor.pop_scope(db);

            // TODO: Check for missing constant values
        });
    }
}
