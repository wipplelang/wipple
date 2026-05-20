use crate::{
    constraints::visit_constraint,
    types::{parse_type, type_parameter::parse_named_type_parameter},
};

use serde::{Deserialize, Serialize};
use wipple_core::{
    db::{Db, Node},
    span::Span,
    typecheck::constraints::default_constraint::DefaultConstraint as TypecheckDefaultConstraint,
    visit::{Visit, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, ParseToken, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DefaultConstraint {
    pub span: Span,
    pub parameter: Box<dyn Visit>,
    pub value: Box<dyn Visit>,
}

pub fn parse_default_constraint(parser: &mut Parser) -> Result<DefaultConstraint, ParseError> {
    let span = parser.spanned();

    parser
        .token(ParseToken::from(TokenKind::LeftParenthesis).reason("between these parentheses"))?;

    let parameter = Box::new(parse_named_type_parameter(parser)?);
    parser.token(TokenKind::AnnotateOperator)?;
    parser.commit("in this type annotation");
    parser.consume_line_breaks();
    let value = parse_type(parser)?;
    parser.token(TokenKind::RightParenthesis)?;

    Ok(DefaultConstraint {
        span: span(parser),
        parameter,
        value,
    })
}

#[typetag::serde]
impl Visit for DefaultConstraint {
    fn span(&self) -> &Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_constraint(db, node, visitor);

        let parameter = visitor.visit(db, self.parameter);
        let value = visitor.visit(db, self.value);

        db.graph.edge(value, node, "value");

        visitor.constraint(db, TypecheckDefaultConstraint::new(parameter, value));
    }
}
