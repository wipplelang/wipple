use crate::types::{parse_type, visit_type};

use serde::{Deserialize, Serialize};
use wipple_core::{
    arcstr::Substr,
    db::{Db, Node},
    span::Span,
    typecheck::{
        constraints::{
            bound_constraint::InferredParameter, group_constraint::GroupConstraint,
            ty_constraint::TyConstraint,
        },
        ty::ConstructedTy,
    },
    visit::{TypeParameters, Visit, Visitor, definitions::TypeParameterDefinition},
};
use wipple_parse::{
    lexer::TokenKind,
    names::parse_type_parameter_name,
    parse_alt,
    parser::{ParseError, ParseToken, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeParameter {
    pub span: Span,
    pub name: Substr,
    pub infer: bool,
    pub value: Option<Box<dyn Visit>>,
}

pub fn parse_type_parameter(parser: &mut Parser) -> Result<Box<dyn Visit>, ParseError> {
    parse_alt!(parser, {
        parse_named_type_parameter as value => Box::new(value),
        parse_infer_type_parameter as value => Box::new(value),
        _ => "Expected type parameter",
    })
}

pub fn parse_annotated_type_parameter(parser: &mut Parser) -> Result<TypeParameter, ParseError> {
    let span = parser.spanned();
    let name = parse_type_parameter_name(parser)?;
    parser.token(TokenKind::AnnotateOperator)?;
    parser.commit("in this type annotation");
    parser.consume_line_breaks();
    let value = parse_type(parser)?;
    Ok(TypeParameter {
        span: span(parser),
        name,
        infer: false,
        value: Some(value),
    })
}

pub fn parse_named_type_parameter(parser: &mut Parser) -> Result<TypeParameter, ParseError> {
    let span = parser.spanned();
    let name = parse_type_parameter_name(parser)?;
    Ok(TypeParameter {
        span: span(parser),
        name,
        infer: false,
        value: None,
    })
}

pub fn parse_infer_type_parameter(parser: &mut Parser) -> Result<TypeParameter, ParseError> {
    let span = parser.spanned();
    parser
        .token(ParseToken::from(TokenKind::LeftParenthesis).reason("between these parentheses"))?;
    parser.token(TokenKind::InferKeyword)?;
    let name = parse_type_parameter_name(parser)?;
    parser.token(TokenKind::RightParenthesis)?;
    Ok(TypeParameter {
        span: span(parser),
        name,
        infer: true,
        value: None,
    })
}

pub fn parse_type_parameters(parser: &mut Parser) -> Result<Vec<Box<dyn Visit>>, ParseError> {
    let parameters = parser.parse_many(0, parse_type_parameter)?;

    if !parameters.is_empty() {
        parser.token(TokenKind::TypeFunctionOperator)?;
        parser.consume_line_breaks();
    }

    Ok(parameters)
}

#[typetag::serde]
impl Visit for TypeParameter {
    fn span(&self) -> &Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_type(db, node, visitor);

        if let Some((existing_node, _)) = visitor
            .peek_as::<TypeParameterDefinition>(&self.name)
            .into_iter()
            .next()
        {
            visitor.constraint(db, GroupConstraint::new(node, existing_node));
        } else if visitor
            .current_definition
            .as_ref()
            .is_some_and(|definition| definition.implicit_type_parameters)
        {
            visitor.define(
                db,
                node,
                Box::new(TypeParameterDefinition {
                    name: Some(self.name.clone()),
                }),
            );

            if let Some(value) = self.value {
                let value = visitor.visit(db, value);
                db.graph.edge(value, node, "value");

                visitor.constraint(db, GroupConstraint::new(node, value));
            } else {
                visitor.constraint(db, TyConstraint::new(node, ConstructedTy::parameter(node)));
            }

            if self.infer {
                db.insert(node, InferredParameter);
            }

            db.get_mut_or_default::<TypeParameters>(
                visitor.current_definition.as_ref().unwrap().node,
            )
            .0
            .push(node);
        }

        // Update the `Resolved` fact
        visitor.resolve_as::<TypeParameterDefinition>(db, &self.name, node);
    }
}
