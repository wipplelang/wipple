use crate::{
    expressions::{
        call_expression::CallExpression, constructor_expression::ConstructorExpression,
        parse_expression_element, placeholder_expression::PlaceholderExpression, visit_expression,
    },
    types::named_type::NamedType,
};

use serde::{Deserialize, Serialize};
use wipple_core::{
    ast::AstKey,
    db::{Db, Node},
    facts::{Description, DescriptionEntry},
    render::Comments,
    span::{Span, Str},
    typecheck::{
        constraints::{default_constraint::DefaultConstraint, ty_constraint::TyConstraint},
        ty::Ty,
    },
    util::Link,
    visit::{Hidden, Visit, VisitAs, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CollectionExpression {
    pub span: Span,
    pub elements: Vec<AstKey>,
}

pub fn parse_empty_collection_expression(
    parser: &mut Parser<'_>,
) -> Result<CollectionExpression, ParseError> {
    let span = parser.spanned();
    parser.token(TokenKind::CollectionOperator)?;
    Ok(CollectionExpression {
        span: span(parser),
        elements: Vec::new(),
    })
}

pub fn parse_collection_expression(
    parser: &mut Parser<'_>,
) -> Result<CollectionExpression, ParseError> {
    let span = parser.spanned();
    let elements = parser
        .parse_sep(1, parse_expression_element, |parser| {
            parser.token(TokenKind::CollectionOperator)?;
            parser.consume_line_breaks();
            Ok(())
        })?
        .into_iter()
        .map(|(element, _)| element)
        .collect::<Vec<_>>();

    if elements.len() == 1 {
        parser.token(TokenKind::CollectionOperator)?;
    } else {
        let _ = parser.parse_optional(|parser| parser.token(TokenKind::CollectionOperator))?;
    }

    Ok(CollectionExpression {
        span: span(parser),
        elements,
    })
}

#[typetag::serde]
impl Visit for CollectionExpression {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        let mut collection = visitor.in_ast(
            db,
            Hidden::new(ConstructorExpression {
                span: self.span.clone(),
                constructor: Str::from("Initial-Collection"),
            }),
        );

        let elements = self
            .elements
            .iter()
            .map(|element| {
                let element_node = db.node();
                if let Some(graph) = &mut db.graph {
                    graph.edge(element_node, node, "element");
                }

                let function = visitor.in_ast(
                    db,
                    Hidden::new(ConstructorExpression {
                        span: self.span.clone(),
                        constructor: Str::from("Build-Collection"),
                    }),
                );

                let input = visitor.in_ast(
                    db,
                    Box::new(VisitAs {
                        node: element_node,
                        syntax: element.clone(),
                    }),
                );

                collection = visitor.in_ast(
                    db,
                    Hidden::new(CallExpression {
                        span: self.span.clone(),
                        function,
                        inputs: vec![collection.clone(), input],
                        desugared: true,
                    }),
                );

                element_node
            })
            .collect::<Vec<_>>();

        collection.get(db).clone().visit(db, node, visitor);

        // Default to a `List`

        let default_element_type = visitor.in_ast(
            db,
            Hidden::new(PlaceholderExpression {
                span: self.span.clone(),
            }),
        );

        let default_type = visitor.in_ast(
            db,
            Hidden::new(NamedType {
                span: self.span.clone(),
                name: Str::from("List"),
                parameters: vec![default_element_type],
            }),
        );

        let default_node = visitor.visit(db, &default_type);

        visitor.constraint(db, DefaultConstraint::new(node, default_node));

        // Constraint all elements to be the same type

        if let Some((&first, rest)) = elements.split_first() {
            for &element in rest {
                visitor.constraint(db, TyConstraint::new(first, Ty::Node(element)));
            }

            db.get_mut_or_default::<Description>(node)
                .summary
                .push(DescriptionEntry::primary(Comments::builtin(
                    node,
                    "[`collection`] is a collection of [`element@type`] elements.",
                    [
                        ("collection", Some(Link::node(node))),
                        ("element", Some(Link::node(first))),
                    ],
                )));
        } else {
            db.get_mut_or_default::<Description>(node)
                .summary
                .push(DescriptionEntry::primary(Comments::builtin(
                    node,
                    "[`collection`] is an empty [`collection@type`].",
                    [("collection", Some(Link::node(node)))],
                )));
        }

        if let Some(&first) = elements.first() {
            db.get_mut_or_default::<Description>(node).consequences.push(
                DescriptionEntry::primary(Comments::builtin(
                    first,
                    "Since the first element, [`first`], is a [`first@type`], the remaining elements must also be a [`first@type`].",
                    [("first", Some(Link::node(first)))],
                )),
            );
        }
    }
}
