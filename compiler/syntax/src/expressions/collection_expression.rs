use crate::expressions::{
    call_expression::CallExpression, constructor_expression::ConstructorExpression,
    parse_expression_element, visit_expression,
};

use serde::{Deserialize, Serialize};
use wipple_core::{
    ast::AstKey,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    span::{Span, Str},
    typecheck::{constraints::ty_constraint::TyConstraint, groups::Typed, ty::Ty},
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

        let element_type = db.node();
        db.hide(element_type);
        db.insert(element_type, Typed::default());

        let mut collection = visitor.in_ast(
            db,
            Hidden::new(ConstructorExpression {
                span: self.span.clone(),
                constructor: Str::from("Initial-Collection"),
            }),
        );

        for element in &self.elements {
            let element_node = db.node();
            db.graph.edge(element_node, node, "element");
            visitor.constraint(db, TyConstraint::new(element_node, Ty::Node(element_type)));

            let function = visitor.in_ast(
                db,
                Hidden::new(ConstructorExpression {
                    span: db.ast(element).span(db).clone(),
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
                    span: db.ast(element).span(db).clone(),
                    function,
                    inputs: vec![input, collection],
                }),
            );
        }

        let collection_node = visitor.visit(db, &collection);
        db.graph.edge(collection_node, node, "collection");
        visitor.constraint(db, TyConstraint::new(collection_node, Ty::Node(node)));

        visitor.codegen(
            db,
            node,
            CollectionExpressionCodegen {
                node,
                collection_node,
            },
        );
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CollectionExpressionCodegen {
    node: Node,
    collection_node: Node,
}

#[typetag::serde]
impl CodegenValue for CollectionExpressionCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        ctx.codegen(db, self.collection_node)?;

        ctx.instruction(ir::Instruction::Value {
            node: self.node,
            value: ir::Value::Variable(self.collection_node),
        });

        Ok(())
    }
}
