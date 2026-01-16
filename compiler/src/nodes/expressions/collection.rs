use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Fact, HiddenNode, Node, NodeRef, Render},
    nodes::{
        CallExpressionNode, ConstructorExpressionNode, parse_expression_element, visit_expression,
    },
    syntax::{ParseError, Parser, TokenKind},
    typecheck::{GroupConstraint, Typed},
    visit::{Visit, Visitor},
};

#[derive(Debug, Clone)]
pub struct ResolvedCollection(pub NodeRef);

impl Fact for ResolvedCollection {}

impl Render for ResolvedCollection {}

#[derive(Debug)]
pub struct CollectionExpressionNode {
    pub elements: Vec<NodeRef>,
}

impl Node for CollectionExpressionNode {}

pub fn parse_empty_collection_expression(
    parser: &mut Parser<'_>,
) -> Result<CollectionExpressionNode, ParseError> {
    parser.token(TokenKind::CollectionOperator)?;

    Ok(CollectionExpressionNode {
        elements: Vec::new(),
    })
}

pub fn parse_collection_expression(
    parser: &mut Parser<'_>,
) -> Result<CollectionExpressionNode, ParseError> {
    let elements = parser
        .parse_many(1, parse_expression_element, |parser| {
            parser.token(TokenKind::CollectionOperator)?;
            parser.consume_line_breaks();
            Ok(())
        })?
        .into_iter()
        .map(|(node, _)| node)
        .collect::<Vec<_>>();

    if elements.len() == 1 {
        parser.token(TokenKind::CollectionOperator)?;
    } else {
        let _ = parser.parse_optional(|parser| {
            parser.token(TokenKind::CollectionOperator)?;
            Ok(())
        })?;
    }

    Ok(CollectionExpressionNode { elements })
}

impl Visit for CollectionExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        let span = visitor.span(node);
        let element_type = visitor.node(span.clone(), HiddenNode(None));
        visitor.insert(&element_type, Typed::default());

        if let Some(first) = self.elements.first() {
            visitor.constraint(GroupConstraint::new(first.clone(), element_type));
        }

        let mut collection_node = visitor.node(
            span,
            HiddenNode::new(ConstructorExpressionNode {
                constructor: String::from("Initial-Collection"),
            }),
        );

        for element in &self.elements {
            let element_span = visitor.span(element);

            let builder = visitor.node(
                element_span.clone(),
                HiddenNode::new(ConstructorExpressionNode {
                    constructor: String::from("Build-Collection"),
                }),
            );

            collection_node = visitor.node(
                element_span,
                HiddenNode::new(CallExpressionNode {
                    function: builder,
                    inputs: vec![element.clone(), collection_node],
                }),
            );
        }

        visitor.visit(&collection_node);
        visitor.edge(&collection_node, node, "collection");
        visitor.constraint(GroupConstraint::new(collection_node.clone(), node.clone()));

        visitor.insert(node, ResolvedCollection(collection_node));
    }
}

impl Codegen for CollectionExpressionNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        let ResolvedCollection(collection_node) = ctx
            .db
            .get::<ResolvedCollection>(ctx.current_node())
            .ok_or_else(|| ctx.error())?;

        ctx.write(&collection_node)
    }
}
