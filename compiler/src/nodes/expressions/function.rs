use crate::{
    codegen::{Codegen, CodegenCtx, ir},
    database::{Fact, HiddenNode, Node, NodeRef, Render},
    nodes::{parse_atomic_pattern, parse_expression, visit_expression},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::TypeConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug, Clone)]
pub struct InputTemporaries(pub Vec<NodeRef>);

impl Fact for InputTemporaries {}

impl Render for InputTemporaries {}

#[derive(Debug)]
pub struct FunctionExpressionNode {
    pub inputs: Vec<NodeRef>,
    pub output: NodeRef,
}

impl Node for FunctionExpressionNode {}

pub fn parse_function_expression(
    parser: &mut Parser<'_>,
) -> Result<FunctionExpressionNode, ParseError> {
    let inputs = parse_function_expression_inputs(parser)?;
    let output = parse_expression(parser)?;

    Ok(FunctionExpressionNode { inputs, output })
}

pub fn parse_function_expression_inputs(
    parser: &mut Parser<'_>,
) -> Result<Vec<NodeRef>, ParseError> {
    let inputs = parser
        .parse_many(1, parse_atomic_pattern, |parser| parser.parse_nothing())?
        .into_iter()
        .map(|(node, _)| node)
        .collect::<Vec<_>>();

    parser.token(TokenKind::FunctionOperator)?;
    parser.commit("in this function");
    parser.consume_line_breaks();

    Ok(inputs)
}

impl Visit for FunctionExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        visitor.push_scope();

        let input_temporaries = self
            .inputs
            .iter()
            .map(|pattern| {
                let span = visitor.db.span(pattern);
                let temporary = visitor.db.node(span, HiddenNode(None));

                visitor.matching(&temporary, false, false, |visitor| {
                    visitor.current_match().root = Some(temporary.clone());
                    visitor.current_match().arm = Some(pattern.clone());
                    visitor.visit(pattern);
                });

                visitor.edge(pattern, node, "input");
                temporary
            })
            .collect::<Vec<_>>();

        visitor.visit(&self.output);
        visitor.edge(&self.output, node, "output");

        visitor.pop_scope();

        visitor.constraint(TypeConstraint::new(
            node.clone(),
            visitor.function_type(self.inputs.iter().cloned(), self.output.clone()),
        ));

        visitor.insert(node, InputTemporaries(input_temporaries));
    }
}

impl Codegen for FunctionExpressionNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> Option<ir::SpannedExpression> {
        let InputTemporaries(mut inputs) = ctx.get::<InputTemporaries>(node)?;

        inputs.extend(
            self.inputs
                .iter()
                .flat_map(|pattern| ctx.db.temporaries(pattern)),
        );

        let mut body = Vec::new();
        for pattern in &self.inputs {
            body.push(
                ir::Expression::If(vec![(ctx.codegen(pattern)?, None)], None).at(pattern, ctx)?,
            );
        }

        body.push(
            ir::Expression::Return(Some(Box::new(ctx.codegen(&self.output)?)))
                .at(&self.output, ctx)?,
        );

        ir::Expression::Function(
            inputs,
            Box::new(ir::Expression::Sequence(body).at(node, ctx)?),
        )
        .at(node, ctx)
    }
}
