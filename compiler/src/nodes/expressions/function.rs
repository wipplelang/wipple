use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Fact, Node, NodeRef, Render},
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
                let temporary = visitor.visit_matching(pattern);
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
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        let InputTemporaries(input_temporaries) = ctx
            .db
            .get::<InputTemporaries>(ctx.current_node())
            .ok_or_else(|| ctx.error())?;

        ctx.write_string("(async (");
        for temporary in &input_temporaries {
            ctx.write_node(temporary);
            ctx.write_string(", ");
        }
        ctx.write_string(") => {");
        ctx.write_line();

        for pattern in &self.inputs {
            for temporary in ctx.db.temporaries(pattern) {
                ctx.write_string("var ");
                ctx.write_node(&temporary);
                ctx.write_string(";");
                ctx.write_line();
            }
        }

        for pattern in &self.inputs {
            ctx.write_string("if (true");
            ctx.write(pattern)?;
            ctx.write_string(") {} else { throw new Error(\"unreachable\"); }");
            ctx.write_line();
        }

        ctx.write_string("return ");
        ctx.write(&self.output)?;
        ctx.write_string(";");
        ctx.write_line();
        ctx.write_string("})");

        Ok(())
    }
}
