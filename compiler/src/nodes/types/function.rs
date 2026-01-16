use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Node, NodeRef},
    nodes::{parse_atomic_type, parse_type, visit_type},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::TypeConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct FunctionTypeNode {
    pub inputs: Vec<NodeRef>,
    pub output: NodeRef,
}

impl Node for FunctionTypeNode {}

pub fn parse_function_type(parser: &mut Parser<'_>) -> Result<FunctionTypeNode, ParseError> {
    let inputs = parse_function_type_inputs(parser)?;
    let output = parse_type(parser)?;

    Ok(FunctionTypeNode { inputs, output })
}

pub fn parse_function_type_inputs(parser: &mut Parser<'_>) -> Result<Vec<NodeRef>, ParseError> {
    let inputs = parser
        .parse_many(1, parse_atomic_type, |parser| parser.parse_nothing())?
        .into_iter()
        .map(|(node, _)| node)
        .collect::<Vec<_>>();

    parser.token(TokenKind::FunctionOperator)?;
    parser.commit("in this function type");
    parser.consume_line_breaks();

    Ok(inputs)
}

impl Visit for FunctionTypeNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_type(node, visitor);

        for input in &self.inputs {
            visitor.visit(input);
            visitor.edge(input, node, "input");
        }

        visitor.visit(&self.output);
        visitor.edge(&self.output, node, "output");

        visitor.constraint(TypeConstraint::new(
            node.clone(),
            visitor.function_type(self.inputs.iter().cloned(), self.output.clone()),
        ));
    }
}

impl Codegen for FunctionTypeNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        Err(ctx.error())
    }
}
