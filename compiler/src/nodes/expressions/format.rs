use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Db, Fact, HiddenNode, Node, NodeRef, Render},
    nodes::{ConstructorExpressionNode, NamedTypeNode, parse_atomic_expression, visit_expression},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::{GroupConstraint, TypeConstraint},
    visit::{Visit, Visitor},
};

#[derive(Debug, Clone)]
pub struct MissingFormatInputs(pub usize);

impl Fact for MissingFormatInputs {}

impl Render for MissingFormatInputs {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is missing {} format inputs", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct ExtraFormatInput;

impl Fact for ExtraFormatInput {}

impl Render for ExtraFormatInput {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is extra format input")
    }
}

#[derive(Debug, Clone)]
pub struct FormatSegments {
    pub segments: Vec<FormatSegment>,
    pub trailing: String,
}

impl Fact for FormatSegments {}

impl Render for FormatSegments {}

#[derive(Debug, Clone)]
pub struct FormatSegment {
    pub string: String,
    pub describe_node: NodeRef,
    pub input: NodeRef,
}

#[derive(Debug)]
pub struct FormatExpressionNode {
    pub string: String,
    pub inputs: Vec<NodeRef>,
}

impl Node for FormatExpressionNode {}

pub fn parse_format_expression(
    parser: &mut Parser<'_>,
) -> Result<FormatExpressionNode, ParseError> {
    let string = parser.token(TokenKind::String)?;
    let inputs = parser
        .parse_many(1, parse_atomic_expression, |parser| parser.parse_nothing())?
        .into_iter()
        .map(|(node, _)| node)
        .collect();

    Ok(FormatExpressionNode { string, inputs })
}

impl Visit for FormatExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        let mut segments = self.string.split('_').collect::<Vec<_>>();
        let trailing = segments.pop().unwrap_or_default().to_string();

        let span = visitor.span(node);
        let string_type = visitor.node(
            span,
            HiddenNode::new(NamedTypeNode {
                name: String::from("String"),
                parameters: Vec::new(),
            }),
        );

        visitor.visit(&string_type);
        visitor.constraint(GroupConstraint::new(node.clone(), string_type.clone()));

        let mut segments = segments.into_iter();
        let mut inputs = self.inputs.iter().cloned();

        let format_segments = segments
            .by_ref()
            .zip(inputs.by_ref())
            .map(|(segment, input)| {
                visitor.visit(&input);
                visitor.edge(&input, node, "input");

                let span = visitor.span(node);
                let describe_node = visitor.node(
                    span,
                    HiddenNode::new(ConstructorExpressionNode {
                        constructor: String::from("Describe"),
                    }),
                );

                visitor.visit(&describe_node);
                visitor.constraint(TypeConstraint::new(
                    describe_node.clone(),
                    visitor.function_type([input.clone()], string_type.clone()),
                ));

                FormatSegment {
                    string: segment.to_string(),
                    describe_node,
                    input,
                }
            })
            .collect::<Vec<_>>();

        let missing = segments.collect::<Vec<_>>();
        let extra = inputs.collect::<Vec<_>>();

        if !missing.is_empty() {
            visitor.insert(node, MissingFormatInputs(missing.len()));
        }

        for node in extra {
            visitor.insert(&node, ExtraFormatInput);
        }

        visitor.insert(
            node,
            FormatSegments {
                segments: format_segments,
                trailing,
            },
        );
    }
}

impl Codegen for FormatExpressionNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        let FormatSegments { segments, trailing } = ctx
            .db
            .get::<FormatSegments>(ctx.current_node())
            .ok_or_else(|| ctx.error())?;

        ctx.write_string("(\"\"");

        for segment in segments {
            ctx.write_string(" + ");
            ctx.write_string(serde_json::to_string(&segment.string).unwrap());
            ctx.write_string(" + ");

            ctx.write_string("await (");
            ctx.write(&segment.describe_node)?;
            ctx.write_string(")(");
            ctx.write(&segment.input)?;
            ctx.write_string(")");
        }

        ctx.write_string(" + ");
        ctx.write_string(serde_json::to_string(&trailing).unwrap());
        ctx.write_string(")");

        Ok(())
    }
}
