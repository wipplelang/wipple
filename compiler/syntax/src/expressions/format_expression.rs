use crate::{
    expressions::{
        constructor_expression::ConstructorExpression, parse_atomic_expression, visit_expression,
    },
    types::named_type::NamedType,
};
use serde::{Deserialize, Serialize};
use wipple_core::{
    anyhow,
    ast::AstKey,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Fact, Node},
    render::Render,
    span::{Span, Str},
    typecheck::{
        constraints::ty_constraint::TyConstraint,
        ty::{ConstructedTy, Ty},
    },
    visit::{Visit, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MissingFormatInputs(pub usize);

#[typetag::serde]
impl Fact for MissingFormatInputs {}

impl Render for MissingFormatInputs {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExtraFormatInput;

#[typetag::serde]
impl Fact for ExtraFormatInput {}

impl Render for ExtraFormatInput {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FormatExpression {
    pub span: Span,
    pub string: Str,
    pub inputs: Vec<AstKey>,
}

pub fn parse_format_expression(parser: &mut Parser<'_>) -> Result<FormatExpression, ParseError> {
    let span = parser.spanned();
    let string = parser.token(TokenKind::String)?;
    let inputs = parser.parse_many(1, parse_atomic_expression)?;
    Ok(FormatExpression {
        span: span(parser),
        string,
        inputs,
    })
}

#[typetag::serde]
impl Visit for FormatExpression {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        let mut segments = self
            .string
            .split('_')
            .map(str::to_string)
            .collect::<Vec<_>>();
        let trailing = segments.pop().unwrap_or_default();

        let string_type = visitor.in_ast(
            db,
            Box::new(NamedType {
                span: self.span.clone(),
                name: Str::from("String"),
                parameters: Vec::new(),
            }),
        );
        let string_type = visitor.visit(db, &string_type);

        visitor.constraint(db, TyConstraint::new(node, Ty::Node(string_type)));

        let inputs = self
            .inputs
            .into_iter()
            .map(|input| visitor.visit(db, &input))
            .collect::<Vec<_>>();

        let mut format_segments = Vec::new();
        for (segment, input) in segments.iter().zip(&inputs) {
            db.graph.edge(*input, node, "input");

            let describe_node = visitor.in_ast(
                db,
                Box::new(ConstructorExpression {
                    span: self.span.clone(),
                    constructor: Str::from("Describe"),
                }),
            );
            let describe_node = visitor.visit(db, &describe_node);

            visitor.constraint(
                db,
                TyConstraint::new(
                    describe_node,
                    Ty::Constructed(ConstructedTy::function(vec![*input], string_type)),
                ),
            );

            let string_temporary = db.node();
            visitor.constraint(
                db,
                TyConstraint::new(string_temporary, Ty::Node(string_type)),
            );

            let described_temporary = db.node();
            visitor.constraint(
                db,
                TyConstraint::new(described_temporary, Ty::Node(string_type)),
            );

            let concat_temporary = db.node();
            visitor.constraint(
                db,
                TyConstraint::new(concat_temporary, Ty::Node(string_type)),
            );

            let output_temporary = db.node();
            visitor.constraint(
                db,
                TyConstraint::new(output_temporary, Ty::Node(string_type)),
            );

            format_segments.push(FormatExpressionSegment {
                string: segment.clone(),
                string_temporary,
                describe_node,
                describe_input: *input,
                described_temporary,
                concat_temporary,
                output_temporary,
            });
        }

        if segments.len() > inputs.len() {
            db.insert(node, MissingFormatInputs(segments.len() - inputs.len()));
        }

        for &input in inputs.iter().skip(segments.len()) {
            db.insert(input, ExtraFormatInput);
        }

        let trailing_temporary = db.node();
        visitor.constraint(
            db,
            TyConstraint::new(trailing_temporary, Ty::Node(string_type)),
        );

        visitor.codegen(
            db,
            node,
            FormatExpressionCodegen {
                node,
                segments: format_segments,
                trailing,
                trailing_temporary,
            },
        );
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct FormatExpressionSegment {
    string: String,
    string_temporary: Node,
    describe_node: Node,
    describe_input: Node,
    described_temporary: Node,
    concat_temporary: Node,
    output_temporary: Node,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct FormatExpressionCodegen {
    node: Node,
    segments: Vec<FormatExpressionSegment>,
    trailing_temporary: Node,
    trailing: String,
}

#[typetag::serde]
impl CodegenValue for FormatExpressionCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        let mut prev = None;
        for segment in &self.segments {
            ctx.instruction(ir::Instruction::Value {
                node: segment.string_temporary,
                value: ir::Value::String(segment.string.clone()),
            });

            ctx.codegen(db, segment.describe_node)?;
            ctx.codegen(db, segment.describe_input)?;

            ctx.instruction(ir::Instruction::Value {
                node: segment.described_temporary,
                value: ir::Value::Call {
                    function: segment.describe_node,
                    inputs: vec![segment.describe_input],
                },
            });

            ctx.instruction(ir::Instruction::Value {
                node: segment.concat_temporary,
                value: ir::Value::Runtime {
                    name: String::from("string-concat"),
                    inputs: vec![segment.string_temporary, segment.described_temporary],
                },
            });

            ctx.instruction(ir::Instruction::Value {
                node: segment.output_temporary,
                value: match prev {
                    Some(prev) => ir::Value::Runtime {
                        name: String::from("string-concat"),
                        inputs: vec![prev, segment.concat_temporary],
                    },
                    None => ir::Value::Variable(segment.concat_temporary),
                },
            });

            prev = Some(segment.output_temporary);
        }

        let prev = prev.ok_or_else(|| anyhow::format_err!("no inputs to format string"))?;

        ctx.instruction(ir::Instruction::Value {
            node: self.trailing_temporary,
            value: ir::Value::String(self.trailing.clone()),
        });

        ctx.instruction(ir::Instruction::Value {
            node: self.node,
            value: ir::Value::Runtime {
                name: String::from("string-concat"),
                inputs: vec![prev, self.trailing_temporary],
            },
        });

        Ok(())
    }
}
