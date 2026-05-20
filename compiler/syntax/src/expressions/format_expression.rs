use crate::{
    expressions::{
        constructor_expression::ConstructorExpression, parse_atomic_expression, visit_expression,
    },
    types::named_type::NamedType,
};

use serde::{Deserialize, Serialize};
use wipple_core::{
    arcstr::Substr,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Fact, Node},
    render::Render,
    span::Span,
    typecheck::{
        constraints::{group_constraint::GroupConstraint, ty_constraint::TyConstraint},
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
    pub string: Substr,
    pub inputs: Vec<Box<dyn Visit>>,
}

pub fn parse_format_expression(parser: &mut Parser) -> Result<FormatExpression, ParseError> {
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
    fn span(&self) -> &Span {
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

        let string_type = Box::new(NamedType {
            span: self.span.clone(),
            name: Substr::from("String"),
            parameters: Vec::new(),
        }) as Box<dyn Visit>;
        let string_type = visitor.visit(db, string_type);

        visitor.constraint(db, GroupConstraint::new(node, string_type));

        let inputs = self
            .inputs
            .into_iter()
            .map(|input| visitor.visit(db, input))
            .collect::<Vec<_>>();

        let mut format_segments = Vec::new();
        for (segment, input) in segments.iter().zip(&inputs) {
            db.graph.edge(*input, node, "input");

            let describe_node = Box::new(ConstructorExpression {
                span: self.span.clone(),
                constructor: Substr::from("Describe"),
            }) as Box<dyn Visit>;
            let describe_node = visitor.visit(db, describe_node);

            visitor.constraint(
                db,
                TyConstraint::new(
                    describe_node,
                    ConstructedTy::function(vec![Ty::Node(*input)], Ty::Node(string_type)),
                ),
            );

            let temporary = db.node();
            visitor.constraint(db, GroupConstraint::new(temporary, string_type));

            format_segments.push(FormatExpressionSegment {
                string: segment.clone(),
                describe_node,
                input: *input,
                temporary,
            });
        }

        if segments.len() > inputs.len() {
            db.insert(node, MissingFormatInputs(segments.len() - inputs.len()));
        }

        for &input in inputs.iter().skip(segments.len()) {
            db.insert(input, ExtraFormatInput);
        }

        visitor.codegen(
            db,
            node,
            FormatExpressionCodegen {
                node,
                segments: format_segments,
                trailing,
            },
        );
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct FormatExpressionSegment {
    string: String,
    describe_node: Node,
    input: Node,
    temporary: Node,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct FormatExpressionCodegen {
    node: Node,
    segments: Vec<FormatExpressionSegment>,
    trailing: String,
}

#[typetag::serde]
impl CodegenValue for FormatExpressionCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        let segments = self
            .segments
            .iter()
            .map(|segment| {
                ctx.codegen(db, segment.describe_node)?;

                ctx.codegen(db, segment.input)?;

                ctx.instruction(ir::Instruction::Value {
                    node: segment.temporary,
                    value: ir::Value::Call {
                        function: segment.describe_node,
                        inputs: vec![segment.input],
                    },
                });

                Ok((segment.string.clone(), segment.temporary))
            })
            .collect::<Result<Vec<_>, CodegenError>>()?;

        ctx.instruction(ir::Instruction::Value {
            node: self.node,
            value: ir::Value::Concat {
                segments,
                trailing: self.trailing.clone(),
            },
        });

        Ok(())
    }
}
