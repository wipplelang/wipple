use crate::{
    codegen::{Codegen, CodegenCtx, CodegenResult, ir},
    database::{Fact, HiddenNode, Node, NodeRef, Render},
    nodes::{parse_comments, parse_statements, visit_expression},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::{Type, TypeConstraint},
    visit::{Captures, Visit, Visitor},
};

#[derive(Debug, Clone, Default)]
struct ResolvedBlock {
    unit_temporary: Option<NodeRef>,
}

impl Fact for ResolvedBlock {}

impl Render for ResolvedBlock {}

#[derive(Debug)]
pub struct BlockExpressionNode {
    pub statements: Vec<NodeRef>,
}

impl Node for BlockExpressionNode {}

pub fn parse_block_expression(parser: &mut Parser<'_>) -> Result<BlockExpressionNode, ParseError> {
    parser.token(TokenKind::LeftBrace)?;

    let statements = parse_statements(parser)?;
    let _ = parse_comments(parser)?;

    parser.token(TokenKind::RightBrace)?;

    Ok(BlockExpressionNode { statements })
}

impl Visit for BlockExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        visitor.push_scope();

        for statement in &self.statements {
            visitor.visit(statement);
            visitor.edge(statement, node, "statement");
        }

        visitor.after_all_expressions({
            let node = node.clone();
            move |visitor| {
                let captures = visitor.peek_scope_mut().captured_variables.clone();
                visitor.insert(&node, Captures(captures));
            }
        });

        visitor.pop_scope();

        let output = self
            .statements
            .last()
            .cloned()
            .map(Type::from)
            .unwrap_or_else(|| {
                let span = visitor.span(node);
                let unit_temporary = visitor.node(span, HiddenNode::default());

                visitor.insert(
                    node,
                    ResolvedBlock {
                        unit_temporary: Some(unit_temporary),
                    },
                );

                Type::from(visitor.unit_type())
            });

        visitor.constraint(TypeConstraint::new(
            node.clone(),
            visitor.block_type(output),
        ));
    }
}

impl Codegen for BlockExpressionNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> CodegenResult {
        let ResolvedBlock { unit_temporary } = ctx.get(node).unwrap_or_default();
        let Captures(captures) = ctx.get(node).unwrap_or_default();

        ctx.push_instructions();

        for (index, statement) in self.statements.iter().enumerate() {
            ctx.instruction(ir::Instruction::Trace {
                location: statement.clone(),
            });

            ctx.codegen(statement)?;

            if unit_temporary.is_none() && index + 1 == self.statements.len() {
                ctx.instruction(ir::Instruction::Return {
                    value: statement.clone(),
                });
            }
        }

        if let Some(unit_temporary) = unit_temporary {
            ctx.instruction(ir::Instruction::Value {
                node: unit_temporary.clone(),
                value: ir::Value::Tuple(Vec::new()),
            });

            ctx.instruction(ir::Instruction::Return {
                value: unit_temporary,
            });
        }

        let instructions = ctx.pop_instructions();

        ctx.instruction(ir::Instruction::Value {
            node: node.clone(),
            value: ir::Value::Function {
                inputs: Vec::new(),
                captures: Vec::from_iter(captures),
                instructions,
            },
        });

        Ok(())
    }
}
