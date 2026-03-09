use crate::{
    codegen::{Codegen, CodegenCtx, ir},
    database::{Fact, HiddenNode, Node, NodeRef, Render},
    nodes::{VariablePatternNode, parse_comments, parse_expression, parse_pattern},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::{GroupConstraint, TypeConstraint},
    visit::{Definition, Visit, Visitor},
};

#[derive(Debug, Clone)]
pub struct ResolvedConstantAssignment;

impl Fact for ResolvedConstantAssignment {}

impl Render for ResolvedConstantAssignment {}

#[derive(Debug, Clone)]
pub struct ResolvedVariableAssignment {
    pub temporary: NodeRef,
}

impl Fact for ResolvedVariableAssignment {}

impl Render for ResolvedVariableAssignment {}

#[derive(Debug)]
pub struct AssignmentNode {
    pub pattern: NodeRef,
    pub value: NodeRef,
}

impl Node for AssignmentNode {
    fn is_hidden(&self) -> bool {
        true
    }
}

pub fn parse_assignment_statement(parser: &mut Parser<'_>) -> Result<AssignmentNode, ParseError> {
    let _ = parse_comments(parser)?;

    let pattern = parse_pattern(parser)?;

    parser.token(TokenKind::AssignOperator)?;
    parser.commit("in this variable assignment");
    parser.consume_line_breaks();

    let value = parse_expression(parser)?;

    Ok(AssignmentNode { pattern, value })
}

impl Visit for AssignmentNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visitor.constraint(TypeConstraint::new(node.clone(), visitor.unit_type()));

        let pattern = self.pattern.clone();
        let value = self.value.clone();
        let node = node.clone();
        visitor.after_all_definitions(move |visitor| {
            // Try assigning to an existing constant if possible
            if let Some(pattern_node) = pattern.downcast_ref::<VariablePatternNode>() {
                let found_constant = visitor.with(&pattern_node.variable, |visitor, definition| {
                    let Definition::Constant(definition) = definition else {
                        return None;
                    };

                    if definition.value.is_some() {
                        // TODO: Already assigned
                        return None;
                    }

                    let mut definition = definition.clone();
                    definition.value = Some(value.clone());

                    let definition_node = definition.node.clone();
                    visitor.defining(&definition_node, |visitor| {
                        visitor.within_constant_value(|visitor| {
                            visitor.visit(&value);
                            visitor.constraint(GroupConstraint::new(
                                value.clone(),
                                definition_node.clone(),
                            ));
                        });

                        definition.clone()
                    });

                    visitor.insert(&node, ResolvedConstantAssignment);

                    Some(definition.into())
                });

                if found_constant {
                    return;
                }
            }

            visitor.visit(&value);
            visitor.edge(&value, &pattern, "value");

            let span = visitor.db.span(&node);
            let temporary = visitor.db.node(span, HiddenNode(None));

            visitor.matching(&temporary, false, true, |visitor| {
                visitor.current_match().root = Some(value.clone());
                visitor.current_match().arm = Some(pattern.clone());
                visitor.visit(&pattern);
            });

            visitor.constraint(GroupConstraint::new(pattern, value));

            visitor.insert(&node, ResolvedVariableAssignment { temporary });
        });
    }
}

impl Codegen for AssignmentNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> Option<ir::SpannedExpression> {
        let Some(ResolvedVariableAssignment {
            temporary: assignment_temporary,
        }) = ctx.get::<ResolvedVariableAssignment>(node)
        else {
            return Some(ir::Expression::NoOp.at(node, ctx)); // assigned to constant
        };

        let mut statements = Vec::new();

        statements.push(ir::Expression::Declare(assignment_temporary.clone()).at(node, ctx));

        for temporary in ctx.db.temporaries(&self.pattern) {
            if temporary == assignment_temporary {
                continue;
            }

            statements.push(ir::Expression::Declare(temporary.clone()).at(node, ctx));
        }

        statements.extend([
            ir::Expression::AssignTo(Box::new(ctx.codegen(&self.value)?), assignment_temporary)
                .at(node, ctx),
            ir::Expression::If(vec![(ctx.codegen(&self.pattern)?, None)], None).at(node, ctx),
        ]);

        Some(ir::Expression::Sequence(statements).at(node, ctx))
    }
}
