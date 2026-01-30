use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Fact, HiddenNode, Node, NodeRef, Render},
    nodes::{VariablePatternNode, parse_comments, parse_expression, parse_pattern},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::GroupConstraint,
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

            visitor.matching(temporary.clone(), true, |visitor| {
                visitor.visit(&pattern);
            });

            visitor.constraint(GroupConstraint::new(pattern, value));

            visitor.insert(&node, ResolvedVariableAssignment { temporary });
        });
    }
}

impl Codegen for AssignmentNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        let Some(ResolvedVariableAssignment {
            temporary: assignment_temporary,
        }) = ctx.db.get::<ResolvedVariableAssignment>(ctx.current_node())
        else {
            return Ok(()); // assigned to constant
        };

        ctx.write_string("var ");
        ctx.write_node(&assignment_temporary);
        ctx.write_string(";");
        ctx.write_line();

        for temporary in ctx.db.temporaries(&self.pattern) {
            if temporary == assignment_temporary {
                continue;
            }

            ctx.write_string("var ");
            ctx.write_node(&temporary);
            ctx.write_string(";");
            ctx.write_line();
        }

        ctx.write_node(&assignment_temporary);
        ctx.write_string(" = ");
        ctx.write(&self.value)?;
        ctx.write_string(";");
        ctx.write_line();

        ctx.write_string("if (true");
        ctx.write(&self.pattern)?;
        ctx.write_string(") {} else { throw new Error(\"unreachable\"); }");
        ctx.write_line();

        Ok(())
    }
}
