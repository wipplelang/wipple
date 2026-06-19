use crate::{
    expressions::{
        block_expression::BlockExpression, function_expression::FunctionExpression,
        parse_expression,
    },
    patterns::{parse_pattern, variable_pattern::VariablePattern},
    statements::{parse_comments, visit_statement},
};
use serde::{Deserialize, Serialize};
use wipple_core::{
    ast::AstKey,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    span::Span,
    typecheck::{
        constraints::ty_constraint::TyConstraint,
        ty::{ConstructedTy, Ty},
    },
    visit::{
        Visit, Visitor,
        definitions::{ConstantDefinition, ConstantValue},
    },
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AssignmentStatement {
    pub span: Span,
    pub pattern: AstKey,
    pub value: AstKey,
}

pub fn parse_assignment_statement(
    parser: &mut Parser<'_>,
) -> Result<AssignmentStatement, ParseError> {
    let span = parser.spanned();
    let _ = parse_comments(parser)?;
    let pattern = parse_pattern(parser)?;
    parser.token(TokenKind::AssignOperator)?;
    parser.commit("in this variable assignment");
    parser.consume_line_breaks();
    let value = parse_expression(parser)?;
    Ok(AssignmentStatement {
        span: span(parser),
        pattern,
        value,
    })
}

#[typetag::serde]
impl Visit for AssignmentStatement {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn is_hidden(&self, _db: &Db) -> bool {
        true
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_statement(db, node, visitor);

        visitor.constraint(
            db,
            TyConstraint::new(node, Ty::Constructed(ConstructedTy::unit())),
        );

        // Try assigning to an existing constant in the current scope if possible
        if let Some(pattern) = db.ast(&self.pattern).downcast_ref::<VariablePattern>()
            && let Some((definition_node, _)) = visitor
                .current_scope_mut()
                .peek_as::<ConstantDefinition>(&pattern.variable)
                .into_iter()
                .next()
        {
            if db.contains::<ConstantValue>(definition_node) {
                // TODO: Already assigned
                return;
            }

            visitor.within_definition::<ConstantDefinition>(
                db,
                definition_node,
                |db, visitor, _| {
                    visitor.with_definition_flag(
                        |d| &mut d.within_constant_value,
                        |visitor| {
                            let value = visitor.visit(db, &self.value);
                            visitor.constraint(
                                db,
                                TyConstraint::new(value, Ty::Node(definition_node)),
                            );
                            db.insert(definition_node, ConstantValue(value));
                        },
                    );
                },
            );

            visitor.codegen(db, node, AssignmentStatementCodegen::Constant);
            return;
        }

        let pattern = |db: &mut Db, value: Node, visitor: &mut Visitor| {
            visitor.matching(
                value,
                |current_match| current_match.allow_set = true,
                |visitor| {
                    let current_match = visitor.current_match.as_mut().unwrap();

                    current_match.root = Some(value);

                    let pattern = db.node();
                    current_match.arm = Some(pattern);

                    visitor.visit_as(db, &self.pattern, pattern);

                    pattern
                },
            )
        };

        let allow_recursion = {
            let value = self.value.get(db);

            value.downcast_ref::<FunctionExpression>().is_some()
                || value.downcast_ref::<BlockExpression>().is_some()
        };

        // If recursion is allowed, visit the pattern before the value
        let (pattern, value) = if allow_recursion {
            let value = db.node();
            let pattern = pattern(db, value, visitor);
            visitor.visit_as(db, &self.value, value);
            (pattern, value)
        } else {
            let value = visitor.visit(db, &self.value);
            let pattern = pattern(db, value, visitor);
            (pattern, value)
        };

        visitor.constraint(db, TyConstraint::new(pattern, Ty::Node(value)));

        visitor.codegen(
            db,
            node,
            AssignmentStatementCodegen::Variable {
                node,
                pattern,
                value,
            },
        );
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum AssignmentStatementCodegen {
    Constant,
    Variable {
        node: Node,
        pattern: Node,
        value: Node,
    },
}

#[typetag::serde]
impl CodegenValue for AssignmentStatementCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        match *self {
            AssignmentStatementCodegen::Constant => {}
            AssignmentStatementCodegen::Variable {
                node,
                pattern,
                value,
            } => {
                ctx.codegen(db, value)?;

                ctx.push_conditions();
                ctx.codegen(db, pattern)?;
                let conditions = ctx.pop_conditions();

                ctx.instruction(ir::Instruction::If {
                    node: None,
                    branches: vec![(conditions, Vec::new(), None)],
                    else_branch: None,
                });

                ctx.instruction(ir::Instruction::Value {
                    node,
                    value: ir::Value::Tuple(Vec::new()),
                });
            }
        }

        Ok(())
    }
}
