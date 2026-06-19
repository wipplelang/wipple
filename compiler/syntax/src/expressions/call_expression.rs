use crate::expressions::{
    parse_atomic_expression, variable_expression::VariableExpression, visit_expression,
};

use serde::{Deserialize, Serialize};
use wipple_core::{
    ast::AstKey,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Fact, Node},
    render::Render,
    span::Span,
    typecheck::{
        constraints::{ConstraintTrace, ty_constraint::TyConstraint},
        ty::{ConstructedTy, Ty},
    },
    visit::{Visit, Visitor, definitions::ConstantDefinition},
};
use wipple_parse::parser::{ParseError, Parser};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedCall {
    pub function: Node,
    pub inputs: Vec<Node>,
    pub is_unit: bool,
}

#[typetag::serde]
impl Fact for ResolvedCall {}

impl Render for ResolvedCall {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CallExpression {
    pub span: Span,
    pub function: AstKey,
    pub inputs: Vec<AstKey>,
}

pub fn parse_call_expression(parser: &mut Parser<'_>) -> Result<CallExpression, ParseError> {
    let span = parser.spanned();
    let function = parse_atomic_expression(parser)?;
    let inputs = parser.parse_many(1, parse_atomic_expression)?;
    Ok(CallExpression {
        span: span(parser),
        function,
        inputs,
    })
}

#[typetag::serde]
impl Visit for CallExpression {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        if self.inputs.len() == 1
            && let Some(variable) = db.ast(&self.inputs[0]).downcast_ref::<VariableExpression>()
        {
            let unit_constant = visitor
                .peek_as::<ConstantDefinition>(&variable.variable)
                .into_iter()
                .find(|(_, definition)| definition.attributes.unit);

            if unit_constant.is_some() {
                let unit = visitor.visit(db, &self.inputs.into_iter().next().unwrap());
                let number = visitor.visit(db, &self.function);

                visitor.constraint(
                    db,
                    TyConstraint::new(
                        unit,
                        Ty::Constructed(ConstructedTy::function(vec![number], node)),
                    ),
                );

                db.graph.edge(number, node, "number");
                db.graph.edge(unit, node, "unit");

                db.insert(
                    node,
                    ResolvedCall {
                        function: unit,
                        inputs: vec![number],
                        is_unit: true,
                    },
                );

                visitor.codegen(db, node, CallExpressionCodegen::Unit { node, number, unit });
                return;
            }
        }

        let function = visitor.visit(db, &self.function);

        let inputs = self
            .inputs
            .into_iter()
            .map(|input| visitor.visit(db, &input))
            .collect::<Vec<_>>();

        let mut constraint = TyConstraint::new(
            function,
            Ty::Constructed(ConstructedTy::function(inputs.clone(), node)),
        )
        .with_trace(CallOutputConstraintTrace { function, node });

        for &input in &inputs {
            constraint = constraint.with_trace(CallInputConstraintTrace {
                node,
                function,
                input,
            });
        }

        visitor.constraint(db, constraint);

        db.insert(
            node,
            ResolvedCall {
                function,
                inputs: inputs.clone(),
                is_unit: false,
            },
        );

        visitor.codegen(
            db,
            node,
            CallExpressionCodegen::Function {
                node,
                function,
                inputs,
            },
        );
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CallInputConstraintTrace {
    node: Node,
    function: Node,
    input: Node,
}

#[typetag::serde]
impl ConstraintTrace for CallInputConstraintTrace {
    fn nodes_mut(&mut self) -> Vec<&mut Node> {
        vec![&mut self.node, &mut self.function, &mut self.input]
    }

    fn nodes(&self, _db: &Db) -> Vec<Node> {
        vec![self.node, self.function, self.input]
    }
}

impl Render for CallInputConstraintTrace {}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CallOutputConstraintTrace {
    function: Node,
    node: Node,
}

#[typetag::serde]
impl ConstraintTrace for CallOutputConstraintTrace {
    fn nodes_mut(&mut self) -> Vec<&mut Node> {
        vec![&mut self.node, &mut self.function]
    }

    fn nodes(&self, _db: &Db) -> Vec<Node> {
        vec![self.node, self.function]
    }
}

impl Render for CallOutputConstraintTrace {}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum CallExpressionCodegen {
    Function {
        node: Node,
        function: Node,
        inputs: Vec<Node>,
    },
    Unit {
        node: Node,
        number: Node,
        unit: Node,
    },
}

#[typetag::serde]
impl CodegenValue for CallExpressionCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        match self {
            CallExpressionCodegen::Function {
                node,
                function,
                inputs,
            } => {
                ctx.codegen(db, *function)?;

                for &input in inputs {
                    ctx.codegen(db, input)?;
                }

                ctx.instruction(ir::Instruction::Value {
                    node: *node,
                    value: ir::Value::Call {
                        function: *function,
                        inputs: inputs.clone(),
                    },
                });
            }
            CallExpressionCodegen::Unit { node, number, unit } => {
                ctx.codegen(db, *number)?;

                ctx.codegen(db, *unit)?;

                ctx.instruction(ir::Instruction::Value {
                    node: *node,
                    value: ir::Value::Call {
                        function: *unit,
                        inputs: vec![*number],
                    },
                });
            }
        }

        Ok(())
    }
}
