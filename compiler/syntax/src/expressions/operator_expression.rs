use crate::{
    expressions::{
        block_expression::BlockExpression, call_expression::CallExpression,
        constructor_expression::ConstructorExpression, parse_expression_element, visit_expression,
    },
    statements::expression_statement::ExpressionStatement,
};

use serde::{Deserialize, Serialize};
use wipple_core::{
    arcstr::Substr,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    span::Span,
    typecheck::{
        constraints::{group_constraint::GroupConstraint, ty_constraint::TyConstraint},
        ty::{ConstructedTy, Ty},
    },
    visit::{Visit, VisitAs, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, ParseToken, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OperatorExpression {
    pub span: Span,
    pub operator: Substr,
    pub operator_span: Span,
    pub left: Box<dyn Visit>,
    pub right: Box<dyn Visit>,
}

#[derive(Clone, Copy)]
enum Associativity {
    Left,
    Right,
}

pub fn parse_parenthesized_operator_expression(
    parser: &mut Parser,
) -> Result<ConstructorExpression, ParseError> {
    let span = parser.spanned();
    parser
        .token(ParseToken::from(TokenKind::LeftParenthesis).reason("between these parentheses"))?;
    parser.consume_line_breaks();
    let operator = parser.token(ParseToken::new(
        TokenKind::is_binary_operator,
        "an operator",
    ))?;
    parser.consume_line_breaks();
    parser.token(TokenKind::RightParenthesis)?;

    let Some(constructor) = name_for_trait_operator(&operator) else {
        return Err(parser.error("This operator cannot be used as a function", None));
    };

    Ok(ConstructorExpression {
        span: span(parser),
        constructor: Substr::from(constructor),
    })
}

pub fn parse_operator_expression(parser: &mut Parser) -> Result<Box<dyn Visit>, ParseError> {
    parse_apply_expression(parser)
}

fn parse_apply_expression(parser: &mut Parser) -> Result<Box<dyn Visit>, ParseError> {
    parse_operator(
        parser,
        &[TokenKind::ApplyOperator],
        Associativity::Left,
        parse_or_expression,
    )
}

fn parse_or_expression(parser: &mut Parser) -> Result<Box<dyn Visit>, ParseError> {
    parse_operator(
        parser,
        &[TokenKind::OrOperator],
        Associativity::Left,
        parse_and_expression,
    )
}

fn parse_and_expression(parser: &mut Parser) -> Result<Box<dyn Visit>, ParseError> {
    parse_operator(
        parser,
        &[TokenKind::AndOperator],
        Associativity::Left,
        parse_equal_expression,
    )
}

fn parse_equal_expression(parser: &mut Parser) -> Result<Box<dyn Visit>, ParseError> {
    parse_operator(
        parser,
        &[TokenKind::EqualOperator, TokenKind::NotEqualOperator],
        Associativity::Left,
        parse_compare_expression,
    )
}

fn parse_compare_expression(parser: &mut Parser) -> Result<Box<dyn Visit>, ParseError> {
    parse_operator(
        parser,
        &[
            TokenKind::LessThanOrEqualOperator,
            TokenKind::LessThanOperator,
            TokenKind::GreaterThanOrEqualOperator,
            TokenKind::GreaterThanOperator,
        ],
        Associativity::Left,
        parse_add_expression,
    )
}

fn parse_add_expression(parser: &mut Parser) -> Result<Box<dyn Visit>, ParseError> {
    parse_operator(
        parser,
        &[TokenKind::AddOperator, TokenKind::SubtractOperator],
        Associativity::Left,
        parse_multiply_expression,
    )
}

fn parse_multiply_expression(parser: &mut Parser) -> Result<Box<dyn Visit>, ParseError> {
    parse_operator(
        parser,
        &[
            TokenKind::MultiplyOperator,
            TokenKind::DivideOperator,
            TokenKind::RemainderOperator,
        ],
        Associativity::Left,
        parse_power_expression,
    )
}

fn parse_power_expression(parser: &mut Parser) -> Result<Box<dyn Visit>, ParseError> {
    parse_operator(
        parser,
        &[TokenKind::PowerOperator],
        Associativity::Right,
        parse_by_expression,
    )
}

fn parse_by_expression(parser: &mut Parser) -> Result<Box<dyn Visit>, ParseError> {
    parse_operator(
        parser,
        &[TokenKind::ByOperator],
        Associativity::Left,
        parse_to_expression,
    )
}

fn parse_to_expression(parser: &mut Parser) -> Result<Box<dyn Visit>, ParseError> {
    parse_operator(
        parser,
        &[TokenKind::ToOperator],
        Associativity::Right,
        parse_expression_element,
    )
}

fn parse_operator(
    parser: &mut Parser,
    operators: &[TokenKind],
    associativity: Associativity,
    parse_element: fn(&mut Parser) -> Result<Box<dyn Visit>, ParseError>,
) -> Result<Box<dyn Visit>, ParseError> {
    let elements = parser.parse_sep(1, parse_element, |parser| {
        parser.consume_line_breaks();

        for &operator in operators {
            if let Some(value) = parser.parse_optional(|parser| parser.token(operator))? {
                parser.consume_line_breaks();
                return Ok(value);
            }
        }

        Err(parser.error("Expected operator", None))
    })?;

    let mut elements = elements.into_iter();
    let (mut result, _) = elements.next().unwrap();
    let mut rest = elements.collect::<Vec<_>>();

    if let Associativity::Right = associativity {
        rest.reverse();
    }

    for (right, (operator, operator_span)) in rest {
        result = Box::new(OperatorExpression {
            span: result.span().join_in(right.span(), &parser.source),
            operator: operator.unwrap(),
            operator_span,
            left: result,
            right,
        });
    }

    Ok(result)
}

pub fn name_for_trait_operator(operator: &str) -> Option<&'static str> {
    match operator {
        "to" => Some("To"),
        "by" => Some("By"),
        "^" => Some("Power"),
        "*" => Some("Multiply"),
        "/" => Some("Divide"),
        "%" => Some("Remainder"),
        "+" => Some("Add"),
        "-" => Some("Subtract"),
        "<" => Some("Less-Than"),
        "<=" => Some("Less-Than-Or-Equal"),
        ">" => Some("Greater-Than"),
        ">=" => Some("Greater-Than-Or-Equal"),
        "=" => Some("Equal"),
        "/=" => Some("Not-Equal"),
        _ => None,
    }
}

#[typetag::serde]
impl Visit for OperatorExpression {
    fn span(&self) -> &Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        let resolved = if let Some(trait_name) = name_for_trait_operator(&self.operator) {
            visit_trait_operator(
                db,
                visitor,
                self.operator_span.clone(),
                self.left.clone(),
                self.right.clone(),
                trait_name,
            )
        } else {
            match self.operator.as_str() {
                "and" => visit_logic_operator(
                    db,
                    visitor,
                    self.operator_span.clone(),
                    self.left.clone(),
                    self.right.clone(),
                    "And",
                ),
                "or" => visit_logic_operator(
                    db,
                    visitor,
                    self.operator_span.clone(),
                    self.left.clone(),
                    self.right.clone(),
                    "Or",
                ),
                "." => visit_apply_operator(db, visitor, self.left.clone(), self.right.clone()),
                _ => unreachable!("unknown operator: {}", self.operator),
            }
        };

        db.graph.replace(node, resolved);
        visitor.constraint(db, GroupConstraint::new(node, resolved));
        visitor.codegen(db, node, OperatorExpressionCodegen { node, resolved });
    }
}

fn visit_trait_operator(
    db: &mut Db,
    visitor: &mut Visitor,
    span: Span,
    left: Box<dyn Visit>,
    right: Box<dyn Visit>,
    trait_name: &str,
) -> Node {
    visitor.visit(
        db,
        Box::new(CallExpression {
            span: span.clone(),
            function: Box::new(ConstructorExpression {
                span,
                constructor: Substr::from(trait_name),
            }),
            inputs: vec![left, right],
        }),
    )
}

fn visit_logic_operator(
    db: &mut Db,
    visitor: &mut Visitor,
    span: Span,
    left: Box<dyn Visit>,
    right: Box<dyn Visit>,
    trait_name: &str,
) -> Node {
    let node = db.node();
    let operator_node = db.node();
    let left_node = db.node();
    let right_node = db.node();

    visitor.constraint(
        db,
        TyConstraint::new(
            operator_node,
            ConstructedTy::function(
                vec![
                    Ty::Node(left_node),
                    Ty::Constructed(ConstructedTy::block(Ty::Node(right_node))),
                ],
                Ty::Node(node),
            ),
        ),
    );

    visitor.visit_as(
        db,
        Box::new(CallExpression {
            span: span.clone(),
            function: Box::new(VisitAs {
                node: operator_node,
                syntax: Box::new(ConstructorExpression {
                    span: span.clone(),
                    constructor: Substr::from(trait_name),
                }),
            }),
            inputs: vec![
                Box::new(VisitAs {
                    node: left_node,
                    syntax: left,
                }),
                Box::new(BlockExpression {
                    span: right.span().clone(),
                    statements: vec![Box::new(ExpressionStatement {
                        span: right.span().clone(),
                        expression: Box::new(VisitAs {
                            node: right_node,
                            syntax: right,
                        }),
                    })],
                }),
            ],
        }),
        node,
    );

    node
}

fn visit_apply_operator(
    db: &mut Db,
    visitor: &mut Visitor,
    left: Box<dyn Visit>,
    right: Box<dyn Visit>,
) -> Node {
    let node = db.node();
    let left_node = db.node();
    let right_node = db.node();

    visitor.constraint(
        db,
        TyConstraint::new(
            right_node,
            ConstructedTy::function(vec![Ty::Node(left_node)], Ty::Node(node)),
        ),
    );

    visitor.visit_as(
        db,
        Box::new(CallExpression {
            span: right.span().clone(),
            function: Box::new(VisitAs {
                node: right_node,
                syntax: right,
            }),
            inputs: vec![Box::new(VisitAs {
                node: left_node,
                syntax: left,
            })],
        }),
        node,
    );

    node
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct OperatorExpressionCodegen {
    node: Node,
    resolved: Node,
}

#[typetag::serde]
impl CodegenValue for OperatorExpressionCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        ctx.codegen(db, self.resolved)?;

        ctx.instruction(ir::Instruction::Value {
            node: self.node,
            value: ir::Value::Variable(self.resolved),
        });

        Ok(())
    }
}
