use crate::{
    expressions::{
        block_expression::BlockExpression, call_expression::CallExpression,
        constructor_expression::ConstructorExpression, parse_expression_element, visit_expression,
    },
    statements::expression_statement::ExpressionStatement,
};

use serde::{Deserialize, Serialize};
use wipple_core::{
    ast::AstKey,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    span::{Span, Str},
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
    pub operator: Str,
    pub operator_span: Span,
    pub left: AstKey,
    pub right: AstKey,
}

#[derive(Clone, Copy)]
enum Associativity {
    Left,
    Right,
}

pub fn parse_parenthesized_operator_expression(
    parser: &mut Parser<'_>,
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
        constructor: Str::from(constructor),
    })
}

pub fn parse_operator_expression(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_apply_expression(parser)
}

fn parse_apply_expression(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_operator(
        parser,
        &[TokenKind::ApplyOperator],
        Associativity::Left,
        parse_or_expression,
    )
}

fn parse_or_expression(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_operator(
        parser,
        &[TokenKind::OrOperator],
        Associativity::Left,
        parse_and_expression,
    )
}

fn parse_and_expression(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_operator(
        parser,
        &[TokenKind::AndOperator],
        Associativity::Left,
        parse_equal_expression,
    )
}

fn parse_equal_expression(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_operator(
        parser,
        &[TokenKind::EqualOperator, TokenKind::NotEqualOperator],
        Associativity::Left,
        parse_compare_expression,
    )
}

fn parse_compare_expression(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
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

fn parse_add_expression(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_operator(
        parser,
        &[TokenKind::AddOperator, TokenKind::SubtractOperator],
        Associativity::Left,
        parse_multiply_expression,
    )
}

fn parse_multiply_expression(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
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

fn parse_power_expression(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_operator(
        parser,
        &[TokenKind::PowerOperator],
        Associativity::Right,
        parse_by_expression,
    )
}

fn parse_by_expression(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_operator(
        parser,
        &[TokenKind::ByOperator],
        Associativity::Left,
        parse_to_expression,
    )
}

fn parse_to_expression(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_operator(
        parser,
        &[TokenKind::ToOperator],
        Associativity::Right,
        parse_expression_element,
    )
}

fn parse_operator(
    parser: &mut Parser<'_>,
    operators: &[TokenKind],
    associativity: Associativity,
    parse_element: fn(&mut Parser<'_>) -> Result<AstKey, ParseError>,
) -> Result<AstKey, ParseError> {
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
        result = parser.in_ast(OperatorExpression {
            span: result
                .get(parser.db)
                .span(parser.db)
                .join_in(right.get(parser.db).span(parser.db), parser.source.clone()),
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
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
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
    left: AstKey,
    right: AstKey,
    trait_name: &str,
) -> Node {
    let function = visitor.in_ast(
        db,
        Box::new(ConstructorExpression {
            span: span.clone(),
            constructor: Str::from(trait_name),
        }),
    );

    let call = visitor.in_ast(
        db,
        Box::new(CallExpression {
            span,
            function,
            inputs: vec![left, right],
        }),
    );

    visitor.visit(db, &call)
}

fn visit_logic_operator(
    db: &mut Db,
    visitor: &mut Visitor,
    span: Span,
    left: AstKey,
    right: AstKey,
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

    let function = visitor.in_ast(
        db,
        Box::new(ConstructorExpression {
            span: span.clone(),
            constructor: Str::from(trait_name),
        }),
    );

    let left_input = visitor.in_ast(
        db,
        Box::new(VisitAs {
            node: left_node,
            syntax: left,
        }),
    );

    let right_input = visitor.in_ast(
        db,
        Box::new(VisitAs {
            node: right_node,
            syntax: right.clone(),
        }),
    );

    let statement = visitor.in_ast(
        db,
        Box::new(ExpressionStatement {
            span: db.ast(&right).span(db).clone(),
            expression: right_input,
        }),
    );

    let block = visitor.in_ast(
        db,
        Box::new(BlockExpression {
            span: db.ast(&right).span(db).clone(),
            statements: vec![statement],
        }),
    );

    let call = visitor.in_ast(
        db,
        Box::new(CallExpression {
            span,
            function,
            inputs: vec![left_input, block],
        }),
    );

    visitor.visit_as(db, &call, node);

    node
}

fn visit_apply_operator(db: &mut Db, visitor: &mut Visitor, left: AstKey, right: AstKey) -> Node {
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

    let function = visitor.in_ast(
        db,
        Box::new(VisitAs {
            node: right_node,
            syntax: right.clone(),
        }),
    );

    let input = visitor.in_ast(
        db,
        Box::new(VisitAs {
            node: left_node,
            syntax: left,
        }),
    );

    let call = visitor.in_ast(
        db,
        Box::new(CallExpression {
            span: db.ast(&right).span(db).clone(),
            function,
            inputs: vec![input],
        }),
    );

    visitor.visit_as(db, &call, node);

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
