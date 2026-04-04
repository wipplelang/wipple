use crate::{
    codegen::{Codegen, CodegenCtx, CodegenResult, ir},
    database::{Fact, Node, NodeRef, Render, Span},
    nodes::{
        BlockExpressionNode, CallExpressionNode, ConstructorExpressionNode,
        ExpressionStatementNode, parse_expression_element, visit_expression,
    },
    syntax::{ParseError, Parser, TokenKind},
    typecheck::{Type, TypeConstraint},
    visit::{Visit, Visitor},
};

#[derive(Debug, Clone)]
pub struct ResolvedOperator(pub NodeRef);

impl Fact for ResolvedOperator {}

impl Render for ResolvedOperator {}

#[derive(Debug, Clone, Copy)]
enum Associativity {
    Left,
    Right,
}

#[derive(Debug)]
pub struct OperatorExpressionNode {
    pub operator: String,
    pub operator_span: Span,
    pub left: NodeRef,
    pub right: NodeRef,
}

impl Node for OperatorExpressionNode {}

pub fn parse_parenthesized_operator_expression(
    parser: &mut Parser<'_>,
) -> Result<ConstructorExpressionNode, ParseError> {
    parser.token_with_reason(TokenKind::LeftParenthesis, "between these parentheses")?;

    parser.consume_line_breaks();
    let operator = parser.token_with(|k| k.is_binary_operator(), "an operator", None)?;
    parser.consume_line_breaks();

    parser.token(TokenKind::RightParenthesis)?;

    let Some(trait_name) = name_for_trait_operator(&operator) else {
        return Err(parser.error("This operator cannot be used as a function"));
    };

    Ok(ConstructorExpressionNode {
        constructor: String::from(trait_name),
    })
}

pub fn parse_operator_expression(parser: &mut Parser<'_>) -> Result<NodeRef, ParseError> {
    // From highest precedence to lowest precedence

    let parse_to_expression = |parser: &mut Parser<'_>| {
        parse_operator(
            parser,
            &[TokenKind::ToOperator],
            Associativity::Right,
            parse_expression_element,
        )
    };

    let parse_by_expression = |parser: &mut Parser<'_>| {
        parse_operator(
            parser,
            &[TokenKind::ByOperator],
            Associativity::Left,
            parse_to_expression,
        )
    };

    let parse_power_expression = |parser: &mut Parser<'_>| {
        parse_operator(
            parser,
            &[TokenKind::PowerOperator],
            Associativity::Right,
            parse_by_expression,
        )
    };

    let parse_multiply_expression = |parser: &mut Parser<'_>| {
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
    };

    let parse_add_expression = |parser: &mut Parser<'_>| {
        parse_operator(
            parser,
            &[TokenKind::AddOperator, TokenKind::SubtractOperator],
            Associativity::Left,
            parse_multiply_expression,
        )
    };

    let parse_compare_expression = |parser: &mut Parser<'_>| {
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
    };

    let parse_equal_expression = |parser: &mut Parser<'_>| {
        parse_operator(
            parser,
            &[TokenKind::EqualOperator, TokenKind::NotEqualOperator],
            Associativity::Left,
            parse_compare_expression,
        )
    };

    let parse_and_expression = |parser: &mut Parser<'_>| {
        parse_operator(
            parser,
            &[TokenKind::AndOperator],
            Associativity::Left,
            parse_equal_expression,
        )
    };

    let parse_or_expression = |parser: &mut Parser<'_>| {
        parse_operator(
            parser,
            &[TokenKind::OrOperator],
            Associativity::Left,
            parse_and_expression,
        )
    };

    let parse_apply_expression = |parser: &mut Parser<'_>| {
        parse_operator(
            parser,
            &[TokenKind::ApplyOperator],
            Associativity::Left,
            parse_or_expression,
        )
    };

    parse_apply_expression(parser)
}

fn parse_operator(
    parser: &mut Parser<'_>,
    operators: &[TokenKind],
    associativity: Associativity,
    parse_element: impl Copy + Fn(&mut Parser<'_>) -> Result<NodeRef, ParseError>,
) -> Result<NodeRef, ParseError> {
    let elements = parser.parse_many(1, parse_element, |parser| {
        parser.consume_line_breaks();

        for operator in operators {
            if let Some(value) = parser.parse_optional(|parser| parser.token(*operator))? {
                parser.consume_line_breaks();
                return Ok(value);
            }
        }

        Err(parser.error("Expected operator"))
    })?;

    let (first, rest) = elements.split_first().unwrap();
    let mut result = first.0.clone();

    match associativity {
        Associativity::Left => {
            for (right, (operator, operator_span)) in rest {
                let left_span = parser.span(&result);
                let right_span = parser.span(right);
                let span = parser.join_spans(&left_span, &right_span);
                result = parser.register(
                    span,
                    OperatorExpressionNode {
                        operator: operator.clone(),
                        operator_span: operator_span.clone(),
                        left: result,
                        right: right.clone(),
                    },
                );
            }
        }
        Associativity::Right => {
            for (right, (operator, operator_span)) in rest.iter().rev() {
                let left_span = parser.span(&result);
                let right_span = parser.span(right);
                let span = parser.join_spans(&left_span, &right_span);
                result = parser.register(
                    span,
                    OperatorExpressionNode {
                        operator: operator.clone(),
                        operator_span: operator_span.clone(),
                        left: result,
                        right: right.clone(),
                    },
                );
            }
        }
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

impl Visit for OperatorExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        let operator_node = if let Some(trait_name) = name_for_trait_operator(&self.operator) {
            trait_operator(
                visitor,
                &self.operator_span,
                node,
                &self.left,
                &self.right,
                trait_name,
            )
        } else {
            match self.operator.as_str() {
                "and" => logic_operator(
                    visitor,
                    &self.operator_span,
                    node,
                    &self.left,
                    &self.right,
                    "And",
                ),
                "or" => logic_operator(
                    visitor,
                    &self.operator_span,
                    node,
                    &self.left,
                    &self.right,
                    "Or",
                ),
                "." => apply_operator(visitor, node, &self.left, &self.right),
                _ => panic!("unknown operator: {}", self.operator),
            }
        };

        visitor.graph.replace(node, &operator_node);
        visitor.visit(&operator_node);
        visitor.insert(node, ResolvedOperator(operator_node));
    }
}

impl Codegen for OperatorExpressionNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> CodegenResult {
        let ResolvedOperator(resolved) = ctx
            .get::<ResolvedOperator>(node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?;

        ctx.codegen(&resolved)?;

        ctx.instruction(ir::Instruction::Value {
            node: node.clone(),
            value: ir::Value::Variable(resolved),
        });

        Ok(())
    }
}

fn trait_operator(
    visitor: &mut Visitor<'_>,
    operator_span: &Span,
    node: &NodeRef,
    left: &NodeRef,
    right: &NodeRef,
    trait_name: &str,
) -> NodeRef {
    let operator_node = visitor.node(
        operator_span.clone(),
        ConstructorExpressionNode {
            constructor: trait_name.to_string(),
        },
    );

    visitor.constraint(TypeConstraint::new(
        operator_node.clone(),
        visitor.function_type([left.clone(), right.clone()], node.clone()),
    ));

    let span = visitor.span(node);
    visitor.node(
        span,
        CallExpressionNode {
            function: operator_node,
            inputs: vec![left.clone(), right.clone()],
        },
    )
}

fn logic_operator(
    visitor: &mut Visitor<'_>,
    operator_span: &Span,
    node: &NodeRef,
    left: &NodeRef,
    right: &NodeRef,
    trait_name: &str,
) -> NodeRef {
    let operator_node = visitor.node(
        operator_span.clone(),
        ConstructorExpressionNode {
            constructor: trait_name.to_string(),
        },
    );

    let right_span = visitor.span(right);

    let statement = visitor.node(
        right_span.clone(),
        ExpressionStatementNode {
            expression: right.clone(),
        },
    );

    let block = visitor.node(
        right_span,
        BlockExpressionNode {
            statements: vec![statement],
        },
    );

    let inputs = vec![
        Type::from(left.clone()),
        Type::from(visitor.db.block_type(right.clone())),
    ];

    visitor.constraint(TypeConstraint::new(
        operator_node.clone(),
        visitor.function_type::<Type>(inputs, Type::from(node.clone())),
    ));

    let span = visitor.span(node);

    visitor.node(
        span,
        CallExpressionNode {
            function: operator_node,
            inputs: vec![left.clone(), block],
        },
    )
}

fn apply_operator(
    visitor: &mut Visitor<'_>,
    node: &NodeRef,
    left: &NodeRef,
    right: &NodeRef,
) -> NodeRef {
    visitor.constraint(TypeConstraint::new(
        right.clone(),
        visitor.function_type([left.clone()], node.clone()),
    ));

    let span = visitor.span(node);

    visitor.node(
        span,
        CallExpressionNode {
            function: right.clone(),
            inputs: vec![left.clone()],
        },
    )
}
