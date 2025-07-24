use crate::{
    syntax::{
        BinaryOperator, Location,
        parse::{
            Direction, FieldValue, Pattern, Statement, SyntaxKind, Type, base::Rule, name, number,
            pattern, statement, text, r#type,
        },
        tokenize::{Keyword, NonAssociativeOperator, Operator, VariadicOperator},
    },
    util::{DefaultFromInfo, WithInfo},
};
use std::fmt::Debug;

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Error,
    Annotate {
        value: WithInfo<Box<Expression>>,
        r#type: WithInfo<Type>,
    },
    Name(String),
    Number(String),
    Text(String),
    Block(Vec<WithInfo<Statement>>),
    Do(WithInfo<Box<Expression>>),
    Function {
        inputs: Vec<WithInfo<Pattern>>,
        body: WithInfo<Box<Expression>>,
    },
    Call {
        function: WithInfo<Box<Expression>>,
        inputs: Vec<WithInfo<Expression>>,
    },
    Apply {
        input: WithInfo<Box<Expression>>,
        function: WithInfo<Box<Expression>>,
    },
    BinaryOperator {
        operator: WithInfo<BinaryOperator>,
        left: WithInfo<Box<Expression>>,
        right: WithInfo<Box<Expression>>,
    },
    As {
        value: WithInfo<Box<Expression>>,
        r#type: WithInfo<Type>,
    },
    Is {
        value: WithInfo<Box<Expression>>,
        pattern: WithInfo<Pattern>,
    },
    When {
        input: WithInfo<Box<Expression>>,
        arms: Vec<WithInfo<Arm>>,
    },
    Intrinsic {
        name: WithInfo<Option<String>>,
        inputs: Vec<WithInfo<Expression>>,
    },
    Tuple(Vec<WithInfo<Expression>>),
    Collection(Vec<WithInfo<Expression>>),
    Structure(Vec<WithInfo<FieldValue>>),
}

impl DefaultFromInfo for Expression {
    fn default_from_info(info: Location) -> WithInfo<Self> {
        WithInfo {
            info,
            item: Expression::Error,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Arm {
    pub pattern: WithInfo<Pattern>,
    pub body: WithInfo<Expression>,
}

impl DefaultFromInfo for Arm {
    fn default_from_info(info: Location) -> WithInfo<Self> {
        WithInfo {
            info: info.clone(),
            item: Arm {
                pattern: Pattern::default_from_info(info.clone()),
                body: Expression::default_from_info(info),
            },
        }
    }
}

pub fn expression() -> Rule<Expression> {
    Rule::switch(
        SyntaxKind::Expression,
        [
            annotate_expression,
            name_expression,
            number_expression,
            text_expression,
            apply_expression,
            binary_operator_expression,
            as_expression,
            is_expression,
            when_expression,
            intrinsic_expression,
            tuple_expression,
            collection_expression,
            structure_expression,
            block_expression,
            function_expression,
            do_expression,
            call_expression,
        ],
    )
    .unwrap_parentheses()
    .no_backtrack()
    .named("An expression.")
}

pub fn annotate_expression() -> Rule<Expression> {
    Rule::non_associative_operator(
        SyntaxKind::AnnotateExpression,
        NonAssociativeOperator::Annotate,
        expression,
        r#type,
        |_, info, value, r#type, _| WithInfo {
            info,
            item: Expression::Annotate {
                value: value.boxed(),
                r#type,
            },
        },
    )
    .named("Annotate an expression with a type.")
}

pub fn name_expression() -> Rule<Expression> {
    name()
        .wrapped()
        .map(SyntaxKind::NameExpression, |name| {
            Expression::Name(name.item.unwrap())
        })
        .named("A name.")
}

pub fn number_expression() -> Rule<Expression> {
    number()
        .wrapped()
        .map(SyntaxKind::NumberExpression, |number| {
            Expression::Number(number.item.unwrap())
        })
        .named("A number.")
}

pub fn text_expression() -> Rule<Expression> {
    text()
        .wrapped()
        .map(SyntaxKind::TextExpression, |text| {
            Expression::Text(text.item.unwrap())
        })
        .named("A piece of text.")
}

pub fn call_expression() -> Rule<Expression> {
    Rule::list(
        SyntaxKind::CallExpression,
        expression,
        |_, info, expressions, stack| match expressions.len() {
            0 => {
                stack.error_expected(
                    WithInfo {
                        info: Location::clone(&info),
                        item: SyntaxKind::Expression,
                    },
                    Direction::After(SyntaxKind::LeftParenthesis),
                );

                WithInfo {
                    info,
                    item: Expression::Error,
                }
            }
            1 => expressions.into_iter().next().unwrap(),
            _ => {
                let mut expressions = expressions.into_iter();

                WithInfo {
                    info,
                    item: Expression::Call {
                        function: expressions.next().unwrap().boxed(),
                        inputs: expressions.collect(),
                    },
                }
            }
        },
    )
    .named("Call a function with at least one input.")
}

pub fn apply_expression() -> Rule<Expression> {
    Rule::operator(
        SyntaxKind::ApplyExpression,
        Operator::Apply,
        expression,
        expression,
        |_, info, input, function, _| WithInfo {
            info,
            item: Expression::Apply {
                input: input.boxed(),
                function: function.boxed(),
            },
        },
    )
    .named("Function application using the <code>.</code> operator.")
}

pub fn binary_operator_expression() -> Rule<Expression> {
    macro_rules! binary_operator {
            ($op:ident) => {
                Rule::operator(
                    SyntaxKind::BinaryOperatorExpression,
                    Operator::$op,
                    expression,
                    expression,
                    |_, info, left, right, _| WithInfo {
                        info: Location::clone(&info),
                        item: Expression::BinaryOperator {
                            operator: WithInfo {
                                info,
                                item: BinaryOperator::$op,
                            },
                            left: left.boxed(),
                            right: right.boxed(),
                        },
                    },
                )
            };
            ($($op:ident),* $(,)?) => {
                [$(|| binary_operator!($op),)*]
            };
        }

    Rule::switch(
        SyntaxKind::BinaryOperatorExpression,
        binary_operator!(
            To,
            By,
            Power,
            Multiply,
            Divide,
            Remainder,
            Add,
            Subtract,
            LessThan,
            LessThanOrEqual,
            GreaterThan,
            GreaterThanOrEqual,
            Equal,
            NotEqual,
            And,
            Or,
        ),
    )
    .named("An expression involving a binary operator.")
}

pub fn as_expression() -> Rule<Expression> {
    Rule::operator(
        SyntaxKind::AsExpression,
        Operator::As,
        expression,
        r#type,
        |_, info, value, r#type, _| WithInfo {
            info,
            item: Expression::As {
                value: value.boxed(),
                r#type,
            },
        },
    )
    .named("Convert a value of one type to a value of a different type.")
}

pub fn is_expression() -> Rule<Expression> {
    Rule::operator(
        SyntaxKind::IsExpression,
        Operator::Is,
        expression,
        pattern,
        |_, info, value, pattern, _| WithInfo {
            info,
            item: Expression::Is {
                value: value.boxed(),
                pattern,
            },
        },
    )
    .named("Check if a value matches a pattern.")
}

pub fn when_expression() -> Rule<Expression> {
    Rule::keyword2(
        SyntaxKind::WhenExpression,
        Keyword::When,
        expression,
        || {
            Rule::block(SyntaxKind::WhenBody, when_arm, |_, info, arms, _| {
                WithInfo { info, item: arms }
            })
            .no_backtrack()
        },
        |_, info, input, arms, _| WithInfo {
            info,
            item: Expression::When {
                input: input.boxed(),
                arms: arms.item,
            },
        },
    )
    .named("Match a value against a set of patterns.")
}

pub fn when_arm() -> Rule<Arm> {
    Rule::require_operator(
        SyntaxKind::WhenArm,
        Operator::Function,
        pattern,
        expression,
        |_, info, pattern, body, _| WithInfo {
            info,
            item: Arm { pattern, body },
        },
    )
    .no_backtrack()
    .named("An arm in a <code>when</code> expression.")
}

pub fn intrinsic_expression() -> Rule<Expression> {
    Rule::keyword_prefixn(
        SyntaxKind::IntrinsicExpression,
        Keyword::Intrinsic,
        || text().wrapped().no_backtrack(),
        expression,
        |_, info, name, inputs, _| WithInfo {
            info,
            item: Expression::Intrinsic { name, inputs },
        },
    )
    .named("Call an intrinsic function provided by the runtime.")
}

pub fn do_expression() -> Rule<Expression> {
    Rule::keyword1(
        SyntaxKind::DoExpression,
        Keyword::Do,
        expression,
        |_, info, block, _| WithInfo {
            info,
            item: Expression::Do(block.boxed()),
        },
    )
    .named("Call an intrinsic function provided by the runtime.")
}

pub fn tuple_expression() -> Rule<Expression> {
    Rule::variadic_operator(
        SyntaxKind::TupleExpression,
        VariadicOperator::Tuple,
        expression,
        |_, info, expressions, _| WithInfo {
            info,
            item: Expression::Tuple(expressions),
        },
    )
    .named("A tuple.")
}

pub fn collection_expression() -> Rule<Expression> {
    Rule::variadic_operator(
        SyntaxKind::CollectionExpression,
        VariadicOperator::Collection,
        expression,
        |_, info, expressions, _| WithInfo {
            info,
            item: Expression::Collection(expressions),
        },
    )
    .named("A collection.")
}

pub fn structure_expression() -> Rule<Expression> {
    Rule::block(
        SyntaxKind::StructureExpression,
        || {
            Rule::non_associative_operator(
                SyntaxKind::StructureField,
                NonAssociativeOperator::Assign,
                || name().wrapped().in_list(),
                expression,
                |_, info, name, value, _| WithInfo {
                    info,
                    item: FieldValue { name, value },
                },
            )
        },
        |_, info, fields, _| WithInfo {
            info,
            item: Expression::Structure(fields),
        },
    )
    .named("A structure.")
}

pub fn block_expression() -> Rule<Expression> {
    Rule::block(
        SyntaxKind::BlockExpression,
        statement,
        |_, info, statements, _| WithInfo {
            info,
            item: Expression::Block(statements),
        },
    )
    .named("A block expression.")
}

pub fn function_expression() -> Rule<Expression> {
    Rule::operator(
        SyntaxKind::FunctionExpression,
        Operator::Function,
        || {
            Rule::switch(
                SyntaxKind::FunctionInputs,
                [
                    || {
                        Rule::list(
                            SyntaxKind::FunctionInputs,
                            pattern,
                            |_, info, patterns, _| WithInfo {
                                info,
                                item: patterns,
                            },
                        )
                    },
                    || pattern().map(SyntaxKind::FunctionInputs, |pattern| vec![pattern]),
                ],
            )
        },
        expression,
        |_, info, inputs, body, _| WithInfo {
            info,
            item: Expression::Function {
                inputs: inputs.item,
                body: body.boxed(),
            },
        },
    )
    .named("A function expression.")
}
