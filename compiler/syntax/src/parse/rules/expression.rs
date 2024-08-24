use crate::{
    parse::{
        base::Rule, name, number, pattern, r#type, statement, text, FieldValue, Pattern, Statement,
        SyntaxKind, Type,
    },
    tokenize::{Keyword, NonAssociativeOperator, Operator, VariadicOperator},
    BinaryOperator, Driver,
};
use derivative::Derivative;
use serde::Deserialize;
use std::fmt::Debug;
use wipple_util::{DefaultFromInfo, WithInfo};

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum Expression<D: Driver> {
    Error,
    #[serde(rename_all = "camelCase")]
    Annotate {
        value: WithInfo<D::Info, Box<Expression<D>>>,
        r#type: WithInfo<D::Info, Type<D>>,
    },
    Name(String),
    Number(String),
    Text(String),
    Unit,
    Block(Vec<WithInfo<D::Info, Statement<D>>>),
    Do(WithInfo<D::Info, Box<Expression<D>>>),
    #[serde(rename_all = "camelCase")]
    Function {
        inputs: Vec<WithInfo<D::Info, Pattern<D>>>,
        body: WithInfo<D::Info, Box<Expression<D>>>,
    },
    Call {
        function: WithInfo<D::Info, Box<Expression<D>>>,
        inputs: Vec<WithInfo<D::Info, Expression<D>>>,
    },
    Apply {
        input: WithInfo<D::Info, Box<Expression<D>>>,
        function: WithInfo<D::Info, Box<Expression<D>>>,
    },
    #[serde(rename_all = "camelCase")]
    BinaryOperator {
        operator: WithInfo<D::Info, BinaryOperator>,
        left: WithInfo<D::Info, Box<Expression<D>>>,
        right: WithInfo<D::Info, Box<Expression<D>>>,
    },
    #[serde(rename_all = "camelCase")]
    As {
        value: WithInfo<D::Info, Box<Expression<D>>>,
        r#type: WithInfo<D::Info, Type<D>>,
    },
    #[serde(rename_all = "camelCase")]
    Is {
        value: WithInfo<D::Info, Box<Expression<D>>>,
        pattern: WithInfo<D::Info, Pattern<D>>,
    },
    #[serde(rename_all = "camelCase")]
    When {
        input: WithInfo<D::Info, Box<Expression<D>>>,
        arms: Vec<WithInfo<D::Info, Arm<D>>>,
    },
    #[serde(rename_all = "camelCase")]
    Intrinsic {
        name: WithInfo<D::Info, Option<String>>,
        inputs: Vec<WithInfo<D::Info, Expression<D>>>,
    },
    Tuple(Vec<WithInfo<D::Info, Expression<D>>>),
    Collection(Vec<WithInfo<D::Info, Expression<D>>>),
    Structure(Vec<WithInfo<D::Info, FieldValue<D>>>),
}

impl<D: Driver> DefaultFromInfo<D::Info> for Expression<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info,
            item: Expression::Error,
        }
    }
}

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct Arm<D: Driver> {
    pub pattern: WithInfo<D::Info, Pattern<D>>,
    pub body: WithInfo<D::Info, Expression<D>>,
}

impl<D: Driver> DefaultFromInfo<D::Info> for Arm<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info: info.clone(),
            item: Arm {
                pattern: Pattern::default_from_info(info.clone()),
                body: Expression::default_from_info(info),
            },
        }
    }
}

pub fn expression<D: Driver>() -> Rule<D, Expression<D>> {
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

pub fn annotate_expression<D: Driver>() -> Rule<D, Expression<D>> {
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

pub fn name_expression<D: Driver>() -> Rule<D, Expression<D>> {
    name()
        .wrapped()
        .map(SyntaxKind::NameExpression, |name| {
            Expression::Name(name.item.unwrap())
        })
        .named("A name.")
}

pub fn number_expression<D: Driver>() -> Rule<D, Expression<D>> {
    number()
        .wrapped()
        .map(SyntaxKind::NumberExpression, |number| {
            Expression::Number(number.item.unwrap())
        })
        .named("A number.")
}

pub fn text_expression<D: Driver>() -> Rule<D, Expression<D>> {
    text()
        .wrapped()
        .map(SyntaxKind::TextExpression, |text| {
            Expression::Text(text.item.unwrap())
        })
        .named("A piece of text.")
}

pub fn call_expression<D: Driver>() -> Rule<D, Expression<D>> {
    Rule::list(
        SyntaxKind::CallExpression,
        expression,
        |_, info, expressions, _| match expressions.len() {
            0 => WithInfo {
                info,
                item: Expression::Unit,
            },
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

pub fn apply_expression<D: Driver>() -> Rule<D, Expression<D>> {
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

pub fn binary_operator_expression<D: Driver>() -> Rule<D, Expression<D>> {
    macro_rules! binary_operator {
            ($op:ident) => {
                Rule::operator(
                    SyntaxKind::BinaryOperatorExpression,
                    Operator::$op,
                    expression,
                    expression,
                    |_, info, left, right, _| WithInfo {
                        info: D::Info::clone(&info),
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

pub fn as_expression<D: Driver>() -> Rule<D, Expression<D>> {
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

pub fn is_expression<D: Driver>() -> Rule<D, Expression<D>> {
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

pub fn when_expression<D: Driver>() -> Rule<D, Expression<D>> {
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

pub fn when_arm<D: Driver>() -> Rule<D, Arm<D>> {
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

pub fn intrinsic_expression<D: Driver>() -> Rule<D, Expression<D>> {
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

pub fn do_expression<D: Driver>() -> Rule<D, Expression<D>> {
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

pub fn tuple_expression<D: Driver>() -> Rule<D, Expression<D>> {
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

pub fn collection_expression<D: Driver>() -> Rule<D, Expression<D>> {
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

pub fn structure_expression<D: Driver>() -> Rule<D, Expression<D>> {
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

pub fn block_expression<D: Driver>() -> Rule<D, Expression<D>> {
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

pub fn function_expression<D: Driver>() -> Rule<D, Expression<D>> {
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
