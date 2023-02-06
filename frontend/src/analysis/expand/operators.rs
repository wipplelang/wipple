use crate::{
    analysis::expand::{
        syntax::{Expression, ExpressionKind},
        Expander, Operator, ScopeValueKind, Syntax,
    },
    diagnostics::Note,
    helpers::{Backtrace, InternedString},
    parse::Span,
    Compiler, ScopeId,
};
use std::{cmp::Ordering, collections::VecDeque};

// TODO: User-defined precedences
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, strum::EnumString)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum OperatorPrecedence {
    #[strum(serialize = "Cast-Precedence")]
    Cast,
    #[strum(serialize = "Power-Precedence")]
    Power,
    #[strum(serialize = "Multiplication-Precedence")]
    Multiplication,
    #[strum(serialize = "Addition-Precedence")]
    Addition,
    #[strum(serialize = "Comparison-Precedence")]
    Comparison,
    #[strum(serialize = "Conjunction-Precedence")]
    Conjunction,
    #[strum(serialize = "Disjunction-Precedence")]
    Disjunction,
    #[strum(serialize = "Accessor-Precedence")]
    Accessor,
    #[strum(serialize = "Dot-Precedence")]
    Dot,
    #[strum(serialize = "Comma-Precedence")]
    Comma,
    #[strum(serialize = "Where-Precedence")]
    Where,
    #[strum(serialize = "Function-Precedence")]
    Function,
    #[strum(serialize = "Type-Function-Precedence")]
    TypeFunction,
    #[strum(serialize = "Annotation-Precedence")]
    Annotation,
    #[strum(serialize = "Assignment-Precedence")]
    Assignment,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorAssociativity {
    Left,
    Right,
    None { allow_multiple: bool },
}

impl OperatorPrecedence {
    pub fn associativity(&self) -> OperatorAssociativity {
        match self {
            OperatorPrecedence::Cast => OperatorAssociativity::Left,
            OperatorPrecedence::Conjunction => OperatorAssociativity::Left,
            OperatorPrecedence::Disjunction => OperatorAssociativity::Left,
            OperatorPrecedence::Comparison => OperatorAssociativity::Left,
            OperatorPrecedence::Addition => OperatorAssociativity::Left,
            OperatorPrecedence::Multiplication => OperatorAssociativity::Left,
            OperatorPrecedence::Power => OperatorAssociativity::Right,
            OperatorPrecedence::Accessor => OperatorAssociativity::Right,
            OperatorPrecedence::Dot => OperatorAssociativity::Left,
            OperatorPrecedence::Comma => OperatorAssociativity::None {
                allow_multiple: true,
            },
            OperatorPrecedence::Where => OperatorAssociativity::None {
                allow_multiple: false,
            },
            OperatorPrecedence::Function => OperatorAssociativity::Right,
            OperatorPrecedence::TypeFunction => OperatorAssociativity::None {
                allow_multiple: false,
            },
            OperatorPrecedence::Annotation => OperatorAssociativity::Left,
            OperatorPrecedence::Assignment => OperatorAssociativity::None {
                allow_multiple: false,
            },
        }
    }
}

#[derive(Debug)]
pub(super) enum ExpandOperatorsResult {
    Error(Backtrace),
    Empty,
    Single(Expression),
    List(Vec<Expression>),
    Operator(Span, InternedString, Syntax, Expression, Expression),
    Syntax(Span, InternedString, Syntax, Vec<Expression>),
}

impl ExpandOperatorsResult {
    pub(super) fn error(compiler: &Compiler) -> Self {
        ExpandOperatorsResult::Error(compiler.backtrace())
    }
}

impl<'a, 'l> Expander<'a, 'l> {
    pub(super) fn expand_operators(
        &self,
        list_span: Span,
        mut exprs: Vec<Expression>,
        inherited_scope: ScopeId,
    ) -> ExpandOperatorsResult {
        match exprs.len() {
            0 => ExpandOperatorsResult::Empty,
            1 => {
                let expr = exprs.pop().unwrap();

                if let ExpressionKind::Name(scope, name) = expr.kind {
                    match self.get_name(name, scope.unwrap_or(inherited_scope)) {
                        Some(ScopeValueKind::Syntax(syntax)) => {
                            return ExpandOperatorsResult::Syntax(
                                expr.span,
                                name,
                                syntax,
                                Vec::new(),
                            );
                        }
                        Some(ScopeValueKind::Operator(_)) => {
                            self.compiler.add_error(
                                "expected values on both sides of operator",
                                vec![Note::primary(
                                    expr.span,
                                    "try providing values on either side of this",
                                )],
                            );

                            return ExpandOperatorsResult::error(self.compiler);
                        }
                        None => {}
                    }
                }

                ExpandOperatorsResult::Single(expr)
            }
            _ => {
                let operators = self.operators_in_list(exprs.iter().enumerate(), inherited_scope);

                if operators.is_empty() {
                    let mut rest = VecDeque::from(exprs);
                    let first = rest.pop_front().unwrap();

                    let syntax_name = match &first.kind {
                        ExpressionKind::Name(scope, name) => Some((*scope, *name)),
                        ExpressionKind::List(exprs) if exprs.len() == 1 => {
                            if let ExpressionKind::Name(scope, name) = &exprs.first().unwrap().kind
                            {
                                Some((*scope, *name))
                            } else {
                                None
                            }
                        }
                        _ => None,
                    };

                    if let Some((scope, name)) = syntax_name {
                        match self.get_name(name, scope.unwrap_or(inherited_scope)) {
                            Some(ScopeValueKind::Syntax(syntax)) => {
                                return ExpandOperatorsResult::Syntax(
                                    first.span,
                                    name,
                                    syntax,
                                    rest.into(),
                                );
                            }
                            Some(ScopeValueKind::Operator(operator)) => {
                                return ExpandOperatorsResult::Syntax(
                                    first.span,
                                    name,
                                    operator.syntax,
                                    rest.into(),
                                );
                            }
                            None => {}
                        }
                    }

                    return ExpandOperatorsResult::List(
                        std::iter::once(first).chain(rest).collect(),
                    );
                }

                let (mut max_index, mut max_span, mut max_name, mut max_operator) =
                    operators.front().copied().unwrap();

                for (index, span, name, operator) in operators.iter().skip(1).copied() {
                    macro_rules! replace {
                        () => {{
                            max_index = index;
                            max_span = span;
                            max_name = name;
                            max_operator = operator;
                        }};
                    }

                    match operator.precedence.cmp(&max_operator.precedence) {
                        Ordering::Greater => replace!(),
                        Ordering::Less => continue,
                        Ordering::Equal => match operator.precedence.associativity() {
                            OperatorAssociativity::Left => {
                                if index > max_index {
                                    replace!();
                                }
                            }
                            OperatorAssociativity::Right => {
                                if index < max_index {
                                    replace!()
                                }
                            }
                            OperatorAssociativity::None { allow_multiple } => {
                                if !allow_multiple {
                                    self.compiler.add_error(
                                        "operator ambiguity",
                                        vec![
                                            Note::primary(
                                                exprs[index].span,
                                                "only one of this operator may be provided at a time",
                                            ),
                                            Note::secondary(
                                                exprs[max_index].span,
                                                "first use of this operator",
                                            ),
                                        ],
                                    );

                                    return ExpandOperatorsResult::error(self.compiler);
                                }
                            }
                        },
                    }
                }

                if matches!(
                    max_operator.precedence.associativity(),
                    OperatorAssociativity::None {
                        allow_multiple: true
                    }
                ) {
                    let mut grouped_exprs = vec![(None, Vec::new())];
                    {
                        let mut operators = operators.clone();

                        for (index, expr) in exprs.into_iter().enumerate() {
                            let operator_index = operators.front().map(|(index, ..)| *index);

                            if Some(index) == operator_index {
                                operators.pop_front();
                                grouped_exprs.push((Some(index), Vec::new()));
                            } else {
                                grouped_exprs.last_mut().unwrap().1.push(expr);
                            }
                        }
                    }

                    // Allow trailing operators
                    if grouped_exprs.last().unwrap().1.is_empty() {
                        grouped_exprs.pop();
                    }

                    let exprs = grouped_exprs
                        .into_iter()
                        .map(|(operator_index, exprs)| {
                            if exprs.is_empty() {
                                if let Some(operator_index) = operator_index {
                                    let operator_span = operators
                                        .iter()
                                        .find_map(|(index, span, _, _)| {
                                            (*index == operator_index).then_some(*span)
                                        })
                                        .unwrap();

                                    self.compiler.add_error(
                                        "expected values on right side of operator",
                                        vec![Note::primary(
                                            operator_span,
                                            "try providing a value to the right of this",
                                        )],
                                    );
                                } else {
                                    let operator_span = operators.front().unwrap().1;

                                    self.compiler.add_error(
                                        "expected values on left side of operator",
                                        vec![Note::primary(
                                            operator_span,
                                            "try providing a value to the left of this",
                                        )],
                                    );
                                }

                                return Expression {
                                    span: list_span,
                                    scope: Some(inherited_scope),
                                    kind: ExpressionKind::error(self.compiler),
                                };
                            }

                            let span =
                                Span::join(exprs.first().unwrap().span, exprs.last().unwrap().span);

                            Expression {
                                span,
                                scope: Some(inherited_scope),
                                kind: ExpressionKind::List(exprs),
                            }
                        })
                        .collect::<Vec<_>>();

                    debug_assert!(!exprs.is_empty());

                    ExpandOperatorsResult::Syntax(max_span, max_name, max_operator.syntax, exprs)
                } else {
                    let mut rhs = exprs.split_off(max_index + 1);
                    let mut lhs = exprs;
                    lhs.pop().unwrap();

                    if rhs.is_empty() {
                        self.compiler.add_error(
                            "expected values on right side of operator",
                            vec![Note::primary(
                                max_span,
                                "try providing a value to the right of this",
                            )],
                        );

                        ExpandOperatorsResult::error(self.compiler)
                    } else if lhs.is_empty() {
                        self.compiler.add_error(
                            "expected values on left side of operator",
                            vec![Note::primary(
                                max_span,
                                "try providing a value to the left of this",
                            )],
                        );

                        ExpandOperatorsResult::error(self.compiler)
                    } else {
                        macro_rules! expand_list {
                            ($exprs:ident) => {
                                (|| {
                                    // Prevent flattening of a single parenthesized expression that
                                    // doesn't contain any operators
                                    if $exprs.len() == 1 {
                                        let expr = $exprs.first().unwrap();

                                        if let ExpressionKind::List(exprs) = &expr.kind {
                                            let exprs = exprs.into_iter().enumerate();

                                            if exprs.clone().next().is_none() {
                                                return Expression {
                                                    span: expr.span,
                                                    scope: Some(inherited_scope),
                                                    kind: ExpressionKind::List(Vec::new()),
                                                };
                                            }

                                            if self
                                                .operators_in_list(exprs.clone(), inherited_scope)
                                                .is_empty()
                                            {
                                                let expr = $exprs.pop().unwrap();

                                                return Expression {
                                                    span: expr.span,
                                                    scope: Some(inherited_scope),
                                                    kind: ExpressionKind::List(vec![expr]),
                                                };
                                            }
                                        }
                                    }

                                    let span = Span::join(
                                        $exprs.first().unwrap().span,
                                        $exprs.last().unwrap().span,
                                    );

                                    Expression {
                                        span,
                                        scope: Some(inherited_scope),
                                        kind: ExpressionKind::List($exprs),
                                    }
                                })()
                            };
                        }

                        let lhs = expand_list!(lhs);
                        let rhs = expand_list!(rhs);

                        ExpandOperatorsResult::Operator(
                            max_span,
                            max_name,
                            max_operator.syntax,
                            lhs,
                            rhs,
                        )
                    }
                }
            }
        }
    }

    fn operators_in_list<'e>(
        &self,
        exprs: impl IntoIterator<Item = (usize, &'e Expression)>,
        inherited_scope: ScopeId,
    ) -> VecDeque<(usize, Span, InternedString, Operator)> {
        let mut operators = VecDeque::new();
        for (index, expr) in exprs {
            if let ExpressionKind::Name(scope, name) = expr.kind {
                if let Some(ScopeValueKind::Operator(operator)) =
                    self.get_name(name, scope.unwrap_or(inherited_scope))
                {
                    operators.push_back((index, expr.span, name, operator))
                }
            }
        }

        operators
    }
}
