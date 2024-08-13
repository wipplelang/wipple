use crate::{
    tokenize::{
        Associativity, Diagnostic, Keyword, ListDelimiter, NonAssociativeOperator, Operator, Token,
        TokenTree, VariadicOperator,
    },
    Driver, Location,
};
use itertools::Itertools;
use std::mem;
use wipple_util::WithInfo;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    As,
    To,
    By,
    Power,
    Multiply,
    Add,
    Compare,
    Is,
    And,
    Or,
    Apply,
    Function,
}

impl Operator {
    fn precedence(&self) -> Precedence {
        match self {
            Operator::As => Precedence::As,
            Operator::To => Precedence::To,
            Operator::By => Precedence::By,
            Operator::Power => Precedence::Power,
            Operator::Multiply | Operator::Divide | Operator::Remainder => Precedence::Multiply,
            Operator::Add | Operator::Subtract => Precedence::Add,
            Operator::LessThan
            | Operator::LessThanOrEqual
            | Operator::GreaterThan
            | Operator::GreaterThanOrEqual
            | Operator::Equal
            | Operator::NotEqual => Precedence::Compare,
            Operator::Is => Precedence::Is,
            Operator::And => Precedence::And,
            Operator::Or => Precedence::Or,
            Operator::Apply => Precedence::Apply,
            Operator::Function => Precedence::Function,
        }
    }
}

impl ListDelimiter {
    fn start(&self) -> Token<'static> {
        match self {
            ListDelimiter::Parentheses => Token::LeftParenthesis,
            ListDelimiter::Brackets => Token::LeftBracket,
        }
    }

    fn end(&self) -> Token<'static> {
        match self {
            ListDelimiter::Parentheses => Token::RightParenthesis,
            ListDelimiter::Brackets => Token::RightBracket,
        }
    }

    fn match_end<D: Driver>(
        &self,
        start_info: &D::Info,
        end: WithInfo<D::Info, &Token<'_>>,
    ) -> Result<(), WithInfo<D::Info, Diagnostic<D>>> {
        match (self, end.item) {
            (ListDelimiter::Parentheses, Token::RightParenthesis) => Ok(()),
            (ListDelimiter::Brackets, Token::RightBracket) => Ok(()),
            (_, item) => Err(WithInfo {
                info: end.info,
                item: Diagnostic::Mismatch {
                    expected: Some(self.end()),
                    found: Some(item.clone().into_owned()),
                    matching: Some(WithInfo {
                        info: start_info.clone(),
                        item: self.start(),
                    }),
                },
            }),
        }
    }
}

impl<'src, D: Driver> TokenTree<'src, D> {
    pub fn from_top_level(
        driver: &D,
        tokens: impl IntoIterator<Item = WithInfo<D::Info, Token<'src>>>,
    ) -> (
        WithInfo<D::Info, Self>,
        Vec<WithInfo<D::Info, Diagnostic<D>>>,
    )
    where
        D::Info: From<Location>,
    {
        fn parse_operators<'src, D: Driver>(
            delimiter: ListDelimiter,
            expressions: Vec<WithInfo<D::Info, TokenTree<'src, D>>>,
            diagnostics: &mut Vec<WithInfo<D::Info, Diagnostic<D>>>,
        ) -> TokenTree<'src, D> {
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
            enum AnyOperator {
                Operator(Operator),
                VariadicOperator(VariadicOperator),
                NonAssociativeOperator(NonAssociativeOperator),
            }

            let operators = expressions
                .iter()
                .enumerate()
                .filter_map(|(index, token)| match token.item {
                    TokenTree::UnresolvedOperator(operator) => Some((
                        index,
                        WithInfo {
                            info: token.info.clone(),
                            item: AnyOperator::Operator(operator),
                        },
                    )),
                    TokenTree::UnresolvedVariadicOperator(operator) => Some((
                        index,
                        WithInfo {
                            info: token.info.clone(),
                            item: AnyOperator::VariadicOperator(operator),
                        },
                    )),
                    TokenTree::UnresolvedNonAssociativeOperator(operator) => Some((
                        index,
                        WithInfo {
                            info: token.info.clone(),
                            item: AnyOperator::NonAssociativeOperator(operator),
                        },
                    )),
                    _ => None,
                })
                .max_set_by(|(_, left), (_, right)| match (left.item, right.item) {
                    (AnyOperator::Operator(left), AnyOperator::Operator(right)) => {
                        left.precedence().cmp(&right.precedence())
                    }
                    (left, right) => left.cmp(&right),
                });

            if operators.is_empty() {
                return TokenTree::List(delimiter, expressions);
            }

            fn tree<'src, D: Driver>(
                index: WithInfo<D::Info, usize>,
                mut expressions: Vec<WithInfo<<D as Driver>::Info, TokenTree<'src, D>>>,
                diagnostics: &mut Vec<WithInfo<<D as Driver>::Info, Diagnostic<D>>>,
            ) -> (
                WithInfo<D::Info, TokenTree<'src, D>>,
                WithInfo<D::Info, TokenTree<'src, D>>,
            ) {
                let info = index.info;
                let index = index.item;

                match (index > 0, index + 1 < expressions.len()) {
                    (true, true) => {
                        let right = expressions.split_off(index + 1);
                        expressions.pop().unwrap();
                        let left = expressions;

                        let left_info = D::merge_info(
                            left.first().unwrap().info.clone(),
                            left.last().unwrap().info.clone(),
                        );

                        let right_info = D::merge_info(
                            right.first().unwrap().info.clone(),
                            right.last().unwrap().info.clone(),
                        );

                        (
                            WithInfo {
                                info: left_info,
                                item: parse_operators::<D>(
                                    ListDelimiter::Parentheses,
                                    left,
                                    diagnostics,
                                ),
                            },
                            WithInfo {
                                info: right_info,
                                item: parse_operators::<D>(
                                    ListDelimiter::Parentheses,
                                    right,
                                    diagnostics,
                                ),
                            },
                        )
                    }
                    (true, false) => {
                        expressions.pop().unwrap();
                        let left = expressions;

                        let left_info = D::merge_info(
                            left.first().unwrap().info.clone(),
                            left.last().unwrap().info.clone(),
                        );

                        (
                            WithInfo {
                                info: left_info,
                                item: parse_operators::<D>(
                                    ListDelimiter::Parentheses,
                                    left,
                                    diagnostics,
                                ),
                            },
                            WithInfo {
                                info,
                                item: TokenTree::Error,
                            },
                        )
                    }
                    (false, true) => {
                        let right = expressions.split_off(1);

                        let right_info = D::merge_info(
                            right.first().unwrap().info.clone(),
                            right.last().unwrap().info.clone(),
                        );

                        (
                            WithInfo {
                                info,
                                item: TokenTree::Error,
                            },
                            WithInfo {
                                info: right_info,
                                item: parse_operators::<D>(
                                    ListDelimiter::Parentheses,
                                    right,
                                    diagnostics,
                                ),
                            },
                        )
                    }
                    (false, false) => (
                        WithInfo {
                            info: info.clone(),
                            item: TokenTree::Error,
                        },
                        WithInfo {
                            info,
                            item: TokenTree::Error,
                        },
                    ),
                }
            }

            let operator = &operators.first().unwrap().1;
            let info = operator.info.clone();
            match operator.item {
                AnyOperator::Operator(operator) => {
                    let (index, operator) = {
                        let (index, operator) = match operator.associativity() {
                            Associativity::Left => operators.last().unwrap(),
                            Associativity::Right => operators.first().unwrap(),
                        };

                        let info = operator.info.clone();

                        let operator = match operator.item {
                            AnyOperator::Operator(operator) => operator,
                            _ => unreachable!("operators are grouped by type"),
                        };

                        (WithInfo { info, item: *index }, operator)
                    };

                    let (left, right) = tree(index, expressions, diagnostics);

                    TokenTree::Operator(
                        WithInfo {
                            info,
                            item: operator,
                        },
                        left.boxed(),
                        right.boxed(),
                    )
                }
                AnyOperator::VariadicOperator(operator) => {
                    let mut indices = operators.iter().map(|&(index, _)| index).peekable();

                    let mut inputs = vec![Vec::new()];
                    for (expression_index, expression) in expressions.into_iter().enumerate() {
                        if let Some(operator_index) = indices.peek().copied() {
                            use std::cmp::Ordering;

                            match expression_index.cmp(&operator_index) {
                                Ordering::Less => {}
                                Ordering::Equal => continue,
                                Ordering::Greater => {
                                    inputs.push(Vec::new());
                                    indices.next();
                                }
                            }
                        }

                        inputs.last_mut().unwrap().push(expression);
                    }

                    // Allow trailing operators
                    if inputs.last().unwrap().is_empty() {
                        inputs.pop();
                    }

                    TokenTree::VariadicOperator(
                        WithInfo {
                            info: info.clone(),
                            item: operator,
                        },
                        inputs
                            .into_iter()
                            .map(|group| {
                                if group.is_empty() {
                                    return WithInfo {
                                        info: info.clone(),
                                        item: TokenTree::Error,
                                    };
                                }

                                let info = D::merge_info(
                                    group.first().unwrap().info.clone(),
                                    group.last().unwrap().info.clone(),
                                );

                                WithInfo {
                                    info,
                                    item: parse_operators::<D>(
                                        ListDelimiter::Parentheses,
                                        group,
                                        diagnostics,
                                    ),
                                }
                            })
                            .collect(),
                    )
                }
                AnyOperator::NonAssociativeOperator(operator) => {
                    if operators.len() != 1 {
                        return TokenTree::Error;
                    }

                    let index = {
                        let (index, operator) = operators.first().unwrap();

                        WithInfo {
                            info: operator.info.clone(),
                            item: *index,
                        }
                    };

                    let (left, right) = tree(index, expressions, diagnostics);

                    TokenTree::NonAssociativeOperator(
                        WithInfo {
                            info,
                            item: operator,
                        },
                        left.boxed(),
                        right.boxed(),
                    )
                }
            }
        }

        let mut tokens = tokens.into_iter();

        let first_token = match tokens.next() {
            Some(token) => token,
            None => {
                return (
                    WithInfo {
                        info: Location {
                            path: driver.file_path(),
                            visible_path: driver.visible_path(),
                            span: 0..driver.file_size(),
                        }
                        .into(),
                        item: TokenTree::Block(Vec::new()),
                    },
                    Vec::new(),
                )
            }
        };

        let first_info = first_token.info.clone();
        let mut info = first_info.clone();

        let mut stack = vec![(info, TokenTree::Block(Vec::new()))];
        let mut diagnostics = Vec::new();
        for token in [first_token].into_iter().chain(tokens.by_ref()) {
            info = token.info;

            fn push<'src, D: Driver>(
                info: D::Info,
                token: &Token<'src>,
                stack: &mut Vec<(D::Info, TokenTree<'src, D>)>,
                item: WithInfo<<D as Driver>::Info, TokenTree<'src, D>>,
                diagnostics: &mut Vec<WithInfo<<D as Driver>::Info, Diagnostic<D>>>,
            ) -> bool {
                match stack.last_mut() {
                    Some((_, TokenTree::List(_, expressions))) => {
                        expressions.push(item);
                    }
                    Some((begin_info, TokenTree::Block(statements))) => {
                        if statements.is_empty() {
                            statements.push(WithInfo {
                                info: begin_info.clone(),
                                item: TokenTree::List(ListDelimiter::Parentheses, Vec::new()),
                            });
                        }

                        match &mut statements.last_mut().unwrap().item {
                            TokenTree::List(_, expressions) => expressions.push(item),
                            _ => unreachable!(),
                        }
                    }
                    Some((_, TokenTree::UnresolvedAttribute(contents))) => {
                        if contents.is_some() {
                            let (begin_info, attribute) = match stack.pop().unwrap() {
                                (begin_info, TokenTree::UnresolvedAttribute(attribute)) => {
                                    (begin_info, attribute.unwrap())
                                }
                                _ => unreachable!(),
                            };

                            let item = WithInfo {
                                info: D::merge_info(begin_info, info),
                                item: TokenTree::Attribute(attribute, item.boxed()),
                            };

                            if !push(item.info.clone(), token, stack, item, diagnostics) {
                                return false;
                            }
                        } else {
                            contents.replace(item.boxed());
                        }
                    }
                    _ => {
                        diagnostics.push(WithInfo {
                            info,
                            item: Diagnostic::Mismatch {
                                expected: None,
                                found: Some(token.clone().into_owned()),
                                matching: None,
                            },
                        });

                        return false;
                    }
                }

                true
            }

            macro_rules! push {
                ($item:expr) => {
                    if !push(
                        info.clone(),
                        &token.item,
                        &mut stack,
                        $item,
                        &mut diagnostics,
                    ) {
                        continue;
                    }
                };
            }

            match token.item {
                Token::LeftParenthesis => {
                    stack.push((
                        info,
                        TokenTree::List(ListDelimiter::Parentheses, Vec::new()),
                    ));
                }
                Token::LeftBracket => {
                    stack.push((info, TokenTree::List(ListDelimiter::Brackets, Vec::new())));
                }
                Token::RightParenthesis | Token::RightBracket => {
                    let (begin_info, delimiter, expressions) = match stack.pop() {
                        Some((begin_info, TokenTree::List(delimiter, expressions))) => {
                            if let Err(diagnostic) = delimiter.match_end(
                                &begin_info,
                                WithInfo {
                                    info: info.clone(),
                                    item: &token.item,
                                },
                            ) {
                                diagnostics.push(diagnostic);
                            }

                            (begin_info, delimiter, expressions)
                        }
                        Some((begin_info, TokenTree::Block(_))) => {
                            diagnostics.push(WithInfo {
                                info,
                                item: Diagnostic::Mismatch {
                                    expected: Some(Token::RightBrace),
                                    found: Some(token.item.into_owned()),
                                    matching: Some(WithInfo {
                                        info: begin_info,
                                        item: Token::LeftBrace,
                                    }),
                                },
                            });

                            continue;
                        }
                        _ => {
                            diagnostics.push(WithInfo {
                                info,
                                item: Diagnostic::Mismatch {
                                    expected: None,
                                    found: Some(token.item.into_owned()),
                                    matching: None,
                                },
                            });

                            continue;
                        }
                    };

                    push!(WithInfo {
                        info: D::merge_info(begin_info, info),
                        item: parse_operators::<D>(delimiter, expressions, &mut diagnostics),
                    });
                }
                Token::LeftBrace => {
                    stack.push((info, TokenTree::Block(Vec::new())));
                }
                Token::RightBrace => {
                    let (begin_info, mut statements) = match stack.pop() {
                        Some((begin_info, TokenTree::Block(statements))) => {
                            (begin_info, statements)
                        }
                        Some((begin_info, TokenTree::List(delimiter, _))) => {
                            diagnostics.push(WithInfo {
                                info,
                                item: Diagnostic::Mismatch {
                                    expected: Some(delimiter.end()),
                                    found: Some(token.item.into_owned()),
                                    matching: Some(WithInfo {
                                        info: begin_info,
                                        item: delimiter.start(),
                                    }),
                                },
                            });

                            continue;
                        }
                        _ => {
                            diagnostics.push(WithInfo {
                                info,
                                item: Diagnostic::Mismatch {
                                    expected: None,
                                    found: Some(token.item.into_owned()),
                                    matching: None,
                                },
                            });

                            continue;
                        }
                    };

                    if let Some(tree) = statements.last_mut() {
                        // Allow line break before closing brace
                        if matches!(&tree.item, TokenTree::List(_, elements) if elements.is_empty())
                        {
                            statements.pop().unwrap();
                        } else {
                            let (delimiter, expressions) = match &mut tree.item {
                                TokenTree::List(delimiter, expressions) => {
                                    (*delimiter, mem::take(expressions))
                                }
                                _ => unreachable!(),
                            };

                            if let Some(first) = expressions.first() {
                                let last = expressions.last().unwrap();
                                tree.info = D::merge_info(first.info.clone(), last.info.clone());
                            }

                            tree.item =
                                parse_operators::<D>(delimiter, expressions, &mut diagnostics);
                        }
                    }

                    push!(WithInfo {
                        info: D::merge_info(begin_info, info),
                        item: TokenTree::Block(statements),
                    });
                }
                Token::LineBreak => match stack.last_mut() {
                    Some((_, TokenTree::List(_, _) | TokenTree::UnresolvedAttribute(_))) => {
                        continue
                    }
                    Some((begin_info, TokenTree::Block(statements))) => {
                        if let Some(tree) = statements.last_mut() {
                            let (delimiter, expressions) = match &mut tree.item {
                                TokenTree::List(delimiter, expressions) => {
                                    (*delimiter, mem::take(expressions))
                                }
                                _ => unreachable!(),
                            };

                            if let Some(first) = expressions.first() {
                                let last = expressions.last().unwrap();
                                tree.info = D::merge_info(first.info.clone(), last.info.clone());
                            }

                            tree.item =
                                parse_operators::<D>(delimiter, expressions, &mut diagnostics);
                        }

                        statements.push(WithInfo {
                            info: begin_info.clone(),
                            item: TokenTree::List(ListDelimiter::Parentheses, Vec::new()),
                        });
                    }
                    _ => {
                        diagnostics.push(WithInfo {
                            info,
                            item: Diagnostic::Mismatch {
                                expected: None,
                                found: Some(token.item.into_owned()),
                                matching: None,
                            },
                        });
                    }
                },
                Token::Comment(_) => continue,
                Token::Keyword(keyword) => match keyword {
                    Keyword::Attribute => {
                        stack.push((info, TokenTree::UnresolvedAttribute(None)));
                    }
                    Keyword::Mutate => match stack.last_mut() {
                        Some((begin_info, TokenTree::Block(statements))) => {
                            let elements = match &mut statements.last_mut().unwrap().item {
                                TokenTree::List(_, elements) => elements,
                                _ => unreachable!(),
                            };

                            let element = elements.pop().unwrap();

                            elements.push(WithInfo {
                                info: D::merge_info(begin_info.clone(), info),
                                item: TokenTree::Mutate(element.boxed()),
                            });
                        }
                        _ => {
                            diagnostics.push(WithInfo {
                                info,
                                item: Diagnostic::Mismatch {
                                    expected: None,
                                    found: Some(token.item.into_owned()),
                                    matching: None,
                                },
                            });
                        }
                    },
                    _ => push!(WithInfo {
                        info,
                        item: TokenTree::Keyword(keyword),
                    }),
                },
                Token::Operator(operator) => {
                    push!(WithInfo {
                        info,
                        item: TokenTree::UnresolvedOperator(operator),
                    });
                }
                Token::VariadicOperator(operator) => {
                    push!(WithInfo {
                        info,
                        item: TokenTree::UnresolvedVariadicOperator(operator),
                    });
                }
                Token::NonAssociativeOperator(operator) => {
                    push!(WithInfo {
                        info,
                        item: TokenTree::UnresolvedNonAssociativeOperator(operator),
                    });
                }
                Token::Name(ref name) => {
                    push!(WithInfo {
                        info,
                        item: TokenTree::Name(name.clone()),
                    });
                }
                Token::Text(ref text) => {
                    push!(WithInfo {
                        info,
                        item: TokenTree::Text(text.clone()),
                    });
                }
                Token::Number(ref number) => {
                    push!(WithInfo {
                        info,
                        item: TokenTree::Number(number.clone()),
                    });
                }
            }
        }

        // The bottom of the stack is the top level
        let mut stack = stack.into_iter();
        let mut tree = match stack.next() {
            Some((begin_info, tree)) => {
                // Report errors for items remaining on the stack
                for (begin_info, tree) in stack {
                    let (begin_token, end_token, end_info) = match tree {
                        TokenTree::List(delimiter, mut tokens) => (
                            delimiter.start(),
                            delimiter.end(),
                            tokens
                                .pop()
                                .map_or_else(|| begin_info.clone(), |token| token.info),
                        ),
                        TokenTree::Block(mut tokens) => (
                            Token::LeftBrace,
                            Token::RightBrace,
                            // Get the last token in the last statement
                            tokens
                                .pop()
                                .and_then(|token| match token.item {
                                    TokenTree::List(_, mut tokens) => tokens.pop(),
                                    _ => None,
                                })
                                .map_or_else(|| begin_info.clone(), |token| token.info),
                        ),
                        TokenTree::Attribute(_, value_tokens) => {
                            // FIXME: Technically any token is allowed after a `@`, not just a left
                            // parenthesis
                            (
                                Token::Keyword(Keyword::Attribute),
                                Token::LeftParenthesis,
                                value_tokens.info,
                            )
                        }
                        _ => unreachable!(),
                    };

                    diagnostics.push(WithInfo {
                        info: end_info.clone(),
                        item: Diagnostic::Mismatch {
                            expected: Some(end_token),
                            found: None,
                            matching: Some(WithInfo {
                                info: begin_info.clone(),
                                item: begin_token,
                            }),
                        },
                    });
                }

                WithInfo {
                    info: begin_info,
                    item: tree,
                }
            }
            None => {
                let (info, token) = match tokens.next() {
                    Some(token) => (token.info, Some(token.item.into_owned())),
                    None => (first_info.clone(), None),
                };

                diagnostics.push(WithInfo {
                    info,
                    item: Diagnostic::Mismatch {
                        expected: None,
                        found: token,
                        matching: None,
                    },
                });

                WithInfo {
                    info: first_info,
                    item: TokenTree::Error,
                }
            }
        };

        // Allow trailing line break
        if let TokenTree::Block(statements) = &mut tree.item {
            if statements.last().is_some_and(
                |tree| matches!(&tree.item, TokenTree::List(_, elements) if elements.is_empty()),
            ) {
                statements.pop().unwrap();
            }
        }

        (tree, diagnostics)
    }

    pub fn from_inline(
        driver: &D,
        tokens: impl IntoIterator<Item = WithInfo<D::Info, Token<'src>>>,
    ) -> Option<WithInfo<D::Info, Self>>
    where
        D::Info: From<Location>,
    {
        let (tree, diagnostics) = TokenTree::from_top_level(driver, tokens);
        if !diagnostics.is_empty() {
            return None;
        }

        match tree.item {
            TokenTree::Block(statements) => Some(statements.into_iter().next().unwrap()),
            _ => unreachable!("`from_top_level` always returns a block"),
        }
    }
}
