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

/// The precedence of an operator determines how tightly it groups compared to
/// other operators. For example, `*` has a higher precedence than `+`, so
/// `1 + 2 * 3` is equivalent to `1 + (2 * 3)` and not `(1 + 2) * 3`.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    As, // most tightly binding
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
    Function, // least tightly binding
}

impl Operator {
    /// Obtain the precedence of an operator. Some operators the same precedence
    /// and must be disambiguated by `Associativity`.
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
    /// Obtain the token that would have started the list.
    fn start(&self) -> Token<'static> {
        match self {
            ListDelimiter::Parentheses => Token::LeftParenthesis,
            ListDelimiter::Brackets => Token::LeftBracket,
        }
    }

    /// Obtain the token that would end the list.
    fn end(&self) -> Token<'static> {
        match self {
            ListDelimiter::Parentheses => Token::RightParenthesis,
            ListDelimiter::Brackets => Token::RightBracket,
        }
    }

    /// Assert that `end` corresponds to `self.end()`.
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
    /// Produce a token tree from a token stream in preparation for parsing.
    pub fn from_top_level(
        driver: &D,
        tokens: impl IntoIterator<Item = WithInfo<D::Info, Token<'src>>>,
    ) -> (
        WithInfo<D::Info, Self>,               // the root of the tree
        Vec<WithInfo<D::Info, Diagnostic<D>>>, // any diagnostics produced
    )
    where
        D::Info: From<Location>,
    {
        let mut tokens = tokens.into_iter();

        // If the file is empty, return an empty block.
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
                        item: TokenTree::EmptyFile,
                    },
                    Vec::new(),
                )
            }
        };

        // If we don't encounter any other tokens for some reason, just use
        // `first_info` to produce diagnostics.
        let first_info = first_token.info.clone();

        // Maintain our position within the source code.
        let mut info = first_info.clone();

        // Maintain a stack of groups -- when a left parenthesis is encountered,
        // for example, a `TokenTree::List` is pushed onto the stack, and when a
        // right parenthesis is encountered, this list is popped and appended
        // to the group that's now on the top. (This works because the top level
        // is a block that's initially pushed here.) Non-grouping symbols are
        // always appended to the group on the top of the stack. We easily
        // detect mismatched parentheses/brackets and braces by checking if the
        // top of the stack exists and is the expected group.
        let mut stack = vec![(info, TokenTree::Block(Vec::new()))];

        // Maintain a list of diagnostics here. These will be returned to the
        // caller but do not prevent a partial tree from being constructed.
        let mut diagnostics = Vec::new();

        // Iterate over the tokens and build the tree.
        for token in [first_token].into_iter().chain(tokens.by_ref()) {
            info = token.info;

            // Helper for adding a token to the top of the stack. It returns
            // `true` if the token could be added to a group (ie. a list, block,
            // or attribute), and `false` if the top of the stack is not a
            // group. Rather than calling this directly, we use the `push!`
            // macro below.
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

            // Call `push` and move on to the next token if the current token
            // could not be added to the top of the stack.
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
                    // Note that we don't use `push!` here because we are
                    // creating a new group; we will push the group created by
                    // this list onto what was the top of the stack when we
                    // encounter the closing right parenthesis.
                    stack.push((
                        info,
                        TokenTree::List(ListDelimiter::Parentheses, Vec::new()),
                    ));
                }
                Token::LeftBracket => {
                    stack.push((info, TokenTree::List(ListDelimiter::Brackets, Vec::new())));
                }
                Token::LeftBrace => {
                    stack.push((info, TokenTree::Block(Vec::new())));
                }
                Token::RightParenthesis | Token::RightBracket | Token::RightBrace
                    if stack.len() == 1 =>
                {
                    // The bottom of the stack is the top level, which cannot be
                    // closed.
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
                Token::RightParenthesis | Token::RightBracket => {
                    // Now we look for the list created by the opening left
                    // parentheses or bracket above...
                    let (begin_info, delimiter, expressions) = match stack.pop() {
                        Some((begin_info, TokenTree::List(delimiter, expressions))) => {
                            // ...and ensure it has the correct delimiter.
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
                        // If we find a block instead, then the user meant to
                        // put a closing brace instead of a parenthesis.
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
                        // If we find anything else, then there are no groups
                        // on the top of the stack. That means the user closed
                        // all the groups in the file already, so this closing
                        // parenthesis is extra.
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

                    let info = D::merge_info(begin_info, info.clone());

                    let delimiter = WithInfo {
                        info: info.clone(),
                        item: delimiter,
                    };

                    // Call `parse_operators` on the list contents before
                    // merging it back onto the stack.
                    push!(WithInfo {
                        info,
                        item: parse_operators::<D>(delimiter, expressions, &mut diagnostics),
                    });
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

                    // We need to handle the last statement specially. If
                    // there's a line break between it and the closing brace,
                    // don't create a new statement. But normally, line breaks
                    // trigger the end of a statement and parse the statement's
                    // operators, so if there is no line break, we need to do
                    // that here.
                    if let Some(tree) = statements.last_mut() {
                        if matches!(&tree.item, TokenTree::List(_, expressions) if expressions.is_empty())
                        {
                            statements.pop();
                        } else {
                            let (delimiter, expressions) = match &mut tree.item {
                                TokenTree::List(delimiter, expressions) => (
                                    WithInfo {
                                        info: tree.info.clone(),
                                        item: *delimiter,
                                    },
                                    mem::take(expressions),
                                ),
                                _ => panic!("{:#?}", tree),
                            };

                            // Make the statement's info span from its first
                            // expression to its last.
                            if let Some(first) = expressions.first() {
                                let last = expressions.last().unwrap();
                                tree.info = D::merge_info(first.info.clone(), last.info.clone());
                            }

                            tree.item =
                                parse_operators::<D>(delimiter, expressions, &mut diagnostics);
                        }
                    }

                    let info = D::merge_info(begin_info.clone(), info.clone());

                    let item = if statements.is_empty() {
                        diagnostics.push(WithInfo {
                            info: info.clone(),
                            item: Diagnostic::EmptyBraces,
                        });

                        TokenTree::Error
                    } else {
                        TokenTree::Block(statements)
                    };

                    push!(WithInfo { info, item });
                }
                Token::LineBreak => {
                    // Line breaks are only significant in blocks because they
                    // separate statements. Remember that before calling this
                    // function, the tokens should have been passed through
                    // `to_logical_lines`, so the only line breaks remaining are
                    // ones that separate statements.
                    match stack.last_mut() {
                        Some((_, TokenTree::List(_, _) | TokenTree::UnresolvedAttribute(_))) => {
                            // Ignore line breaks within lists and attributes.
                            continue;
                        }
                        Some((begin_info, TokenTree::Block(statements))) => {
                            if let Some(tree) = statements.last_mut() {
                                let (delimiter, expressions) = match &mut tree.item {
                                    TokenTree::List(delimiter, expressions) => (
                                        WithInfo {
                                            info: tree.info.clone(),
                                            item: *delimiter,
                                        },
                                        mem::take(expressions),
                                    ),
                                    _ => unreachable!(),
                                };

                                if expressions.is_empty() {
                                    // Ignore empty statements.
                                    statements.pop();
                                } else {
                                    // Make the statement's info span from its first
                                    // expression to its last.
                                    if let Some(first) = expressions.first() {
                                        let last = expressions.last().unwrap();
                                        tree.info =
                                            D::merge_info(first.info.clone(), last.info.clone());
                                    }

                                    tree.item = parse_operators::<D>(
                                        delimiter,
                                        expressions,
                                        &mut diagnostics,
                                    );
                                }
                            }

                            statements.push(WithInfo {
                                info: begin_info.clone(),
                                item: TokenTree::List(ListDelimiter::Parentheses, Vec::new()),
                            });
                        }
                        _ => {
                            // This will only happen if there is an extra
                            // closing brace somewhere that closed the top-level
                            // group.
                            //
                            // FIXME: Recover from this instead of treating all
                            // remaining line breaks as errors
                            diagnostics.push(WithInfo {
                                info,
                                item: Diagnostic::Mismatch {
                                    expected: None,
                                    found: Some(token.item.into_owned()),
                                    matching: None,
                                },
                            });
                        }
                    }
                }
                Token::Comment(_) => {
                    // Normally comments are removed in `to_logical_lines`, but
                    // if we encounter one here, just skip over it.
                    continue;
                }
                Token::Keyword(keyword) => match keyword {
                    Keyword::Attribute => {
                        // Attributes accept a single group after the `@`, hence
                        // the `Option` value stored in `UnresolvedAttribute`
                        // (instead of a `Vec`).
                        stack.push((info, TokenTree::UnresolvedAttribute(None)));
                    }
                    Keyword::Mutate => {
                        // Currently, `!` is only allowed in statement position,
                        // so assume that the top of the stack is a block and
                        // just modify the most recent statement.
                        match stack.last_mut() {
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
                                // Any other position is an error (for now).
                                diagnostics.push(WithInfo {
                                    info,
                                    item: Diagnostic::Mismatch {
                                        expected: None,
                                        found: Some(token.item.into_owned()),
                                        matching: None,
                                    },
                                });
                            }
                        }
                    }
                    // Leave any other keyword as-is.
                    _ => push!(WithInfo {
                        info,
                        item: TokenTree::Keyword(keyword),
                    }),
                },
                // Operators are handled specially in `parse_operators`.
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
                // Names, numbers, and text are left as-is.
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

        // If there are unclosed parentheses or braces, the stack will have more
        // than one item. But the bottom of the stack is always the top level
        // (ie. the root of the tree).
        let mut stack = stack.into_iter();
        let mut tree = match stack.next() {
            Some((begin_info, tree)) => {
                // Report errors for items remaining on the stack.
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
                // If the stack is empty, then a stray brace closed the top
                // level -- report an error.
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

        if let TokenTree::Block(statements) = &mut tree.item {
            // If there's a trailing line break, remove the inserted empty statement.
            if let Some(statement) = statements.last() {
                if matches!(&statement.item, TokenTree::List(_, expressions) if expressions.is_empty())
                {
                    statements.pop();
                }
            }

            // If there are no statements at all, produce an empty file.
            if statements.is_empty() {
                tree.item = TokenTree::EmptyFile;
            }
        }

        (tree, diagnostics)
    }

    /// Equivalent to [`TokenTree::from_top_level`], but assumes that the input
    /// is a single statement. This is useful for tests, since you can pass a
    /// string like `1 + 2` and get back the operator expression directly
    /// instead of it being wrapped in a block.
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

// Once we have all the expressions in a list or statement, recursively parse
// any operators contained within.
fn parse_operators<'src, D: Driver>(
    delimiter: WithInfo<D::Info, ListDelimiter>,
    expressions: Vec<WithInfo<D::Info, TokenTree<'src, D>>>,
    diagnostics: &mut Vec<WithInfo<D::Info, Diagnostic<D>>>,
) -> TokenTree<'src, D> {
    // The order of the operators here matters because regular operators have
    // a higher precedence than variadic and non-associative operators.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    enum AnyOperator {
        Operator(Operator),
        VariadicOperator(VariadicOperator),
        NonAssociativeOperator(NonAssociativeOperator),
    }

    // Find all operators in the list and wrap them in `AnyOperator`.
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
        });

    // Next, find the operator(s) with the highest predecence.
    let operators = operators.max_set_by(|(_, left), (_, right)| match (left.item, right.item) {
        (AnyOperator::Operator(left), AnyOperator::Operator(right)) => {
            left.precedence().cmp(&right.precedence())
        }
        (left, right) => left.cmp(&right),
    });

    // If there are no operators...
    if operators.is_empty() {
        if expressions.is_empty() {
            // ...and there are no expressions, produce an error.
            diagnostics.push(delimiter.replace(Diagnostic::EmptyParentheses));

            return TokenTree::Error;
        } else {
            // ...otherwise, return the list as-is.
            return TokenTree::List(delimiter.item, expressions);
        }
    }

    // All the operators in `operators` have the same precedence, and all
    // operators with the same precedence have the same associativity, so just
    // use the first operator to get the precedence and associativity.
    let operator = &operators.first().unwrap().1;
    let info = operator.info.clone();
    match operator.item {
        AnyOperator::Operator(operator) => {
            let (index, operator) = {
                // If the operator is left-associative, parse the last operator
                // first so that the left side is grouped recursively, and vice
                // versa.
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

            let (left, right) = split_at_operator(index, expressions, diagnostics);

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
            // Variadic operators act as separators.
            let mut indices = operators.iter().map(|&(index, _)| index).peekable();

            // Form groups of expressions separated by the variadic operators.
            let mut groups = vec![Vec::new()];
            for (expression_index, expression) in expressions.into_iter().enumerate() {
                if let Some(operator_index) = indices.peek().copied() {
                    use std::cmp::Ordering;

                    match expression_index.cmp(&operator_index) {
                        Ordering::Less => {
                            // If the expression is before the separator, add it
                            // to the group below.
                        }
                        Ordering::Equal => {
                            // If the expression _is_ the separator, skip it.
                            continue;
                        }
                        Ordering::Greater => {
                            // If the expression is after the separator, create a
                            // new group and move on to the next separator.
                            groups.push(Vec::new());
                            indices.next();
                        }
                    }
                }

                groups.last_mut().unwrap().push(expression);
            }

            // Allow a single trailing variadic operator. This also handles the
            // case where the list contains a single variadic operator and
            // nothing else (eg. `(,)`), which represents an empty collection.
            if groups.last().unwrap().is_empty() {
                groups.pop();
            }

            // Recursively parse operators within each group.
            let elements = groups
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

                    let delimiter = WithInfo {
                        info: info.clone(),
                        item: ListDelimiter::Parentheses,
                    };

                    WithInfo {
                        info,
                        item: parse_operators::<D>(delimiter, group, diagnostics),
                    }
                })
                .collect::<Vec<_>>();

            TokenTree::VariadicOperator(
                WithInfo {
                    info: info.clone(),
                    item: operator,
                },
                elements,
            )
        }
        AnyOperator::NonAssociativeOperator(operator) => {
            // There can't be more than one non-associative operator with the
            // same precedence in a list at a time.
            if operators.len() != 1 {
                // FIXME: Report a diagnostic here
                return TokenTree::Error;
            }

            let index = {
                let (index, operator) = operators.first().unwrap();

                WithInfo {
                    info: operator.info.clone(),
                    item: *index,
                }
            };

            let (left, right) = split_at_operator(index, expressions, diagnostics);

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

/// Split a list of expressions into the left and right sides of an operator
/// expression, and then recursively parse the operators within each side.
fn split_at_operator<'src, D: Driver>(
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
        // The operator is in the middle of the list.
        (true, true) => {
            let right = expressions.split_off(index + 1);
            expressions.pop().unwrap(); // skip the operator itself
            let left = expressions;

            let left_info = D::merge_info(
                left.first().unwrap().info.clone(),
                left.last().unwrap().info.clone(),
            );

            let right_info = D::merge_info(
                right.first().unwrap().info.clone(),
                right.last().unwrap().info.clone(),
            );

            let left_delimiter = WithInfo {
                info: left_info.clone(),
                item: ListDelimiter::Parentheses,
            };

            let right_delimiter = WithInfo {
                info: right_info.clone(),
                item: ListDelimiter::Parentheses,
            };

            (
                WithInfo {
                    info: left_info,
                    item: parse_operators::<D>(left_delimiter, left, diagnostics),
                },
                WithInfo {
                    info: right_info,
                    item: parse_operators::<D>(right_delimiter, right, diagnostics),
                },
            )
        }
        // The operator is at the end of the list with nothing after it.
        (true, false) => {
            expressions.pop().unwrap(); // skip the operator itself
            let left = expressions;

            let left_info = D::merge_info(
                left.first().unwrap().info.clone(),
                left.last().unwrap().info.clone(),
            );

            let left_delimiter = WithInfo {
                info: left_info.clone(),
                item: ListDelimiter::Parentheses,
            };

            (
                WithInfo {
                    info: left_info,
                    item: parse_operators::<D>(left_delimiter, left, diagnostics),
                },
                WithInfo {
                    info,
                    item: TokenTree::Error,
                },
            )
        }
        // The operator is at the beginning of the list with nothing before it.
        (false, true) => {
            let right = expressions.split_off(1); // skip the operator itself

            let right_info = D::merge_info(
                right.first().unwrap().info.clone(),
                right.last().unwrap().info.clone(),
            );

            let right_delimiter = WithInfo {
                info: right_info.clone(),
                item: ListDelimiter::Parentheses,
            };

            (
                WithInfo {
                    info,
                    item: TokenTree::Error,
                },
                WithInfo {
                    info: right_info,
                    item: parse_operators::<D>(right_delimiter, right, diagnostics),
                },
            )
        }
        // The operator is by itself.
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
