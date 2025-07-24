use crate::{
    syntax::{
        Location,
        parse::{
            Direction, SyntaxKind,
            base::{ParseFn, ParseStack, Parser, Rule},
            render::RuleToRender,
        },
        tokenize::{self, TokenTree},
    },
    util::{DefaultFromInfo, WithInfo},
};
use std::rc::Rc;

impl<Output: 'static> Rule<Output> {
    pub fn operator<L, R>(
        syntax_kind: SyntaxKind,
        expected: tokenize::Operator,
        parse_left: fn() -> Rule<L>,
        parse_right: fn() -> Rule<R>,
        output: impl Fn(
            &mut Parser<'_>,
            Location,
            WithInfo<L>,
            WithInfo<R>,
            &Rc<ParseStack>,
        ) -> WithInfo<Output>
        + Clone
        + 'static,
    ) -> Self
    where
        L: DefaultFromInfo + 'static,
        R: DefaultFromInfo + 'static,
    {
        Rule::nonterminal(
            syntax_kind,
            RuleToRender::List(vec![
                Rc::new(move || parse_left().render_nested()),
                Rc::new(move || RuleToRender::Keyword(expected.to_string())),
                Rc::new(move || parse_right().render_nested()),
            ]),
            || true,
            ParseFn::new(
                {
                    let output = output.clone();
                    move |parser, tree, stack, _| {
                        let (found, left, right) = match &tree.item {
                            TokenTree::Operator(operator, left, right) => (operator, left, right),
                            _ => return None,
                        };

                        if found.item != expected {
                            return None;
                        }

                        let left = match parse_left().try_parse(
                            parser,
                            left.as_deref(),
                            stack,
                            Some(Direction::Before(SyntaxKind::Operator(expected))),
                        )? {
                            Ok(left) => left,
                            Err(progress) => return Some(Err(progress)),
                        };

                        let right = match parse_right().try_parse(
                            parser,
                            right.as_deref(),
                            stack,
                            Some(Direction::After(SyntaxKind::Operator(expected))),
                        )? {
                            Ok(right) => right,
                            Err(progress) => return Some(Err(progress)),
                        };

                        Some(Ok(output(parser, tree.info, left, right, stack)))
                    }
                },
                move |parser, tree, stack, _| {
                    let (found, left, right) = match &tree.item {
                        TokenTree::Operator(operator, left, right) => (operator, left, right),
                        _ => return None,
                    };

                    if found.item != expected {
                        return None;
                    }

                    let left = parse_left().parse(
                        parser,
                        left.as_deref(),
                        stack,
                        Some(Direction::Before(SyntaxKind::Operator(expected))),
                    );

                    let right = parse_right().parse(
                        parser,
                        right.as_deref(),
                        stack,
                        Some(Direction::After(SyntaxKind::Operator(expected))),
                    );

                    Some(output(parser, tree.info, left, right, stack))
                },
            ),
        )
    }

    pub fn require_operator<L, R>(
        syntax_kind: SyntaxKind,
        expected: tokenize::Operator,
        parse_left: fn() -> Rule<L>,
        parse_right: fn() -> Rule<R>,
        output: impl Fn(
            &mut Parser<'_>,
            Location,
            WithInfo<L>,
            WithInfo<R>,
            &Rc<ParseStack>,
        ) -> WithInfo<Output>
        + Clone
        + 'static,
    ) -> Self
    where
        L: DefaultFromInfo + 'static,
        R: DefaultFromInfo + 'static,
    {
        Rule::nonterminal(
            syntax_kind.clone(),
            RuleToRender::List(vec![
                Rc::new(move || parse_left().render_nested()),
                Rc::new(move || RuleToRender::Keyword(expected.to_string())),
                Rc::new(move || parse_right().render_nested()),
            ]),
            || true,
            ParseFn::new(
                {
                    let output = output.clone();
                    move |parser, tree, stack, _| {
                        let (found, left, right) = match &tree.item {
                            TokenTree::Operator(operator, left, right) => (operator, left, right),
                            _ => return Some(Err(stack.len())),
                        };

                        if found.item != expected {
                            return Some(Err(stack.len()));
                        }

                        let left = match parse_left().try_parse(
                            parser,
                            left.as_deref(),
                            stack,
                            Some(Direction::Before(SyntaxKind::Operator(expected))),
                        )? {
                            Ok(left) => left,
                            Err(progress) => return Some(Err(progress)),
                        };

                        let right = match parse_right().try_parse(
                            parser,
                            right.as_deref(),
                            stack,
                            Some(Direction::After(SyntaxKind::Operator(expected))),
                        )? {
                            Ok(right) => right,
                            Err(progress) => return Some(Err(progress)),
                        };

                        Some(Ok(output(parser, tree.info, left, right, stack)))
                    }
                },
                move |parser, tree, stack, _| {
                    let (found, left, right) = match &tree.item {
                        TokenTree::Operator(operator, left, right) => (operator, left, right),
                        _ => {
                            parser.add_diagnostic(stack.error_expected(
                                WithInfo {
                                    info: tree.info,
                                    item: syntax_kind.clone(),
                                },
                                None,
                            ));

                            return None;
                        }
                    };

                    if found.item != expected {
                        parser.add_diagnostic(stack.error_expected(
                            WithInfo {
                                info: tree.info,
                                item: syntax_kind.clone(),
                            },
                            None,
                        ));

                        return None;
                    }

                    let left = parse_left().parse(
                        parser,
                        left.as_deref(),
                        stack,
                        Some(Direction::Before(SyntaxKind::Operator(expected))),
                    );

                    let right = parse_right().parse(
                        parser,
                        right.as_deref(),
                        stack,
                        Some(Direction::After(SyntaxKind::Operator(expected))),
                    );

                    Some(output(parser, tree.info, left, right, stack))
                },
            ),
        )
    }

    pub fn variadic_operator<E>(
        syntax_kind: SyntaxKind,
        expected: tokenize::VariadicOperator,
        parse_element: fn() -> Rule<E>,
        output: impl Fn(
            &mut Parser<'_>,
            Location,
            Vec<WithInfo<E>>,
            &Rc<ParseStack>,
        ) -> WithInfo<Output>
        + Clone
        + 'static,
    ) -> Self
    where
        E: DefaultFromInfo + 'static,
    {
        Rule::nonterminal(
            syntax_kind,
            RuleToRender::List(vec![
                Rc::new(move || parse_element().render_nested()),
                Rc::new(move || RuleToRender::Keyword(expected.to_string())),
                Rc::new(|| RuleToRender::Ellipsis),
            ]),
            || true,
            ParseFn::new(
                {
                    let output = output.clone();

                    move |parser, tree, stack, _| {
                        let (found, elements) = match &tree.item {
                            TokenTree::VariadicOperator(operator, children) => (operator, children),
                            _ => return None,
                        };

                        if found.item != expected {
                            return None;
                        }

                        let mut result = Vec::with_capacity(elements.len());
                        for element in elements {
                            match parse_element().try_parse(
                                parser,
                                element.as_ref(),
                                stack,
                                None,
                            )? {
                                Ok(element) => result.push(element),
                                Err(progress) => return Some(Err(progress)),
                            }
                        }

                        Some(Ok(output(parser, tree.info, result, stack)))
                    }
                },
                move |parser, tree, stack, _| {
                    let (found, elements) = match &tree.item {
                        TokenTree::VariadicOperator(operator, children) => (operator, children),
                        _ => return None,
                    };

                    if found.item != expected {
                        return None;
                    }

                    let elements = elements
                        .iter()
                        .map(|element| parse_element().parse(parser, element.as_ref(), stack, None))
                        .collect();

                    Some(output(parser, tree.info, elements, stack))
                },
            ),
        )
    }

    pub fn non_associative_operator<L, R>(
        syntax_kind: SyntaxKind,
        expected: tokenize::NonAssociativeOperator,
        parse_left: fn() -> Rule<L>,
        parse_right: fn() -> Rule<R>,
        output: impl Fn(
            &mut Parser<'_>,
            Location,
            WithInfo<L>,
            WithInfo<R>,
            &Rc<ParseStack>,
        ) -> WithInfo<Output>
        + Clone
        + 'static,
    ) -> Self
    where
        L: DefaultFromInfo + 'static,
        R: DefaultFromInfo + 'static,
    {
        Rule::nonterminal(
            syntax_kind,
            RuleToRender::List(vec![
                Rc::new(move || parse_left().render_nested()),
                Rc::new(move || RuleToRender::Keyword(expected.to_string())),
                Rc::new(move || parse_right().render_nested()),
            ]),
            || true,
            ParseFn::new(
                {
                    let output = output.clone();

                    move |parser, tree, stack, _| {
                        let (found, left, right) = match &tree.item {
                            TokenTree::NonAssociativeOperator(operator, left, right) => {
                                (operator, left, right)
                            }
                            _ => return None,
                        };

                        if found.item != expected {
                            return None;
                        }

                        let left = match parse_left().try_parse(
                            parser,
                            left.as_deref(),
                            stack,
                            Some(Direction::Before(SyntaxKind::NonAssociativeOperator(
                                expected,
                            ))),
                        )? {
                            Ok(left) => left,
                            Err(progress) => return Some(Err(progress)),
                        };

                        let right = match parse_right().try_parse(
                            parser,
                            right.as_deref(),
                            stack,
                            Some(Direction::After(SyntaxKind::NonAssociativeOperator(
                                expected,
                            ))),
                        )? {
                            Ok(right) => right,
                            Err(progress) => return Some(Err(progress)),
                        };

                        Some(Ok(output(parser, tree.info, left, right, stack)))
                    }
                },
                move |parser, tree, stack, _| {
                    let (found, left, right) = match &tree.item {
                        TokenTree::NonAssociativeOperator(operator, left, right) => {
                            (operator, left, right)
                        }
                        _ => return None,
                    };

                    if found.item != expected {
                        return None;
                    }

                    let left = parse_left().parse(
                        parser,
                        left.as_deref(),
                        stack,
                        Some(Direction::Before(SyntaxKind::NonAssociativeOperator(
                            expected,
                        ))),
                    );

                    let right = parse_right().parse(
                        parser,
                        right.as_deref(),
                        stack,
                        Some(Direction::After(SyntaxKind::NonAssociativeOperator(
                            expected,
                        ))),
                    );

                    Some(output(parser, tree.info, left, right, stack))
                },
            ),
        )
    }
}
