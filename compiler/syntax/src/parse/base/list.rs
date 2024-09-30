use crate::{
    parse::{
        base::{ParseFn, ParseStack, Parser, Rule},
        render::RuleToRender,
        Direction, SyntaxKind,
    },
    tokenize::TokenTree,
    Driver,
};
use std::rc::Rc;
use wipple_util::{DefaultFromInfo, WithInfo};

impl<D: Driver, Output: 'static> Rule<D, Output> {
    pub fn parse(
        &self,
        parser: &mut Parser<'_, D>,
        tree: WithInfo<D::Info, &TokenTree<'_, D>>,
        stack: &Rc<ParseStack<D>>,
        direction: Option<Direction>,
    ) -> WithInfo<D::Info, Output>
    where
        Output: DefaultFromInfo<D::Info>,
    {
        let info = tree.info.clone();

        let stack = stack.push(WithInfo {
            info: tree.info.clone(),
            item: self.syntax_kind.clone(),
        });

        self.parse
            .parse(parser, tree, &stack, direction.clone())
            .unwrap_or_else(|| {
                parser.add_diagnostic(stack.error_expected(
                    WithInfo {
                        info: info.clone(),
                        item: self.syntax_kind.clone(),
                    },
                    direction,
                ));

                Output::default_from_info(info)
            })
    }

    pub fn parse_option(
        &self,
        parser: &mut Parser<'_, D>,
        tree: WithInfo<D::Info, &TokenTree<'_, D>>,
        stack: &Rc<ParseStack<D>>,
        direction: Option<Direction>,
    ) -> WithInfo<D::Info, Option<Output>> {
        let info = tree.info.clone();

        let stack = stack.push(WithInfo {
            info: tree.info.clone(),
            item: self.syntax_kind.clone(),
        });

        let result = self.parse.parse(parser, tree, &stack, direction.clone());
        if result.is_none() {
            parser.add_diagnostic(stack.error_expected(
                WithInfo {
                    info: info.clone(),
                    item: self.syntax_kind.clone(),
                },
                direction,
            ));
        }

        result
            .map(|result| result.map(Some))
            .unwrap_or_else(|| Option::default_from_info(info))
    }

    pub fn try_parse(
        &self,
        parser: &mut Parser<'_, D>,
        tree: WithInfo<D::Info, &TokenTree<'_, D>>,
        stack: &Rc<ParseStack<D>>,
        direction: Option<Direction>,
    ) -> Option<Result<WithInfo<D::Info, Output>, usize>> {
        let stack = stack.push(WithInfo {
            info: tree.info.clone(),
            item: self.syntax_kind.clone(),
        });

        if self.backtracks() {
            self.parse.try_parse(parser, tree, &stack, direction)
        } else {
            self.parse.parse(parser, tree, &stack, direction).map(Ok)
        }
    }

    pub fn backtracks(&self) -> bool {
        (self.backtracks)()
    }
}

impl<D: Driver, Output: 'static> Rule<D, Output> {
    pub fn list<E>(
        syntax_kind: SyntaxKind,
        parse_element: fn() -> Rule<D, E>,
        output: impl Fn(
                &mut Parser<'_, D>,
                D::Info,
                Vec<WithInfo<D::Info, E>>,
                &Rc<ParseStack<D>>,
            ) -> WithInfo<D::Info, Output>
            + Clone
            + 'static,
    ) -> Self
    where
        E: DefaultFromInfo<D::Info> + 'static,
    {
        Rule::nonterminal(
            syntax_kind,
            RuleToRender::List(vec![
                Rc::new(move || parse_element().render_nested()),
                Rc::new(|| RuleToRender::Ellipsis),
            ]),
            || true,
            ParseFn::new(
                {
                    let output = output.clone();

                    move |parser, tree, stack, _| {
                        let elements = match &tree.item {
                            TokenTree::List(_, elements) => elements,
                            _ => return None,
                        };

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
                    let elements = match &tree.item {
                        TokenTree::List(_, elements) => elements,
                        _ => return None,
                    };

                    let elements = elements
                        .iter()
                        .map(|element| parse_element().parse(parser, element.as_ref(), stack, None))
                        .collect();

                    Some(output(parser, tree.info, elements, stack))
                },
            ),
        )
    }

    pub fn list_prefix<P, E>(
        syntax_kind: SyntaxKind,
        parse_prefix: fn() -> Rule<D, P>,
        parse_element: fn() -> Rule<D, E>,
        output: impl Fn(
                &mut Parser<'_, D>,
                D::Info,
                WithInfo<D::Info, P>,
                Vec<WithInfo<D::Info, E>>,
                &Rc<ParseStack<D>>,
            ) -> WithInfo<D::Info, Output>
            + Clone
            + 'static,
    ) -> Self
    where
        P: DefaultFromInfo<D::Info> + 'static,
        E: DefaultFromInfo<D::Info> + 'static,
    {
        Rule::nonterminal(
            syntax_kind,
            RuleToRender::List(vec![
                Rc::new(move || parse_prefix().render_nested()),
                Rc::new(move || parse_element().render_nested()),
                Rc::new(|| RuleToRender::Ellipsis),
            ]),
            || true,
            ParseFn::new(
                {
                    let output = output.clone();

                    move |parser, tree, stack, _| {
                        let elements = match &tree.item {
                            TokenTree::List(_, elements) => elements,
                            _ => return None,
                        };

                        let mut elements = elements.iter();

                        let prefix = match parse_prefix().try_parse(
                            parser,
                            elements.next()?.as_ref(),
                            stack,
                            None,
                        )? {
                            Ok(prefix) => prefix,
                            Err(progress) => return Some(Err(progress)),
                        };

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

                        Some(Ok(output(parser, tree.info, prefix, result, stack)))
                    }
                },
                move |parser, tree, stack, _| {
                    let elements = match &tree.item {
                        TokenTree::List(_, elements) => elements,
                        _ => return None,
                    };

                    let mut elements = elements.iter();

                    let prefix = match elements.next() {
                        Some(prefix) => parse_prefix().parse(parser, prefix.as_ref(), stack, None),
                        None => {
                            parser.add_diagnostic(stack.error_expected(
                                WithInfo {
                                    info: tree.info.clone(),
                                    item: parse_prefix().syntax_kind,
                                },
                                None,
                            ));

                            P::default_from_info(tree.info.clone())
                        }
                    };

                    let elements = elements
                        .map(|element| parse_element().parse(parser, element.as_ref(), stack, None))
                        .collect();

                    Some(output(parser, tree.info, prefix, elements, stack))
                },
            ),
        )
    }
}
