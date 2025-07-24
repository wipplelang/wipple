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

macro_rules! impl_keyword_rule {
    ($pattern:ident($ty:ty), $name:ident($($n:ident),*), $kind:ident) => {
        impl<Output: 'static> Rule<Output> {
            #[allow(unused, non_snake_case, clippy::redundant_clone, clippy::too_many_arguments)]
            pub fn $name<$($n),*>(
                syntax_kind: SyntaxKind,
                expected: $ty,
                $($n: fn() -> Rule<$n>,)*
                output: impl Fn(&mut Parser<'_>, Location, $(WithInfo<$n>, )* &Rc<ParseStack>) -> WithInfo<Output> + Clone + 'static,
            ) -> Rule<Output>
            where
                $($n: DefaultFromInfo + 'static,)*
            {
                Rule::nonterminal(
                    syntax_kind,
                    RuleToRender::List(vec![
                        Rc::new({
                            let expected = expected.clone();
                            move || RuleToRender::Keyword(expected.to_string())
                        }),
                        $(Rc::new(move || $n().render_nested()),)*
                    ]),
                    || true,
                    ParseFn::new(
                        {
                            let expected = expected.clone();
                            let output = output.clone();

                            move |parser, tree, stack, _| {
                                let mut elements = match &tree.item {
                                    TokenTree::List(_, elements) => elements.iter(),
                                    _ => return None,
                                };

                                let info = match elements.next()? {
                                    WithInfo {
                                        item: TokenTree::$pattern(found),
                                        info,
                                    } if *found == expected => info,
                                    _ => return None,
                                };

                                $(
                                    let $n = match $n().try_parse(parser, elements.next()?.as_ref(), stack, None)? {
                                        Ok($n) => $n,
                                        Err(progress) => return Some(Err(progress)),
                                    };
                                )*

                                if elements.next().is_some() {
                                    return None;
                                }

                                Some(Ok(output(parser, tree.info, $($n,)* stack)))
                            }
                        },
                        move |parser, tree, stack, _| {
                            let mut elements = match &tree.item {
                                TokenTree::List(_, elements) => elements.iter(),
                                _ => return None,
                            };

                            let info = match elements.next()? {
                                WithInfo {
                                    item: TokenTree::$pattern(found),
                                    info,
                                } if *found == expected => info,
                                _ => return None,
                            };

                            $(
                                let $n = match elements.next() {
                                    Some(input) => $n().parse(parser, input.as_ref(), stack, None),
                                    None => {
                                        parser.add_diagnostic(
                                            stack.error_expected(
                                                WithInfo {
                                                    info: info.clone(),
                                                    item: $n().syntax_kind,
                                                },
                                                None,
                                            ),
                                        );

                                        $n::default_from_info(info.clone())
                                    }
                                };
                            )*

                            for element in elements {
                                parser.add_diagnostic(
                                    stack.error_expected(
                                        WithInfo {
                                            info: Location::clone(&element.info),
                                            item: SyntaxKind::Nothing,
                                        },
                                        Direction::After(SyntaxKind::$kind(expected.clone())),
                                    )
                                );
                            }

                            Some(output(parser, tree.info, $($n,)* stack))
                        },
                    ),
                )
            }
        }
    };
}

macro_rules! impl_attributed_keyword_rule {
    ($pattern:ident($ty:ty), $name:ident($($n:ident),*), $kind:ident) => {
        impl<Output: 'static> Rule<Output> {
            #[allow(unused, non_snake_case, clippy::redundant_clone, clippy::too_many_arguments)]
            pub fn $name<_A, $($n),*>(
                syntax_kind: SyntaxKind,
                expected: $ty,
                parse_attribute: fn() -> Rule<_A>,
                $($n: fn() -> Rule<$n>,)*
                output: impl Fn(&mut Parser<'_>, Location, Vec<WithInfo<_A>>, $(WithInfo<$n>, )* &Rc<ParseStack>) -> WithInfo<Output> + Clone + 'static,
            ) -> Rule<Output>
            where
                _A: DefaultFromInfo + 'static,
                $($n: DefaultFromInfo + 'static,)*
            {
                Rule::nonterminal(
                    syntax_kind,
                    RuleToRender::List(vec![
                        Rc::new({
                            let expected = expected.clone();
                            move || RuleToRender::Keyword(expected.to_string())
                        }),
                        $(Rc::new(move || $n().render_nested()),)*
                    ]),
                    || true,
                    ParseFn::new(
                        {
                            let expected = expected.clone();
                            let output = output.clone();

                            move |parser, tree, stack, _| {
                                let mut elements = match &tree.item {
                                    TokenTree::List(_, elements) => elements.iter(),
                                    _ => return None,
                                };

                                let mut tree = elements.next()?.as_ref();
                                let mut attributes = Vec::new();
                                while let TokenTree::Attribute(attribute, contents) = tree.item {
                                    let attribute = match parse_attribute().try_parse(
                                        parser,
                                        attribute.as_deref(),
                                        stack,
                                        None,
                                    )? {
                                        Ok(attribute) => attribute,
                                        Err(progress) => return Some(Err(progress)),
                                    };

                                    attributes.push(attribute);

                                    tree = contents.as_deref();
                                }

                                let info = match tree {
                                    WithInfo {
                                        item: TokenTree::$pattern(found),
                                        ref info,
                                    } if *found == expected => Location::clone(&info),
                                    _ => return None,
                                };

                                $(
                                    let $n = match $n().try_parse(parser, elements.next()?.as_ref(), stack, None)? {
                                        Ok($n) => $n,
                                        Err(progress) => return Some(Err(progress)),
                                    };
                                )*

                                if elements.next().is_some() {
                                    return None;
                                }

                                Some(Ok(output(parser, tree.info, attributes, $($n,)* stack)))
                            }
                        },
                        move |parser, tree, stack, _| {
                            let mut elements = match &tree.item {
                                TokenTree::List(_, elements) => elements.iter(),
                                _ => return None,
                            };

                            let mut tree = elements.next()?.as_ref();
                            let mut attributes = Vec::new();
                            while let TokenTree::Attribute(attribute, contents) = tree.item {
                                let attribute = parse_attribute().parse(
                                    parser,
                                    attribute.as_deref(),
                                    stack,
                                    None,
                                );

                                attributes.push(attribute);

                                tree = contents.as_deref();
                            }

                            let info = match elements.next()? {
                                WithInfo {
                                    item: TokenTree::$pattern(found),
                                    info,
                                } if *found == expected => info,
                                _ => return None,
                            };

                            $(
                                let $n = match elements.next() {
                                    Some(input) => $n().parse(parser, input.as_ref(), stack, None),
                                    None => {
                                        parser.add_diagnostic(
                                            stack.error_expected(
                                                WithInfo {
                                                    info: info.clone(),
                                                    item: $n().syntax_kind,
                                                },
                                                None,
                                            ),
                                        );

                                        $n::default_from_info(info.clone())
                                    }
                                };
                            )*

                            for element in elements {
                                parser.add_diagnostic(
                                    stack.error_expected(
                                        WithInfo {
                                            info: Location::clone(&element.info),
                                            item: SyntaxKind::Nothing,
                                        },
                                        Direction::After(SyntaxKind::$kind(expected.clone())),
                                    )
                                );
                            }

                            Some(output(parser, tree.info, attributes, $($n,)* stack))
                        },
                    ),
                )
            }
        }
    };
}

macro_rules! impl_keyword_rules {
    ($pattern:ident($ty:ty), ($name:ident, $attributed_name:ident)($($n:ident),*), $kind:ident) => {
        impl_keyword_rule!($pattern($ty), $name($($n),*), $kind);
        impl_attributed_keyword_rule!($pattern($ty), $attributed_name($($n),*), $kind);
    };
}

impl_keyword_rules!(
    Keyword(tokenize::Keyword),
    (keyword0, attributed_keyword0)(),
    Keyword
);
impl_keyword_rules!(
    Keyword(tokenize::Keyword),
    (keyword1, attributed_keyword1)(A),
    Keyword
);
impl_keyword_rules!(
    Keyword(tokenize::Keyword),
    (keyword2, attributed_keyword2)(A, B),
    Keyword
);
impl_keyword_rules!(
    Keyword(tokenize::Keyword),
    (keyword3, attributed_keyword3)(A, B, C),
    Keyword
);

impl_keyword_rules!(
    Name(String),
    (contextual_keyword0, attributed_contextual_keyword0)(),
    ContextualKeyword
);
impl_keyword_rules!(
    Name(String),
    (contextual_keyword1, attributed_contextual_keyword1)(A),
    ContextualKeyword
);
impl_keyword_rules!(
    Name(String),
    (contextual_keyword2, attributed_contextual_keyword2)(A, B),
    ContextualKeyword
);
impl_keyword_rules!(
    Name(String),
    (contextual_keyword3, attributed_contextual_keyword3)(A, B, C),
    ContextualKeyword
);

impl<Output: 'static> Rule<Output> {
    pub fn keyword_prefixn<P, E>(
        syntax_kind: SyntaxKind,
        expected: tokenize::Keyword,
        parse_prefix: fn() -> Rule<P>,
        parse_element: fn() -> Rule<E>,
        output: impl Fn(
            &mut Parser<'_>,
            Location,
            WithInfo<P>,
            Vec<WithInfo<E>>,
            &Rc<ParseStack>,
        ) -> WithInfo<Output>
        + Clone
        + 'static,
    ) -> Rule<Output>
    where
        P: DefaultFromInfo + 'static,
        E: DefaultFromInfo + 'static,
    {
        Rule::nonterminal(
            syntax_kind,
            RuleToRender::List(vec![
                Rc::new(move || RuleToRender::Keyword(expected.to_string())),
                Rc::new(move || parse_prefix().render_nested()),
                Rc::new(move || parse_element().render_nested()),
                Rc::new(|| RuleToRender::Ellipsis),
            ]),
            || true,
            ParseFn::new(
                {
                    let output = output.clone();

                    move |parser, tree, stack, _| {
                        let mut elements = match &tree.item {
                            TokenTree::List(_, elements) => elements.iter(),
                            _ => return None,
                        };

                        if !matches!(elements.next()?, WithInfo {
                                item: TokenTree::Keyword(found),
                                ..
                            } if *found == expected)
                        {
                            return None;
                        }

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
                    let mut elements = match &tree.item {
                        TokenTree::List(_, elements) => elements.iter(),
                        _ => return None,
                    };

                    let info = match elements.next()? {
                        WithInfo {
                            item: TokenTree::Keyword(found),
                            info,
                        } if *found == expected => info,
                        _ => return None,
                    };

                    let prefix = match elements.next() {
                        Some(prefix) => parse_prefix().parse(parser, prefix.as_ref(), stack, None),
                        None => {
                            parser.add_diagnostic(stack.error_expected(
                                WithInfo {
                                    info: info.clone(),
                                    item: parse_prefix().syntax_kind,
                                },
                                Direction::After(SyntaxKind::Keyword(expected)),
                            ));

                            P::default_from_info(info.clone())
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
