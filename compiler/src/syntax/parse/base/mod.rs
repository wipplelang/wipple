#![allow(missing_docs)]

mod attributed;
mod block;
mod keyword;
mod list;
mod mutate;
mod operator;
mod switch;
mod terminal;

use crate::{
    syntax::{
        parse::{Diagnostic, Direction, SyntaxKind, render::RuleToRender},
        tokenize::TokenTree,
    },
    util::{DefaultFromInfo, WithInfo},
};
use std::{marker::PhantomData, rc::Rc, result::Result};

/// Tracks the parser's progress.
pub struct ParseStack {
    parent: Option<Rc<ParseStack>>,
    current: WithInfo<SyntaxKind>,
}

impl ParseStack {
    pub fn new(root: WithInfo<SyntaxKind>) -> Rc<Self> {
        Rc::new(ParseStack {
            parent: None,
            current: root,
        })
    }

    pub fn push(self: &Rc<Self>, child: WithInfo<SyntaxKind>) -> Rc<Self> {
        // Prevent duplicate stack entries
        if self.current.item == child.item {
            return self.clone();
        }

        Rc::new(ParseStack {
            parent: Some(self.clone()),
            current: child,
        })
    }

    pub fn len(&self) -> usize {
        std::iter::successors(Some(self), |stack| stack.parent.as_deref()).count()
    }

    pub fn error_expected(
        &self,
        expected: WithInfo<SyntaxKind>,
        direction: impl Into<Option<Direction>>,
    ) -> WithInfo<Diagnostic> {
        let mut stack = std::iter::successors(Some(self), |stack| stack.parent.as_deref())
            .map(|stack| stack.current.clone())
            .collect::<Vec<_>>();

        stack.reverse();

        WithInfo {
            info: expected.info,
            item: Diagnostic {
                expected: expected.item,
                direction: direction.into(),
                stack,
            },
        }
    }
}

pub struct Parser<'a> {
    debug: bool,
    diagnostics: Vec<WithInfo<Diagnostic>>,
    _lt: PhantomData<&'a ()>,
}

impl Parser<'_> {
    pub fn new() -> Self {
        Parser {
            debug: false,
            diagnostics: Vec::new(),
            _lt: PhantomData,
        }
    }

    #[allow(unused)]
    #[deprecated(note = "use of `debug`")]
    pub fn debug(mut self) -> Self {
        self.debug = true;
        self
    }

    pub fn add_diagnostic(&mut self, diagnostic: WithInfo<Diagnostic>) {
        self.diagnostics.push(diagnostic);
    }

    pub fn into_diagnostics(self) -> Vec<WithInfo<Diagnostic>> {
        self.diagnostics
    }
}

pub struct ParseFn<Output> {
    try_parse: Rc<
        dyn Fn(
                &mut Parser<'_>,
                WithInfo<&TokenTree<'_>>,
                &Rc<ParseStack>,
                Option<Direction>,
            ) -> Option<Result<WithInfo<Output>, usize>>
            + 'static,
    >,
    parse: Rc<
        dyn Fn(
                &mut Parser<'_>,
                WithInfo<&TokenTree<'_>>,
                &Rc<ParseStack>,
                Option<Direction>,
            ) -> Option<WithInfo<Output>>
            + 'static,
    >,
}

impl<Output> Clone for ParseFn<Output> {
    fn clone(&self) -> Self {
        ParseFn {
            try_parse: self.try_parse.clone(),
            parse: self.parse.clone(),
        }
    }
}

impl<Output> ParseFn<Output> {
    pub fn new(
        try_parse: impl Fn(
            &mut Parser<'_>,
            WithInfo<&TokenTree<'_>>,
            &Rc<ParseStack>,
            Option<Direction>,
        ) -> Option<Result<WithInfo<Output>, usize>>
        + 'static,
        parse: impl Fn(
            &mut Parser<'_>,
            WithInfo<&TokenTree<'_>>,
            &Rc<ParseStack>,
            Option<Direction>,
        ) -> Option<WithInfo<Output>>
        + 'static,
    ) -> Self {
        ParseFn {
            try_parse: Rc::new(try_parse),
            parse: Rc::new(parse),
        }
    }

    pub fn try_parse(
        &self,
        parser: &mut Parser<'_>,
        tree: WithInfo<&TokenTree<'_>>,
        stack: &Rc<ParseStack>,
        direction: Option<Direction>,
    ) -> Option<Result<WithInfo<Output>, usize>> {
        (self.try_parse)(parser, tree, stack, direction)
    }

    pub fn parse(
        &self,
        parser: &mut Parser<'_>,
        tree: WithInfo<&TokenTree<'_>>,
        stack: &Rc<ParseStack>,
        direction: Option<Direction>,
    ) -> Option<WithInfo<Output>> {
        (self.parse)(parser, tree, stack, direction)
    }
}

pub struct Rule<Output> {
    doc: Option<&'static str>,
    syntax_kind: SyntaxKind,
    rendered: RuleToRender,
    backtracks: Rc<dyn Fn() -> bool>,
    parse: ParseFn<Output>,
}

impl<Output> Clone for Rule<Output> {
    fn clone(&self) -> Self {
        Rule {
            doc: self.doc,
            syntax_kind: self.syntax_kind.clone(),
            rendered: self.rendered.clone(),
            backtracks: self.backtracks.clone(),
            parse: self.parse.clone(),
        }
    }
}

impl<Output: 'static> Rule<Output> {
    fn nonterminal(
        syntax_kind: SyntaxKind,
        rendered: RuleToRender,
        backtracks: impl Fn() -> bool + 'static,
        parse: ParseFn<Output>,
    ) -> Self {
        Rule {
            doc: None,
            syntax_kind,
            rendered,
            backtracks: Rc::new(backtracks),
            parse,
        }
    }

    fn terminal(
        syntax_kind: SyntaxKind,
        rendered: RuleToRender,
        backtracks: impl Fn() -> bool + 'static,
        parse: ParseFn<Output>,
    ) -> Self {
        Rule {
            doc: None,
            syntax_kind,
            rendered,
            backtracks: Rc::new(backtracks),
            parse,
        }
    }

    pub fn render(self) -> (&'static str, SyntaxKind, RuleToRender) {
        let doc = self.doc.expect("rule must be named");
        (doc, self.syntax_kind.clone(), self.rendered)
    }

    fn render_nested(&self) -> RuleToRender {
        if self.doc.is_some() {
            RuleToRender::Terminal(self.syntax_kind.clone())
        } else {
            self.rendered.clone()
        }
    }

    #[allow(unused)]
    pub fn todo(syntax_kind: SyntaxKind) -> Self {
        Rule {
            doc: None,
            syntax_kind,
            rendered: RuleToRender::Token("TODO"),
            backtracks: Rc::new(|| true),
            parse: ParseFn::new(|_, _, _, _| todo!(), |_, _, _, _| todo!()),
        }
    }

    pub fn in_list(self) -> Rule<Output>
    where
        Output: DefaultFromInfo,
    {
        Rule {
            doc: self.doc,
            syntax_kind: self.syntax_kind.clone(),
            rendered: RuleToRender::List(vec![Rc::new({
                let rendered = self.rendered.clone();
                move || rendered.clone()
            })]),
            backtracks: self.backtracks.clone(),
            parse: ParseFn::new(
                {
                    let rule = self.clone();

                    move |parser, tree, stack, direction| {
                        let mut elements = match &tree.item {
                            TokenTree::List(_, elements) => elements.iter(),
                            _ => return None,
                        };

                        let output = rule.try_parse(
                            parser,
                            elements.next().map(WithInfo::as_ref)?,
                            stack,
                            direction,
                        )?;

                        if elements.next().is_some() {
                            return Some(Err(stack.len()));
                        }

                        Some(output)
                    }
                },
                move |parser, tree, stack, _| {
                    let mut elements = match &tree.item {
                        TokenTree::List(_, elements) => elements.iter(),
                        _ => return None,
                    };

                    let output =
                        self.parse(parser, elements.next().map(WithInfo::as_ref)?, stack, None);

                    for element in elements {
                        parser.add_diagnostic(stack.error_expected(
                            WithInfo {
                                info: element.info.clone(),
                                item: SyntaxKind::Nothing,
                            },
                            None,
                        ));
                    }

                    Some(output)
                },
            ),
        }
    }

    pub fn unwrap_parentheses(self) -> Rule<Output>
    where
        Output: DefaultFromInfo,
    {
        Rule {
            doc: self.doc,
            syntax_kind: self.syntax_kind.clone(),
            rendered: self.rendered.clone(),
            backtracks: self.backtracks.clone(),
            parse: ParseFn::new(
                {
                    let rule = self.clone();

                    move |parser, mut tree, stack, direction| {
                        while let TokenTree::List(_, elements) = &tree.item {
                            if elements.len() != 1 {
                                break;
                            }

                            tree = elements.iter().next().unwrap().as_ref();
                        }

                        rule.try_parse(parser, tree, stack, direction)
                    }
                },
                move |parser, mut tree, stack, direction| {
                    while let TokenTree::List(_, elements) = &tree.item {
                        if elements.len() != 1 {
                            break;
                        }

                        tree = elements.iter().next().unwrap().as_ref();
                    }

                    Some(self.parse(parser, tree, stack, direction))
                },
            ),
        }
    }

    pub fn map<T>(
        self,
        syntax_kind: SyntaxKind,
        f: impl Fn(WithInfo<Output>) -> T + Clone + 'static,
    ) -> Rule<T>
    where
        Output: DefaultFromInfo,
    {
        Rule {
            doc: self.doc,
            syntax_kind,
            rendered: self.rendered.clone(),
            backtracks: self.backtracks.clone(),
            parse: ParseFn::new(
                {
                    let rule = self.clone();
                    let f = f.clone();

                    move |parser, tree, stack, direction| {
                        let output = rule.try_parse(parser, tree, stack, direction)?;

                        Some(output.map(|output| WithInfo {
                            info: output.info.clone(),
                            item: f(output),
                        }))
                    }
                },
                move |parser, tree, stack, direction| {
                    let output = self.parse(parser, tree, stack, direction);

                    Some(WithInfo {
                        info: output.info.clone(),
                        item: f(output),
                    })
                },
            ),
        }
    }

    pub fn wrapped(self) -> Rule<Option<Output>> {
        Rule {
            doc: self.doc,
            syntax_kind: self.syntax_kind.clone(),
            rendered: self.rendered.clone(),
            backtracks: self.backtracks.clone(),
            parse: ParseFn::new(
                {
                    let rule = self.clone();
                    move |parser, tree, stack, direction| {
                        let output = rule.try_parse(parser, tree, stack, direction)?;
                        Some(output.map(|output| output.map(Some)))
                    }
                },
                move |parser, tree, stack, direction| {
                    Some(self.parse_option(parser, tree, stack, direction))
                },
            ),
        }
    }

    pub fn named(mut self, doc: &'static str) -> Self {
        self.doc = Some(doc);
        self
    }

    pub fn no_backtrack(mut self) -> Self {
        self.backtracks = Rc::new(|| false);
        self
    }
}
