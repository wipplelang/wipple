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
    parse::{render::RuleToRender, Diagnostic, Direction, SyntaxKind},
    tokenize::TokenTree,
    Driver,
};
use derivative::Derivative;
use std::rc::Rc;
use std::result::Result;
use wipple_util::{DefaultFromInfo, WithInfo};

/// Tracks the parser's progress.
pub struct ParseStack<D: Driver> {
    parent: Option<Rc<ParseStack<D>>>,
    current: WithInfo<D::Info, SyntaxKind>,
}

impl<D: Driver> ParseStack<D> {
    pub fn new(root: WithInfo<D::Info, SyntaxKind>) -> Rc<Self> {
        Rc::new(ParseStack {
            parent: None,
            current: root,
        })
    }

    pub fn push(self: &Rc<Self>, child: WithInfo<D::Info, SyntaxKind>) -> Rc<Self> {
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
        expected: WithInfo<D::Info, SyntaxKind>,
        direction: impl Into<Option<Direction>>,
    ) -> WithInfo<D::Info, Diagnostic<D>> {
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

pub struct Parser<'a, D: Driver> {
    _driver: &'a D,
    debug: bool,
    diagnostics: Vec<WithInfo<D::Info, Diagnostic<D>>>,
}

impl<'a, D: Driver + 'a> Parser<'a, D> {
    pub fn new(driver: &'a D) -> Self {
        Parser {
            _driver: driver,
            debug: false,
            diagnostics: Vec::new(),
        }
    }

    #[allow(unused)]
    #[deprecated(note = "use of `debug`")]
    pub fn debug(mut self) -> Self {
        self.debug = true;
        self
    }

    pub fn add_diagnostic(&mut self, diagnostic: WithInfo<D::Info, Diagnostic<D>>) {
        self.diagnostics.push(diagnostic);
    }

    pub fn into_diagnostics(self) -> Vec<WithInfo<D::Info, Diagnostic<D>>> {
        self.diagnostics
    }
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct ParseFn<D: Driver, Output> {
    try_parse: Rc<
        dyn Fn(
                &mut Parser<'_, D>,
                WithInfo<D::Info, &TokenTree<'_, D>>,
                &Rc<ParseStack<D>>,
                Option<Direction>,
            ) -> Option<Result<WithInfo<D::Info, Output>, usize>>
            + 'static,
    >,
    parse: Rc<
        dyn Fn(
                &mut Parser<'_, D>,
                WithInfo<D::Info, &TokenTree<'_, D>>,
                &Rc<ParseStack<D>>,
                Option<Direction>,
            ) -> Option<WithInfo<D::Info, Output>>
            + 'static,
    >,
}

impl<D: Driver, Output> ParseFn<D, Output> {
    pub fn new(
        try_parse: impl Fn(
                &mut Parser<'_, D>,
                WithInfo<D::Info, &TokenTree<'_, D>>,
                &Rc<ParseStack<D>>,
                Option<Direction>,
            ) -> Option<Result<WithInfo<D::Info, Output>, usize>>
            + 'static,
        parse: impl Fn(
                &mut Parser<'_, D>,
                WithInfo<D::Info, &TokenTree<'_, D>>,
                &Rc<ParseStack<D>>,
                Option<Direction>,
            ) -> Option<WithInfo<D::Info, Output>>
            + 'static,
    ) -> Self {
        ParseFn {
            try_parse: Rc::new(try_parse),
            parse: Rc::new(parse),
        }
    }

    pub fn try_parse(
        &self,
        parser: &mut Parser<'_, D>,
        tree: WithInfo<D::Info, &TokenTree<'_, D>>,
        stack: &Rc<ParseStack<D>>,
        direction: Option<Direction>,
    ) -> Option<Result<WithInfo<D::Info, Output>, usize>> {
        (self.try_parse)(parser, tree, stack, direction)
    }

    pub fn parse(
        &self,
        parser: &mut Parser<'_, D>,
        tree: WithInfo<D::Info, &TokenTree<'_, D>>,
        stack: &Rc<ParseStack<D>>,
        direction: Option<Direction>,
    ) -> Option<WithInfo<D::Info, Output>> {
        (self.parse)(parser, tree, stack, direction)
    }
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct Rule<D: Driver, Output> {
    doc: Option<&'static str>,
    syntax_kind: SyntaxKind,
    rendered: RuleToRender,
    backtracks: Rc<dyn Fn() -> bool>,
    parse: ParseFn<D, Output>,
}

impl<D: Driver, Output: 'static> Rule<D, Output> {
    fn nonterminal(
        syntax_kind: SyntaxKind,
        rendered: RuleToRender,
        backtracks: impl Fn() -> bool + 'static,
        parse: ParseFn<D, Output>,
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
        parse: ParseFn<D, Output>,
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

    pub fn in_list(self) -> Rule<D, Output>
    where
        Output: DefaultFromInfo<D::Info>,
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

    pub fn unwrap_parentheses(self) -> Rule<D, Output>
    where
        Output: DefaultFromInfo<D::Info>,
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
        f: impl Fn(WithInfo<D::Info, Output>) -> T + Clone + 'static,
    ) -> Rule<D, T>
    where
        Output: DefaultFromInfo<D::Info>,
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

    pub fn wrapped(self) -> Rule<D, Option<Output>> {
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
