use crate::{
    parse::{
        base::{ParseFn, ParseStack, Parser, Rule},
        render::RuleToRender,
        SyntaxKind,
    },
    tokenize::TokenTree,
    Driver,
};
use std::rc::Rc;
use wipple_util::WithInfo;

impl<D: Driver, Output: 'static> Rule<D, Output> {
    pub fn match_terminal(
        syntax_kind: SyntaxKind,
        rendered: RuleToRender,
        f: impl Fn(
                &mut Parser<'_, D>,
                WithInfo<D::Info, &TokenTree<'_, D>>,
                &Rc<ParseStack<D>>,
            ) -> Option<WithInfo<D::Info, Output>>
            + Clone
            + 'static,
    ) -> Self {
        Rule::terminal(
            syntax_kind,
            rendered,
            || true,
            ParseFn::new(
                {
                    let f = f.clone();
                    move |parser, tree, stack, _| f(parser, tree, stack).map(Ok)
                },
                move |parser, tree, stack, _| f(parser, tree, stack),
            ),
        )
    }
}
