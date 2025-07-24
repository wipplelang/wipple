use crate::{
    syntax::{
        parse::{
            SyntaxKind,
            base::{ParseFn, ParseStack, Parser, Rule},
            render::RuleToRender,
        },
        tokenize::TokenTree,
    },
    util::WithInfo,
};
use std::rc::Rc;

impl<Output: 'static> Rule<Output> {
    pub fn match_terminal(
        syntax_kind: SyntaxKind,
        rendered: RuleToRender,
        f: impl Fn(
            &mut Parser<'_>,
            WithInfo<&TokenTree<'_>>,
            &Rc<ParseStack>,
        ) -> Option<WithInfo<Output>>
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
