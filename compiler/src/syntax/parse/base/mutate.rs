use crate::{
    syntax::{
        Location,
        parse::{
            SyntaxKind,
            base::{ParseFn, ParseStack, Parser, Rule},
            render::RuleToRender,
        },
        tokenize::{Keyword, TokenTree},
    },
    util::{DefaultFromInfo, WithInfo},
};
use std::rc::Rc;

impl<Output: 'static> Rule<Output> {
    pub fn mutate<E>(
        syntax_kind: SyntaxKind,
        parse_element: fn() -> Rule<E>,
        output: impl Fn(&mut Parser<'_>, Location, WithInfo<E>, &Rc<ParseStack>) -> WithInfo<Output>
        + Clone
        + 'static,
    ) -> Self
    where
        E: DefaultFromInfo + 'static,
    {
        Rule::terminal(
            syntax_kind,
            RuleToRender::List(vec![
                Rc::new(move || parse_element().render_nested()),
                Rc::new(|| RuleToRender::Keyword(Keyword::Mutate.to_string())),
            ]),
            || true,
            ParseFn::new(
                {
                    let output = output.clone();
                    move |parser, tree, stack, direction| {
                        let element = match &tree.item {
                            TokenTree::Mutate(element) => element,
                            _ => return None,
                        };

                        let element = match parse_element().try_parse(
                            parser,
                            element.as_deref(),
                            stack,
                            direction,
                        )? {
                            Ok(element) => element,
                            Err(progress) => return Some(Err(progress)),
                        };

                        Some(Ok(output(parser, tree.info, element, stack)))
                    }
                },
                move |parser, tree, stack, _| {
                    let element = match &tree.item {
                        TokenTree::Mutate(element) => element,
                        _ => return None,
                    };

                    let element = parse_element().parse(parser, element.as_deref(), stack, None);

                    Some(output(parser, tree.info, element, stack))
                },
            ),
        )
    }
}
