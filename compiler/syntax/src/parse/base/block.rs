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
use wipple_util::{DefaultFromInfo, WithInfo};

impl<D: Driver, Output: 'static> Rule<D, Output> {
    pub fn block<E>(
        syntax_kind: SyntaxKind,
        parse_statement: fn() -> Rule<D, E>,
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
            RuleToRender::Block(vec![
                Rc::new(move || parse_statement().render_nested()),
                Rc::new(|| RuleToRender::Ellipsis),
            ]),
            || true,
            ParseFn::new(
                {
                    let output = output.clone();

                    move |parser, tree, stack, _| {
                        let statements = match &tree.item {
                            TokenTree::Block(statements) => statements,
                            _ => return None,
                        };

                        let mut result = Vec::with_capacity(statements.len());
                        for statement in statements {
                            match parse_statement().try_parse(
                                parser,
                                statement.as_ref(),
                                stack,
                                None,
                            )? {
                                Ok(statement) => result.push(statement),
                                Err(progress) => return Some(Err(progress)),
                            }
                        }

                        Some(Ok(output(parser, tree.info, result, stack)))
                    }
                },
                move |parser, tree, stack, _| {
                    let statements = match &tree.item {
                        TokenTree::Block(statements) => statements,
                        _ => return None,
                    };

                    let statements = statements
                        .iter()
                        .map(|statement| {
                            parse_statement().parse(parser, statement.as_ref(), stack, None)
                        })
                        .collect();

                    Some(output(parser, tree.info, statements, stack))
                },
            ),
        )
    }
}
