use crate::{
    syntax::parse::{
        SyntaxKind,
        base::{ParseFn, Rule},
        render::RuleToRender,
    },
    util::DefaultFromInfo,
};
use std::rc::Rc;

impl<Output: 'static> Rule<Output> {
    pub fn switch<const N: usize>(
        syntax_kind: SyntaxKind,
        alternatives: [fn() -> Rule<Output>; N],
    ) -> Self
    where
        Output: DefaultFromInfo + 'static,
    {
        assert!(!alternatives.is_empty());

        Rule::nonterminal(
            syntax_kind,
            RuleToRender::Switch(
                alternatives
                    .iter()
                    .cloned()
                    .map(|alternative| {
                        Rc::new(move || alternative().render_nested()) as Rc<dyn Fn() -> _>
                    })
                    .collect(),
            ),
            || true,
            ParseFn::new(
                move |parser, tree, stack, direction| {
                    for alternative in alternatives {
                        let alternative = alternative();

                        match alternative.try_parse(
                            parser,
                            tree.as_deref(),
                            stack,
                            direction.clone(),
                        ) {
                            Some(Ok(result)) => return Some(Ok(result)),
                            Some(Err(progress)) if !alternative.backtracks() => {
                                return Some(Err(progress));
                            }
                            _ => continue,
                        }
                    }

                    None
                },
                move |parser, tree, stack, direction| {
                    for alternative in alternatives {
                        let alternative = alternative();

                        match alternative.try_parse(
                            parser,
                            tree.as_deref(),
                            stack,
                            direction.clone(),
                        ) {
                            Some(Ok(result)) => return Some(result),
                            Some(Err(_)) if !alternative.backtracks() => {
                                // Calling `parse` here should produce a diagnostic
                                return Some(alternative.parse(
                                    parser,
                                    tree.as_deref(),
                                    stack,
                                    None,
                                ));
                            }
                            _ => continue,
                        }
                    }

                    // Produce a diagnostic inside the alternative that made the most progress
                    let alternative = alternatives
                        .iter()
                        .filter_map(|alternative| {
                            let alternative = alternative();

                            match alternative.try_parse(
                                parser,
                                tree.as_deref(),
                                stack,
                                direction.clone(),
                            ) {
                                Some(Ok(_)) => panic!("rule was expected to fail"),
                                Some(Err(progress)) => {
                                    Some((alternative, Some(std::cmp::Reverse(progress))))
                                }
                                None => None,
                            }
                        })
                        .min_by_key(|(_, progress)| *progress)
                        .map(|(alternative, _)| alternative)?;

                    // Calling `parse` here should produce a diagnostic
                    Some(alternative.parse(parser, tree.as_deref(), stack, None))
                },
            ),
        )
    }
}
