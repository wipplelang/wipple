use crate::{
    analysis::expand::{
        syntax::BuiltinSyntaxVisitor, Context, Expander, Expression, ExpressionKind,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::Span,
    ScopeId,
};
use async_trait::async_trait;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct RecursionLimitSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for RecursionLimitSyntax {
    fn name(self) -> &'static str {
        "recursion-limit"
    }

    fn pattern(self) -> Vec<Expression> {
        vec![
            Expression {
                span: Span::builtin(),
                scope: None,
                kind: ExpressionKind::Name(None, InternedString::new(self.name())),
            },
            Expression {
                span: Span::builtin(),
                scope: None,
                kind: ExpressionKind::Variable(InternedString::new("limit")),
            },
        ]
    }

    async fn expand(
        self,
        span: Span,
        mut vars: HashMap<InternedString, Expression>,
        context: Option<Context<'_>>,
        scope: ScopeId,
        expander: &Expander<'_, '_>,
    ) -> Expression {
        let file_attributes = match context {
            Some(Context::FileAttributes(attributes)) => attributes,
            _ => {
                expander.compiler.add_error(
                    "`recursion-limit` may only be used as a file attribute",
                    vec![Note::primary(
                        span,
                        "try putting this between `[[` and `]]` brackets",
                    )],
                );

                return Expression {
                    span,
                    scope: Some(scope),
                    kind: ExpressionKind::error(expander.compiler),
                };
            }
        };

        let limit = vars.remove(&InternedString::new("lhs")).unwrap();

        let limit = match (|| {
            if let ExpressionKind::Number(n) = limit.kind {
                if let Ok(n) = n.parse::<usize>() {
                    return Some(n);
                }
            }

            expander.compiler.add_error(
                "`recursion-limit` expects a positive integer",
                vec![Note::primary(limit.span, "this is not an integer")],
            );

            None
        })() {
            Some(n) => n,
            None => {
                return Expression {
                    span,
                    scope: Some(scope),
                    kind: ExpressionKind::error(expander.compiler),
                }
            }
        };

        file_attributes.recursion_limit = Some(limit);

        Expression {
            span,
            scope: Some(scope),
            kind: ExpressionKind::EmptySideEffect,
        }
    }
}
