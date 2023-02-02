use crate::{
    analysis::expand::{
        syntax::BuiltinSyntaxVisitor, Context, Expander, Expression, ExpressionKind,
    },
    helpers::InternedString,
    parse::Span,
    ScopeId,
};
use async_trait::async_trait;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct NoStdSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for NoStdSyntax {
    fn name(self) -> &'static str {
        "no-std"
    }

    fn pattern(self) -> Expression {
        Expression {
            span: Span::builtin(),
            kind: ExpressionKind::Name(None, InternedString::new(self.name())),
        }
    }

    async fn expand(
        self,
        span: Span,
        _vars: HashMap<InternedString, Expression>,
        context: Option<Context<'_>>,
        _scope: ScopeId,
        expander: &Expander<'_, '_>,
    ) -> Expression {
        let file_attributes = match context {
            Some(Context::FileAttributes(attributes)) => attributes,
            _ => {
                return Expression {
                    span,
                    kind: ExpressionKind::error(expander.compiler),
                };
            }
        };

        file_attributes.no_std = true;

        Expression {
            span,
            kind: ExpressionKind::EmptySideEffect,
        }
    }
}
