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
pub struct NoStdSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for NoStdSyntax {
    fn name(self) -> &'static str {
        "no-std"
    }

    fn pattern(self) -> Vec<Expression> {
        vec![Expression {
            span: Span::builtin(),
            scope: None,
            kind: ExpressionKind::Name(None, InternedString::new(self.name())),
        }]
    }

    async fn expand(
        self,
        span: Span,
        _vars: HashMap<InternedString, Expression>,
        context: Option<Context<'_>>,
        scope: ScopeId,
        expander: &Expander<'_, '_>,
    ) -> Expression {
        let file_attributes = match context {
            Some(Context::FileAttributes(attributes)) => attributes,
            _ => {
                expander.compiler.add_error(
                    "`no-std` may only be used as a file attribute",
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

        file_attributes.no_std = true;

        Expression {
            span,
            scope: Some(scope),
            kind: ExpressionKind::EmptySideEffect,
        }
    }
}
