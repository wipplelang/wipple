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
pub struct OnUnimplementedSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for OnUnimplementedSyntax {
    fn name(self) -> &'static str {
        "on-unimplemented"
    }

    fn pattern(self) -> Vec<Expression> {
        vec![
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Name(None, InternedString::new(self.name())),
            },
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Variable(InternedString::new("message")),
            },
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Variable(InternedString::new("expr")),
            },
        ]
    }

    async fn expand(
        self,
        span: Span,
        mut vars: HashMap<InternedString, Expression>,
        context: Option<Context<'_>>,
        _scope: ScopeId,
        expander: &Expander<'_, '_>,
    ) -> Expression {
        let message = vars.remove(&InternedString::new("message")).unwrap();
        let expr = vars.remove(&InternedString::new("expr")).unwrap();

        let statement_attributes = match context {
            Some(Context::StatementAttributes(attributes)) => attributes,
            _ => {
                expander.compiler.add_error(
                    "`on-unimplemented` may only be used as an attribute",
                    vec![Note::primary(
                        span,
                        "try putting this between `[` and `]` brackets",
                    )],
                );

                return expr;
            }
        };

        let message = match message.kind {
            ExpressionKind::Text(text) => text,
            _ => {
                expander.compiler.add_error(
                    "`on-unimplemented` requires a `Text` value",
                    vec![Note::primary(span, "try removing this")],
                );

                return expr;
            }
        };

        statement_attributes.on_unimplemented = Some(message);

        expr
    }
}
