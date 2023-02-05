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
pub struct OnMismatchSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for OnMismatchSyntax {
    fn name(self) -> &'static str {
        "on-mismatch"
    }

    fn pattern(self) -> Vec<Expression> {
        vec![
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Name(None, InternedString::new(self.name())),
            },
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Variable(InternedString::new("parameter")),
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
        let parameter = vars.remove(&InternedString::new("parameter")).unwrap();
        let message = vars.remove(&InternedString::new("message")).unwrap();
        let expr = vars.remove(&InternedString::new("expr")).unwrap();

        let statement_attributes = match context {
            Some(Context::StatementAttributes(attributes)) => attributes,
            _ => {
                expander.compiler.add_error(
                    "`on-mismatch` may only be used as an attribute",
                    vec![Note::primary(
                        span,
                        "try putting this between `[` and `]` brackets",
                    )],
                );

                return expr;
            }
        };

        let parameter = match parameter.kind {
            ExpressionKind::Name(_, name) => (parameter.span, name),
            _ => {
                expander.compiler.add_error(
                    "`on-mismatch` requires the name of a type parameter for its first input",
                    vec![Note::primary(span, "try removing this")],
                );

                return expr;
            }
        };

        let message = match message.kind {
            ExpressionKind::Text(text) => text,
            _ => {
                expander.compiler.add_error(
                    "`on-mismatch` requires a `Text` value for its second input",
                    vec![Note::primary(span, "try removing this")],
                );

                return expr;
            }
        };

        statement_attributes
            .on_mismatch
            .push_back((Some(parameter), message));

        expr
    }
}
