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
pub struct InstanceSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for InstanceSyntax {
    fn name(self) -> &'static str {
        "instance"
    }

    fn pattern(self) -> Expression {
        Expression {
            span: Span::builtin(),
            kind: ExpressionKind::List(vec![
                Expression {
                    span: Span::builtin(),
                    kind: ExpressionKind::Name(None, InternedString::new(self.name())),
                },
                Expression {
                    span: Span::builtin(),
                    kind: ExpressionKind::RepeatedVariable(InternedString::new("exprs")),
                },
            ]),
        }
    }

    async fn expand(
        self,
        span: Span,
        mut vars: HashMap<InternedString, Expression>,
        _context: Option<Context<'_>>,
        _scope: ScopeId,
        expander: &Expander<'_, '_>,
    ) -> Expression {
        let mut exprs = match vars.remove(&InternedString::new("exprs")).unwrap().kind {
            ExpressionKind::List(exprs) => exprs,
            _ => unreachable!(),
        };

        for expr in exprs.split_off(1) {
            expander.compiler.add_error(
                "unexpected input to `instance`",
                vec![Note::primary(expr.span, "try removing this")],
            );
        }

        let expr = match exprs.pop() {
            Some(expr) => expr,
            None => {
                expander.compiler.add_error(
                    "expected 1 input to `instance`",
                    vec![Note::primary(
                        span,
                        "try adding the name of a trait after this",
                    )],
                );

                return Expression {
                    span,
                    kind: ExpressionKind::error(expander.compiler),
                };
            }
        };

        Expression {
            span,
            kind: ExpressionKind::Instance(Box::new(expr)),
        }
    }
}
