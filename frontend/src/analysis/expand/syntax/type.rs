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
pub struct TypeSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for TypeSyntax {
    fn name(self) -> &'static str {
        "type"
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
                kind: ExpressionKind::RepeatedVariable(InternedString::new("exprs")),
            },
        ]
    }

    async fn expand(
        self,
        span: Span,
        mut vars: HashMap<InternedString, Expression>,
        _context: Option<Context<'_>>,
        scope: ScopeId,
        expander: &Expander<'_, '_>,
    ) -> Expression {
        let mut exprs = match vars.remove(&InternedString::new("exprs")).unwrap().kind {
            ExpressionKind::List(exprs) => exprs,
            _ => unreachable!(),
        };

        if !exprs.is_empty() {
            for expr in exprs.split_off(1) {
                expander.compiler.add_error(
                    "unexpected input to `type`",
                    vec![Note::primary(expr.span, "try removing this")],
                );
            }
        }

        let expr = exprs.pop();

        Expression {
            span,
            scope: Some(scope),
            kind: ExpressionKind::Type(expr.map(Box::new)),
        }
    }
}
