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
pub struct TraitSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for TraitSyntax {
    fn name(self) -> &'static str {
        "trait"
    }

    fn pattern(self) -> Vec<Expression> {
        vec![
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Name(None, InternedString::new(self.name())),
            },
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::RepeatedVariable(InternedString::new("exprs")),
            },
        ]
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
                "unexpected input to `trait`",
                vec![Note::primary(expr.span, "try removing this")],
            );
        }

        let expr = exprs.pop();

        Expression {
            span,
            kind: ExpressionKind::Trait(expr.map(Box::new)),
        }
    }
}
