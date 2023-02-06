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
pub struct ExternalSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for ExternalSyntax {
    fn name(self) -> &'static str {
        "external"
    }

    fn pattern(self) -> Vec<Expression> {
        vec![
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Name(None, InternedString::new(self.name())),
            },
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Variable(InternedString::new("namespace")),
            },
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Variable(InternedString::new("identifier")),
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
        _expander: &Expander<'_, '_>,
    ) -> Expression {
        let namespace = vars.remove(&InternedString::new("namespace")).unwrap();
        let identifier = vars.remove(&InternedString::new("identifier")).unwrap();

        let exprs = match vars.remove(&InternedString::new("exprs")).unwrap().kind {
            ExpressionKind::List(exprs) => exprs,
            _ => unreachable!(),
        };

        Expression {
            span,
            kind: ExpressionKind::External(Box::new(namespace), Box::new(identifier), exprs),
        }
    }
}
