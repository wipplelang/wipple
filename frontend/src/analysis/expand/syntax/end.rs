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
pub struct EndSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for EndSyntax {
    fn name(self) -> &'static str {
        "end"
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
                kind: ExpressionKind::Variable(InternedString::new("expr")),
            },
        ]
    }

    async fn expand(
        self,
        span: Span,
        mut vars: HashMap<InternedString, Expression>,
        _context: Option<Context<'_>>,
        scope: ScopeId,
        _expander: &Expander<'_, '_>,
    ) -> Expression {
        let expr = vars.remove(&InternedString::new("expr")).unwrap();

        Expression {
            span,
            scope: Some(scope),
            kind: ExpressionKind::End(Box::new(expr)),
        }
    }
}
