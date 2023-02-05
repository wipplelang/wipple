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
pub struct WhenSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for WhenSyntax {
    fn name(self) -> &'static str {
        "when"
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
                    kind: ExpressionKind::Variable(InternedString::new("expr")),
                },
                Expression {
                    span: Span::builtin(),
                    kind: ExpressionKind::Variable(InternedString::new("arms")),
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
        _expander: &Expander<'_, '_>,
    ) -> Expression {
        let expr = vars.remove(&InternedString::new("expr")).unwrap();
        let arms = vars.remove(&InternedString::new("arms")).unwrap();

        Expression {
            span,
            kind: ExpressionKind::When(Box::new(expr), Box::new(arms)),
        }
    }
}
