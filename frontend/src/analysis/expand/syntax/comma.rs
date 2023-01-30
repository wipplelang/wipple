use crate::{
    analysis::expand::{
        operators::OperatorPrecedence, syntax::BuiltinSyntaxVisitor, Context, Expander, Expression,
        ExpressionKind, Operator, ScopeValueKind, Syntax,
    },
    helpers::InternedString,
    parse::Span, ScopeId,
};
use async_trait::async_trait;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct CommaSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for CommaSyntax {
    fn name(self) -> &'static str {
        ","
    }

    fn kind(self, syntax: Syntax) -> ScopeValueKind {
        ScopeValueKind::Operator(Operator {
            precedence: OperatorPrecedence::Comma,
            syntax,
        })
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
        _expander: &Expander<'_, '_>,
    ) -> Expression {
        let exprs = match vars.remove(&InternedString::new("exprs")).unwrap().kind {
            ExpressionKind::List(exprs) => exprs,
            _ => unreachable!(),
        };

        Expression {
            span,
            kind: ExpressionKind::Tuple(exprs),
        }
    }
}
