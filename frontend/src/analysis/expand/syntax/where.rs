use crate::{
    analysis::expand::{
        operators::OperatorPrecedence, syntax::BuiltinSyntaxVisitor, Context, Expander, Expression,
        ExpressionKind, Operator, ScopeValueKind, Syntax,
    },
    helpers::InternedString,
    parse::Span,
    ScopeId,
};
use async_trait::async_trait;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct WhereSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for WhereSyntax {
    fn name(self) -> &'static str {
        "where"
    }

    fn kind(self, syntax: Syntax) -> ScopeValueKind {
        ScopeValueKind::Operator(Operator {
            precedence: OperatorPrecedence::Where,
            syntax,
        })
    }

    fn pattern(self) -> Vec<Expression> {
        vec![
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Variable(InternedString::new("lhs")),
            },
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Name(InternedString::new(self.name())),
            },
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Variable(InternedString::new("rhs")),
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
        let lhs = vars.remove(&InternedString::new("lhs")).unwrap();
        let rhs = vars.remove(&InternedString::new("rhs")).unwrap();

        let lhs_tys = match &lhs.kind {
            ExpressionKind::List(exprs) => exprs.clone(),
            _ => unreachable!(),
        };

        let rhs_tys = match &rhs.kind {
            ExpressionKind::List(exprs) => exprs.clone(),
            _ => unreachable!(),
        };

        Expression {
            span,
            kind: ExpressionKind::Where((lhs_tys, rhs_tys), (Box::new(lhs), Box::new(rhs))),
        }
    }
}
