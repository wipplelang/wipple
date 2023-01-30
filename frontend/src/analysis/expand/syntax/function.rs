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
pub struct FunctionSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for FunctionSyntax {
    fn name(self) -> &'static str {
        "->"
    }

    fn kind(self, syntax: Syntax) -> ScopeValueKind {
        ScopeValueKind::Operator(Operator {
            precedence: OperatorPrecedence::Function,
            syntax,
        })
    }

    fn pattern(self) -> Expression {
        Expression {
            span: Span::builtin(),
            kind: ExpressionKind::List(vec![
                Expression {
                    span: Span::builtin(),
                    kind: ExpressionKind::Variable(InternedString::new("lhs")),
                },
                Expression {
                    span: Span::builtin(),
                    kind: ExpressionKind::Name(None, InternedString::new(self.name())),
                },
                Expression {
                    span: Span::builtin(),
                    kind: ExpressionKind::Variable(InternedString::new("rhs")),
                },
            ]),
        }
    }

    async fn expand(
        self,
        span: Span,
        mut vars: HashMap<InternedString, Expression>,
        _context: Option<Context<'_>>,
        scope: ScopeId,
        expander: &Expander<'_, '_>,
    ) -> Expression {
        let lhs = vars.remove(&InternedString::new("lhs")).unwrap();
        let rhs = vars.remove(&InternedString::new("rhs")).unwrap();

        Expression {
            span,
            kind: ExpressionKind::Function(
                Some(expander.child_scope(scope)),
                Box::new(lhs),
                Box::new(rhs),
            ),
        }
    }
}
