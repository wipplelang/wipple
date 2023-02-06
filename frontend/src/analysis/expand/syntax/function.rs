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

    fn pattern(self) -> Vec<Expression> {
        vec![
            Expression {
                span: Span::builtin(),
                scope: None,
                kind: ExpressionKind::Variable(InternedString::new("lhs")),
            },
            Expression {
                span: Span::builtin(),
                scope: None,
                kind: ExpressionKind::Name(None, InternedString::new(self.name())),
            },
            Expression {
                span: Span::builtin(),
                scope: None,
                kind: ExpressionKind::Variable(InternedString::new("rhs")),
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
        let lhs = vars.remove(&InternedString::new("lhs")).unwrap();
        let rhs = vars.remove(&InternedString::new("rhs")).unwrap();

        let function_scope = expander.child_scope(span, scope);
        let pattern = expander
            .expand_pattern(lhs.clone(), function_scope)
            .await
            .ok()
            .map(|mut pattern| {
                let mut expr = rhs.clone();
                expander.update_scopes_for_pattern(&mut pattern, &mut expr, function_scope);
                (Some(function_scope), pattern, Box::new(expr))
            });

        Expression {
            span,
            scope: Some(scope),
            kind: ExpressionKind::Function(pattern, (Box::new(lhs), Box::new(rhs))),
        }
    }
}
