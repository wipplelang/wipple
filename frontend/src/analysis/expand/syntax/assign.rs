use crate::{
    analysis::expand::{
        operators::{ExpandOperatorsResult, OperatorPrecedence},
        syntax::{r#use::UseSyntax, BuiltinSyntax, BuiltinSyntaxVisitor},
        Context, Expander, Expression, ExpressionKind, Operator, ScopeValueKind, Syntax,
    },
    helpers::InternedString,
    parse::Span,
    ScopeId,
};
use async_trait::async_trait;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct AssignSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for AssignSyntax {
    fn name(self) -> &'static str {
        ":"
    }

    fn kind(self, syntax: Syntax) -> ScopeValueKind {
        ScopeValueKind::Operator(Operator {
            precedence: OperatorPrecedence::Assignment,
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

        let lhs_exprs = match &lhs.kind {
            ExpressionKind::List(exprs) => exprs,
            _ => unreachable!(),
        };

        let rhs_exprs = match &rhs.kind {
            ExpressionKind::List(exprs) => exprs.clone(),
            _ => unreachable!(),
        };

        if let ExpandOperatorsResult::Syntax(
            span,
            _,
            Syntax::Builtin(BuiltinSyntax::Use(_)),
            exprs,
        ) = expander.expand_operators(rhs.span, rhs_exprs, scope)
        {
            if UseSyntax::try_import(Some(lhs_exprs), span, exprs, scope, expander)
                .await
                .is_ok()
            {
                return Expression {
                    span,
                    kind: ExpressionKind::EmptySideEffect,
                };
            }
        }

        Expression {
            span,
            kind: ExpressionKind::Assign(Box::new(lhs), Box::new(rhs)),
        }
    }
}
