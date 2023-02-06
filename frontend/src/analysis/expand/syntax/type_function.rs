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
pub struct TypeFunctionSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for TypeFunctionSyntax {
    fn name(self) -> &'static str {
        "=>"
    }

    fn kind(self, syntax: Syntax) -> ScopeValueKind {
        ScopeValueKind::Operator(Operator {
            precedence: OperatorPrecedence::TypeFunction,
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
        let mut rhs = vars.remove(&InternedString::new("rhs")).unwrap();

        let type_function_scope = expander.child_scope(span, scope);

        let (params, bounds) = expander
            .expand_type_function(lhs, type_function_scope)
            .await;

        expander.update_scopes_for_type_function(&params, &mut rhs, type_function_scope);

        Expression {
            span,
            scope: Some(scope),
            kind: ExpressionKind::TypeFunction(
                Some(type_function_scope),
                (params, bounds),
                Box::new(rhs),
            ),
        }
    }
}
