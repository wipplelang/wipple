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
pub struct AnnotateSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for AnnotateSyntax {
    fn name(self) -> &'static str {
        "::"
    }

    fn kind(self, syntax: Syntax) -> ScopeValueKind {
        ScopeValueKind::Operator(Operator {
            precedence: OperatorPrecedence::Annotation,
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
                kind: ExpressionKind::Name(None, InternedString::new(self.name())),
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

        Expression {
            span,
            kind: ExpressionKind::Annotate(Box::new(lhs), Box::new(rhs)),
        }
    }
}
