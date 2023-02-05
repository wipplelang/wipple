use crate::{
    analysis::expand::{
        syntax::BuiltinSyntaxVisitor, Context, Expander, Expression, ExpressionKind,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::Span,
    ScopeId,
};
use async_trait::async_trait;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct SpecializeSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for SpecializeSyntax {
    fn name(self) -> &'static str {
        "specialize"
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
            ]),
        }
    }

    async fn expand(
        self,
        span: Span,
        mut vars: HashMap<InternedString, Expression>,
        context: Option<Context<'_>>,
        _scope: ScopeId,
        expander: &Expander<'_, '_>,
    ) -> Expression {
        let expr = vars.remove(&InternedString::new("expr")).unwrap();

        let statement_attributes = match context {
            Some(Context::StatementAttributes(attributes)) => attributes,
            _ => {
                expander.compiler.add_error(
                    "`specialize` may only be used as an attribute",
                    vec![Note::primary(
                        span,
                        "try putting this between `[` and `]` brackets",
                    )],
                );

                return expr;
            }
        };

        statement_attributes.specialize = true;

        expr
    }
}
