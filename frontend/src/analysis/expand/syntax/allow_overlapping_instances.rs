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
pub struct AllowOverlappingInstancesSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for AllowOverlappingInstancesSyntax {
    fn name(self) -> &'static str {
        "allow-overlapping-instances"
    }

    fn pattern(self) -> Vec<Expression> {
        vec![
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Name(InternedString::new(self.name())),
            },
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Variable(InternedString::new("expr")),
            },
        ]
    }

    async fn expand(
        self,
        span: Span,
        mut vars: HashMap<InternedString, Expression>,
        context: Option<Context<'_>>,
        _scope: ScopeId,
        expander: &Expander,
    ) -> Expression {
        let expr = vars.remove(&InternedString::new("expr")).unwrap();

        let statement_attributes = match context {
            Some(Context::StatementAttributes(attributes)) => attributes,
            _ => {
                expander.compiler.add_error(
                    "`allow-overlapping-instances` may only be used as an attribute",
                    vec![Note::primary(
                        span,
                        "try putting this between `[` and `]` brackets",
                    )],
                );

                return expr;
            }
        };

        statement_attributes.allow_overlapping_instances = true;

        expr
    }
}
