use crate::{
    analysis::expand::{
        operators::OperatorPrecedence, syntax::BuiltinSyntaxVisitor, Context, Expander, Expression,
        ExpressionKind,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::Span,
    ScopeId,
};
use async_trait::async_trait;
use std::{collections::HashMap, str::FromStr};

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct OperatorSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for OperatorSyntax {
    fn name(self) -> &'static str {
        "operator"
    }

    fn pattern(self) -> Vec<Expression> {
        vec![
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Name(InternedString::new(self.name())),
            },
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Variable(InternedString::new("precedence")),
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
        let precedence = vars.remove(&InternedString::new("precedence")).unwrap();
        let expr = vars.remove(&InternedString::new("expr")).unwrap();

        let statement_attributes = match context {
            Some(Context::StatementAttributes(attributes)) => attributes,
            _ => {
                expander.compiler.add_error(
                    "`operator` may only be used as an attribute",
                    vec![Note::primary(
                        span,
                        "try putting this between `[` and `]` brackets",
                    )],
                );

                return expr;
            }
        };

        let precedence_name = match precedence.kind {
            // TODO: Resolve precedence name in scope
            ExpressionKind::Name(name) => name,
            _ => {
                expander.compiler.add_error(
                    "`operator` expects a name of a precedence",
                    vec![Note::primary(span, "try removing this")],
                );

                return expr;
            }
        };

        let precedence = match OperatorPrecedence::from_str(&precedence_name) {
            Ok(precedence) => precedence,
            Err(_) => {
                expander.compiler.add_error(
                    "invalid operator precedence name",
                    vec![Note::primary(
                        span,
                        "custom precedence names are not yet supported",
                    )],
                );

                return expr;
            }
        };

        statement_attributes.operator_precedence = Some(precedence);

        expr
    }
}
