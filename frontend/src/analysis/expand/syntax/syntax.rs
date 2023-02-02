use crate::{
    analysis::expand::{
        operators::ExpandOperatorsResult,
        syntax::{BuiltinSyntax, BuiltinSyntaxVisitor, SyntaxDefinition, SyntaxRule},
        Context, Expander, Expression, ExpressionKind, Syntax,
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
pub struct SyntaxSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for SyntaxSyntax {
    fn name(self) -> &'static str {
        "syntax"
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
                    kind: ExpressionKind::Variable(InternedString::new("definition")),
                },
            ]),
        }
    }

    async fn expand(
        self,
        span: Span,
        _vars: HashMap<InternedString, Expression>,
        _context: Option<Context<'_>>,
        _scope: ScopeId,
        expander: &Expander<'_, '_>,
    ) -> Expression {
        // Syntax definitions are handled by the assignment operator

        expander.compiler.add_error(
            "`syntax` definition not allowed here",
            vec![Note::primary(
                span,
                "try assigning this definition to a name",
            )],
        );

        Expression {
            span,
            kind: ExpressionKind::error(expander.compiler),
        }
    }
}

impl SyntaxSyntax {
    pub(crate) async fn try_parse_syntax_definition(
        span: Span,
        mut exprs: Vec<Expression>,
        scope: ScopeId,
        expander: &Expander<'_, '_>,
    ) -> Option<SyntaxDefinition> {
        if exprs.len() != 1 {
            expander.report_syntax_error(span, None);
            return None;
        }

        let expr = exprs.pop().unwrap();

        let block = match expr.kind {
            ExpressionKind::Block(_, statements) => statements,
            _ => {
                expander.report_syntax_error(span, None);
                return None;
            }
        };

        let mut rules = Vec::new();
        for statement in block {
            for attribute in statement.unexpanded_attributes {
                expander.compiler.add_error(
                    "`syntax` rule may not contain attributes",
                    vec![Note::primary(attribute.span, "try removing this")],
                );
            }

            let exprs = match statement.expr.kind {
                ExpressionKind::List(exprs) => exprs,
                _ => {
                    expander.compiler.add_error(
                        "malformed `syntax` rule",
                        vec![Note::primary(statement.expr.span, "expected a function")],
                    );

                    continue;
                }
            };

            let (mut lhs, mut rhs) =
                match expander.expand_operators(statement.expr.span, exprs, scope) {
                    ExpandOperatorsResult::Operator(
                        _,
                        _,
                        Syntax::Builtin(BuiltinSyntax::Function(_)),
                        lhs,
                        rhs,
                    ) => (lhs, rhs),
                    _ => {
                        expander.compiler.add_error(
                            "malformed `syntax` rule",
                            vec![Note::primary(statement.expr.span, "expected a function")],
                        );

                        continue;
                    }
                };

            convert_variables(&mut lhs, scope, expander);
            convert_variables(&mut rhs, scope, expander);

            rules.push(SyntaxRule {
                span: statement.span,
                pattern: lhs,
                body: rhs,
            });
        }

        Some(SyntaxDefinition { rules })
    }
}

fn convert_variables(expr: &mut Expression, inherited_scope: ScopeId, expander: &Expander) {
    // NOTE: Make sure to update this any time a new expression with its own
    // scope is added
    expr.traverse_mut_with(inherited_scope, |expr, inherited_scope| {
        match &mut expr.kind {
            ExpressionKind::Name(scope, name) => {
                assert!(scope.is_none());

                *scope = Some(inherited_scope);

                if name.starts_with('\'') {
                    expr.kind = ExpressionKind::Variable(*name);
                } else if name.starts_with("...") {
                    expr.kind = ExpressionKind::RepeatedVariable(*name);
                }

                inherited_scope
            }
            ExpressionKind::Block(scope, _)
            | ExpressionKind::Function(scope, _, _)
            | ExpressionKind::Type(scope, _)
            | ExpressionKind::Trait(scope, _)
            | ExpressionKind::TypeFunction(scope, _, _) => {
                assert!(scope.is_none());

                let child_scope = expander.child_scope(inherited_scope);
                *scope = Some(child_scope);

                child_scope
            }
            _ => inherited_scope,
        }
    })
}
