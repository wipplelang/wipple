use crate::{
    analysis::expand::{
        operators::ExpandOperatorsResult,
        syntax::{BuiltinSyntax, BuiltinSyntaxVisitor, SyntaxDefinition, SyntaxRule},
        Context, Expander, Expression, ExpressionKind, Syntax,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::Span,
    ScopeId, TemplateId,
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

    fn pattern(self) -> Vec<Expression> {
        vec![
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Name(InternedString::new(self.name())),
            },
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Variable(InternedString::new("definition")),
            },
        ]
    }

    async fn expand(
        self,
        span: Span,
        _vars: HashMap<InternedString, Expression>,
        _context: Option<Context<'_>>,
        _scope: ScopeId,
        expander: &Expander,
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
            kind: ExpressionKind::error(&expander.compiler),
        }
    }
}

impl SyntaxSyntax {
    pub(crate) async fn try_parse_syntax_definition(
        span: Span,
        mut exprs: Vec<Expression>,
        operator_pattern: Option<(TemplateId, ScopeId)>,
        scope: ScopeId,
        expander: &Expander,
    ) -> Option<SyntaxDefinition> {
        if exprs.len() != 1 {
            expander.report_syntax_error(span, None);
            return None;
        }

        let expr = exprs.pop().unwrap();

        let block = match expr.kind {
            ExpressionKind::Block(statements) => statements,
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
                        mut lhs,
                        rhs,
                    ) => {
                        // Implicitly convert `a b c + d e f` to `(a b c) + (d e f)` in pattern
                        if let ExpressionKind::List(exprs) = &lhs.kind {
                            if let Some((operator_id, scope)) = operator_pattern {
                                if let ExpandOperatorsResult::Operator(
                                    operator_span,
                                    operator_name,
                                    Syntax::Defined(id),
                                    mut pattern_lhs,
                                    mut pattern_rhs,
                                ) = expander.expand_operators(lhs.span, exprs.clone(), scope)
                                {
                                    // If the pattern contains a single expression, flatten it (only
                                    // do this for operator syntax patterns so they can accept
                                    // multiple expressions on either side)
                                    // TODO: Remove this once syntactic support for repeated
                                    // variables is added
                                    for pattern in [&mut pattern_lhs, &mut pattern_rhs] {
                                        let exprs = match &mut pattern.kind {
                                            ExpressionKind::List(exprs) => exprs,
                                            _ => unreachable!(),
                                        };

                                        if exprs.len() == 1 {
                                            *pattern = exprs.pop().unwrap();
                                        }
                                    }

                                    if id == operator_id {
                                        lhs.kind = ExpressionKind::List(vec![
                                            pattern_lhs,
                                            Expression {
                                                span: operator_span,
                                                kind: ExpressionKind::Name(operator_name),
                                            },
                                            pattern_rhs,
                                        ]);
                                    }
                                }
                            }
                        }

                        (lhs, rhs)
                    }
                    _ => {
                        expander.compiler.add_error(
                            "malformed `syntax` rule",
                            vec![Note::primary(statement.expr.span, "expected a function")],
                        );

                        continue;
                    }
                };

            convert_variables(&mut lhs);
            convert_variables(&mut rhs);

            rules.push(SyntaxRule {
                span: statement.span,
                pattern: lhs,
                body: rhs,
            });
        }

        Some(SyntaxDefinition { rules })
    }
}

fn convert_variables(expr: &mut Expression) {
    expr.traverse_mut(|expr| {
        if let ExpressionKind::Name(name) = expr.kind {
            if name.starts_with('\'') {
                expr.kind = ExpressionKind::Variable(name);
            } else if name.starts_with("...") {
                expr.kind = ExpressionKind::RepeatedVariable(name);
            }
        }
    });
}
