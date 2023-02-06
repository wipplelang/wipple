use crate::{
    analysis::{
        expand::{
            operators::{ExpandOperatorsResult, OperatorPrecedence},
            syntax::{r#use::UseSyntax, syntax::SyntaxSyntax, BuiltinSyntax, BuiltinSyntaxVisitor},
            Context, Expander, Expression, ExpressionKind, Operator, ScopeValueKind, Syntax,
            SyntaxDeclaration,
        },
        lower::SyntaxDeclarationAttributes,
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
        context: Option<Context<'_>>,
        scope: ScopeId,
        expander: &Expander<'_, '_>,
    ) -> Expression {
        let statement_attributes = match context {
            Some(Context::Statement(attributes)) => attributes,
            _ => {
                expander.compiler.add_error(
                    "`:` may not be nested inside another expression",
                    vec![Note::primary(span, "try making this its own statement")],
                );

                return Expression {
                    span,
                    kind: ExpressionKind::error(expander.compiler),
                };
            }
        };

        let lhs = vars.remove(&InternedString::new("lhs")).unwrap();
        let mut rhs = vars.remove(&InternedString::new("rhs")).unwrap();

        let lhs_exprs = match &lhs.kind {
            ExpressionKind::List(exprs) => exprs,
            _ => unreachable!(),
        };

        let rhs_exprs = match &rhs.kind {
            ExpressionKind::List(exprs) => exprs.clone(),
            _ => unreachable!(),
        };

        if let ExpandOperatorsResult::Syntax(span, _, Syntax::Builtin(syntax), exprs) =
            expander.expand_operators(rhs.span, rhs_exprs, scope)
        {
            match syntax {
                BuiltinSyntax::Use(_) => {
                    if UseSyntax::try_import(Some(lhs_exprs), span, exprs, expander)
                        .await
                        .is_ok()
                    {
                        return Expression {
                            span,
                            kind: ExpressionKind::EmptySideEffect,
                        };
                    }
                }
                BuiltinSyntax::Syntax(_) => {
                    if lhs_exprs.len() == 1 {
                        if let ExpressionKind::Name(_, name) = lhs_exprs.first().unwrap().kind {
                            let (id, syntax_definition) = if let Some(precedence) =
                                statement_attributes.operator_precedence
                            {
                                let id = expander.compiler.new_template_id_in(expander.file);
                                let parse_scope = expander.child_scope(span, scope);

                                expander.set_name(
                                    name,
                                    ScopeValueKind::Operator(Operator {
                                        precedence,
                                        syntax: Syntax::Defined(id),
                                    }),
                                    parse_scope,
                                );

                                let definition = SyntaxSyntax::try_parse_syntax_definition(
                                    span,
                                    exprs,
                                    Some((id, parse_scope)),
                                    scope,
                                    expander,
                                )
                                .await;

                                (Some(id), definition)
                            } else {
                                let definition = SyntaxSyntax::try_parse_syntax_definition(
                                    span, exprs, None, scope, expander,
                                )
                                .await;

                                (None, definition)
                            };

                            if let Some(syntax_definition) = syntax_definition {
                                let attributes = SyntaxDeclarationAttributes {
                                    keyword: statement_attributes.keyword,
                                    help: statement_attributes.help.clone(),
                                };

                                let id = id.unwrap_or_else(|| {
                                    expander.compiler.new_template_id_in(expander.file)
                                });

                                if let Some(precedence) = statement_attributes.operator_precedence {
                                    expander.declarations.lock().operators.insert(
                                        id,
                                        SyntaxDeclaration {
                                            name,
                                            span: lhs.span,
                                            uses: Default::default(),
                                            attributes: attributes.clone(),
                                            value: Operator {
                                                precedence,
                                                syntax: Syntax::Defined(id),
                                            },
                                        },
                                    );
                                }

                                expander.declarations.lock().syntaxes.insert(
                                    id,
                                    SyntaxDeclaration {
                                        name,
                                        span: lhs.span,
                                        uses: Default::default(),
                                        attributes,
                                        value: syntax_definition,
                                    },
                                );

                                expander.set_name(
                                    name,
                                    if let Some(precedence) =
                                        statement_attributes.operator_precedence
                                    {
                                        ScopeValueKind::Operator(Operator {
                                            precedence,
                                            syntax: Syntax::Defined(id),
                                        })
                                    } else {
                                        ScopeValueKind::Syntax(Syntax::Defined(id))
                                    },
                                    scope,
                                );

                                return Expression {
                                    span,
                                    kind: ExpressionKind::EmptySideEffect,
                                };
                            }
                        }

                        expander.compiler.add_error(
                            "`syntax` definitions may only be assigned to a name",
                            vec![Note::primary(
                                lhs.span,
                                "try changing this to be a single name",
                            )],
                        );

                        return Expression {
                            span,
                            kind: ExpressionKind::error(expander.compiler),
                        };
                    }

                    todo!()
                }
                _ => {}
            }
        }

        Expression {
            span,
            kind: match expander.expand_pattern(lhs, scope).await {
                Ok(pattern) => ExpressionKind::AssignToPattern(pattern, Box::new(rhs)),
                Err(lhs) => {
                    let expanded_lhs = expander.expand_completely(lhs.clone(), scope).await;

                    let type_function_scope = expander.child_scope(span, scope);

                    if let Some((params, mut bounds)) = expander
                        .expand_type_function(expanded_lhs.clone(), type_function_scope)
                        .await
                    {
                        Expander::update_scopes_for_type_function(
                            &params,
                            &mut rhs,
                            type_function_scope,
                        );

                        for bound in &mut bounds {
                            for ty in &mut bound.parameters {
                                Expander::update_scopes_for_type_function(
                                    &params,
                                    ty,
                                    type_function_scope,
                                );
                            }
                        }

                        ExpressionKind::Assign(Box::new(expanded_lhs), Box::new(rhs))
                    } else {
                        ExpressionKind::Assign(Box::new(lhs), Box::new(rhs))
                    }
                }
            },
        }
    }
}
