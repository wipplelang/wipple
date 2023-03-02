use crate::{
    analysis::ast::{
        assignment_pattern::AssignmentPatternSyntaxContext,
        syntax::{Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules},
        Type, TypeSyntax, TypeSyntaxContext,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::{self, SpanList},
    ScopeId,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct InstanceAssignmentPattern {
    pub span: SpanList,
    pub instance_span: SpanList,
    pub pattern_span: SpanList,
    pub trait_span: SpanList,
    pub trait_name: InternedString,
    pub trait_scope: ScopeId,
    pub trait_parameters: Vec<Result<Type, SyntaxError>>,
}

impl InstanceAssignmentPattern {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct InstanceAssignmentPatternSyntax;

impl Syntax for InstanceAssignmentPatternSyntax {
    type Context = AssignmentPatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "instance",
            |context, span, instance_span, mut exprs, scope| async move {
                if exprs.len() != 1 {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(span, "`instance` accepts 1 input")],
                    );

                    return Err(context.ast_builder.syntax_error(span));
                }

                let input = exprs.pop().unwrap();
                let pattern_span = input.span;

                let (trait_name, trait_span, trait_scope, trait_parameters) =
                    match input.try_into_list_exprs() {
                        Ok((span, mut exprs)) => {
                            let trait_name = match exprs.next() {
                                Some(expr) => expr,
                                None => {
                                    context.ast_builder.compiler.add_error(
                                        "syntax error",
                                        vec![Note::primary(span, "`instance` requires a trait")],
                                    );

                                    return Err(context.ast_builder.syntax_error(span));
                                }
                            };

                            let trait_span = trait_name.span;

                            let (trait_name, trait_scope) = match trait_name.kind {
                                parse::ExprKind::Name(name, name_scope) => {
                                    (name, name_scope.unwrap_or(scope))
                                }
                                _ => {
                                    context.ast_builder.compiler.add_error(
                                        "syntax error",
                                        vec![Note::primary(span, "`instance` requires a trait")],
                                    );

                                    return Err(context.ast_builder.syntax_error(span));
                                }
                            };

                            let context = TypeSyntaxContext::new(context.ast_builder)
                                .with_statement_attributes(context.statement_attributes.unwrap());

                            let params = stream::iter(exprs)
                                .then(|expr| {
                                    context.ast_builder.build_expr::<TypeSyntax>(
                                        context.clone(),
                                        expr,
                                        scope,
                                    )
                                })
                                .collect()
                                .await;

                            (trait_name, trait_span, trait_scope, params)
                        }
                        Err(expr) => match expr.kind {
                            parse::ExprKind::Name(name, name_scope) => {
                                (name, expr.span, name_scope.unwrap_or(scope), Vec::new())
                            }
                            _ => {
                                context.ast_builder.compiler.add_error(
                                    "syntax error",
                                    vec![Note::primary(span, "`instance` requires a trait")],
                                );

                                return Err(context.ast_builder.syntax_error(span));
                            }
                        },
                    };

                Ok(InstanceAssignmentPattern {
                    span,
                    instance_span,
                    pattern_span,
                    trait_span,
                    trait_name,
                    trait_scope,
                    trait_parameters,
                }
                .into())
            },
        ))
    }
}
