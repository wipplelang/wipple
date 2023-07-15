use crate::{
    ast::{
        assignment_pattern::AssignmentPatternSyntaxContext,
        format::Format,
        syntax::{Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules},
        Type, TypeSyntax, TypeSyntaxContext,
    },
    parse, Driver,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct InstanceAssignmentPattern<D: Driver> {
    pub span: D::Span,
    pub instance_span: D::Span,
    pub pattern_span: D::Span,
    pub trait_span: D::Span,
    pub trait_name: D::InternedString,
    pub trait_scope: D::Scope,
    pub trait_parameters: Vec<Result<Type<D>, SyntaxError<D>>>,
}

impl<D: Driver> InstanceAssignmentPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for InstanceAssignmentPattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "(instance ({} {}))",
            self.trait_name.as_ref(),
            self.trait_parameters
                .into_iter()
                .map(|result| result?.format())
                .collect::<Result<Vec<_>, _>>()?
                .join(" ")
        ))
    }
}

pub struct InstanceAssignmentPatternSyntax;

impl<D: Driver> Syntax<D> for InstanceAssignmentPatternSyntax {
    type Context = AssignmentPatternSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "instance",
            |context, span, instance_span, mut exprs, scope| async move {
                if exprs.len() != 1 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`instance` accepts 1 input");

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
                                    context
                                        .ast_builder
                                        .driver
                                        .syntax_error(span, "`instance` requires a trait");

                                    return Err(context.ast_builder.syntax_error(span));
                                }
                            };

                            let trait_span = trait_name.span;

                            let (trait_name, trait_scope) = match trait_name.kind {
                                parse::ExprKind::Name(name, name_scope) => {
                                    (name, name_scope.unwrap_or(scope))
                                }
                                _ => {
                                    context
                                        .ast_builder
                                        .driver
                                        .syntax_error(span, "`instance` requires a trait");

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
                                context
                                    .ast_builder
                                    .driver
                                    .syntax_error(span, "`instance` requires a trait");

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

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::INSTANCE]
}
