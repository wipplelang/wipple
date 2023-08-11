use crate::{
    ast::{
        format::Format,
        statement::StatementSyntaxContext,
        syntax::{Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules},
        StatementAttributes, Type, TypeSyntax, TypeSyntaxContext,
    },
    parse, Driver,
};
use futures::{stream, StreamExt};
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub struct InstanceStatement<D: Driver> {
    pub span: D::Span,
    pub instance_span: D::Span,
    pub pattern_span: D::Span,
    pub trait_span: D::Span,
    pub trait_name: D::InternedString,
    pub trait_scope: HashSet<D::Scope>,
    pub trait_parameters: Vec<Result<Type<D>, SyntaxError<D>>>,
    pub attributes: StatementAttributes<D>,
}

impl<D: Driver> InstanceStatement<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for InstanceStatement<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "{}instance ({}{})",
            self.attributes.format()?,
            self.trait_name.as_ref(),
            self.trait_parameters
                .into_iter()
                .map(|param| Ok(format!(" {}", param?.format()?)))
                .collect::<Result<String, _>>()?
        ))
    }
}

pub struct InstanceStatementSyntax;

impl<D: Driver> Syntax<D> for InstanceStatementSyntax {
    type Context = StatementSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "instance",
            |context, span, instance_span, mut exprs, scope_set| async move {
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
                                    (name, name_scope.unwrap_or_else(|| scope_set.lock().clone()))
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
                                .with_statement_attributes(
                                    context.statement_attributes.as_ref().unwrap().clone(),
                                );

                            let params = stream::iter(exprs)
                                .then(|expr| {
                                    context.ast_builder.build_expr::<TypeSyntax>(
                                        context.clone(),
                                        expr,
                                        scope_set.clone(),
                                    )
                                })
                                .collect()
                                .await;

                            (trait_name, trait_span, trait_scope, params)
                        }
                        Err(expr) => match expr.kind {
                            parse::ExprKind::Name(name, name_scope) => (
                                name,
                                expr.span,
                                name_scope.unwrap_or_else(|| scope_set.lock().clone()),
                                Vec::new(),
                            ),
                            _ => {
                                context
                                    .ast_builder
                                    .driver
                                    .syntax_error(span, "`instance` requires a trait");

                                return Err(context.ast_builder.syntax_error(span));
                            }
                        },
                    };

                Ok(InstanceStatement {
                    span,
                    instance_span,
                    pattern_span,
                    trait_span,
                    trait_name,
                    trait_scope,
                    trait_parameters,
                    attributes: context.statement_attributes.unwrap().lock().clone(),
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::INSTANCE]
}
