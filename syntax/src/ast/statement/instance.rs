use crate::{
    ast::{
        statement::StatementSyntaxContext,
        syntax::{Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules},
        StatementAttributes, Type, TypeSyntax, TypeSyntaxContext,
    },
    parse, Driver,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct InstanceStatement<D: Driver> {
    pub span: D::Span,
    pub instance_span: D::Span,
    pub pattern_span: D::Span,
    pub trait_span: D::Span,
    pub trait_name: D::InternedString,
    pub trait_scope: D::Scope,
    pub trait_parameters: Vec<Result<Type<D>, SyntaxError<D>>>,
    pub attributes: StatementAttributes<D>,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for InstanceStatement<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(InstanceStatement {
            span: Default::default(),
            instance_span: Default::default(),
            pattern_span: Default::default(),
            trait_span: Default::default(),
            trait_name: arbitrary::Arbitrary::arbitrary(u)?,
            trait_scope: Default::default(),
            trait_parameters: arbitrary::Arbitrary::arbitrary(u)?,
            attributes: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> InstanceStatement<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct InstanceStatementSyntax;

impl<D: Driver> Syntax<D> for InstanceStatementSyntax {
    type Context = StatementSyntaxContext<D>;

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
                                .with_statement_attributes(
                                    context.statement_attributes.as_ref().unwrap().clone(),
                                );

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
