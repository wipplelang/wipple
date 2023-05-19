use crate::{
    ast::{
        format::Format,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        type_pattern::TypePatternSyntaxContext,
        Type, TypePattern, TypePatternSyntax, TypeSyntax, TypeSyntaxContext,
    },
    parse, Driver,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct WhereTypePattern<D: Driver> {
    pub span: D::Span,
    pub where_span: D::Span,
    pub pattern: Result<Box<TypePattern<D>>, SyntaxError<D>>,
    pub bounds: Vec<Result<WhereTypePatternBound<D>, SyntaxError<D>>>,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for WhereTypePattern<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(WhereTypePattern {
            span: Default::default(),
            where_span: Default::default(),
            pattern: arbitrary::Arbitrary::arbitrary(u)?,
            bounds: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> WhereTypePattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for WhereTypePattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "({} where {})",
            self.pattern?.format()?,
            self.bounds
                .into_iter()
                .map(|bound| bound?.format())
                .collect::<Result<Vec<_>, _>>()?
                .join(" ")
        ))
    }
}

#[derive(Debug, Clone)]
pub struct WhereTypePatternBound<D: Driver> {
    pub span: D::Span,
    pub trait_span: D::Span,
    pub trait_name: D::InternedString,
    pub trait_scope: D::Scope,
    pub parameters: Vec<Result<Type<D>, SyntaxError<D>>>,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for WhereTypePatternBound<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(WhereTypePatternBound {
            span: Default::default(),
            trait_span: Default::default(),
            trait_name: arbitrary::Arbitrary::arbitrary(u)?,
            trait_scope: Default::default(),
            parameters: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> WhereTypePatternBound<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for WhereTypePatternBound<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "({}{})",
            self.trait_name.as_ref(),
            self.parameters
                .into_iter()
                .map(|param| Ok(format!(" {}", param?.format()?)))
                .collect::<Result<String, _>>()?
        ))
    }
}

pub struct WhereTypePatternSyntax;

impl<D: Driver> Syntax<D> for WhereTypePatternSyntax {
    type Context = TypePatternSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "where",
            OperatorAssociativity::None,
            |context, span, (lhs_span, lhs_exprs), where_span, (rhs_span, rhs_exprs), scope| async move {
                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);

                let pattern = context
                    .ast_builder
                    .build_expr::<TypePatternSyntax>(context.clone(), lhs, scope)
                    .await;

                let bounds = stream::iter(rhs_exprs)
                    .then(|expr| async {
                        match expr.try_into_list_exprs() {
                            Ok((span, mut exprs)) => {
                                let trait_name = match exprs.next() {
                                    Some(expr) => expr,
                                    None => {
                                        context
                                            .ast_builder
                                            .driver
                                            .syntax_error(rhs_span, "expected a trait here");

                                        return Err(context.ast_builder.syntax_error(span));
                                    }
                                };

                                let trait_span = trait_name.span;

                                let (trait_name, trait_scope) = match trait_name.kind {
                                    parse::ExprKind::Name(name, name_scope) => (name, name_scope.unwrap_or(scope)),
                                    _ => {
                                        context
                                            .ast_builder
                                            .driver
                                            .syntax_error(trait_span, "expected a trait here");

                                        return Err(context.ast_builder.syntax_error(trait_span));
                                    }
                                };

                                let parameters = stream::iter(exprs)
                                    .then(|expr| {
                                        context.ast_builder.build_expr::<TypeSyntax>(
                                            TypeSyntaxContext::new(context.ast_builder.clone())
                                                .with_statement_attributes(
                                                    context
                                                        .statement_attributes
                                                        .as_ref()
                                                        .unwrap()
                                                        .clone(),
                                                ),
                                            expr,
                                            scope,
                                        )
                                    })
                                    .collect()
                                    .await;

                                Ok(WhereTypePatternBound {
                                    span,
                                    trait_span,
                                    trait_name,
                                    trait_scope,
                                    parameters,
                                })
                            }
                            Err(expr) => {
                                let (trait_name, trait_scope) = match expr.kind {
                                    parse::ExprKind::Name(name, name_scope) => (name, name_scope.unwrap_or(scope)),
                                    _ => {
                                        context
                                            .ast_builder
                                            .driver
                                            .syntax_error(expr.span, "expected a bound here");

                                        return Err(context.ast_builder.syntax_error(expr.span));
                                    }
                                };

                                Ok(WhereTypePatternBound {
                                    span: expr.span,
                                    trait_span: expr.span,
                                    trait_name,
                                    trait_scope,
                                    parameters: Vec::new(),
                                })
                            }
                        }
                    })
                    .collect()
                    .await;

                Ok(WhereTypePattern {
                    span,
                    where_span,
                    pattern: pattern.map(Box::new),
                    bounds,
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::WHERE]
}
