use crate::{
    ast::{
        format::Format,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        syntax_rule::SyntaxRuleSyntaxContext,
        SyntaxPattern, SyntaxPatternSyntax, SyntaxPatternSyntaxContext,
    },
    parse, Driver,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct FunctionSyntaxRule<D: Driver> {
    pub span: D::Span,
    pub arrow_span: D::Span,
    pub pattern: Vec<Result<SyntaxPattern<D>, SyntaxError<D>>>,
    pub body: Result<Box<SyntaxPattern<D>>, SyntaxError<D>>,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for FunctionSyntaxRule<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(FunctionSyntaxRule {
            span: Default::default(),
            arrow_span: Default::default(),
            pattern: arbitrary::Arbitrary::arbitrary(u)?,
            body: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> FunctionSyntaxRule<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for FunctionSyntaxRule<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "{} -> {}",
            self.pattern
                .into_iter()
                .map(|pattern| pattern?.format())
                .collect::<Result<Vec<_>, _>>()?
                .join(" "),
            self.body?.format()?
        ))
    }
}

pub struct FunctionSyntaxRuleSyntax;

impl<D: Driver> Syntax<D> for FunctionSyntaxRuleSyntax {
    type Context = SyntaxRuleSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "->",
            OperatorAssociativity::Right,
            |context, span, (_lhs_span, lhs_exprs), arrow_span, (rhs_span, rhs_exprs), scope| async move {
                let input = stream::iter(lhs_exprs)
                    .then(|expr| {
                        context
                            .ast_builder
                            .build_expr::<SyntaxPatternSyntax>(
                                SyntaxPatternSyntaxContext::new(context.ast_builder.clone())
                                    .with_statement_attributes(
                                        context.statement_attributes.as_ref().unwrap().clone(),
                                    ),
                                expr,
                                scope
                            )
                    })
                    .collect::<Vec<_>>()
                    .await
                    .into_iter()
                    .collect();

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);
                let output = context
                    .ast_builder
                    .build_expr::<SyntaxPatternSyntax>(
                        SyntaxPatternSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        rhs,
                        scope
                    )
                    .await;

                Ok(FunctionSyntaxRule {
                    span,
                    arrow_span,
                    pattern: input,
                    body: output.map(Box::new),
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::FUNCTION]
}