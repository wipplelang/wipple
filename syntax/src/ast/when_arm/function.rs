use crate::{
    ast::{
        format::Format,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        when_arm::WhenArmSyntaxContext,
        Expression, ExpressionSyntax, ExpressionSyntaxContext, WhenPattern, WhenPatternSyntax,
        WhenPatternSyntaxContext,
    },
    parse, Driver, File,
};
use wipple_util::Shared;

#[derive(Debug, Clone)]
pub struct FunctionWhenArm<D: Driver> {
    pub span: D::Span,
    pub arrow_span: D::Span,
    pub pattern: Result<WhenPattern<D>, SyntaxError<D>>,
    pub body: Result<Expression<D>, SyntaxError<D>>,
}

impl<D: Driver> FunctionWhenArm<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for FunctionWhenArm<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "({} -> {})",
            self.pattern?.format()?,
            self.body?.format()?,
        ))
    }
}

pub struct FunctionWhenArmSyntax;

impl<D: Driver> Syntax<D> for FunctionWhenArmSyntax {
    type Context = WhenArmSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "->",
            OperatorAssociativity::Right,
            |context, span, (lhs_span, lhs), arrow_span, (rhs_span, rhs), scope_set| async move {
                let mut scope_set = scope_set.lock().clone();
                scope_set.insert(context.ast_builder.file.make_scope());
                let scope_set = Shared::new(scope_set);

                let lhs = parse::Expr::list_or_expr(lhs_span, lhs);
                let pattern = context
                    .ast_builder
                    .build_expr::<WhenPatternSyntax>(
                        WhenPatternSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        lhs,
                        scope_set.clone(),
                    )
                    .await;

                let rhs = parse::Expr::list(rhs_span, rhs);
                let body = context
                    .ast_builder
                    .build_expr::<ExpressionSyntax>(
                        ExpressionSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        rhs,
                        scope_set,
                    )
                    .await;

                Ok(FunctionWhenArm {
                    span,
                    arrow_span,
                    pattern,
                    body,
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::FUNCTION]
}
