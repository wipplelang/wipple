use crate::ast::{
    DefaultTypeParameter, DefaultTypeParameterSyntax, DefaultTypeParameterSyntaxContext,
};
use crate::{
    ast::{
        format::Format,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        Type, TypePatternSyntaxContext, TypeSyntax, TypeSyntaxContext,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct DefaultTypePattern<D: Driver> {
    pub span: D::Span,
    pub colon_span: D::Span,
    pub type_parameter: Result<DefaultTypeParameter<D>, SyntaxError<D>>,
    pub ty: Result<Type<D>, SyntaxError<D>>,
}

impl<D: Driver> DefaultTypePattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for DefaultTypePattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "({} : {})",
            self.type_parameter?.format()?,
            self.ty?.format()?
        ))
    }
}

pub struct DefaultTypePatternSyntax;

impl<D: Driver> Syntax<D> for DefaultTypePatternSyntax {
    type Context = TypePatternSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            ":",
            OperatorAssociativity::None,
            |context,
             span,
             (lhs_span, lhs_exprs),
             colon_span,
             (rhs_span, rhs_exprs),
             scope_set| async move {
                let default_type_parameter_context =
                    DefaultTypeParameterSyntaxContext::new(context.ast_builder.clone())
                        .with_statement_attributes(context.statement_attributes.clone().unwrap());

                let type_parameter = context
                    .ast_builder
                    .build_expr::<DefaultTypeParameterSyntax>(
                        default_type_parameter_context,
                        parse::Expr::list_or_expr(lhs_span, lhs_exprs),
                        scope_set.clone(),
                    )
                    .await;

                let type_context = TypeSyntaxContext::new(context.ast_builder.clone())
                    .with_statement_attributes(context.statement_attributes.unwrap());

                let ty = context
                    .ast_builder
                    .build_expr::<TypeSyntax>(
                        type_context,
                        parse::Expr::list_or_expr(rhs_span, rhs_exprs),
                        scope_set,
                    )
                    .await;

                Ok(DefaultTypePattern {
                    span,
                    colon_span,
                    type_parameter,
                    ty,
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::ASSIGN]
}
