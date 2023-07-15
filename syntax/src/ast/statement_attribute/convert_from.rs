use crate::{
    ast::{
        format::Format,
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxContext, SyntaxRule, SyntaxRules},
        SyntaxError, Type, TypeSyntax, TypeSyntaxContext,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct ConvertFromStatementAttribute<D: Driver> {
    pub span: D::Span,
    pub convert_from_span: D::Span,
    pub ty: Result<Type<D>, SyntaxError<D>>,
    pub replacement: parse::Expr<D>,
}

impl<D: Driver> ConvertFromStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for ConvertFromStatementAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("call `StatementAttributes::format` instead")
    }
}

pub struct ConvertFromStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for ConvertFromStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "convert-from",
            |context, span, convert_from_span, exprs, scope| async move {
                if exprs.len() != 2 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`convert-from` accepts 2 inputs");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let mut exprs = exprs.into_iter();

                let ty = exprs.next().unwrap();
                let ty = context
                    .ast_builder
                    .build_expr::<TypeSyntax>(
                        TypeSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        ty,
                        scope,
                    )
                    .await;

                let replacement = exprs.next().unwrap();

                let attribute = ConvertFromStatementAttribute {
                    span,
                    convert_from_span,
                    ty,
                    replacement,
                };

                context
                    .statement_attributes
                    .unwrap()
                    .lock()
                    .convert_from
                    .push(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}
