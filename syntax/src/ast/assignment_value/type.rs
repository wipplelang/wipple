use crate::{
    ast::{
        assignment_value::AssignmentValueSyntaxContext,
        syntax::{Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules},
        TypeBody, TypeBodySyntax, TypeBodySyntaxContext,
    },
    Driver,
};

#[derive(Debug, Clone)]
pub struct TypeAssignmentValue<D: Driver> {
    pub span: D::Span,
    pub type_span: D::Span,
    pub body: Option<Result<TypeBody<D>, SyntaxError<D>>>,
}

impl<D: Driver> TypeAssignmentValue<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct TypeAssignmentValueSyntax;

impl<D: Driver> Syntax<D> for TypeAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "type",
            |context, span, type_span, mut exprs, scope| async move {
                match exprs.len() {
                    0 => Ok(TypeAssignmentValue {
                        span,
                        type_span,
                        body: None,
                    }
                    .into()),
                    1 => {
                        let body = context
                            .ast_builder
                            .build_expr::<TypeBodySyntax>(
                                TypeBodySyntaxContext::new(context.ast_builder.clone())
                                    .with_statement_attributes(
                                        context.statement_attributes.as_ref().unwrap().clone(),
                                    ),
                                exprs.pop().unwrap(),
                                scope,
                            )
                            .await;

                        Ok(TypeAssignmentValue {
                            span,
                            type_span,
                            body: Some(body),
                        }
                        .into())
                    }
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(span, "`type` accepts 1 input");

                        Err(context.ast_builder.syntax_error(span))
                    }
                }
            },
        ))
    }
}
