use crate::{
    analysis::ast::{
        assignment_value::AssignmentValueSyntaxContext,
        syntax::{Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules},
        TypeBody, TypeBodySyntax, TypeBodySyntaxContext,
    },
    diagnostics::Note,
    parse::Span,
};

#[derive(Debug, Clone)]
pub struct TypeAssignmentValue {
    pub type_span: Span,
    pub body: Option<Result<TypeBody, SyntaxError>>,
}

impl TypeAssignmentValue {
    pub fn span(&self) -> Span {
        match &self.body {
            Some(body) => {
                let body_span = match body {
                    Ok(body) => body.span(),
                    Err(error) => error.span,
                };

                Span::join(self.type_span, body_span)
            }
            None => self.type_span,
        }
    }
}

pub struct TypeAssignmentValueSyntax;

impl Syntax for TypeAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "type",
            |context, span, mut exprs, scope| async move {
                match exprs.len() {
                    0 => Ok(TypeAssignmentValue {
                        type_span: span,
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
                            type_span: span,
                            body: Some(body),
                        }
                        .into())
                    }
                    _ => {
                        context.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(span, "`type` accepts 1 input")],
                        );

                        Err(context.ast_builder.syntax_error(span))
                    }
                }
            },
        ))
    }
}
