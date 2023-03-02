use crate::{
    analysis::ast::{
        assignment_value::AssignmentValueSyntaxContext,
        syntax::{Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules},
        TypeBody, TypeBodySyntax, TypeBodySyntaxContext,
    },
    diagnostics::Note,
    parse::SpanList,
};

#[derive(Debug, Clone)]
pub struct TypeAssignmentValue {
    pub span: SpanList,
    pub type_span: SpanList,
    pub body: Option<Result<TypeBody, SyntaxError>>,
}

impl TypeAssignmentValue {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct TypeAssignmentValueSyntax;

impl Syntax for TypeAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
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
