use crate::{
    analysis::ast_v2::{
        assignment_value::AssignmentValueSyntaxContext,
        syntax::{
            FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        TypeBody, TypeBodySyntax, TypeBodySyntaxContext,
    },
    diagnostics::Note,
};

#[derive(Debug, Clone)]
pub struct TypeAssignmentValue {
    pub body: Option<Result<TypeBody, SyntaxError>>,
}

pub struct TypeAssignmentValueSyntax;

impl Syntax for TypeAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "type",
            |context, span, mut exprs| async move {
                match exprs.len() {
                    0 => Ok(TypeAssignmentValue { body: None }.into()),
                    1 => {
                        let body = context
                            .ast_builder
                            .build_expr::<TypeBodySyntax>(
                                TypeBodySyntaxContext::new(context.ast_builder.clone())
                                    .with_statement_attributes(
                                        context.statement_attributes.as_ref().unwrap().clone(),
                                    ),
                                exprs.pop().unwrap(),
                            )
                            .await;

                        Ok(TypeAssignmentValue { body: Some(body) }.into())
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
