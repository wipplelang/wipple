use crate::{
    analysis::ast::{
        assignment_value::AssignmentValueSyntaxContext,
        syntax::{FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxRule, SyntaxRules},
        KeywordStatementAttribute, OperatorPrecedenceStatementAttribute, SyntaxBody,
        SyntaxBodySyntax, SyntaxBodySyntaxContext, SyntaxError,
    },
    diagnostics::Note,
    parse::Span,
};

#[derive(Debug, Clone)]
pub struct SyntaxAssignmentValue {
    pub syntax_span: Span,
    pub body: Result<SyntaxBody, SyntaxError>,
    pub operator_precedence: Option<OperatorPrecedenceStatementAttribute>,
    pub keyword: Option<KeywordStatementAttribute>,
}

pub struct SyntaxAssignmentValueSyntax;

impl Syntax for SyntaxAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "syntax",
            |context, span, mut exprs, scope| async move {
                if exprs.len() != 1 {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(span, "`syntax` accepts 1 input")],
                    );

                    return Err(context.ast_builder.syntax_error(span));
                }

                let body = context
                    .ast_builder
                    .build_expr::<SyntaxBodySyntax>(
                        SyntaxBodySyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        exprs.pop().unwrap(),
                        scope,
                    )
                    .await;

                let statement_attributes = context.statement_attributes.as_ref().unwrap().lock();

                let value = SyntaxAssignmentValue {
                    syntax_span: span,
                    body,
                    operator_precedence: statement_attributes.operator_precedence.clone(),
                    keyword: statement_attributes.keyword.clone(),
                };

                if let Some((name, span)) = context.assigned_name {
                    context
                        .ast_builder
                        .add_syntax(name, span, value.clone(), scope);
                }

                Ok(value.into())
            },
        ))
    }
}
