use crate::{
    analysis::ast::{
        assignment_value::AssignmentValueSyntaxContext,
        syntax::{Syntax, SyntaxContext, SyntaxRule, SyntaxRules},
        KeywordStatementAttribute, OperatorPrecedenceStatementAttribute, SyntaxBody,
        SyntaxBodySyntax, SyntaxBodySyntaxContext, SyntaxError,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::Span,
};
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub struct SyntaxAssignmentValue {
    pub syntax_span: Span,
    pub name: Option<InternedString>,
    pub body: Result<SyntaxBody, SyntaxError>,
    pub operator_precedence: Option<OperatorPrecedenceStatementAttribute>,
    pub keyword: Option<KeywordStatementAttribute>,
    pub uses: HashSet<Span>,
}

impl SyntaxAssignmentValue {
    pub fn span(&self) -> Span {
        let body_span = match &self.body {
            Ok(body) => body.span(),
            Err(error) => error.span,
        };

        Span::join(self.syntax_span, body_span)
    }
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

                let mut value = SyntaxAssignmentValue {
                    syntax_span: span,
                    name: None,
                    body,
                    operator_precedence: statement_attributes.operator_precedence.clone(),
                    keyword: statement_attributes.keyword.clone(),
                    uses: HashSet::new(),
                };

                if let Some((name, _, scope, did_create_syntax)) = context.assigned_name {
                    value.name = Some(name);
                    context.ast_builder.add_syntax(name, value.clone(), scope);
                    *did_create_syntax.lock() = true;
                }

                Ok(value.into())
            },
        ))
    }
}