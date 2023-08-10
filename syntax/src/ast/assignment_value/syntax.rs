use crate::{
    ast::{
        assignment_value::AssignmentValueSyntaxContext,
        format::Format,
        syntax::{Syntax, SyntaxContext, SyntaxRule, SyntaxRules},
        KeywordStatementAttribute, OperatorPrecedenceStatementAttribute, StatementAttributes,
        SyntaxBody, SyntaxBodySyntax, SyntaxBodySyntaxContext, SyntaxError,
    },
    Driver, File,
};
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub struct SyntaxAssignmentValue<D: Driver> {
    pub span: D::Span,
    pub syntax_span: D::Span,
    pub name: Option<(D::InternedString, HashSet<D::Scope>)>,
    pub body: Result<SyntaxBody<D>, SyntaxError<D>>,
    pub operator_precedence: Option<OperatorPrecedenceStatementAttribute<D>>,
    pub keyword: Option<KeywordStatementAttribute<D>>,
    pub attributes: StatementAttributes<D>,
    pub uses: Vec<D::Span>,
}

impl<D: Driver> SyntaxAssignmentValue<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for SyntaxAssignmentValue<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!("(syntax {})", self.body?.format()?))
    }
}

pub struct SyntaxAssignmentValueSyntax;

impl<D: Driver> Syntax<D> for SyntaxAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "syntax",
            |context, span, syntax_span, mut exprs, scope_set| async move {
                if exprs.len() != 1 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`syntax` accepts 1 input");

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
                        scope_set,
                    )
                    .await;

                let statement_attributes = context.statement_attributes.as_ref().unwrap().lock();

                let mut value = SyntaxAssignmentValue {
                    span,
                    syntax_span,
                    name: None,
                    body,
                    operator_precedence: statement_attributes.operator_precedence.clone(),
                    keyword: statement_attributes.keyword.clone(),
                    attributes: statement_attributes.clone(),
                    uses: Vec::new(),
                };

                if let Some(assigned_name) = context.assigned_name {
                    value.name =
                        Some((assigned_name.name.clone(), assigned_name.scope_set.clone()));

                    context.ast_builder.file.define_syntax(
                        assigned_name.name,
                        assigned_name.scope_set,
                        value.clone(),
                    );

                    *assigned_name.did_create_syntax.lock() = true;
                }

                Ok(value.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::SYNTAX]
}
