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

#[derive(Debug, Clone)]
pub struct SyntaxAssignmentValue<D: Driver> {
    pub span: D::Span,
    pub syntax_span: D::Span,
    pub name: Option<D::InternedString>,
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
            |context, span, syntax_span, mut exprs, scope| async move {
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
                        scope,
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

                if let Some((name, _, scope, did_create_syntax)) = context.assigned_name {
                    value.name = Some(name.clone());

                    context
                        .ast_builder
                        .file
                        .define_syntax(name, scope, value.clone());

                    *did_create_syntax.lock() = true;
                }

                Ok(value.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::SYNTAX]
}
