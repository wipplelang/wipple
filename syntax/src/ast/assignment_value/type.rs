use crate::{
    ast::{
        assignment_value::AssignmentValueSyntaxContext,
        format::Format,
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

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for TypeAssignmentValue<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(TypeAssignmentValue {
            span: Default::default(),
            type_span: Default::default(),
            body: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> TypeAssignmentValue<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for TypeAssignmentValue<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(match self.body {
            Some(body) => format!("(type {})", body?.format()?),
            None => format!("type"),
        })
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

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::TYPE]
}