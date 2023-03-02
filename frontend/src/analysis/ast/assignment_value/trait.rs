use crate::{
    analysis::ast::{
        assignment_value::AssignmentValueSyntaxContext,
        syntax::{Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules},
        Type, TypeSyntax, TypeSyntaxContext,
    },
    diagnostics::Note,
    parse::SpanList,
};

#[derive(Debug, Clone)]
pub struct TraitAssignmentValue {
    pub span: SpanList,
    pub trait_span: SpanList,
    pub ty: Option<Result<Type, SyntaxError>>,
}

impl TraitAssignmentValue {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct TraitAssignmentValueSyntax;

impl Syntax for TraitAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "trait",
            |context, span, trait_span, mut exprs, scope| async move {
                match exprs.len() {
                    0 => Ok(TraitAssignmentValue {
                        span,
                        trait_span,
                        ty: None,
                    }
                    .into()),
                    1 => {
                        let ty = context
                            .ast_builder
                            .build_expr::<TypeSyntax>(
                                TypeSyntaxContext::new(context.ast_builder.clone())
                                    .with_statement_attributes(
                                        context.statement_attributes.as_ref().unwrap().clone(),
                                    ),
                                exprs.pop().unwrap(),
                                scope,
                            )
                            .await;

                        Ok(TraitAssignmentValue {
                            span,
                            trait_span,
                            ty: Some(ty),
                        }
                        .into())
                    }
                    _ => {
                        context.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(span, "`trait` accepts 1 input")],
                        );

                        Err(context.ast_builder.syntax_error(span))
                    }
                }
            },
        ))
    }
}
