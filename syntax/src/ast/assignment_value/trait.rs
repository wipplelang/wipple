use crate::{
    ast::{
        assignment_value::AssignmentValueSyntaxContext,
        format::Format,
        syntax::{Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules},
        Type, TypeSyntax, TypeSyntaxContext,
    },
    Driver,
};

#[derive(Debug, Clone)]
pub struct TraitAssignmentValue<D: Driver> {
    pub span: D::Span,
    pub trait_span: D::Span,
    pub ty: Option<Result<Type<D>, SyntaxError<D>>>,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for TraitAssignmentValue<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(TraitAssignmentValue {
            span: Default::default(),
            trait_span: Default::default(),
            ty: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> TraitAssignmentValue<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for TraitAssignmentValue<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(match self.ty {
            Some(ty) => format!("(trait {})", ty?.format()?),
            None => format!("trait"),
        })
    }
}

pub struct TraitAssignmentValueSyntax;

impl<D: Driver> Syntax<D> for TraitAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
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
                        context
                            .ast_builder
                            .driver
                            .syntax_error(span, "`trait` accepts 1 input");

                        Err(context.ast_builder.syntax_error(span))
                    }
                }
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::TRAIT]
}
