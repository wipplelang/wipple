use crate::{
    ast::{
        format::Format,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        Type, TypeMemberSyntaxContext, TypeSyntax, TypeSyntaxContext,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct FieldTypeMember<D: Driver> {
    pub span: D::Span,
    pub colon_span: D::Span,
    pub name_span: D::Span,
    pub name: D::InternedString,
    pub ty: Result<Type<D>, SyntaxError<D>>,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for FieldTypeMember<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(FieldTypeMember {
            span: Default::default(),
            colon_span: Default::default(),
            name_span: Default::default(),
            name: arbitrary::Arbitrary::arbitrary(u)?,
            ty: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> FieldTypeMember<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for FieldTypeMember<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!("{} :: {}", self.name.as_ref(), self.ty?.format()?))
    }
}

pub struct FieldTypeMemberSyntax;

impl<D: Driver> Syntax<D> for FieldTypeMemberSyntax {
    type Context = TypeMemberSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "::",
            OperatorAssociativity::Left,
            |context, span, (lhs_span, mut lhs_exprs), colon_span, (rhs_span, rhs_exprs), scope| async move {
                if lhs_exprs.len() != 1 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(lhs_span, "expected name");

                    return Err(context.ast_builder.syntax_error(lhs_span));
                }

                let lhs = lhs_exprs.pop().unwrap();
                let name = match lhs.kind {
                    parse::ExprKind::Name(name, _) => name,
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(lhs_span, "expected name");

                        return Err(context.ast_builder.syntax_error(lhs.span));
                    }
                };

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);
                let ty = context
                    .ast_builder
                    .build_expr::<TypeSyntax>(
                        TypeSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        rhs,
                        scope,
                    )
                    .await;

                Ok(FieldTypeMember {
                    span,
                    colon_span,
                    name_span: lhs.span,
                    name,
                    ty,
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::ANNOTATE]
}
