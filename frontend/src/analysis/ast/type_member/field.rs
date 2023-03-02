use crate::{
    analysis::ast::{
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        Type, TypeMemberSyntaxContext, TypeSyntax, TypeSyntaxContext,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::{self, SpanList},
};

#[derive(Debug, Clone)]
pub struct FieldTypeMember {
    pub span: SpanList,
    pub colon_span: SpanList,
    pub name_span: SpanList,
    pub name: InternedString,
    pub ty: Result<Type, SyntaxError>,
}

impl FieldTypeMember {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct FieldTypeMemberSyntax;

impl Syntax for FieldTypeMemberSyntax {
    type Context = TypeMemberSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "::",
            OperatorAssociativity::Left,
            |context, span, (lhs_span, mut lhs_exprs), colon_span, (rhs_span, rhs_exprs), scope| async move {
                if lhs_exprs.len() != 1 {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(lhs_span, "expected name")],
                    );

                    return Err(context.ast_builder.syntax_error(lhs_span));
                }

                let lhs = lhs_exprs.pop().unwrap();
                let name = match lhs.kind {
                    parse::ExprKind::Name(name, _) => name,
                    _ => {
                        context.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(lhs_span, "expected name")],
                        );

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
