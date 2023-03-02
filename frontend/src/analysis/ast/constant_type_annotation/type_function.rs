use crate::{
    analysis::ast::{
        constant_type_annotation::ConstantTypeAnnotationSyntaxContext,
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        ConstantTypeAnnotation, ConstantTypeAnnotationSyntax, TypePattern, TypePatternSyntax,
        TypePatternSyntaxContext,
    },
    parse::{self, SpanList},
    ScopeId,
};

#[derive(Debug, Clone)]
pub struct TypeFunctionConstantTypeAnnotation {
    pub span: SpanList,
    pub arrow_span: SpanList,
    pub pattern: Result<TypePattern, SyntaxError>,
    pub annotation: Result<Box<ConstantTypeAnnotation>, SyntaxError>,
    pub scope: ScopeId,
}

impl TypeFunctionConstantTypeAnnotation {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct TypeFunctionConstantTypeAnnotationSyntax;

impl Syntax for TypeFunctionConstantTypeAnnotationSyntax {
    type Context = ConstantTypeAnnotationSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "=>",
            OperatorAssociativity::None,
            |context, span, (lhs_span, lhs), arrow_span, (rhs_span, rhs), scope| async move {
                let scope = context.ast_builder.child_scope(scope);

                let lhs = parse::Expr::list_or_expr(lhs_span, lhs);

                let pattern = context
                    .ast_builder
                    .build_expr::<TypePatternSyntax>(
                        TypePatternSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        lhs,
                        scope,
                    )
                    .await;

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs);

                let annotation = context
                    .ast_builder
                    .build_expr::<ConstantTypeAnnotationSyntax>(context.clone(), rhs, scope)
                    .await;

                Ok(TypeFunctionConstantTypeAnnotation {
                    span,
                    arrow_span,
                    pattern,
                    annotation: annotation.map(Box::new),
                    scope,
                }
                .into())
            },
        ))
    }
}
