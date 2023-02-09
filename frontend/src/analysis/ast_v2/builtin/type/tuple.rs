use crate::{
    analysis::ast_v2::builtin::{
        r#type::{Type, TypeSyntaxContext},
        syntax::{OperatorAssociativity, Syntax, SyntaxRule, SyntaxRules},
    },
    parse::Span,
};

#[derive(Debug, Clone)]
pub struct TupleType {
    pub span: Span,
    pub tys: Vec<Type>,
}

pub struct TupleTypeSyntax;

impl Syntax for TupleTypeSyntax {
    type Context = TypeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            ",",
            OperatorAssociativity::Variadic,
            |context, (span, exprs), (_unused_span, unused_exprs)| async move {
                // HACK: All of the expressions are contained in `lhs`. In the
                // future, handle variadic operators specially.
                assert!(unused_exprs.is_empty());

                // let tys = stream::iter(exprs)
                //     .then(|expr| {
                //         context
                //             .builder
                //             .apply_syntax::<TypeSyntax>(context, expr.spa)
                //     })
                //     .collect::<Result<Vec<_>>>()
                //     .await;

                todo!()
            },
        ))
    }
}
