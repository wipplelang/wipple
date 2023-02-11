use crate::{
    analysis::ast_v2::{
        syntax::{OperatorAssociativity, Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        Type, TypeMemberSyntaxContext,
    },
    helpers::InternedString,
    parse::Span,
};

#[derive(Debug, Clone)]
pub struct FieldTypeMember {
    pub colon_span: Span,
    pub name_span: Span,
    pub name: InternedString,
    pub ty: Result<Type, SyntaxError>,
}

pub struct FieldTypeMemberSyntax;

impl Syntax for FieldTypeMemberSyntax {
    type Context = TypeMemberSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "::",
            OperatorAssociativity::Left,
            |context, (lhs_span, mut lhs_exprs), operator_span, (rhs_span, rhs_exprs)| async move {
                todo!()
            },
        ))
    }
}
