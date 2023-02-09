use crate::{
    analysis::ast_v2::{
        builtin::{
            assignment_pattern::AssignmentPatternSyntaxContext,
            syntax::{Syntax, SyntaxRule, SyntaxRules},
        },
        Type,
    },
    helpers::InternedString,
    parse::Span,
};

#[derive(Debug, Clone)]
pub struct InstanceAssignmentPattern {
    pub trait_span: Span,
    pub trait_name: InternedString,
    pub trait_parameters: Vec<Type>,
}

pub struct InstanceAssignmentPatternSyntax;

impl Syntax for InstanceAssignmentPatternSyntax {
    type Context = AssignmentPatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "instance",
            |context, exprs| async move {
                todo!();
            },
        ))
    }
}
