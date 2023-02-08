use crate::{
    analysis::ast_v2::{
        builtin::{Syntax, SyntaxRule, SyntaxRules},
        Bound, TypeAnnotation, TypeParameter,
    },
    helpers::InternedString,
    parse::Span,
    Compiler,
};

pub struct InstanceAssignmentPatternSyntax;

#[derive(Debug, Clone)]
pub struct InstanceAssignmentPattern {
    pub parameters: Option<(Vec<TypeParameter>, Vec<Bound>)>,
    pub trait_span: Span,
    pub trait_name: InternedString,
    pub trait_parameters: Vec<TypeAnnotation>,
}

pub struct InstanceAssignmentPatternSyntaxContext<'a> {
    _compiler: &'a Compiler<'a>,
}

impl<'a> Syntax<'a> for InstanceAssignmentPatternSyntax {
    type Context = InstanceAssignmentPatternSyntaxContext<'a>;
    type Body = InstanceAssignmentPattern;

    fn rules() -> SyntaxRules<'a, Self> {
        SyntaxRules::new().with(SyntaxRule::new(|_, exprs| {
            Some(Box::pin(async move {
                todo!();
            }))
        }))
    }
}
