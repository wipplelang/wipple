use crate::analysis::ast_v2::{
    constant_type_annotation::ConstantTypeAnnotationSyntaxContext,
    r#type::{Type, TypeSyntax, TypeSyntaxContext},
    syntax::{Syntax, SyntaxContext, SyntaxRules},
};

pub type TypeConstantTypeAnnotation = Type;

pub struct TypeConstantTypeAnnotationSyntax;

impl Syntax for TypeConstantTypeAnnotationSyntax {
    type Context = ConstantTypeAnnotationSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().combine(TypeSyntax::rules())
    }
}

impl From<ConstantTypeAnnotationSyntaxContext> for TypeSyntaxContext {
    fn from(context: ConstantTypeAnnotationSyntaxContext) -> Self {
        TypeSyntaxContext::new(context.ast_builder)
    }
}
