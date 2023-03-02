use crate::{
    analysis::ast::{
        constant_type_annotation::ConstantTypeAnnotationSyntaxContext,
        r#type::{Type, TypeSyntax, TypeSyntaxContext},
        syntax::{Syntax, SyntaxContext, SyntaxRules},
        ConstantTypeAnnotation,
    },
    parse::SpanList,
};

#[derive(Debug, Clone)]
pub struct TypeConstantTypeAnnotation {
    pub ty: Type,
}

impl TypeConstantTypeAnnotation {
    pub fn span(&self) -> SpanList {
        self.ty.span()
    }
}

impl From<Type> for ConstantTypeAnnotation {
    fn from(ty: Type) -> Self {
        TypeConstantTypeAnnotation { ty }.into()
    }
}

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
            .with_statement_attributes(context.statement_attributes.unwrap())
    }
}
