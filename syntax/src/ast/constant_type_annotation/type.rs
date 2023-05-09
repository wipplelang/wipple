use crate::{
    ast::{
        constant_type_annotation::ConstantTypeAnnotationSyntaxContext,
        r#type::{Type, TypeSyntax, TypeSyntaxContext},
        syntax::{Syntax, SyntaxContext, SyntaxRules},
        ConstantTypeAnnotation,
    },
    Driver,
};

#[derive(Debug, Clone)]
pub struct TypeConstantTypeAnnotation<D: Driver> {
    pub ty: Type<D>,
}

impl<D: Driver> TypeConstantTypeAnnotation<D> {
    pub fn span(&self) -> D::Span {
        self.ty.span()
    }
}

impl<D: Driver> From<Type<D>> for ConstantTypeAnnotation<D> {
    fn from(ty: Type<D>) -> Self {
        TypeConstantTypeAnnotation { ty }.into()
    }
}

pub struct TypeConstantTypeAnnotationSyntax;

impl<D: Driver> Syntax<D> for TypeConstantTypeAnnotationSyntax {
    type Context = ConstantTypeAnnotationSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().combine(TypeSyntax::rules())
    }
}

impl<D: Driver> From<ConstantTypeAnnotationSyntaxContext<D>> for TypeSyntaxContext<D> {
    fn from(context: ConstantTypeAnnotationSyntaxContext<D>) -> Self {
        TypeSyntaxContext::new(context.ast_builder)
            .with_statement_attributes(context.statement_attributes.unwrap())
    }
}
