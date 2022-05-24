use crate::{
    compile::expand, helpers::InternedString, parse::Span, Compiler, FilePath, Loader, VariableId,
};
use rust_decimal::Decimal;

#[derive(Debug, Clone)]
pub struct File {
    pub path: FilePath,
    pub span: Span,
    pub attributes: Vec<FileAttribute>,
    pub dependencies: Vec<Dependency>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct FileAttribute {
    pub span: Span,
    pub name: InternedString,
}

#[derive(Debug, Clone)]
pub struct Dependency {
    pub span: Span,
    pub name: InternedString,
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Type((Span, InternedString), TypeDeclaration),
    Trait((Span, InternedString), TraitDeclaration),
    Constant((Span, InternedString), ConstantDeclaration),
    Instance(Instance),
    Assign(Pattern, Expression),
    Expression(Expression),
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub parameters: Vec<TypeParameter>,
    pub bounds: Vec<Bound>,
    pub trait_span: Span,
    pub trait_name: InternedString,
    pub trait_parameters: Vec<TypeAnnotation>,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub span: Span,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Unit,
    Name(InternedString),
    Number(Decimal),
    Text(InternedString),
    Block(Vec<Statement>),
    List(Vec<Expression>),
    Function(Pattern, Box<Expression>),
    When(Box<Expression>, Vec<Arm>),
    External(InternedString, InternedString, Vec<Expression>),
    Annotate(Box<Expression>, TypeAnnotation),

    FunctionInput,
    Variable(VariableId),
    Member(Box<Expression>, InternedString),
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub span: Span,
    pub pattern: Pattern,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub struct TypeAnnotation {
    pub span: Span,
    pub kind: TypeAnnotationKind,
}

#[derive(Debug, Clone)]
pub enum TypeAnnotationKind {
    Placeholder,
    Unit,
    Named(InternedString, Vec<TypeAnnotation>),
    Function(Box<TypeAnnotation>, Box<TypeAnnotation>),
}

#[derive(Debug, Clone)]
pub struct TypeParameter {
    pub span: Span,
    pub name: InternedString,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub span: Span,
    pub kind: PatternKind,
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    Name(InternedString),
    Wildcard,
    Destructure(Vec<(Span, InternedString)>),
}

#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    pub parameters: Vec<TypeParameter>,
    pub bounds: Vec<Bound>,
    pub kind: TypeKind,
}

#[derive(Debug, Clone)]
pub struct TraitDeclaration {
    pub parameters: Vec<TypeParameter>,
    pub bounds: Vec<Bound>,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Marker,
    Alias(TypeAnnotation),
    Structure(Vec<DataField>),
    Enumeration(Vec<DataVariant>),
}

#[derive(Debug, Clone)]
pub struct DataField {
    pub name: InternedString,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct DataVariant {
    pub name: InternedString,
    pub values: Vec<TypeAnnotation>,
}

#[derive(Debug, Clone)]
pub struct ConstantDeclaration {
    pub parameters: Vec<TypeParameter>,
    pub bounds: Vec<Bound>,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct Bound {
    pub span: Span,
    pub trait_span: Span,
    pub trait_name: InternedString,
    pub parameters: Vec<TypeAnnotation>,
}

impl<L: Loader> Compiler<L> {
    pub fn build_ast(&mut self, file: expand::File) -> File {
        // TODO
        File {
            path: file.path,
            span: file.span,
            attributes: Default::default(),
            dependencies: Default::default(),
            statements: Default::default(),
        }
    }
}
