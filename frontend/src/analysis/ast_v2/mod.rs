mod builtin;

use crate::{
    diagnostics::*,
    helpers::{Backtrace, InternedString, Shared},
    parse::{self, Span},
    Compiler, FilePath, ScopeId, TemplateId,
};
use futures::future::BoxFuture;
use std::{
    collections::{BTreeMap, HashMap, HashSet, VecDeque},
    sync::Arc,
};

#[derive(Debug, Clone)]
pub struct File {
    pub path: FilePath,
    pub span: Span,
    pub attributes: FileAttributes,
    pub syntax_declarations: BTreeMap<TemplateId, SyntaxDeclaration>,
    pub root_scope: ScopeId,
    pub scopes: BTreeMap<ScopeId, (Option<Span>, Option<ScopeId>)>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, Default)]
pub struct FileAttributes {
    pub no_std: bool,
    pub recursion_limit: Option<usize>,
}

#[derive(Debug, Clone, Default)]
pub struct StatementAttributes {
    pub language_item: Option<LanguageItem>,
    pub help: VecDeque<InternedString>,
    pub on_unimplemented: Option<InternedString>,
    pub on_mismatch: VecDeque<(Option<TypeParameter>, InternedString)>,
    pub specialize: bool,
    pub allow_overlapping_instances: bool,
    pub operator_precedence: Option<OperatorPrecedence>,
    pub keyword: bool,
}

// TODO: User-defined precedences
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, strum::EnumString)]
pub enum OperatorPrecedence {
    #[strum(serialize = "Cast-Precedence")]
    Cast,
    #[strum(serialize = "Power-Precedence")]
    Power,
    #[strum(serialize = "Multiplication-Precedence")]
    Multiplication,
    #[strum(serialize = "Addition-Precedence")]
    Addition,
    #[strum(serialize = "Comparison-Precedence")]
    Comparison,
    #[strum(serialize = "Conjunction-Precedence")]
    Conjunction,
    #[strum(serialize = "Disjunction-Precedence")]
    Disjunction,
    #[strum(serialize = "Accessor-Precedence")]
    Accessor,
    #[strum(serialize = "Composition-Precedence")]
    Dot,
}

#[derive(Debug, Clone, Copy, strum::EnumString)]
#[strum(serialize_all = "kebab-case")]
pub enum LanguageItem {
    Boolean,
}

#[derive(Debug, Clone)]
pub struct SyntaxDeclaration {
    pub name: InternedString,
    pub span: Span,
    pub uses: HashSet<Span>,
    pub operator: bool,
    pub attributes: SyntaxDeclarationAttributes,
}

#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct SyntaxDeclarationAttributes {
    pub keyword: bool,
    pub help: VecDeque<InternedString>,
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub span: Span,
    pub attributes: StatementAttributes,
    pub kind: StatementKind,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Empty,
    Declaration(Declaration),
    Assign(Pattern, Expression),
    Use((Span, InternedString)),
    Expression(ExpressionKind),
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Type((Span, InternedString), TypeDeclaration),
    Trait((Span, InternedString), TraitDeclaration),
    Constant((Span, InternedString), ConstantDeclaration),
    Instance(Instance),
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub parameters: Option<(Vec<TypeParameter>, Vec<Bound>)>,
    pub trait_span: Span,
    pub trait_name: InternedString,
    pub trait_parameters: Vec<TypeAnnotation>,
    pub value: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub span: Span,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Error(Backtrace),
    Name(InternedString),
    Number(InternedString),
    Text(InternedString),
    Block(Vec<Statement>),
    End(Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Function(Pattern, Box<Expression>),
    When(Box<Expression>, Vec<Arm>),
    External(InternedString, InternedString, Vec<Expression>),
    Annotate(Box<Expression>, TypeAnnotation),
    Tuple(Vec<Expression>),
}

impl ExpressionKind {
    fn error(compiler: &Compiler) -> Self {
        ExpressionKind::Error(compiler.backtrace())
    }
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub span: Span,
    pub attributes: StatementAttributes,
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
    Error,
    Placeholder,
    Named(InternedString, Vec<TypeAnnotation>),
    Function(Box<TypeAnnotation>, Box<TypeAnnotation>),
    Tuple(Vec<TypeAnnotation>),
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub span: Span,
    pub kind: PatternKind,
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    Error(Backtrace),
    Wildcard,
    Number(InternedString),
    Text(InternedString),
    Name(InternedString),
    Destructure(Vec<(InternedString, Pattern)>),
    Variant((Span, InternedString), Vec<Pattern>),
    Annotate(Box<Pattern>, TypeAnnotation),
    Or(Box<Pattern>, Box<Pattern>),
    Where(Box<Pattern>, Box<Expression>),
    Tuple(Vec<Pattern>),
}

impl PatternKind {
    fn error(compiler: &Compiler) -> Self {
        PatternKind::Error(compiler.backtrace())
    }
}

#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    pub parameters: Option<Vec<TypeParameter>>,
    pub kind: TypeKind,
}

#[derive(Debug, Clone)]
pub struct TraitDeclaration {
    pub parameters: Option<Vec<TypeParameter>>,
    pub ty: Option<TypeAnnotation>,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Marker,
    Structure(Vec<DataField>),
    Enumeration(Vec<DataVariant>),
}

#[derive(Debug, Clone)]
pub struct DataField {
    pub name: InternedString,
    pub attributes: StatementAttributes,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct DataVariant {
    pub name: InternedString,
    pub span: Span,
    pub attributes: StatementAttributes,
    pub values: Vec<TypeAnnotation>,
}

#[derive(Debug, Clone)]
pub struct ConstantDeclaration {
    pub name: InternedString,
    pub parameters: Option<(Vec<TypeParameter>, Vec<Bound>)>,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct TypeParameter {
    pub span: Span,
    pub name: InternedString,
}

#[derive(Debug, Clone)]
pub struct Bound {
    pub span: Span,
    pub trait_span: Span,
    pub trait_name: InternedString,
    pub parameters: Vec<TypeAnnotation>,
}

impl<'l> Compiler<'l> {
    pub(crate) fn build_ast_v2(
        &self,
        file: parse::File,
        load: impl for<'a> Fn(&'a Compiler<'l>, Span, FilePath) -> BoxFuture<'a, Option<Arc<File>>>
            + 'static
            + Send
            + Sync,
    ) -> File {
        const GLOBAL_ROOT_SCOPE: ScopeId = ScopeId {
            file: None,
            counter: 0,
        };

        todo!()
    }
}

#[derive(Debug, Clone)]
struct Scope {
    id: ScopeId,
    span: Option<Span>,
    parent: Option<ScopeId>,
    syntaxes: HashMap<InternedString, UserDefinedSyntax>,
}

#[derive(Debug, Clone)]
struct UserDefinedSyntax {
    rules: Vec<UserDefinedSyntaxRule>,
}

#[derive(Debug, Clone)]
struct UserDefinedSyntaxRule {
    span: Span,
    pattern: Expression,
    body: Expression,
}

struct AstBuilder<'a, 'l> {
    file: FilePath,
    compiler: &'a Compiler<'l>,
    dependencies: Shared<HashMap<FilePath, (Arc<File>, Option<HashMap<InternedString, Span>>)>>,
    attributes: Shared<FileAttributes>,
    scopes: Shared<BTreeMap<ScopeId, Scope>>,
    file_scope: Option<ScopeId>,
    load: Arc<
        dyn Fn(&'a Compiler<'l>, Span, FilePath) -> BoxFuture<'a, Option<Arc<File>>> + Send + Sync,
    >,
    expanded: Shared<HashSet<Span>>,
}
