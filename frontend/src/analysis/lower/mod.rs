mod builtins;

use crate::{
    analysis::{ast, parse, Analysis, ScopeSet, Span, SpanList},
    diagnostics::{Fix, FixRange, Note},
    helpers::{did_you_mean, Backtrace, InternedString, Shared},
    BuiltinSyntaxId, BuiltinTypeId, Compiler, ConstantId, ExpressionId, FieldIndex, FilePath,
    ScopeId, SyntaxId, TraitId, TypeId, TypeParameterId, VariableId, VariantIndex,
};
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, HashMap, VecDeque},
    hash::Hash,
    mem,
    sync::Arc,
};

#[derive(Debug, Clone)]
pub struct File<Decls = Declarations> {
    pub span: SpanList,
    pub attributes: BTreeMap<FilePath, FileAttributes>,
    pub declarations: Decls,
    pub info: FileInfo,
    pub specializations: BTreeMap<ConstantId, Vec<ConstantId>>,
    pub statements: Vec<Expression>,
    pub root_scope: ScopeSet,
    pub exported: im::HashMap<InternedString, Vec<(ScopeSet, AnyDeclaration)>>,
}

#[derive(Debug, Clone, Default)]
#[non_exhaustive]
pub struct FileAttributes {
    pub imported_by: Shared<Vec<FilePath>>,
    pub help_url: Option<InternedString>,
}

#[derive(Debug, Clone, Default)]
pub struct Declarations {
    pub syntaxes: im::OrdMap<SyntaxId, Declaration<SyntaxDeclaration>>,
    pub types: im::OrdMap<TypeId, Declaration<TypeDeclaration>>,
    pub type_parameters: im::OrdMap<TypeParameterId, Declaration<TypeParameterDeclaration>>,
    pub traits: im::OrdMap<TraitId, Declaration<TraitDeclaration>>,
    pub builtin_types: im::OrdMap<BuiltinTypeId, Declaration<BuiltinTypeDeclaration>>,
    pub constants: im::OrdMap<ConstantId, Declaration<ConstantDeclaration>>,
    pub instances: im::OrdMap<ConstantId, Declaration<InstanceDeclaration>>,
    pub variables: im::OrdMap<VariableId, Declaration<VariableDeclaration>>,
    pub builtin_syntaxes: im::OrdMap<BuiltinSyntaxId, Declaration<BuiltinSyntaxDeclaration>>,
}

#[derive(Debug, Clone, Default)]
struct UnresolvedDeclarations {
    syntaxes: im::OrdMap<SyntaxId, Declaration<SyntaxDeclaration>>,
    types: im::OrdMap<TypeId, Declaration<Option<TypeDeclaration>>>,
    type_parameters: im::OrdMap<TypeParameterId, Declaration<TypeParameterDeclaration>>,
    traits: im::OrdMap<TraitId, Declaration<Option<TraitDeclaration>>>,
    builtin_types: im::OrdMap<BuiltinTypeId, Declaration<BuiltinTypeDeclaration>>,
    constants: im::OrdMap<ConstantId, Declaration<Option<UnresolvedConstantDeclaration>>>,
    instances: im::OrdMap<ConstantId, Declaration<Option<UnresolvedInstanceDeclaration>>>,
    variables: im::OrdMap<VariableId, Declaration<VariableDeclaration>>,
    builtin_syntaxes: im::OrdMap<BuiltinSyntaxId, Declaration<BuiltinSyntaxDeclaration>>,
}

impl UnresolvedDeclarations {
    fn resolve(self) -> Declarations {
        Declarations {
            syntaxes: self
                .syntaxes
                .into_iter()
                .map(|(id, decl)| (id, decl.resolve()))
                .collect(),
            types: self
                .types
                .into_iter()
                .map(|(id, decl)| (id, decl.resolve()))
                .collect(),
            type_parameters: self.type_parameters,
            traits: self
                .traits
                .into_iter()
                .map(|(id, decl)| (id, decl.resolve()))
                .collect(),
            builtin_types: self.builtin_types,
            constants: self
                .constants
                .into_iter()
                .map(|(id, decl)| (id, decl.resolve()))
                .collect(),
            instances: self
                .instances
                .into_iter()
                .map(|(id, decl)| (id, decl.resolve()))
                .collect(),
            variables: self.variables,
            builtin_syntaxes: self.builtin_syntaxes,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Declaration<T> {
    pub name: Option<InternedString>,
    pub span: SpanList,
    pub uses: im::HashSet<SpanList>,
    pub value: T,
    imported: bool,
}

trait Resolve<T> {
    fn resolve(self) -> T;
}

impl<T> Resolve<T> for T {
    fn resolve(self) -> T {
        self
    }
}

impl<T> Declaration<Option<T>> {
    fn unresolved(name: Option<InternedString>, span: impl Into<SpanList>) -> Self {
        Declaration {
            name,
            span: span.into(),
            uses: im::HashSet::new(),
            value: None,
            imported: false,
        }
    }

    fn resolve<U>(self) -> Declaration<U>
    where
        T: Resolve<U>,
    {
        Declaration {
            name: self.name,
            span: self.span,
            uses: self.uses,
            value: self
                .value
                .unwrap_or_else(|| {
                    panic!("unresolved declaration: {:?} @ {:?}", self.name, self.span)
                })
                .resolve(),
            imported: false,
        }
    }
}

impl<T> Declaration<T> {
    fn resolved(name: Option<InternedString>, span: impl Into<SpanList>, value: T) -> Self {
        Declaration {
            name,
            span: span.into(),
            uses: im::HashSet::new(),
            value,
            imported: false,
        }
    }

    fn make_unresolved<U>(self) -> Declaration<Option<U>>
    where
        U: From<T>,
    {
        Declaration {
            name: self.name,
            span: self.span,
            uses: self.uses,
            value: Some(self.value.into()),
            imported: self.imported,
        }
    }
}

#[derive(Debug, Clone, Default)]
#[non_exhaustive]
pub struct DeclarationAttributes {
    pub help: Vec<InternedString>,
    pub help_group: Option<InternedString>,
    pub help_playground: Option<InternedString>,
    pub help_template: Option<InternedString>,
    pub private: bool,
}

#[derive(Debug, Clone, Default)]
pub struct SyntaxDeclaration {
    pub operator: bool,
    pub keyword: bool,
    pub attributes: SyntaxAttributes,
}

#[derive(Debug, Clone, Default)]
#[non_exhaustive]
pub struct SyntaxAttributes {
    pub decl_attributes: DeclarationAttributes,
}

#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    pub parameters: Vec<TypeParameterId>,
    pub kind: TypeDeclarationKind,
    pub attributes: TypeAttributes,
}

#[derive(Debug, Clone)]
pub enum TypeDeclarationKind {
    Marker,
    Structure(Vec<StructureField>, HashMap<InternedString, FieldIndex>),
    Enumeration(
        Vec<EnumerationVariant>,
        HashMap<InternedString, VariantIndex>,
    ),
    Alias(TypeAnnotation),
}

#[derive(Debug, Clone, Default)]
#[non_exhaustive]
pub struct TypeAttributes {
    pub decl_attributes: DeclarationAttributes,
    pub on_mismatch: Vec<(Option<TypeParameterId>, InternedString)>,
    pub convert_from: Vec<(TypeAnnotation, wipple_syntax::parse::Expr<Analysis>)>,
    pub no_reuse: Option<Option<InternedString>>,
}

#[derive(Debug, Clone)]
pub struct StructureField {
    pub name_span: SpanList,
    pub name: InternedString,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct EnumerationVariant {
    pub name_span: SpanList,
    pub name: InternedString,
    pub name_scope: ScopeSet,
    pub tys: Vec<TypeAnnotation>,
    pub constructor: ConstantId,
}

#[derive(Debug, Clone)]
pub struct TypeParameterDeclaration {
    pub infer: bool,
    pub default: Option<TypeAnnotation>,
}

#[derive(Debug, Clone)]
pub struct TraitDeclaration {
    pub parameters: Vec<TypeParameterId>,
    pub ty: Option<TypeAnnotation>,
    pub attributes: TraitAttributes,
}

#[derive(Debug, Clone, Default)]
#[non_exhaustive]
pub struct TraitAttributes {
    pub decl_attributes: DeclarationAttributes,
    pub on_unimplemented: Option<(
        Vec<(InternedString, TypeParameterId)>,
        Option<InternedString>,
    )>,
    pub sealed: bool,
    pub allow_overlapping_instances: bool,
    pub derive: Option<(SpanList, InternedString, InternedString)>,
}

#[derive(Debug, Clone)]
pub struct BuiltinTypeDeclaration {
    pub kind: BuiltinTypeDeclarationKind,
    pub attributes: DeclarationAttributes,
}

#[derive(Debug, Clone)]
pub enum BuiltinTypeDeclarationKind {
    Number,
    Integer,
    Natural,
    Byte,
    Signed,
    Unsigned,
    Float,
    Double,
    Text,
    List,
    Mutable,
    Ui,
    TaskGroup,
}

#[derive(Debug, Clone)]
pub struct ConstantDeclaration {
    pub parameters: Vec<TypeParameterId>,
    pub bounds: Vec<Bound>,
    pub ty: TypeAnnotation,
    pub value: Expression,
    pub is_variant: bool,
    pub attributes: ConstantAttributes,
}

#[derive(Debug, Clone)]
pub struct UnresolvedConstantDeclaration {
    pub parameters: Vec<TypeParameterId>,
    pub bounds: Vec<Bound>,
    pub ty: TypeAnnotation,
    pub value: Shared<Option<Expression>>,
    pub is_variant: bool,
    pub attributes: ConstantAttributes,
}

impl From<ConstantDeclaration> for UnresolvedConstantDeclaration {
    fn from(decl: ConstantDeclaration) -> Self {
        UnresolvedConstantDeclaration {
            parameters: decl.parameters,
            bounds: decl.bounds,
            ty: decl.ty,
            value: Shared::new(Some(decl.value)),
            is_variant: decl.is_variant,
            attributes: decl.attributes,
        }
    }
}

impl Resolve<ConstantDeclaration> for UnresolvedConstantDeclaration {
    fn resolve(self) -> ConstantDeclaration {
        ConstantDeclaration {
            parameters: self.parameters,
            bounds: self.bounds,
            ty: self.ty,
            value: self
                .value
                .try_into_unique()
                .expect("uninitialized constant"),
            is_variant: self.is_variant,
            attributes: self.attributes,
        }
    }
}

#[derive(Debug, Clone, Default)]
#[non_exhaustive]
pub struct ConstantAttributes {
    pub decl_attributes: DeclarationAttributes,
    pub is_specialization: bool,
    pub is_contextual: bool,
}

#[derive(Debug, Clone)]
pub struct UnresolvedInstanceDeclaration {
    pub parameters: Vec<TypeParameterId>,
    pub bounds: Vec<Bound>,
    pub tr_span: SpanList,
    pub tr: TraitId,
    pub tr_parameters: Vec<TypeAnnotation>,
    pub value: Shared<Option<InstanceValue>>,
}

#[derive(Debug, Clone)]
pub struct InstanceDeclaration {
    pub parameters: Vec<TypeParameterId>,
    pub bounds: Vec<Bound>,
    pub tr_span: SpanList,
    pub tr: TraitId,
    pub tr_parameters: Vec<TypeAnnotation>,
    pub value: Option<InstanceValue>,
}

impl From<InstanceDeclaration> for UnresolvedInstanceDeclaration {
    fn from(decl: InstanceDeclaration) -> Self {
        UnresolvedInstanceDeclaration {
            parameters: decl.parameters,
            bounds: decl.bounds,
            tr_span: decl.tr_span,
            tr: decl.tr,
            tr_parameters: decl.tr_parameters,
            value: Shared::new(decl.value),
        }
    }
}

impl Resolve<InstanceDeclaration> for UnresolvedInstanceDeclaration {
    fn resolve(self) -> InstanceDeclaration {
        InstanceDeclaration {
            parameters: self.parameters,
            bounds: self.bounds,
            tr_span: self.tr_span,
            tr: self.tr,
            tr_parameters: self.tr_parameters,
            value: self.value.try_into_unique(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct InstanceValue {
    pub colon_span: Option<SpanList>,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration;

#[derive(Debug, Clone)]
pub struct BuiltinSyntaxDeclaration {
    pub definition: wipple_syntax::ast::BuiltinSyntaxDefinition,
}

#[derive(Debug, Clone, Default)]
pub struct FileInfo {
    pub recursion_limit: Option<usize>,
    pub language_items: LanguageItems,
    pub entrypoint: Option<ConstantId>,
    pub diagnostic_items: DiagnosticItems,
    pub diagnostic_aliases: DiagnosticAliases,
}

impl FileInfo {
    fn merge(&mut self, other: FileInfo) {
        self.recursion_limit = match (self.recursion_limit, other.recursion_limit) {
            (None, None) => None,
            (None, Some(limit)) | (Some(limit), None) => Some(limit),
            (Some(limit), Some(other)) => Some(limit.max(other)),
        };

        self.language_items.merge(other.language_items);

        self.entrypoint = other.entrypoint.or(self.entrypoint);

        self.diagnostic_items.merge(other.diagnostic_items);

        self.diagnostic_aliases.merge(other.diagnostic_aliases);
    }
}

#[derive(Debug, Clone, Default)]
#[non_exhaustive]
pub struct LanguageItems {
    pub boolean: Option<TypeId>,
    pub show: Option<TraitId>,
}

impl LanguageItems {
    fn merge(&mut self, other: LanguageItems) {
        if let Some(boolean) = other.boolean {
            self.boolean.get_or_insert(boolean);
        }

        if let Some(show) = other.show {
            self.show.get_or_insert(show);
        }
    }
}

#[derive(Debug, Clone, Default)]
#[non_exhaustive]
pub struct DiagnosticItems {
    pub accepts_text: Vec<ConstantId>,
    pub collection_elements: Vec<ConstantId>,
}

impl DiagnosticItems {
    fn merge(&mut self, other: DiagnosticItems) {
        self.accepts_text.extend(other.accepts_text);
        self.collection_elements.extend(other.collection_elements);
    }
}

#[derive(Debug, Clone, Default)]
#[non_exhaustive]
pub struct DiagnosticAliases {
    pub aliases: BTreeMap<InternedString, InternedString>,
}

impl DiagnosticAliases {
    fn merge(&mut self, other: DiagnosticAliases) {
        self.aliases.extend(other.aliases);
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct Expression {
    pub id: ExpressionId,
    pub span: SpanList,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone, Deserialize)]
pub enum ExpressionKind {
    Error(#[serde(skip)] Backtrace),
    Marker(TypeId),
    Constant(ConstantId),
    Trait(TraitId),
    Variable(VariableId),
    Text(InternedString),
    Number(InternedString),
    Block(Vec<Expression>, bool),
    Call(Box<Expression>, Box<Expression>, bool),
    Function(Pattern, Box<Expression>, CaptureList),
    When(Box<Expression>, Vec<Arm>),
    External(InternedString, InternedString, Vec<Expression>),
    Intrinsic(Intrinsic, Vec<Expression>),
    Plugin(InternedString, InternedString, Vec<Expression>),
    Annotate(Box<Expression>, TypeAnnotation),
    Initialize(Pattern, Box<Expression>),
    Instantiate(TypeId, Vec<((SpanList, InternedString), Expression)>),
    Variant(TypeId, VariantIndex, Vec<Expression>),
    Tuple(Vec<Expression>),
    Format(Vec<(InternedString, Expression)>, Option<InternedString>),
    With((Option<ConstantId>, Box<Expression>), Box<Expression>),
    End(Box<Expression>),
    Extend(
        Box<Expression>,
        Vec<((SpanList, InternedString), Expression)>,
    ),
    Semantics(Semantics, Box<Expression>),
}

impl ExpressionKind {
    pub(crate) fn error(compiler: &Compiler) -> Self {
        ExpressionKind::Error(compiler.backtrace())
    }
}

impl Expression {
    fn traverse_mut(&mut self, mut f: impl FnMut(&mut Self)) {
        self.traverse_mut_inner(&mut f);
    }

    fn traverse_mut_inner(&mut self, f: &mut impl FnMut(&mut Self)) {
        f(self);

        match &mut self.kind {
            ExpressionKind::Error(_)
            | ExpressionKind::Marker(_)
            | ExpressionKind::Constant(_)
            | ExpressionKind::Trait(_)
            | ExpressionKind::Variable(_)
            | ExpressionKind::Text(_)
            | ExpressionKind::Number(_)
            | ExpressionKind::Plugin(_, _, _) => {}
            ExpressionKind::Block(statements, _) => {
                for statement in statements {
                    statement.traverse_mut_inner(f);
                }
            }
            ExpressionKind::Call(func, input, _) => {
                func.traverse_mut_inner(f);
                input.traverse_mut_inner(f);
            }
            ExpressionKind::Function(_, body, _) => {
                body.traverse_mut_inner(f);
            }
            ExpressionKind::When(input, arms) => {
                input.traverse_mut_inner(f);

                for arm in arms {
                    arm.body.traverse_mut_inner(f);
                }
            }
            ExpressionKind::External(_, _, exprs) | ExpressionKind::Intrinsic(_, exprs) => {
                for expr in exprs {
                    expr.traverse_mut_inner(f);
                }
            }
            ExpressionKind::Annotate(expr, _) => {
                expr.traverse_mut_inner(f);
            }
            ExpressionKind::Initialize(_, expr) => {
                expr.traverse_mut_inner(f);
            }
            ExpressionKind::Instantiate(_, fields) => {
                for (_, expr) in fields {
                    expr.traverse_mut_inner(f);
                }
            }
            ExpressionKind::Variant(_, _, exprs) => {
                for expr in exprs {
                    expr.traverse_mut_inner(f);
                }
            }
            ExpressionKind::Tuple(exprs) => {
                for expr in exprs {
                    expr.traverse_mut_inner(f);
                }
            }
            ExpressionKind::Format(segments, _) => {
                for (_, expr) in segments {
                    expr.traverse_mut_inner(f);
                }
            }
            ExpressionKind::With((_, value), body) => {
                value.traverse_mut_inner(f);
                body.traverse_mut_inner(f);
            }
            ExpressionKind::End(expr) => {
                expr.traverse_mut_inner(f);
            }
            ExpressionKind::Extend(expr, fields) => {
                expr.traverse_mut_inner(f);

                for (_, expr) in fields {
                    expr.traverse_mut_inner(f);
                }
            }
            ExpressionKind::Semantics(_, expr) => {
                expr.traverse_mut_inner(f);
            }
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct Arm {
    pub span: SpanList,
    pub pattern: Pattern,
    pub guard: Option<Expression>,
    pub body: Expression,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Pattern {
    pub span: SpanList,
    pub kind: PatternKind,
}

#[derive(Debug, Clone, Deserialize)]
pub enum PatternKind {
    Error(#[serde(skip)] Backtrace),
    Wildcard,
    Number(InternedString),
    Text(InternedString),
    Variable(VariableId),
    Destructure(HashMap<InternedString, Pattern>),
    Variant(TypeId, VariantIndex, Vec<Pattern>),
    Annotate(Box<Pattern>, TypeAnnotation),
    Or(Box<Pattern>, Box<Pattern>),
    Tuple(Vec<Pattern>),
}

impl PatternKind {
    pub(crate) fn error(compiler: &Compiler) -> Self {
        PatternKind::Error(compiler.backtrace())
    }
}

impl Pattern {
    pub fn variables(&self) -> BTreeSet<VariableId> {
        let mut variables = BTreeSet::new();
        self.traverse(|pattern| {
            if let PatternKind::Variable(var) = &pattern.kind {
                variables.insert(*var);
            }
        });

        variables
    }

    pub fn traverse(&self, mut f: impl FnMut(&Self)) {
        self.traverse_inner(&mut f);
    }

    fn traverse_inner(&self, f: &mut impl FnMut(&Self)) {
        f(self);

        match &self.kind {
            PatternKind::Destructure(fields) => {
                for pattern in fields.values() {
                    pattern.traverse_inner(f);
                }
            }
            PatternKind::Variant(_, _, patterns) => {
                for pattern in patterns {
                    pattern.traverse_inner(f);
                }
            }
            PatternKind::Annotate(pattern, _) => {
                pattern.traverse_inner(f);
            }
            PatternKind::Or(left, right) => {
                left.traverse_inner(f);
                right.traverse_inner(f);
            }
            PatternKind::Tuple(patterns) => {
                for pattern in patterns {
                    pattern.traverse_inner(f);
                }
            }
            _ => {}
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct TypeAnnotation {
    pub span: SpanList,
    pub kind: TypeAnnotationKind,
}

#[derive(Debug, Clone, Deserialize)]
pub enum TypeAnnotationKind {
    Error(#[serde(skip)] Backtrace),
    Placeholder,
    Named(TypeId, Vec<TypeAnnotation>),
    Parameter(TypeParameterId),
    Builtin(BuiltinTypeId, Vec<TypeAnnotation>),
    Function(Box<TypeAnnotation>, Box<TypeAnnotation>),
    Tuple(Vec<TypeAnnotation>),
}

impl TypeAnnotationKind {
    pub(crate) fn error(compiler: &Compiler) -> Self {
        TypeAnnotationKind::Error(compiler.backtrace())
    }
}

#[derive(Debug, Clone)]
pub struct Bound {
    pub span: SpanList,
    pub tr_span: SpanList,
    pub tr: TraitId,
    pub parameters: Vec<TypeAnnotation>,
}

pub type CaptureList = Vec<(VariableId, SpanList)>;

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    strum::EnumString,
    strum::Display,
    Serialize,
    Deserialize,
)]
#[serde(rename_all = "kebab-case")]
#[strum(serialize_all = "kebab-case")]
pub enum Intrinsic {
    Crash,
    Display,
    Prompt,
    Choice,
    WithUi,
    MessageUi,
    WithContinuation,
    WithTaskGroup,
    Task,
    InBackground,
    Delay,
    NumberToText,
    IntegerToText,
    NaturalToText,
    ByteToText,
    SignedToText,
    UnsignedToText,
    FloatToText,
    DoubleToText,
    TextToNumber,
    TextToInteger,
    TextToNatural,
    TextToByte,
    TextToSigned,
    TextToUnsigned,
    TextToFloat,
    TextToDouble,
    // TODO: All the conversions
    NaturalToNumber,
    NumberToNatural,
    NaturalToInteger,
    IntegerToNatural,
    AddNumber,
    SubtractNumber,
    MultiplyNumber,
    DivideNumber,
    ModuloNumber,
    PowerNumber,
    FloorNumber,
    CeilNumber,
    SqrtNumber,
    NegateNumber,
    AddInteger,
    SubtractInteger,
    MultiplyInteger,
    DivideInteger,
    ModuloInteger,
    PowerInteger,
    NegateInteger,
    AddNatural,
    SubtractNatural,
    MultiplyNatural,
    DivideNatural,
    ModuloNatural,
    PowerNatural,
    AddByte,
    SubtractByte,
    MultiplyByte,
    DivideByte,
    ModuloByte,
    PowerByte,
    AddSigned,
    SubtractSigned,
    MultiplySigned,
    DivideSigned,
    ModuloSigned,
    PowerSigned,
    NegateSigned,
    AddUnsigned,
    SubtractUnsigned,
    MultiplyUnsigned,
    DivideUnsigned,
    ModuloUnsigned,
    PowerUnsigned,
    AddFloat,
    SubtractFloat,
    MultiplyFloat,
    DivideFloat,
    ModuloFloat,
    PowerFloat,
    FloorFloat,
    CeilFloat,
    SqrtFloat,
    NegateFloat,
    AddDouble,
    SubtractDouble,
    MultiplyDouble,
    DivideDouble,
    ModuloDouble,
    PowerDouble,
    FloorDouble,
    CeilDouble,
    SqrtDouble,
    NegateDouble,
    TextEquality,
    NumberEquality,
    IntegerEquality,
    NaturalEquality,
    ByteEquality,
    SignedEquality,
    UnsignedEquality,
    FloatEquality,
    DoubleEquality,
    TextOrdering,
    NumberOrdering,
    IntegerOrdering,
    NaturalOrdering,
    ByteOrdering,
    SignedOrdering,
    UnsignedOrdering,
    FloatOrdering,
    DoubleOrdering,
    MakeMutable,
    GetMutable,
    SetMutable,
    MakeEmptyList,
    ListFirst,
    ListLast,
    ListInitial,
    ListTail,
    ListNth,
    ListAppend,
    ListPrepend,
    ListInsertAt,
    ListRemoveAt,
    ListCount,
    ListSlice,
    TextCharacters,
    RandomNumber,
    RandomInteger,
    RandomNatural,
    RandomByte,
    RandomSigned,
    RandomUnsigned,
    RandomFloat,
    RandomDouble,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    strum::EnumString,
    strum::Display,
    Serialize,
    Deserialize,
)]
#[serde(rename_all = "kebab-case")]
#[strum(serialize_all = "kebab-case")]
pub enum Semantics {
    Pure,
    Nonconsuming,
}

#[derive(Debug, Clone, Copy)]
pub enum AnyDeclaration {
    Type(TypeId),
    BuiltinType(BuiltinTypeId),
    Trait(TraitId),
    TypeParameter(TypeParameterId),
    Constant(ConstantId, Option<(TypeId, VariantIndex)>),
    Variable(VariableId),
}

impl PartialEq for AnyDeclaration {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

impl Eq for AnyDeclaration {}

impl std::hash::Hash for AnyDeclaration {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
    }
}

impl AnyDeclaration {
    pub fn as_any(self) -> Option<Self> {
        Some(self)
    }

    pub fn as_type(self) -> Option<TypeId> {
        match self {
            AnyDeclaration::Type(id) => Some(id),
            _ => None,
        }
    }

    pub fn as_builtin_type(self) -> Option<BuiltinTypeId> {
        match self {
            AnyDeclaration::BuiltinType(id) => Some(id),
            _ => None,
        }
    }

    pub fn as_trait(self) -> Option<TraitId> {
        match self {
            AnyDeclaration::Trait(id) => Some(id),
            _ => None,
        }
    }

    pub fn as_type_parameter(self) -> Option<TypeParameterId> {
        match self {
            AnyDeclaration::TypeParameter(id) => Some(id),
            _ => None,
        }
    }

    pub fn as_constant(self) -> Option<(ConstantId, Option<(TypeId, VariantIndex)>)> {
        match self {
            AnyDeclaration::Constant(id, variant) => Some((id, variant)),
            _ => None,
        }
    }

    pub fn as_variable(self) -> Option<VariableId> {
        match self {
            AnyDeclaration::Variable(id) => Some(id),
            _ => None,
        }
    }
}

impl Compiler {
    pub(crate) fn lower(&self, file: &ast::File<Analysis>, dependencies: Vec<Arc<File>>) -> File {
        let mut lowerer = Lowerer {
            compiler: self.clone(),
            file: file.span.first().path,
            file_info: Default::default(),
            declarations: Default::default(),
            specializations: Default::default(),
            scopes: Default::default(),
            captures: Default::default(),
            variables: Default::default(),
        };

        let ctx = Context::default();
        lowerer.load_builtins(&ScopeSet::new());

        for (&name, (id, definition, uses)) in &*file.file.builtin_syntax_uses.lock() {
            lowerer
                .declarations
                .builtin_syntaxes
                .entry(*id)
                .or_insert_with(|| Declaration {
                    name: Some(InternedString::new(name)),
                    span: SpanList::from(Span::builtin()),
                    uses: Default::default(),
                    value: BuiltinSyntaxDeclaration {
                        definition: definition.clone(),
                    },
                    imported: false,
                })
                .uses
                .extend(uses.clone());
        }

        for (id, value) in &*file.file.syntax_declarations.lock() {
            if !lowerer.declarations.syntaxes.contains_key(id) {
                let decl = Declaration {
                    name: value.name.as_ref().map(|(name, _)| *name),
                    span: value.span(),
                    uses: value.uses.iter().copied().collect(),
                    value: SyntaxDeclaration {
                        operator: value.operator_precedence.is_some(),
                        keyword: value.keyword.is_some(),
                        attributes: lowerer.lower_syntax_attributes(&value.attributes, &ctx),
                    },
                    imported: false,
                };

                lowerer.declarations.syntaxes.insert(*id, decl);
            }
        }

        let mut file_attributes = BTreeMap::new();

        for dependency in dependencies {
            dependency
                .attributes
                .get(&dependency.span.first().path)
                .unwrap()
                .imported_by
                .lock()
                .push(file.file.path);

            file_attributes.extend(dependency.attributes.clone());

            macro_rules! merge_dependency {
                ($($kind:ident$(($transform:expr))?),* $(,)?) => {
                    $(
                        for (&id, decl) in &dependency.declarations.$kind {
                            let mut uses = im::HashSet::new();
                            let merged_decl = lowerer.declarations.$kind.entry(id).or_insert_with(|| {
                                uses = decl.uses.clone();

                                let mut decl = $($transform)?(decl.clone());
                                decl.imported = true;

                                decl
                            });

                            merged_decl.uses.extend(uses);
                        }
                    )*
                };
            }

            merge_dependency!(
                syntaxes,
                types(Declaration::make_unresolved),
                type_parameters,
                traits(Declaration::make_unresolved),
                builtin_types,
                constants(Declaration::make_unresolved),
                instances(Declaration::make_unresolved),
                variables,
            );

            lowerer.file_info.merge(dependency.info.clone());

            for (&id, specializations) in &dependency.specializations {
                for &specialization in specializations {
                    lowerer.specializations.insert(specialization, id);
                }
            }

            for (&name, decls) in &dependency.exported {
                let mut scopes = lowerer.scopes.get(&name).cloned().unwrap_or_default();
                for (scope, decl) in decls {
                    let private = lowerer.attributes_of(*decl).private;

                    // So that this file can access top-level declarations
                    if scope == &dependency.root_scope {
                        scopes.push((file.root_scope.clone(), *decl, private));
                    }

                    // So that hygienic syntaxes can continue to access the
                    // file's declarations
                    if !scope.is_empty() {
                        scopes.push((scope.clone(), *decl, false));
                    }
                }

                lowerer.scopes.insert(name, scopes);
            }
        }

        file_attributes.insert(
            file.file.path,
            lowerer.lower_file_attributes(&file.attributes),
        );

        let statements = lowerer.lower_statements(&file.statements, &ctx);

        for (&constant_id, constant) in &lowerer.declarations.constants {
            if constant.imported {
                continue;
            }

            constant
                .value
                .as_ref()
                .unwrap()
                .value
                .lock()
                .get_or_insert_with(|| {
                    self.add_error(
                        "uninitialized constant",
                        vec![Note::primary(
                            constant.span,
                            format!(
                                "`{}` is never initialized with a value",
                                constant.name.unwrap()
                            ),
                        )],
                        "uninitialized-constant",
                    );

                    Expression {
                        id: lowerer.compiler.new_expression_id(constant_id),
                        span: constant.span,
                        kind: ExpressionKind::error(self),
                    }
                });
        }

        for (&instance_id, instance) in &lowerer.declarations.instances {
            if instance.imported {
                continue;
            }

            let tr = instance.value.as_ref().unwrap().tr;
            let tr_decl = lowerer.declarations.traits.get(&tr).unwrap();

            let attributes = &tr_decl.value.as_ref().unwrap().attributes;

            if attributes.sealed && instance.span.first().path != tr_decl.span.first().path {
                self.add_error(
                    "instance of sealed trait must be in the same file in which the trait is defined",
                    vec![Note::primary(instance.span, "instance disallowed here")],
                    "",
                );
            }

            if attributes.allow_overlapping_instances && !attributes.sealed {
                self.add_error(
                    "trait that allows overlapping instances must be `sealed`",
                    vec![Note::primary(
                        tr_decl.span,
                        "add `[sealed]` to this trait declaration",
                    )],
                    "",
                );
            }

            let trait_has_value = tr_decl.value.as_ref().unwrap().ty.is_some();
            let instance_value = &mut *instance.value.as_ref().unwrap().value.lock();

            match (trait_has_value, instance_value) {
                (true, instance_value @ None) => {
                    self.add_diagnostic(
                        self.error(
                            "expected instance to have value",
                            vec![Note::primary(
                                instance.span,
                                "try adding `:` after the instance declaration to give it a value",
                            )],
                            "",
                        )
                        .fix_with(
                            "give this instance a value",
                            FixRange::after(instance.span.first()),
                            " : {%value%}",
                        ),
                    );

                    *instance_value = Some(InstanceValue {
                        colon_span: None,
                        value: Expression {
                            id: lowerer.compiler.new_expression_id(instance_id),
                            span: instance.span,
                            kind: ExpressionKind::error(self),
                        },
                    });
                }
                (false, Some(instance_value)) => {
                    self.add_diagnostic(
                        self.error(
                            "instance has value, but the trait doesn't store a value",
                            vec![Note::primary(
                                instance.span,
                                "try removing this instance's value",
                            )],
                            "",
                        )
                        .fix_with(
                            "remove this value",
                            FixRange::replace(
                                SpanList::join(
                                    instance_value.colon_span.unwrap(),
                                    instance_value.value.span,
                                )
                                .first(),
                            ),
                            "",
                        ),
                    );
                }
                _ => {}
            }
        }

        let mut specializations = BTreeMap::<ConstantId, Vec<ConstantId>>::new();
        for (constant, specialized_constant) in lowerer.specializations {
            specializations
                .entry(specialized_constant)
                .or_default()
                .push(constant);
        }

        File {
            span: file.span,
            attributes: file_attributes,
            declarations: lowerer.declarations.resolve(),
            info: lowerer.file_info,
            specializations,
            statements,
            root_scope: file.root_scope.clone(),
            exported: lowerer
                .scopes
                .into_iter()
                .map(|(name, scopes)| {
                    (
                        name,
                        scopes
                            .into_iter()
                            .map(|(scope, decl, _)| (scope, decl))
                            .collect(),
                    )
                })
                .collect(),
        }
    }
}

#[derive(Debug)]
struct Lowerer {
    compiler: Compiler,
    file: FilePath,
    declarations: UnresolvedDeclarations,
    file_info: FileInfo,
    specializations: BTreeMap<ConstantId, ConstantId>,
    scopes: im::HashMap<InternedString, Vec<(ScopeSet, AnyDeclaration, bool)>>,
    captures: BTreeMap<ScopeId, Shared<CaptureList>>,
    variables: BTreeMap<VariableId, ScopeSet>,
}

impl Lowerer {
    fn insert(&mut self, name: InternedString, value: AnyDeclaration, scope_set: &ScopeSet) {
        if let AnyDeclaration::Variable(var) = value {
            self.variables.insert(var, scope_set.clone());
        }

        self.scopes
            .entry(name)
            .or_default()
            .push((scope_set.clone(), value, false));
    }

    fn get<T: Copy + Eq + Hash>(
        &mut self,
        name: InternedString,
        span: SpanList,
        kind: fn(AnyDeclaration) -> Option<T>,
        scope: &ScopeSet,
    ) -> Result<Option<T>, Vec<T>> {
        self.get_inner(name, Some(span), kind, scope)
    }

    fn peek<T: Copy + Eq + Hash>(
        &mut self,
        name: InternedString,
        kind: fn(AnyDeclaration) -> Option<T>,
        scope: &ScopeSet,
    ) -> Result<Option<T>, Vec<T>> {
        self.get_inner(name, None, kind, scope)
    }

    fn get_inner<T: Copy + Eq + Hash>(
        &mut self,
        name: InternedString,
        use_span: Option<SpanList>,
        kind: fn(AnyDeclaration) -> Option<T>,
        scope: &ScopeSet,
    ) -> Result<Option<T>, Vec<T>> {
        let scopes = match self.scopes.get(&name) {
            Some(scopes) => scopes,
            None => return Ok(None),
        };

        let mut candidates = scopes
            .iter()
            .filter(|(candidate, _, private)| !private && candidate.is_subset(scope))
            .max_set_by_key(|(candidate, _, _)| candidate.clone().intersection(scope.clone()).len())
            .into_iter()
            .filter_map(|(scopes, decl, _)| {
                let decl = *decl;

                let resolved = kind(decl)?;

                if let Some(use_span) = use_span {
                    for &scope in scopes {
                        if let Some(id) = decl.as_variable() {
                            self.captures
                                .entry(scope)
                                .or_default()
                                .lock()
                                .push((id, use_span));
                        }
                    }
                }

                Some((decl, resolved))
            })
            .unique_by(|(_, resolved)| *resolved)
            .collect::<Vec<_>>();

        match candidates.len() {
            0 => Ok(None),
            1 => Ok(Some(candidates.pop().unwrap().1)),
            _ => {
                // Allow declarations made in this file to shadow declarations
                // made in other files

                let mut local_candidates = candidates
                    .iter()
                    .filter(|(decl, _)| self.span_of(*decl).first().path == self.file)
                    .map(|(_, candidate)| *candidate)
                    .collect::<Vec<_>>();

                if local_candidates.len() == 1 {
                    Ok(Some(local_candidates.pop().unwrap()))
                } else {
                    Err(candidates
                        .into_iter()
                        .map(|(_, candidate)| candidate)
                        .collect())
                }
            }
        }
    }

    fn collect<'a>(
        &'a self,
        kind: impl Fn(AnyDeclaration) -> bool + 'a,
        scope_set: &'a ScopeSet,
    ) -> impl Iterator<Item = InternedString> + 'a {
        self.scopes.iter().filter_map(move |(&name, candidates)| {
            candidates
                .iter()
                .any(|(candidate, decl, private)| {
                    !private && candidate.is_subset(scope_set) && kind(*decl)
                })
                .then_some(name)
        })
    }
}

#[derive(Debug, Clone, Default)]
#[non_exhaustive]
struct Context {
    owner: Option<ConstantId>,
    caller_accepts_text: bool,
    in_function: bool,
}

#[derive(Debug)]
struct StatementDeclaration<'a> {
    span: SpanList,
    kind: StatementDeclarationKind<'a>,
    attributes: &'a ast::StatementAttributes<Analysis>,
}

#[derive(Debug)]
enum StatementDeclarationKind<'a> {
    Type(
        TypeId,
        ScopeSet,
        Option<ScopeSet>,
        Option<(Vec<TypeParameterId>, Vec<Bound>)>,
        &'a ast::TypeAssignmentValue<Analysis>,
    ),
    Trait(
        TraitId,
        ScopeSet,
        Option<ScopeSet>,
        Option<(Vec<TypeParameterId>, Vec<Bound>)>,
        &'a ast::TraitAssignmentValue<Analysis>,
    ),
    Constant(
        ConstantId,
        (ScopeSet, (Vec<TypeParameterId>, Vec<Bound>)),
        &'a ast::Type<Analysis>,
    ),
    Instance(
        ConstantId,
        Option<(Vec<TypeParameterId>, Vec<Bound>)>,
        (
            SpanList,
            InternedString,
            ScopeSet,
            &'a Vec<Result<ast::Type<Analysis>, ast::SyntaxError<Analysis>>>,
        ),
        Option<(SpanList, &'a ast::Expression<Analysis>)>,
    ),
    Queued(QueuedStatement<'a>),
}

#[derive(Debug)]
enum QueuedStatement<'a> {
    Assign(&'a ast::Pattern<Analysis>, &'a ast::Expression<Analysis>),
    Expression(Cow<'a, ast::Expression<Analysis>>),
}

impl Lowerer {
    fn lower_file_attributes(
        &self,
        file_attributes: &ast::FileAttributes<Analysis>,
    ) -> FileAttributes {
        // TODO: Raise errors for misused attributes

        FileAttributes {
            imported_by: Default::default(),
            help_url: file_attributes
                .help_url
                .as_ref()
                .map(|attribute| attribute.help_url_text),
        }
    }

    fn lower_statements(
        &mut self,
        statements: &[Result<ast::Statement<Analysis>, ast::SyntaxError<Analysis>>],
        ctx: &Context,
    ) -> Vec<Expression> {
        let declarations = statements
            .iter()
            .filter_map(|statement| statement.as_ref().ok())
            .map(|statement| self.lower_statement(statement, ctx))
            .collect::<Vec<_>>();

        let mut queue = Vec::new();
        let mut current_constant = None;

        for decl in declarations {
            let decl = match decl {
                Some(decl) => decl,
                None => continue,
            };

            if !matches!(decl.kind, StatementDeclarationKind::Queued(_)) {
                current_constant = None;
            }

            let scope_value = match decl.kind {
                StatementDeclarationKind::Type(id, scope, pattern_scope, ty_pattern, value) => {
                    let (parameters, bounds) = ty_pattern.unwrap_or_default();

                    if let Some(bound) = bounds.first() {
                        self.compiler.add_error(
                            "`type` declarations may not have bounds",
                            vec![Note::primary(bound.span, "try removing this")],
                            "syntax-error",
                        );
                    }

                    let kind = match &value.body {
                        Some(ty) => {
                            let body = match ty {
                                Ok(body) => body,
                                Err(_) => continue,
                            };

                            match body {
                                ast::TypeBody::Block(ty) => (|| {
                                    if let [Ok(ast::TypeMember::Variant(member))] =
                                        ty.members.as_slice()
                                    {
                                        if let Some(name_list) = &member.name_list {
                                            let variants = name_list
                                                .iter()
                                                .enumerate()
                                                .map(|(index, (span, name, scope))| {
                                                    let index = VariantIndex::new(index);

                                                    EnumerationVariant {
                                                        name_span: *span,
                                                        name: *name,
                                                        name_scope: scope.clone(),
                                                        tys: Vec::new(),
                                                        constructor: self
                                                            .generate_variant_constructor(
                                                                id,
                                                                ctx.owner,
                                                                *name,
                                                                *span,
                                                                index,
                                                                &parameters,
                                                                &[],
                                                            ),
                                                    }
                                                })
                                                .collect::<Vec<_>>();

                                            let variant_names = variants
                                                .iter()
                                                .enumerate()
                                                .map(|(index, variant)| {
                                                    let index = VariantIndex::new(index);

                                                    self.insert(
                                                        variant.name,
                                                        AnyDeclaration::Constant(
                                                            variant.constructor,
                                                            Some((id, index)),
                                                        ),
                                                        &scope,
                                                    );

                                                    (variant.name, index)
                                                })
                                                .collect();

                                            return TypeDeclarationKind::Enumeration(
                                                variants,
                                                variant_names,
                                            );
                                        }
                                    }

                                    let mut fields = Vec::new();
                                    let mut variants = Vec::new();
                                    for (index, member) in ty.members.iter().enumerate() {
                                        let member = match member {
                                            Ok(member) => member,
                                            Err(_) => continue,
                                        };

                                        match member {
                                            ast::TypeMember::Field(field) => {
                                                let ty = match &field.ty {
                                                    Ok(ty) => self.lower_type(ty, ctx, None),
                                                    Err(error) => TypeAnnotation {
                                                        span: error.span,
                                                        kind: TypeAnnotationKind::error(
                                                            &self.compiler,
                                                        ),
                                                    },
                                                };

                                                fields.push(StructureField {
                                                    name_span: field.name_span,
                                                    name: field.name,
                                                    ty,
                                                });
                                            }
                                            ast::TypeMember::Variant(variant) => {
                                                let index = VariantIndex::new(index);

                                                let tys = variant
                                                    .tys
                                                    .iter()
                                                    .map(|ty| match ty {
                                                        Ok(ty) => self.lower_type(ty, ctx, None),
                                                        Err(error) => TypeAnnotation {
                                                            span: error.span,
                                                            kind: TypeAnnotationKind::error(
                                                                &self.compiler,
                                                            ),
                                                        },
                                                    })
                                                    .collect::<Vec<_>>();

                                                let constructor = self
                                                    .generate_variant_constructor(
                                                        id,
                                                        ctx.owner,
                                                        variant.name,
                                                        variant.span,
                                                        index,
                                                        &parameters,
                                                        &tys,
                                                    );

                                                variants.push(EnumerationVariant {
                                                    name_span: variant.name_span,
                                                    name: variant.name,
                                                    name_scope: variant.name_scope.clone(),
                                                    tys,
                                                    constructor,
                                                });
                                            }
                                        }
                                    }

                                    if !fields.is_empty() && !variants.is_empty() {
                                        self.compiler.add_error(
                                            "cannot mix fields and variants in a single `type` declaration",
                                            vec![Note::primary(
                                                ty.span,
                                                "type must contain all fields or all variants",
                                            )],
                                            "mixed-type"
                                        );

                                        TypeDeclarationKind::Marker
                                    } else if !fields.is_empty() {
                                        let field_names = fields
                                            .iter()
                                            .enumerate()
                                            .map(|(index, field)| {
                                                (field.name, FieldIndex::new(index))
                                            })
                                            .collect();

                                        TypeDeclarationKind::Structure(fields, field_names)
                                    } else if !variants.is_empty() {
                                        let variant_names = variants
                                            .iter()
                                            .enumerate()
                                            .map(|(index, variant)| {
                                                let index = VariantIndex::new(index);

                                                self.insert(
                                                    variant.name,
                                                    AnyDeclaration::Constant(
                                                        variant.constructor,
                                                        Some((id, index)),
                                                    ),
                                                    &scope,
                                                );

                                                (variant.name, index)
                                            })
                                            .collect();

                                        TypeDeclarationKind::Enumeration(variants, variant_names)
                                    } else {
                                        self.compiler.add_error(
                                            "`type` must contain at least one field or variant",
                                            vec![Note::primary(
                                                ty.span,
                                                "to create a marker type, remove the `{}`",
                                            )],
                                            "empty-type",
                                        );

                                        TypeDeclarationKind::Marker
                                    }
                                })(),
                                ast::TypeBody::Alias(body) => {
                                    let ty = self.lower_type(&body.ty, ctx, None);
                                    TypeDeclarationKind::Alias(ty)
                                }
                            }
                        }
                        None => TypeDeclarationKind::Marker,
                    };

                    let attributes = self.lower_type_attributes(
                        decl.attributes,
                        ctx,
                        pattern_scope.as_ref().unwrap_or(&scope),
                    );

                    self.declarations.types.get_mut(&id).unwrap().value = Some(TypeDeclaration {
                        parameters,
                        kind,
                        attributes,
                    });

                    Some(AnyDeclaration::Type(id))
                }
                StatementDeclarationKind::Trait(id, scope, pattern_scope, ty_pattern, value) => {
                    let (parameters, bounds) = ty_pattern.unwrap_or_default();

                    if let Some(bound) = bounds.first() {
                        self.compiler.add_error(
                            "`trait` declarations may not have bounds",
                            vec![Note::primary(bound.span, "try removing this")],
                            "syntax-error",
                        );
                    }

                    let ty = value.ty.as_ref().map(|ty| match ty {
                        Ok(ty) => self.lower_type(ty, ctx, None),
                        Err(error) => TypeAnnotation {
                            span: error.span,
                            kind: TypeAnnotationKind::error(&self.compiler),
                        },
                    });

                    let attributes = self.lower_trait_attributes(
                        decl.attributes,
                        ctx,
                        pattern_scope.as_ref().unwrap_or(&scope),
                    );

                    self.declarations.traits.get_mut(&id).unwrap().value = Some(TraitDeclaration {
                        parameters,
                        ty,
                        attributes,
                    });

                    Some(AnyDeclaration::Trait(id))
                }
                StatementDeclarationKind::Constant(id, (scope, (parameters, bounds)), ty) => {
                    let ty = self.lower_type(ty, ctx, None);

                    let attributes = self.lower_constant_attributes(decl.attributes, ctx);

                    self.declarations.constants.get_mut(&id).unwrap().value =
                        Some(UnresolvedConstantDeclaration {
                            parameters,
                            bounds,
                            ty,
                            value: Default::default(),
                            is_variant: false,
                            attributes,
                        });

                    current_constant = Some((id, scope));

                    Some(AnyDeclaration::Constant(id, None))
                }
                StatementDeclarationKind::Instance(
                    id,
                    ty_pattern,
                    (trait_span, trait_name, trait_scope, trait_params),
                    value,
                ) => {
                    let (parameters, bounds) = ty_pattern.unwrap_or_default();

                    let tr = match self.get(
                        trait_name,
                        trait_span,
                        AnyDeclaration::as_trait,
                        &trait_scope,
                    ) {
                        Ok(Some(tr)) => {
                            self.declarations
                                .traits
                                .get_mut(&tr)
                                .unwrap()
                                .uses
                                .insert(trait_span);

                            tr
                        }
                        Ok(None) => {
                            let (notes, fix) = self.diagnostic_notes_for_unresolved_name(
                                trait_span,
                                trait_name,
                                ctx,
                                AnyDeclaration::as_trait,
                                &trait_scope,
                            );

                            self.compiler.add_diagnostic(
                                self.compiler
                                    .error(
                                        format!("cannot find trait `{trait_name}`"),
                                        std::iter::once(Note::primary(trait_span, "no such trait"))
                                            .chain(notes)
                                            .collect(),
                                        "undefined-name",
                                    )
                                    .fix(fix),
                            );

                            self.declarations.instances.remove(&id);
                            continue;
                        }
                        Err(candidates) => {
                            self.compiler.add_error(
                                format!("multiple definitions of trait `{trait_name}`"),
                                std::iter::once(Note::primary(
                                    trait_span,
                                    format!("`{trait_name}` could refer to..."),
                                ))
                                .chain(
                                    self.diagnostic_notes_for_ambiguous_name(
                                        candidates
                                            .into_iter()
                                            .map(|id| {
                                                self.declarations.traits.get(&id).unwrap().span
                                            })
                                            .collect(),
                                    ),
                                )
                                .collect(),
                                "",
                            );

                            self.declarations.instances.remove(&id);
                            continue;
                        }
                    };

                    let trait_params = trait_params
                        .iter()
                        .map(|ty| match ty {
                            Ok(ty) => self.lower_type(ty, ctx, None),
                            Err(error) => TypeAnnotation {
                                span: error.span,
                                kind: TypeAnnotationKind::error(&self.compiler),
                            },
                        })
                        .collect::<Vec<_>>();

                    let value = value
                        .map(|(colon_span, value)| InstanceValue {
                            colon_span: Some(colon_span),
                            value: self.lower_expr(value, ctx),
                        })
                        .or_else(|| {
                            let tr_decl = self.declarations.traits.get(&tr).unwrap();

                            tr_decl.value.as_ref().unwrap().attributes.derive.map(
                                |(span, path, name)| InstanceValue {
                                    colon_span: None,
                                    value: Expression {
                                        id: self.compiler.new_expression_id(id),
                                        span: span.merge(decl.span),
                                        kind: ExpressionKind::Plugin(path, name, Vec::new()),
                                    },
                                },
                            )
                        });

                    self.declarations.instances.get_mut(&id).unwrap().value =
                        Some(UnresolvedInstanceDeclaration {
                            parameters,
                            bounds,
                            tr_span: trait_span,
                            tr,
                            tr_parameters: trait_params,
                            value: Shared::new(value),
                        });

                    Some(AnyDeclaration::Constant(id, None))
                }
                StatementDeclarationKind::Queued(statement) => {
                    queue.push((decl.span, statement, mem::take(&mut current_constant)));
                    None
                }
            };

            'language_items: {
                if let Some(language_item) = &decl.attributes.language_item {
                    match &language_item.language_item_kind {
                        ast::LanguageItemStatementAttributeKind::Boolean => {
                            let ty = match scope_value {
                                Some(AnyDeclaration::Type(id)) => id,
                                _ => {
                                    self.compiler.add_error(
                                        "`boolean` language item expects a type",
                                        vec![Note::primary(
                                            decl.span,
                                            "expected type declaration here",
                                        )],
                                        "",
                                    );

                                    break 'language_items;
                                }
                            };

                            if self.file_info.language_items.boolean.is_some() {
                                self.compiler.add_error(
                                    "`language` item may only be defined once",
                                    vec![Note::primary(
                                        decl.span,
                                        "`language` item already defined elsewhere",
                                    )],
                                    "",
                                );

                                break 'language_items;
                            }

                            self.file_info.language_items.boolean = Some(ty);
                        }
                        ast::LanguageItemStatementAttributeKind::Show => {
                            let tr = match scope_value {
                                Some(AnyDeclaration::Trait(id)) => id,
                                _ => {
                                    self.compiler.add_error(
                                        "`show` language item expects a trait",
                                        vec![Note::primary(
                                            decl.span,
                                            "expected trait declaration here",
                                        )],
                                        "",
                                    );

                                    break 'language_items;
                                }
                            };

                            if self.file_info.language_items.show.is_some() {
                                self.compiler.add_error(
                                    "`language` item may only be defined once",
                                    vec![Note::primary(
                                        decl.span,
                                        "`language` item already defined elsewhere",
                                    )],
                                    "",
                                );

                                break 'language_items;
                            }

                            self.file_info.language_items.show = Some(tr);
                        }
                    }
                }
            }

            'entrypoint: {
                if decl.attributes.entrypoint.is_some() {
                    if self.file_info.entrypoint.is_some() {
                        self.compiler.add_error(
                            "`entrypoint` may only be defined once",
                            vec![Note::primary(
                                decl.span,
                                "`entrypoint` already defined elsewhere",
                            )],
                            "",
                        );

                        break 'entrypoint;
                    }

                    let constant = match scope_value {
                        Some(AnyDeclaration::Constant(id, _)) => id,
                        _ => {
                            self.compiler.add_error(
                                "`entrypoint` expects a constant",
                                vec![Note::primary(
                                    decl.span,
                                    "expected constant declaration here",
                                )],
                                "",
                            );

                            break 'entrypoint;
                        }
                    };

                    self.file_info.entrypoint = Some(constant);
                }
            }

            'diagnostic_items: {
                if let Some(diagnostic_item) = &decl.attributes.diagnostic_item {
                    match &diagnostic_item.diagnostic_item_kind {
                        ast::DiagnosticItemStatementAttributeKind::AcceptsText => {
                            let constant = match scope_value {
                                Some(AnyDeclaration::Constant(id, _)) => id,
                                _ => {
                                    self.compiler.add_error(
                                        "`accepts-text` diagnostic item expects a constant",
                                        vec![Note::primary(
                                            decl.span,
                                            "expected constant declaration here",
                                        )],
                                        "",
                                    );

                                    break 'diagnostic_items;
                                }
                            };

                            self.file_info.diagnostic_items.accepts_text.push(constant);
                        }
                        ast::DiagnosticItemStatementAttributeKind::CollectionElements => {
                            let constant = match scope_value {
                                Some(AnyDeclaration::Constant(id, _)) => id,
                                _ => {
                                    self.compiler.add_error(
                                        "`unique-elements` diagnostic item expects a constant",
                                        vec![Note::primary(
                                            decl.span,
                                            "expected constant declaration here",
                                        )],
                                        "",
                                    );

                                    break 'diagnostic_items;
                                }
                            };

                            self.file_info
                                .diagnostic_items
                                .collection_elements
                                .push(constant);
                        }
                    }
                }
            }

            'diagnostic_aliases: {
                if !decl.attributes.diagnostic_aliases.is_empty() {
                    let constant = match scope_value {
                        Some(AnyDeclaration::Constant(id, _)) => id,
                        _ => {
                            self.compiler.add_error(
                                "`diagnostic-alias` expects a constant",
                                vec![Note::primary(
                                    decl.span,
                                    "expected constant declaration here",
                                )],
                                "",
                            );

                            break 'diagnostic_aliases;
                        }
                    };

                    if let Some(name) = self.declarations.constants.get(&constant).unwrap().name {
                        for attr in &decl.attributes.diagnostic_aliases {
                            self.file_info
                                .diagnostic_aliases
                                .aliases
                                .insert(attr.diagnostic_alias, name);
                        }
                    }
                }
            }
        }

        queue
            .into_iter()
            .filter_map(|(span, statement, prev_constant)| match statement {
                QueuedStatement::Assign(pattern, expr) => {
                    macro_rules! assign_pattern {
                        () => {{
                            let value = self.lower_expr(expr, ctx);
                            let pattern = self.lower_pattern(pattern, ctx);

                            Some(Expression {
                                id: self.compiler.new_expression_id(ctx.owner),
                                span: span.into(),
                                kind: ExpressionKind::Initialize(pattern, Box::new(value)),
                            })
                        }};
                    }

                    match &pattern {
                        ast::Pattern::Name(pattern) => {
                            if let Some((prev_constant_id, prev_constant_scope)) = prev_constant {
                                let decl = self
                                    .declarations
                                    .constants
                                    .get(&prev_constant_id)
                                    .unwrap()
                                    .clone();

                                if pattern.name != decl.name.unwrap() {
                                    return assign_pattern!();
                                }

                                let value = decl.value.as_ref().unwrap();
                                let associated_parameters = value.parameters.clone();
                                let associated_constant = value.value.clone();

                                if let Ok(Some((id, _))) =
                                    self.peek(decl.name.unwrap(), AnyDeclaration::as_constant, &prev_constant_scope)
                                {
                                    if id == prev_constant_id
                                        && associated_constant.lock().is_some()
                                    {
                                        self.compiler.add_error(
                                            format!(
                                                "constant `{}` already exists in this file",
                                                pattern.name
                                            ),
                                            vec![
                                                Note::primary(
                                                    pattern.span,
                                                    "try giving this constant a unique name",
                                                ),
                                                Note::secondary(
                                                    decl.span,
                                                    "other constant declared here",
                                                ),
                                            ],
                                            "duplicate-constant",
                                        );

                                        return assign_pattern!();
                                    }
                                }

                                self.declarations
                                    .constants
                                    .get_mut(&prev_constant_id)
                                    .unwrap()
                                    .uses
                                    .insert(pattern.span);

                                let mut scope = prev_constant_scope.clone();
                                scope.insert(self.compiler.new_scope_id_in(expr.span().first().path));

                                for id in associated_parameters {
                                    let parameter =
                                        self.declarations.type_parameters.get(&id).unwrap();

                                    self.insert(
                                        parameter.name.unwrap(),
                                        AnyDeclaration::TypeParameter(id),
                                        &scope,
                                    );
                                }

                                let mut value = self.lower_expr(expr, ctx);

                                let used_variables =
                                    self.generate_capture_list(None, &mut value);

                                if !used_variables.is_empty() {
                                    self.compiler.add_error(
                                        "constant cannot capture outside variables",
                                        used_variables
                                            .into_iter()
                                            .map(|(_, span)| {
                                                Note::primary(span, "captured variable")
                                            })
                                            .collect(),
                                        "constant-captured-variables",
                                    );
                                }

                                *associated_constant.lock() = Some(value);
                                None
                            } else {
                                assign_pattern!()
                            }
                        }
                        _ => assign_pattern!(),
                    }
                }
                QueuedStatement::Expression(expr) => {
                    if let Some((prev_constant, _)) = prev_constant {
                        let span = self
                            .declarations
                            .constants
                            .get(&prev_constant)
                            .unwrap()
                            .span;

                        self.compiler.add_error(
                            "constant must be initialized immediately following its type annotation",
                            vec![Note::primary(span, "try initializing the constant below this")],
                            "uninitialized-constant"
                        );
                    }

                    Some(self.lower_expr(&expr, ctx))
                },
            })
            .collect()
    }

    fn lower_statement<'a>(
        &mut self,
        statement: &'a ast::Statement<Analysis>,
        ctx: &Context,
    ) -> Option<StatementDeclaration<'a>> {
        match statement {
            ast::Statement::Annotate(statement) => {
                let id = self.compiler.new_constant_id_in(self.file);

                let (span, name, scope) = match &statement.value {
                    Ok(name) => (name.span, name.name, name.scope_set.clone()),
                    Err(expr) => {
                        let expr = expr.clone().map(Box::new);

                        let ty = match &statement.annotation {
                            Ok(annotation) => match annotation {
                                ast::ConstantTypeAnnotation::TypeFunction(func) => {
                                    let pattern_span = match &func.pattern {
                                        Ok(pattern) => pattern.span(),
                                        Err(error) => error.span,
                                    };

                                    self.compiler.add_error(
                                        "type functions may only be used when declaring a constant",
                                        vec![Note::primary(pattern_span, "try removing this")],
                                        "syntax-error",
                                    );

                                    return None;
                                }
                                ast::ConstantTypeAnnotation::Type(ty) => Ok(ty.ty.clone()),
                            },
                            Err(error) => Err(error.clone()),
                        };

                        return Some(StatementDeclaration {
                            span: statement.span(),
                            kind: StatementDeclarationKind::Queued(QueuedStatement::Expression(
                                Cow::Owned(
                                    ast::AnnotateExpression {
                                        span: statement.span,
                                        colon_span: statement.colon_span,
                                        expr,
                                        ty,
                                    }
                                    .into(),
                                ),
                            )),
                            attributes: &statement.attributes,
                        });
                    }
                };

                let ((parameters, bounds), ty) = match statement.annotation.as_ref().ok()? {
                    ast::ConstantTypeAnnotation::Type(annotation) => {
                        (Default::default(), annotation)
                    }
                    ast::ConstantTypeAnnotation::TypeFunction(annotation) => {
                        let (params, bounds, _) = annotation
                            .pattern
                            .as_ref()
                            .map(|annotation| self.lower_type_pattern(annotation, ctx, None))
                            .unwrap_or_default();

                        let ty = match annotation.annotation.as_deref().ok()? {
                            ast::ConstantTypeAnnotation::Type(annotation) => annotation,
                            ast::ConstantTypeAnnotation::TypeFunction(annotation) => {
                                self.compiler.add_error(
                                    "type annotation may not contain multiple type functions",
                                    vec![Note::primary(annotation.span(), "try removing this")],
                                    "syntax-error",
                                );

                                return None;
                            }
                        };

                        ((params, bounds), ty)
                    }
                };

                if let Ok(Some((existing_id, variant_info))) =
                    self.get(name, span, AnyDeclaration::as_constant, &scope)
                {
                    if statement.attributes.specialize.is_some() {
                        if variant_info.is_some() {
                            self.compiler.add_error(
                                "cannot specialize a `type` variant",
                                vec![Note::primary(span, "cannot specialize this")],
                                "syntax-error",
                            );

                            return None;
                        }

                        if self.specializations.contains_key(&existing_id) {
                            self.compiler.add_error(
                                "cannot specialize constant which is a specialization of another constant",
                                vec![Note::primary(span, "cannot specialize this")],
                                "syntax-error",
                            );

                            return None;
                        }

                        self.specializations.insert(id, existing_id);
                    } else if existing_id.file == Some(self.file) {
                        let existing_span =
                            self.declarations.constants.get(&existing_id).unwrap().span;

                        self.compiler.add_error(
                            format!("constant `{name}` already exists in this file"),
                            vec![
                                Note::primary(span, "try giving this constant a different name"),
                                Note::primary(existing_span, "original constant declared here"),
                            ],
                            "duplicate-constant",
                        );

                        return None;
                    } else {
                        self.insert(name, AnyDeclaration::Constant(id, None), &scope);
                    }
                } else {
                    self.insert(name, AnyDeclaration::Constant(id, None), &scope);
                }

                self.declarations
                    .constants
                    .insert(id, Declaration::unresolved(Some(name), span));

                Some(StatementDeclaration {
                    span: statement.span(),
                    kind: StatementDeclarationKind::Constant(
                        id,
                        (scope, (parameters, bounds)),
                        &ty.ty,
                    ),
                    attributes: &statement.attributes,
                })
            }
            ast::Statement::Assign(statement) => match statement.value.as_ref().ok()? {
                ast::AssignmentValue::Trait(value) => {
                    let (span, name, scope) =
                        self.get_name_from_assignment(statement.pattern.as_ref().ok()?)?;

                    let id = self.compiler.new_trait_id_in(self.file);
                    self.insert(name, AnyDeclaration::Trait(id), scope);

                    self.declarations
                        .traits
                        .insert(id, Declaration::unresolved(Some(name), span));

                    Some(StatementDeclaration {
                        span: statement.span(),
                        kind: StatementDeclarationKind::Trait(id, scope.clone(), None, None, value),
                        attributes: &statement.attributes,
                    })
                }
                ast::AssignmentValue::Type(value) => {
                    let (span, name, scope) =
                        self.get_name_from_assignment(statement.pattern.as_ref().ok()?)?;

                    let id = self.compiler.new_type_id_in(self.file);
                    self.insert(name, AnyDeclaration::Type(id), scope);

                    self.declarations
                        .types
                        .insert(id, Declaration::unresolved(Some(name), span));

                    Some(StatementDeclaration {
                        span: statement.span(),
                        kind: StatementDeclarationKind::Type(id, scope.clone(), None, None, value),
                        attributes: &statement.attributes,
                    })
                }
                ast::AssignmentValue::Syntax(_) => None,
                ast::AssignmentValue::TypeFunction(value) => match value.value.as_deref().ok()? {
                    ast::AssignmentValue::Trait(trait_value) => {
                        let (span, name, scope) =
                            self.get_name_from_assignment(statement.pattern.as_ref().ok()?)?;

                        let (parameters, bounds, pattern_scope) =
                            self.lower_type_pattern(value.pattern.as_ref().ok()?, ctx, None);

                        let id = self.compiler.new_trait_id_in(self.file);
                        self.insert(name, AnyDeclaration::Trait(id), scope);

                        self.declarations
                            .traits
                            .insert(id, Declaration::unresolved(Some(name), span));

                        Some(StatementDeclaration {
                            span: statement.span(),
                            kind: StatementDeclarationKind::Trait(
                                id,
                                scope.clone(),
                                pattern_scope,
                                Some((parameters, bounds)),
                                trait_value,
                            ),
                            attributes: &statement.attributes,
                        })
                    }
                    ast::AssignmentValue::Type(ty_value) => {
                        let (span, name, scope) =
                            self.get_name_from_assignment(statement.pattern.as_ref().ok()?)?;

                        let (parameters, bounds, pattern_scope) =
                            self.lower_type_pattern(value.pattern.as_ref().ok()?, ctx, None);

                        let id = self.compiler.new_type_id_in(self.file);
                        self.insert(name, AnyDeclaration::Type(id), scope);

                        self.declarations
                            .types
                            .insert(id, Declaration::unresolved(Some(name), span));

                        Some(StatementDeclaration {
                            span: statement.span(),
                            kind: StatementDeclarationKind::Type(
                                id,
                                scope.clone(),
                                pattern_scope,
                                Some((parameters, bounds)),
                                ty_value,
                            ),
                            attributes: &statement.attributes,
                        })
                    }
                    _ => {
                        let value_span = match &statement.value {
                            Ok(value) => value.span(),
                            Err(error) => error.span,
                        };

                        self.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(
                                value_span,
                                "expected a `type` or `trait` definition",
                            )],
                            "syntax-error",
                        );

                        None
                    }
                },
                ast::AssignmentValue::Expression(value) => {
                    match statement.pattern.as_ref().ok()? {
                        ast::AssignmentPattern::Pattern(pattern) => Some(StatementDeclaration {
                            span: statement.span(),
                            kind: StatementDeclarationKind::Queued(QueuedStatement::Assign(
                                &pattern.pattern,
                                &value.expression,
                            )),
                            attributes: &statement.attributes,
                        }),
                        ast::AssignmentPattern::Instance(pattern) => {
                            let id = self.compiler.new_constant_id_in(self.file);

                            self.declarations
                                .instances
                                .insert(id, Declaration::unresolved(None, pattern.span()));

                            Some(StatementDeclaration {
                                span: statement.span(),
                                kind: StatementDeclarationKind::Instance(
                                    id,
                                    None,
                                    (
                                        pattern.trait_span,
                                        pattern.trait_name,
                                        pattern.trait_scope.clone(),
                                        &pattern.trait_parameters,
                                    ),
                                    Some((statement.colon_span, &value.expression)),
                                ),
                                attributes: &statement.attributes,
                            })
                        }
                        ast::AssignmentPattern::TypeFunction(pattern) => {
                            let (parameters, bounds, _) = self.lower_type_pattern(
                                pattern.type_pattern.as_ref().ok()?,
                                ctx,
                                None,
                            );

                            match pattern.assignment_pattern.as_deref().ok()? {
                                ast::AssignmentPattern::Instance(pattern) => {
                                    let id = self.compiler.new_constant_id_in(self.file);

                                    self.declarations
                                        .instances
                                        .insert(id, Declaration::unresolved(None, pattern.span()));

                                    Some(StatementDeclaration {
                                        span: statement.span(),
                                        kind: StatementDeclarationKind::Instance(
                                            id,
                                            Some((parameters, bounds)),
                                            (
                                                pattern.trait_span,
                                                pattern.trait_name,
                                                pattern.trait_scope.clone(),
                                                &pattern.trait_parameters,
                                            ),
                                            Some((statement.colon_span, &value.expression)),
                                        ),
                                        attributes: &statement.attributes,
                                    })
                                }
                                pattern => {
                                    self.compiler.add_error(
                                        "syntax error",
                                        vec![Note::primary(
                                            pattern.span(),
                                            "expected an `instance` definition",
                                        )],
                                        "syntax-error",
                                    );

                                    None
                                }
                            }
                        }
                    }
                }
            },
            ast::Statement::Instance(statement) => {
                let id = self.compiler.new_constant_id_in(self.file);

                self.declarations
                    .instances
                    .insert(id, Declaration::unresolved(None, statement.span()));

                Some(StatementDeclaration {
                    span: statement.span(),
                    kind: StatementDeclarationKind::Instance(
                        id,
                        None,
                        (
                            statement.trait_span,
                            statement.trait_name,
                            statement.trait_scope.clone(),
                            &statement.trait_parameters,
                        ),
                        None,
                    ),
                    attributes: &statement.attributes,
                })
            }
            ast::Statement::TypeFunction(statement) => {
                let (parameters, bounds, _) =
                    self.lower_type_pattern(statement.pattern.as_ref().ok()?, ctx, None);

                match statement.value.as_deref().ok()? {
                    ast::Statement::Instance(statement) => {
                        let id = self.compiler.new_constant_id_in(self.file);

                        self.declarations
                            .instances
                            .insert(id, Declaration::unresolved(None, statement.span()));

                        Some(StatementDeclaration {
                            span: statement.span(),
                            kind: StatementDeclarationKind::Instance(
                                id,
                                Some((parameters, bounds)),
                                (
                                    statement.trait_span,
                                    statement.trait_name,
                                    statement.trait_scope.clone(),
                                    &statement.trait_parameters,
                                ),
                                None,
                            ),
                            attributes: &statement.attributes,
                        })
                    }
                    statement => {
                        self.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(
                                statement.span(),
                                "expected an `instance` declaration",
                            )],
                            "syntax-error",
                        );

                        None
                    }
                }
            }
            ast::Statement::Use(statement) => match statement.kind.as_ref().ok()? {
                ast::UseStatementKind::File(..) => None,
            },
            ast::Statement::Expression(statement) => Some(StatementDeclaration {
                span: statement.span(),
                kind: StatementDeclarationKind::Queued(QueuedStatement::Expression(Cow::Borrowed(
                    &statement.expression,
                ))),
                attributes: &statement.attributes,
            }),
        }
    }

    fn lower_expr(&mut self, expr: &ast::Expression<Analysis>, ctx: &Context) -> Expression {
        macro_rules! function_call {
            ($function:expr, $inputs:expr) => {{
                let inputs = $inputs;
                let len = inputs.len();

                inputs
                    .into_iter()
                    .enumerate()
                    .fold($function, |result, (index, next)| {
                        let mut ctx = ctx.clone();
                        ctx.caller_accepts_text = false;
                        if let ExpressionKind::Constant(id) = result.kind {
                            ctx.caller_accepts_text =
                                self.file_info.diagnostic_items.accepts_text.contains(&id);
                        }

                        let next = match next {
                            Ok(expr) => self.lower_expr(expr, &ctx),
                            Err(error) => Expression {
                                id: self.compiler.new_expression_id(ctx.owner),
                                span: error.span,
                                kind: ExpressionKind::error(&self.compiler),
                            },
                        };

                        Expression {
                            id: self.compiler.new_expression_id(ctx.owner),
                            span: SpanList::join(result.span, next.span),
                            kind: ExpressionKind::Call(
                                Box::new(result),
                                Box::new(next),
                                index + 1 == len,
                            ),
                        }
                    })
            }};
        }

        match expr {
            ast::Expression::Text(expr) => Expression {
                id: self.compiler.new_expression_id(ctx.owner),
                span: expr.span,
                kind: ExpressionKind::Text(expr.text.ignoring_escaped_underscores()),
            },
            ast::Expression::Number(expr) => Expression {
                id: self.compiler.new_expression_id(ctx.owner),
                span: expr.span,
                kind: ExpressionKind::Number(expr.number),
            },
            ast::Expression::Asset(expr) => Expression {
                id: self.compiler.new_expression_id(ctx.owner),
                span: expr.span,
                kind: ExpressionKind::Text(expr.raw),
            },
            ast::Expression::Name(expr) => {
                let name_scope = expr.scope_set.clone();
                match self.resolve_value(expr.span, expr.name, &name_scope) {
                    Ok(Some(value)) => Expression {
                        id: self.compiler.new_expression_id(ctx.owner),
                        span: expr.span,
                        kind: value,
                    },
                    Ok(None) => {
                        let (notes, fix) = self.diagnostic_notes_for_unresolved_name(
                            expr.span,
                            expr.name,
                            ctx,
                            AnyDeclaration::as_any,
                            &name_scope,
                        );

                        self.compiler.add_diagnostic(
                            self.compiler
                                .error(
                                    format!("cannot find `{}`", expr.name),
                                    std::iter::once(Note::primary(
                                        expr.span,
                                        "this name is not defined",
                                    ))
                                    .chain(notes)
                                    .collect(),
                                    "undefined-name",
                                )
                                .fix(fix),
                        );

                        Expression {
                            id: self.compiler.new_expression_id(ctx.owner),
                            span: expr.span,
                            kind: ExpressionKind::error(&self.compiler),
                        }
                    }
                    Err(candidates) => {
                        self.compiler.add_error(
                            format!("multiple definitions of `{}`", expr.name),
                            std::iter::once(Note::primary(
                                expr.span,
                                format!("`{}` could refer to...", expr.name),
                            ))
                            .chain(self.diagnostic_notes_for_ambiguous_name(candidates))
                            .collect(),
                            "",
                        );

                        Expression {
                            id: self.compiler.new_expression_id(ctx.owner),
                            span: expr.span,
                            kind: ExpressionKind::error(&self.compiler),
                        }
                    }
                }
            }
            ast::Expression::Block(expr) => {
                let statements = self.lower_statements(&expr.statements, ctx);

                Expression {
                    id: self.compiler.new_expression_id(ctx.owner),
                    span: expr.span,
                    kind: ExpressionKind::Block(statements, false),
                }
            }
            ast::Expression::Call(expr) => {
                let function = match expr.function.as_deref() {
                    Ok(expr) => expr,
                    Err(_) => {
                        return Expression {
                            id: self.compiler.new_expression_id(ctx.owner),
                            span: expr.span,
                            kind: ExpressionKind::error(&self.compiler),
                        };
                    }
                };

                match function {
                    ast::Expression::Text(function) => {
                        let (segments, trailing_segment) = segments(&function.text);

                        let inputs = expr
                            .inputs
                            .iter()
                            .map(|expr| match expr {
                                Ok(expr) => self.lower_expr(expr, ctx),
                                Err(error) => Expression {
                                    id: self.compiler.new_expression_id(ctx.owner),
                                    span: error.span,
                                    kind: ExpressionKind::error(&self.compiler),
                                },
                            })
                            .collect::<Vec<_>>();

                        if segments.len() != inputs.len() {
                            self.compiler.add_error(
                                "wrong number of inputs to placeholder text",
                                vec![Note::primary(
                                    function.span(),
                                    format!(
                                        "text contains {} placeholders, but {} inputs were provided",
                                        segments.len(),
                                        inputs.len()
                                    ),
                                )],
                                "",
                            );

                            return Expression {
                                id: self.compiler.new_expression_id(ctx.owner),
                                span: expr.span,
                                kind: ExpressionKind::error(&self.compiler),
                            };
                        }

                        Expression {
                            id: self.compiler.new_expression_id(ctx.owner),
                            span: expr.span,
                            kind: ExpressionKind::Format(
                                segments
                                    .into_iter()
                                    .map(InternedString::new)
                                    .zip(inputs)
                                    .collect(),
                                trailing_segment.map(InternedString::new),
                            ),
                        }
                    }
                    ast::Expression::Name(ty_name) => {
                        let input = match expr.inputs.first() {
                            Some(input) => input,
                            None => {
                                self.compiler.add_error(
                                    "function received no input",
                                    vec![Note::primary(
                                        function.span(),
                                        "try providing an input to this function",
                                    )],
                                    "",
                                );

                                return Expression {
                                    id: self.compiler.new_expression_id(ctx.owner),
                                    span: expr.span,
                                    kind: ExpressionKind::error(&self.compiler),
                                };
                            }
                        };

                        let input = match input {
                            Ok(expr) => expr,
                            Err(error) => {
                                return Expression {
                                    id: self.compiler.new_expression_id(ctx.owner),
                                    span: error.span,
                                    kind: ExpressionKind::error(&self.compiler),
                                };
                            }
                        };

                        let ty_name_scope = ty_name.scope_set.clone();

                        if let Ok(Some(id)) = self.get(
                            ty_name.name,
                            function.span(),
                            AnyDeclaration::as_type,
                            &ty_name_scope,
                        ) {
                            self.declarations
                                .types
                                .get_mut(&id)
                                .unwrap()
                                .uses
                                .insert(function.span());

                            match input {
                                ast::Expression::Block(block) => {
                                    if expr.inputs.len() > 1 {
                                        self.compiler.add_error(
                                            "too many inputs in structure instantiation",
                                            vec![Note::primary(
                                                expr.span,
                                                "this structure requires a single block containing its fields",
                                            )],
                                            "syntax-error",
                                        );
                                    }

                                    let fields =
                                        self.extract_fields(block, ctx).unwrap_or_default();

                                    let ty = self
                                        .declarations
                                        .types
                                        .get(&id)
                                        .unwrap()
                                        .value
                                        .as_ref()
                                        .unwrap();

                                    if !matches!(ty.kind, TypeDeclarationKind::Structure(_, _)) {
                                        self.compiler.add_error(
                                            "only structures may be instantiated like this",
                                            vec![Note::primary(function.span(), "not a structure")],
                                            "syntax-error",
                                        );

                                        return Expression {
                                            id: self.compiler.new_expression_id(ctx.owner),
                                            span: expr.span,
                                            kind: ExpressionKind::error(&self.compiler),
                                        };
                                    }

                                    Expression {
                                        id: self.compiler.new_expression_id(ctx.owner),
                                        span: expr.span,
                                        kind: ExpressionKind::Instantiate(id, fields),
                                    }
                                }
                                ast::Expression::Name(name) => {
                                    let ty_decl = self.declarations.types.get(&id).unwrap();

                                    let (variant_types, variants) =
                                        match &ty_decl.value.as_ref().unwrap().kind {
                                            TypeDeclarationKind::Enumeration(types, variants) => {
                                                (types, variants)
                                            }
                                            _ => {
                                                self.compiler.add_error(
                                                "only enumerations may be instantiated like this",
                                                vec![Note::primary(
                                                    function.span(),
                                                    "not an enumeration",
                                                )],
                                                "syntax-error",
                                            );

                                                return Expression {
                                                    id: self.compiler.new_expression_id(ctx.owner),
                                                    span: expr.span,
                                                    kind: ExpressionKind::error(&self.compiler),
                                                };
                                            }
                                        };

                                    let index = match variants.get(&name.name) {
                                        Some(index) => *index,
                                        None => {
                                            self.compiler.add_error(
                                                format!(
                                                    "enumeration `{}` does not declare a variant named `{}`",
                                                    ty_name.name,
                                                    name.name
                                                ),
                                                vec![Note::primary(name.span, "no such variant")],
                                                "undefined-name",
                                            );

                                            return Expression {
                                                id: self.compiler.new_expression_id(ctx.owner),
                                                span: expr.span,
                                                kind: ExpressionKind::error(&self.compiler),
                                            };
                                        }
                                    };

                                    function_call!(
                                        Expression {
                                            id: self.compiler.new_expression_id(ctx.owner),
                                            span: expr.span,
                                            kind: ExpressionKind::Constant(
                                                variant_types[index.into_inner()].constructor
                                            )
                                        },
                                        expr.inputs.iter().skip(1)
                                    )
                                }
                                _ => {
                                    function_call!(self.lower_expr(function, ctx), &expr.inputs)
                                }
                            }
                        } else if let Ok(Some(id)) = self.get(
                            ty_name.name,
                            function.span(),
                            AnyDeclaration::as_type_parameter,
                            &ty_name_scope,
                        ) {
                            self.declarations
                                .type_parameters
                                .get_mut(&id)
                                .unwrap()
                                .uses
                                .insert(function.span());

                            self.compiler.add_error(
                                "cannot instantiate type parameter",
                                vec![Note::primary(
                                    function.span(),
                                    "the actual type this represents is not known here",
                                )],
                                "instantiate-type-parameter",
                            );

                            Expression {
                                id: self.compiler.new_expression_id(ctx.owner),
                                span: expr.span,
                                kind: ExpressionKind::error(&self.compiler),
                            }
                        } else if let Ok(Some(id)) = self.get(
                            ty_name.name,
                            function.span(),
                            AnyDeclaration::as_builtin_type,
                            &ty_name_scope,
                        ) {
                            self.declarations
                                .builtin_types
                                .get_mut(&id)
                                .unwrap()
                                .uses
                                .insert(function.span());

                            self.compiler.add_error(
                                "cannot instantiate built-in type",
                                vec![Note::primary(
                                    function.span(),
                                    "try using a literal or built-in function instead",
                                )],
                                "instantiate-builtin",
                            );

                            Expression {
                                id: self.compiler.new_expression_id(ctx.owner),
                                span: expr.span,
                                kind: ExpressionKind::error(&self.compiler),
                            }
                        } else if let Err(candidates) = self.get(
                            ty_name.name,
                            function.span(),
                            |decl| {
                                (decl.as_type().is_some()
                                    || decl.as_type_parameter().is_some()
                                    || decl.as_builtin_type().is_some())
                                .then_some(decl)
                            },
                            &ty_name_scope,
                        ) {
                            self.compiler.add_error(
                                format!("multiple definitions of `{}`", ty_name.name),
                                std::iter::once(Note::primary(
                                    function.span(),
                                    format!("`{}` could refer to...", ty_name.name),
                                ))
                                .chain(
                                    self.diagnostic_notes_for_ambiguous_name(
                                        candidates
                                            .into_iter()
                                            .map(|candidate| self.span_of(candidate))
                                            .collect(),
                                    ),
                                )
                                .collect(),
                                "",
                            );

                            return Expression {
                                id: self.compiler.new_expression_id(ctx.owner),
                                span: expr.span,
                                kind: ExpressionKind::error(&self.compiler),
                            };
                        } else {
                            function_call!(self.lower_expr(function, ctx), &expr.inputs)
                        }
                    }
                    _ => function_call!(self.lower_expr(function, ctx), &expr.inputs),
                }
            }
            ast::Expression::Function(expr) => {
                let pattern = match &expr.pattern {
                    Ok(pattern) => self.lower_pattern(pattern, ctx),
                    Err(error) => Pattern {
                        span: error.span,
                        kind: PatternKind::error(&self.compiler),
                    },
                };

                let mut ctx = ctx.clone();
                ctx.in_function = true;

                let mut body = match &expr.body {
                    Ok(expr) => self.lower_expr(expr, &ctx),
                    Err(error) => Expression {
                        id: self.compiler.new_expression_id(ctx.owner),
                        span: error.span,
                        kind: ExpressionKind::error(&self.compiler),
                    },
                };

                let captures = self.generate_capture_list(Some(&pattern), &mut body);

                Expression {
                    id: self.compiler.new_expression_id(ctx.owner),
                    span: expr.span(),
                    kind: ExpressionKind::Function(pattern, Box::new(body), captures),
                }
            }
            ast::Expression::When(expr) => {
                let input = match &expr.input {
                    Ok(expr) => self.lower_expr(expr, ctx),
                    Err(error) => Expression {
                        id: self.compiler.new_expression_id(ctx.owner),
                        span: error.span,
                        kind: ExpressionKind::error(&self.compiler),
                    },
                };

                let ast::WhenBody::Block(body) = match &expr.body {
                    Ok(body) => body,
                    Err(error) => {
                        return Expression {
                            id: self.compiler.new_expression_id(ctx.owner),
                            span: error.span,
                            kind: ExpressionKind::error(&self.compiler),
                        };
                    }
                };

                Expression {
                    id: self.compiler.new_expression_id(ctx.owner),
                    span: expr.span(),
                    kind: ExpressionKind::When(
                        Box::new(input),
                        body.arms
                            .iter()
                            .filter_map(|arm| {
                                let ast::WhenArm::Function(arm) = arm.as_ref().ok()?;

                                let (pattern, guard) = match &arm.pattern {
                                    Ok(pattern) => match pattern {
                                        ast::WhenPattern::Where(pattern) => {
                                            let inner_pattern = match pattern.pattern.as_deref() {
                                                Ok(value) => self.lower_pattern(value, ctx),
                                                Err(error) => Pattern {
                                                    span: error.span,
                                                    kind: PatternKind::error(&self.compiler),
                                                },
                                            };

                                            let condition = match pattern.condition.as_deref() {
                                                Ok(value) => self.lower_expr(value, ctx),
                                                Err(error) => Expression {
                                                    id: self.compiler.new_expression_id(ctx.owner),
                                                    span: error.span,
                                                    kind: ExpressionKind::error(&self.compiler),
                                                },
                                            };

                                            (inner_pattern, Some(condition))
                                        }
                                        ast::WhenPattern::Pattern(pattern) => {
                                            (self.lower_pattern(&pattern.pattern, ctx), None)
                                        }
                                    },
                                    Err(error) => (
                                        Pattern {
                                            span: error.span,
                                            kind: PatternKind::error(&self.compiler),
                                        },
                                        None,
                                    ),
                                };

                                let body = match &arm.body {
                                    Ok(expr) => self.lower_expr(expr, ctx),
                                    Err(error) => Expression {
                                        id: self.compiler.new_expression_id(ctx.owner),
                                        span: error.span,
                                        kind: ExpressionKind::error(&self.compiler),
                                    },
                                };

                                Some(Arm {
                                    span: arm.span(),
                                    pattern,
                                    guard,
                                    body,
                                })
                            })
                            .collect(),
                    ),
                }
            }
            ast::Expression::External(expr) => {
                let inputs = expr
                    .inputs
                    .iter()
                    .map(|expr| match expr {
                        Ok(expr) => self.lower_expr(expr, ctx),
                        Err(error) => Expression {
                            id: self.compiler.new_expression_id(ctx.owner),
                            span: error.span,
                            kind: ExpressionKind::error(&self.compiler),
                        },
                    })
                    .collect::<Vec<_>>();

                Expression {
                    id: self.compiler.new_expression_id(ctx.owner),
                    span: expr.span(),
                    kind: ExpressionKind::External(expr.path, expr.name, inputs),
                }
            }
            ast::Expression::Intrinsic(expr) => {
                let func = match expr.name.as_str().parse::<Intrinsic>() {
                    Ok(func) => func,
                    Err(_) => {
                        self.compiler.add_error(
                            "unknown runtime function",
                            vec![Note::primary(
                                expr.span(),
                                "check the Wipple source code for the latest list of runtime functions"
                            )],
                            "",
                        );

                        return Expression {
                            id: self.compiler.new_expression_id(ctx.owner),
                            span: expr.span(),
                            kind: ExpressionKind::error(&self.compiler),
                        };
                    }
                };

                let inputs = expr
                    .inputs
                    .iter()
                    .map(|expr| match expr {
                        Ok(expr) => self.lower_expr(expr, ctx),
                        Err(error) => Expression {
                            id: self.compiler.new_expression_id(ctx.owner),
                            span: error.span,
                            kind: ExpressionKind::error(&self.compiler),
                        },
                    })
                    .collect::<Vec<_>>();

                Expression {
                    id: self.compiler.new_expression_id(ctx.owner),
                    span: expr.span(),
                    kind: ExpressionKind::Intrinsic(func, inputs),
                }
            }
            ast::Expression::Plugin(expr) => {
                let inputs = expr
                    .inputs
                    .iter()
                    .map(|expr| match expr {
                        Ok(expr) => self.lower_expr(expr, ctx),
                        Err(error) => Expression {
                            id: self.compiler.new_expression_id(ctx.owner),
                            span: error.span,
                            kind: ExpressionKind::error(&self.compiler),
                        },
                    })
                    .collect::<Vec<_>>();

                Expression {
                    id: self.compiler.new_expression_id(ctx.owner),
                    span: expr.span(),
                    kind: ExpressionKind::Plugin(expr.path, expr.name, inputs),
                }
            }
            ast::Expression::Annotate(expr) => {
                let value = match &expr.expr {
                    Ok(expr) => self.lower_expr(expr, ctx),
                    Err(error) => Expression {
                        id: self.compiler.new_expression_id(ctx.owner),
                        span: error.span,
                        kind: ExpressionKind::error(&self.compiler),
                    },
                };

                let ty = match &expr.ty {
                    Ok(ty) => self.lower_type(ty, ctx, None),
                    Err(error) => TypeAnnotation {
                        span: error.span,
                        kind: TypeAnnotationKind::error(&self.compiler),
                    },
                };

                Expression {
                    id: self.compiler.new_expression_id(ctx.owner),
                    span: expr.span(),
                    kind: ExpressionKind::Annotate(Box::new(value), ty),
                }
            }
            ast::Expression::Tuple(expr) => Expression {
                id: self.compiler.new_expression_id(ctx.owner),
                span: expr.span(),
                kind: ExpressionKind::Tuple(
                    expr.exprs
                        .iter()
                        .map(|expr| match expr {
                            Ok(expr) => self.lower_expr(expr, ctx),
                            Err(error) => Expression {
                                id: self.compiler.new_expression_id(ctx.owner),
                                span: error.span,
                                kind: ExpressionKind::error(&self.compiler),
                            },
                        })
                        .collect(),
                ),
            },
            ast::Expression::Unit(expr) => Expression {
                id: self.compiler.new_expression_id(ctx.owner),
                span: expr.span(),
                kind: ExpressionKind::Tuple(Vec::new()),
            },
            ast::Expression::With(expr) => {
                let (constant, value) = match &expr.clause {
                    Ok(ast::WithClause::Assign(clause)) => {
                        let constant = match clause.name {
                            Ok((span, name, ref scope)) => {
                                match self.get(name, span, AnyDeclaration::as_constant, scope) {
                                    Ok(Some((id, _))) => Some(id),
                                    Ok(_) => {
                                        let (notes, fix) = self
                                            .diagnostic_notes_for_unresolved_name(
                                                span,
                                                name,
                                                ctx,
                                                AnyDeclaration::as_constant,
                                                scope,
                                            );

                                        self.compiler.add_diagnostic(
                                            self.compiler
                                                .error(
                                                    format!("cannot find constant `{name}`"),
                                                    std::iter::once(Note::primary(
                                                        span,
                                                        "no such constant",
                                                    ))
                                                    .chain(notes)
                                                    .collect(),
                                                    "undefined-name",
                                                )
                                                .fix(fix),
                                        );

                                        None
                                    }
                                    Err(candidates) => {
                                        self.compiler.add_error(
                                            format!("multiple definitions of constant `{}`", name),
                                            std::iter::once(Note::primary(
                                                span,
                                                format!("`{}` could refer to...", name),
                                            ))
                                            .chain(
                                                self.diagnostic_notes_for_ambiguous_name(
                                                    candidates
                                                        .into_iter()
                                                        .map(|(candidate, _)| {
                                                            self.declarations
                                                                .constants
                                                                .get(&candidate)
                                                                .unwrap()
                                                                .span
                                                        })
                                                        .collect(),
                                                ),
                                            )
                                            .collect(),
                                            "",
                                        );

                                        None
                                    }
                                }
                            }
                            Err(_) => None,
                        };

                        let value = match &clause.value {
                            Ok(expr) => self.lower_expr(expr, ctx),
                            Err(error) => Expression {
                                id: self.compiler.new_expression_id(ctx.owner),
                                span: error.span,
                                kind: ExpressionKind::error(&self.compiler),
                            },
                        };

                        (constant, value)
                    }
                    Err(error) => (
                        None,
                        Expression {
                            id: self.compiler.new_expression_id(ctx.owner),
                            span: error.span,
                            kind: ExpressionKind::error(&self.compiler),
                        },
                    ),
                };

                let body = match &expr.body {
                    Ok(expr) => self.lower_expr(expr, ctx),
                    Err(error) => Expression {
                        id: self.compiler.new_expression_id(ctx.owner),
                        span: error.span,
                        kind: ExpressionKind::error(&self.compiler),
                    },
                };

                Expression {
                    id: self.compiler.new_expression_id(ctx.owner),
                    span: expr.span(),
                    kind: ExpressionKind::With((constant, Box::new(value)), Box::new(body)),
                }
            }
            ast::Expression::End(expr) => {
                let value = match &expr.value {
                    Ok(expr) => self.lower_expr(expr, ctx),
                    Err(error) => Expression {
                        id: self.compiler.new_expression_id(ctx.owner),
                        span: error.span,
                        kind: ExpressionKind::error(&self.compiler),
                    },
                };

                if !ctx.in_function {
                    self.compiler.add_error(
                        "cannot use `end` outside a function",
                        vec![Note::primary(expr.span(), "not allowed here")],
                        "end-outside-function",
                    );

                    return Expression {
                        id: self.compiler.new_expression_id(ctx.owner),
                        span: expr.span(),
                        kind: ExpressionKind::error(&self.compiler),
                    };
                }

                Expression {
                    id: self.compiler.new_expression_id(ctx.owner),
                    span: expr.span(),
                    kind: ExpressionKind::End(Box::new(value)),
                }
            }
            ast::Expression::Where(expr) => {
                let value = match &expr.value {
                    Ok(expr) => self.lower_expr(expr, ctx),
                    Err(error) => Expression {
                        id: self.compiler.new_expression_id(ctx.owner),
                        span: error.span,
                        kind: ExpressionKind::error(&self.compiler),
                    },
                };

                let fields = match expr.fields.as_deref() {
                    Ok(ast::Expression::Block(block)) => {
                        self.extract_fields(block, ctx).unwrap_or_default()
                    }
                    Ok(fields) => {
                        self.compiler.add_error(
                            "`where` expects a block",
                            vec![Note::primary(fields.span(), "expected a block here")],
                            "syntax-error",
                        );

                        return Expression {
                            id: self.compiler.new_expression_id(ctx.owner),
                            span: expr.span(),
                            kind: ExpressionKind::error(&self.compiler),
                        };
                    }
                    Err(_) => {
                        return Expression {
                            id: self.compiler.new_expression_id(ctx.owner),
                            span: expr.span(),
                            kind: ExpressionKind::error(&self.compiler),
                        };
                    }
                };

                Expression {
                    id: self.compiler.new_expression_id(ctx.owner),
                    span: expr.span(),
                    kind: ExpressionKind::Extend(Box::new(value), fields),
                }
            }

            ast::Expression::Semantics(expr) => {
                let semantics = match expr.semantics_text.as_str().parse::<Semantics>() {
                    Ok(func) => func,
                    Err(_) => {
                        self.compiler.add_error(
                            "unknown semantics",
                            vec![Note::primary(
                                expr.span(),
                                "check the Wipple source code for the latest list of semantics",
                            )],
                            "",
                        );

                        return Expression {
                            id: self.compiler.new_expression_id(ctx.owner),
                            span: expr.span(),
                            kind: ExpressionKind::error(&self.compiler),
                        };
                    }
                };

                let value = match &expr.value {
                    Ok(expr) => self.lower_expr(expr, ctx),
                    Err(error) => Expression {
                        id: self.compiler.new_expression_id(ctx.owner),
                        span: error.span,
                        kind: ExpressionKind::error(&self.compiler),
                    },
                };

                Expression {
                    id: self.compiler.new_expression_id(ctx.owner),
                    span: expr.span(),
                    kind: ExpressionKind::Semantics(semantics, Box::new(value)),
                }
            }
        }
    }

    fn extract_fields(
        &mut self,
        block: &ast::BlockExpression<Analysis>,
        ctx: &Context,
    ) -> Option<Vec<((SpanList, InternedString), Expression)>> {
        if block.statements.len() == 1 {
            let statement = match block.statements.last().unwrap() {
                Ok(statement) => statement,
                Err(_) => return None,
            };

            if let ast::Statement::Expression(ast::ExpressionStatement {
                expression: ast::Expression::Call(expr),
                ..
            }) = statement
            {
                if let Some(fields) = std::iter::once(expr.function.clone().map(|expr| *expr))
                    .chain(expr.inputs.clone())
                    .map(|expr| match expr {
                        Ok(ast::Expression::Name(ref name_expr)) => {
                            Some(((name_expr.span, name_expr.name), expr.clone()))
                        }
                        _ => None,
                    })
                    .collect::<Option<Vec<_>>>()
                {
                    return Some(
                        fields
                            .into_iter()
                            .filter_map(|(name, expr)| {
                                Some((name, self.lower_expr(&expr.ok()?, ctx)))
                            })
                            .collect(),
                    );
                }
            };
        }

        let fields = block
            .statements
            .iter()
            .filter_map(|s| {
                Some(match s.as_ref().ok()? {
                    ast::Statement::Assign(statement) => match statement.pattern.as_ref().ok()? {
                        ast::AssignmentPattern::Pattern(ast::PatternAssignmentPattern {
                            pattern: ast::Pattern::Name(name),
                        }) => {
                            let value = match statement.value.as_ref().ok()? {
                                ast::AssignmentValue::Expression(value) => &value.expression,
                                value => {
                                    self.compiler.add_error(
                                        "syntax error",
                                        vec![Note::primary(value.span(), "expected expression")],
                                        "syntax-error",
                                    );

                                    return None;
                                }
                            };

                            ((name.span, name.name), value.clone())
                        }
                        pattern => {
                            self.compiler.add_error(
                                "structure instantiation may not contain complex patterns",
                                vec![Note::primary(
                                    pattern.span(),
                                    "try splitting this pattern into multiple names",
                                )],
                                "syntax-error",
                            );

                            return None;
                        }
                    },
                    ast::Statement::Expression(ast::ExpressionStatement {
                        expression: expr @ ast::Expression::Name(name),
                        ..
                    }) => ((name.span, name.name), expr.clone()),
                    // TODO: 'use' inside instantiation
                    _ => {
                        self.compiler.add_error(
                            "structure instantiation may not contain executable statements",
                            vec![Note::primary(block.span, "try removing this")],
                            "syntax-error",
                        );

                        return None;
                    }
                })
            })
            .collect::<Vec<_>>()
            .into_iter()
            .map(|(name, value)| (name, self.lower_expr(&value, ctx)))
            .collect();

        Some(fields)
    }

    fn lower_pattern(&mut self, pattern: &ast::Pattern<Analysis>, ctx: &Context) -> Pattern {
        match pattern {
            ast::Pattern::Wildcard(pattern) => Pattern {
                span: pattern.span,
                kind: PatternKind::Wildcard,
            },
            ast::Pattern::Number(pattern) => Pattern {
                span: pattern.span,
                kind: PatternKind::Number(pattern.number),
            },
            ast::Pattern::Text(pattern) => Pattern {
                span: pattern.span,
                kind: PatternKind::Text(pattern.text.ignoring_escaped_underscores()),
            },
            ast::Pattern::Name(pattern) => {
                match self.peek(pattern.name, AnyDeclaration::as_constant, &pattern.scope) {
                    Ok(Some((id, Some((ty, variant))))) => {
                        self.declarations
                            .constants
                            .get_mut(&id)
                            .unwrap()
                            .uses
                            .insert(pattern.span);

                        Pattern {
                            span: pattern.span,
                            kind: PatternKind::Variant(ty, variant, Vec::new()),
                        }
                    }
                    Ok(_) => {
                        let var = self.compiler.new_variable_id(ctx.owner);

                        self.insert(pattern.name, AnyDeclaration::Variable(var), &pattern.scope);

                        self.declarations.variables.insert(
                            var,
                            Declaration::resolved(
                                Some(pattern.name),
                                pattern.span,
                                VariableDeclaration,
                            ),
                        );

                        Pattern {
                            span: pattern.span,
                            kind: PatternKind::Variable(var),
                        }
                    }
                    Err(candidates) => {
                        self.compiler.add_error(
                            format!("multiple definitions of variant `{}`", pattern.name),
                            std::iter::once(Note::primary(
                                pattern.span,
                                format!("`{}` could refer to...", pattern.name),
                            ))
                            .chain(
                                self.diagnostic_notes_for_ambiguous_name(
                                    candidates
                                        .into_iter()
                                        .map(|(candidate, _)| {
                                            self.declarations
                                                .constants
                                                .get(&candidate)
                                                .unwrap()
                                                .span
                                        })
                                        .collect(),
                                ),
                            )
                            .collect(),
                            "",
                        );

                        Pattern {
                            span: pattern.span,
                            kind: PatternKind::error(&self.compiler),
                        }
                    }
                }
            }
            ast::Pattern::Destructure(pattern) => Pattern {
                span: pattern.span,
                kind: PatternKind::Destructure(
                    pattern
                        .destructurings
                        .iter()
                        .filter_map(|destructuring| match destructuring.as_ref().ok()? {
                            ast::Destructuring::Assign(destructuring) => {
                                let pattern = match &destructuring.pattern {
                                    Ok(pattern) => self.lower_pattern(pattern, ctx),
                                    Err(error) => Pattern {
                                        span: error.span,
                                        kind: PatternKind::error(&self.compiler),
                                    },
                                };

                                Some(vec![(destructuring.name, pattern)])
                            }
                            ast::Destructuring::Name(destructuring) => Some(vec![(
                                destructuring.name,
                                self.lower_pattern(
                                    &ast::Pattern::Name(ast::NamePattern {
                                        span: destructuring.span,
                                        name: destructuring.name,
                                        scope: destructuring.scope.clone(),
                                    }),
                                    ctx,
                                ),
                            )]),
                            ast::Destructuring::List(destructuring) => Some(
                                destructuring
                                    .names
                                    .iter()
                                    .filter_map(|destructuring| {
                                        let destructuring = destructuring.as_ref().ok()?;

                                        Some((
                                            destructuring.name,
                                            self.lower_pattern(
                                                &ast::Pattern::Name(ast::NamePattern {
                                                    span: destructuring.span,
                                                    name: destructuring.name,
                                                    scope: destructuring.scope.clone(),
                                                }),
                                                ctx,
                                            ),
                                        ))
                                    })
                                    .collect(),
                            ),
                        })
                        .flatten()
                        .collect(),
                ),
            },
            ast::Pattern::Variant(pattern) => {
                let mut values = pattern.values.iter();

                if let Ok(Some(ty)) = self.get(
                    pattern.name,
                    pattern.name_span,
                    AnyDeclaration::as_type,
                    &pattern.name_scope_set,
                ) {
                    self.declarations
                        .types
                        .get_mut(&ty)
                        .unwrap()
                        .uses
                        .insert(pattern.name_span);

                    let variants = match &self
                        .declarations
                        .types
                        .get(&ty)
                        .unwrap()
                        .value
                        .as_ref()
                        .unwrap()
                        .kind
                    {
                        TypeDeclarationKind::Enumeration(_, variants) => variants,
                        _ => {
                            self.compiler.add_error(
                                format!("cannot use `{}` in pattern", pattern.name),
                                vec![Note::primary(
                                    pattern.name_span,
                                    "only enumeration types may be used in patterns",
                                )],
                                "syntax-error",
                            );

                            return Pattern {
                                span: pattern.span,
                                kind: PatternKind::error(&self.compiler),
                            };
                        }
                    };

                    let second = match values.next() {
                        Some(value) => match value {
                            Ok(value) => value,
                            Err(_) => {
                                return Pattern {
                                    span: pattern.span,
                                    kind: PatternKind::error(&self.compiler),
                                };
                            }
                        },
                        None => {
                            self.compiler.add_error(
                                "incomplete pattern",
                                vec![Note::primary(
                                    pattern.name_span,
                                    "expected a variant name after this",
                                )],
                                "syntax-error",
                            );

                            return Pattern {
                                span: pattern.span,
                                kind: PatternKind::error(&self.compiler),
                            };
                        }
                    };

                    let variant_name = match second {
                        ast::Pattern::Name(pattern) => pattern.name,
                        _ => {
                            self.compiler.add_error(
                                "invalid pattern",
                                vec![Note::primary(second.span(), "expected a variant name here")],
                                "syntax-error",
                            );

                            return Pattern {
                                span: pattern.span,
                                kind: PatternKind::error(&self.compiler),
                            };
                        }
                    };

                    let variant = match variants.get(&variant_name) {
                        Some(variant) => *variant,
                        None => {
                            self.compiler.add_error(
                                format!(
                                    "enumeration `{}` does not declare a variant named `{}`",
                                    pattern.name, variant_name
                                ),
                                vec![Note::primary(second.span(), "no such variant")],
                                "undefined-name",
                            );

                            return Pattern {
                                span: pattern.span,
                                kind: PatternKind::error(&self.compiler),
                            };
                        }
                    };

                    Pattern {
                        span: pattern.span,
                        kind: PatternKind::Variant(
                            ty,
                            variant,
                            values
                                .map(|value| match value {
                                    Ok(value) => self.lower_pattern(value, ctx),
                                    Err(error) => Pattern {
                                        span: error.span,
                                        kind: PatternKind::error(&self.compiler),
                                    },
                                })
                                .collect(),
                        ),
                    }
                } else if let Ok(Some((id, Some((ty, variant))))) = self.get(
                    pattern.name,
                    pattern.name_span,
                    AnyDeclaration::as_constant,
                    &pattern.name_scope_set,
                ) {
                    self.declarations
                        .constants
                        .get_mut(&id)
                        .unwrap()
                        .uses
                        .insert(pattern.name_span);

                    Pattern {
                        span: pattern.span,
                        kind: PatternKind::Variant(
                            ty,
                            variant,
                            values
                                .map(|value| match value {
                                    Ok(value) => self.lower_pattern(value, ctx),
                                    Err(error) => Pattern {
                                        span: error.span,
                                        kind: PatternKind::error(&self.compiler),
                                    },
                                })
                                .collect(),
                        ),
                    }
                } else {
                    let (notes, fix) = self.diagnostic_notes_for_unresolved_name_with(
                        pattern.name_span,
                        pattern.name,
                        ctx,
                        |decl| {
                            decl.as_constant()
                                .map_or(false, |(_, variant)| variant.is_some())
                                || decl.as_type().is_some()
                        },
                        &pattern.name_scope_set,
                    );

                    self.compiler.add_diagnostic(
                        self.compiler
                            .error(
                                format!("cannot find pattern `{}`", pattern.name),
                                std::iter::once(Note::primary(
                                    pattern.span,
                                    "no such type or variant",
                                ))
                                .chain(notes)
                                .collect(),
                                "undefined-name",
                            )
                            .fix(fix),
                    );

                    Pattern {
                        span: pattern.span,
                        kind: PatternKind::error(&self.compiler),
                    }
                }
            }
            ast::Pattern::Annotate(pattern) => {
                let inner_pattern = match pattern.pattern.as_deref() {
                    Ok(value) => self.lower_pattern(value, ctx),
                    Err(error) => Pattern {
                        span: error.span,
                        kind: PatternKind::error(&self.compiler),
                    },
                };

                let ty = match &pattern.ty {
                    Ok(ty) => self.lower_type(ty, ctx, None),
                    Err(error) => TypeAnnotation {
                        span: error.span,
                        kind: TypeAnnotationKind::error(&self.compiler),
                    },
                };

                Pattern {
                    span: pattern.span(),
                    kind: PatternKind::Annotate(Box::new(inner_pattern), ty),
                }
            }
            ast::Pattern::Or(pattern) => {
                let lhs = match pattern.left.as_deref() {
                    Ok(value) => self.lower_pattern(value, ctx),
                    Err(error) => Pattern {
                        span: error.span,
                        kind: PatternKind::error(&self.compiler),
                    },
                };

                let rhs = match pattern.right.as_deref() {
                    Ok(value) => self.lower_pattern(value, ctx),
                    Err(error) => Pattern {
                        span: error.span,
                        kind: PatternKind::error(&self.compiler),
                    },
                };

                Pattern {
                    span: pattern.span(),
                    kind: PatternKind::Or(Box::new(lhs), Box::new(rhs)),
                }
            }
            ast::Pattern::Tuple(pattern) => Pattern {
                span: pattern.span(),
                kind: PatternKind::Tuple(
                    pattern
                        .patterns
                        .iter()
                        .map(|pattern| match pattern {
                            Ok(pattern) => self.lower_pattern(pattern, ctx),
                            Err(error) => Pattern {
                                span: error.span,
                                kind: PatternKind::error(&self.compiler),
                            },
                        })
                        .collect(),
                ),
            },
            ast::Pattern::Unit(pattern) => Pattern {
                span: pattern.span(),
                kind: PatternKind::Tuple(Vec::new()),
            },
        }
    }

    fn lower_type(
        &mut self,
        ty: &ast::Type<Analysis>,
        ctx: &Context,
        fallback_scope: Option<&ScopeSet>,
    ) -> TypeAnnotation {
        match ty {
            ast::Type::Function(ty) => {
                let input = match &ty.input {
                    Ok(ty) => self.lower_type(ty, ctx, fallback_scope),
                    Err(error) => TypeAnnotation {
                        span: error.span,
                        kind: TypeAnnotationKind::Error(error.trace.clone()),
                    },
                };

                let output = match &ty.output {
                    Ok(ty) => self.lower_type(ty, ctx, fallback_scope),
                    Err(error) => TypeAnnotation {
                        span: error.span,
                        kind: TypeAnnotationKind::Error(error.trace.clone()),
                    },
                };

                TypeAnnotation {
                    span: ty.span(),
                    kind: TypeAnnotationKind::Function(Box::new(input), Box::new(output)),
                }
            }
            ast::Type::Tuple(ty) => TypeAnnotation {
                span: ty.span(),
                kind: TypeAnnotationKind::Tuple(
                    ty.tys
                        .iter()
                        .map(|ty| match ty {
                            Ok(ty) => self.lower_type(ty, ctx, fallback_scope),
                            Err(error) => TypeAnnotation {
                                span: error.span,
                                kind: TypeAnnotationKind::Error(error.trace.clone()),
                            },
                        })
                        .collect(),
                ),
            },
            ast::Type::Placeholder(ty) => TypeAnnotation {
                span: ty.span(),
                kind: TypeAnnotationKind::Placeholder,
            },
            ast::Type::Unit(ty) => TypeAnnotation {
                span: ty.span(),
                kind: TypeAnnotationKind::Tuple(Vec::new()),
            },
            ast::Type::Named(ty) => {
                let parameters = ty
                    .parameters
                    .iter()
                    .map(|ty| match ty {
                        Ok(ty) => self.lower_type(ty, ctx, fallback_scope),
                        Err(error) => TypeAnnotation {
                            span: error.span,
                            kind: TypeAnnotationKind::Error(error.trace.clone()),
                        },
                    })
                    .collect();

                if let Ok(Some(id)) = self.get(
                    ty.name,
                    ty.span,
                    AnyDeclaration::as_type,
                    &ty.name_scope_set,
                ) {
                    self.declarations
                        .types
                        .get_mut(&id)
                        .unwrap()
                        .uses
                        .insert(ty.span);

                    TypeAnnotation {
                        span: ty.span,
                        kind: TypeAnnotationKind::Named(id, parameters),
                    }
                } else if let Some(param) = self
                    .get(
                        ty.name,
                        ty.span,
                        AnyDeclaration::as_type_parameter,
                        &ty.name_scope_set,
                    )
                    .ok()
                    .flatten()
                    .or_else(|| {
                        fallback_scope.and_then(|fallback_scope| {
                            self.get(
                                ty.name,
                                ty.span,
                                AnyDeclaration::as_type_parameter,
                                fallback_scope,
                            )
                            .ok()
                            .flatten()
                        })
                    })
                {
                    self.declarations
                        .type_parameters
                        .get_mut(&param)
                        .unwrap()
                        .uses
                        .insert(ty.span);

                    if !parameters.is_empty() {
                        // TODO: Higher-kinded types
                        self.compiler.add_error(
                            "higher-kinded types are not yet supported",
                            vec![Note::primary(
                                ty.span,
                                "try writing this on its own, with no parameters",
                            )],
                            "syntax-error",
                        );
                    }

                    TypeAnnotation {
                        span: ty.span,
                        kind: TypeAnnotationKind::Parameter(param),
                    }
                } else if let Ok(Some(builtin)) = self.get(
                    ty.name,
                    ty.span,
                    AnyDeclaration::as_builtin_type,
                    &ty.name_scope_set,
                ) {
                    self.declarations
                        .builtin_types
                        .get_mut(&builtin)
                        .unwrap()
                        .uses
                        .insert(ty.span);

                    TypeAnnotation {
                        span: ty.span,
                        kind: TypeAnnotationKind::Builtin(builtin, parameters),
                    }
                } else if let Err(candidates) = self.get(
                    ty.name,
                    ty.span,
                    |decl| {
                        (decl.as_type().is_some()
                            || decl.as_type_parameter().is_some()
                            || decl.as_builtin_type().is_some())
                        .then_some(decl)
                    },
                    &ty.name_scope_set,
                ) {
                    self.compiler.add_error(
                        format!("multiple definitions of `{}`", ty.name),
                        std::iter::once(Note::primary(
                            ty.span,
                            format!("`{}` could refer to...", ty.name),
                        ))
                        .chain(
                            self.diagnostic_notes_for_ambiguous_name(
                                candidates
                                    .into_iter()
                                    .map(|candidate| self.span_of(candidate))
                                    .collect(),
                            ),
                        )
                        .collect(),
                        "",
                    );

                    return TypeAnnotation {
                        span: ty.span,
                        kind: TypeAnnotationKind::error(&self.compiler),
                    };
                } else {
                    let (notes, fix) = self.diagnostic_notes_for_unresolved_name(
                        ty.span,
                        ty.name,
                        ctx,
                        AnyDeclaration::as_type,
                        &ty.name_scope_set,
                    );

                    self.compiler.add_diagnostic(
                        self.compiler
                            .error(
                                format!("cannot find type `{}`", ty.name),
                                std::iter::once(Note::primary(ty.span, "no such type"))
                                    .chain(notes)
                                    .collect(),
                                "undefined-name",
                            )
                            .fix(fix),
                    );

                    TypeAnnotation {
                        span: ty.span,
                        kind: TypeAnnotationKind::error(&self.compiler),
                    }
                }
            }
        }
    }

    fn lower_type_pattern(
        &mut self,
        type_pattern: &ast::TypePattern<Analysis>,
        ctx: &Context,
        fallback_scope: Option<&ScopeSet>,
    ) -> (Vec<TypeParameterId>, Vec<Bound>, Option<ScopeSet>) {
        let mut pattern_scope = None;

        macro_rules! generate_type_parameter {
            ($param:expr) => {{
                let ((name_span, name, scope), default, infer) = $param;

                let id = self.compiler.new_type_parameter_id_in(self.file);

                self.declarations.type_parameters.insert(
                    id,
                    Declaration::resolved(
                        Some(name),
                        name_span,
                        TypeParameterDeclaration { infer, default },
                    ),
                );

                self.insert(name, AnyDeclaration::TypeParameter(id), &scope);

                pattern_scope.get_or_insert(scope);

                id
            }};
        }

        let (params, bounds) = match type_pattern {
            ast::TypePattern::Where(type_pattern) => {
                let params = match &type_pattern.pattern {
                    Ok(lhs) => match lhs.as_ref() {
                        ast::TypePattern::Name(pattern) => {
                            pattern_scope.get_or_insert_with(|| pattern.scope.clone());

                            vec![generate_type_parameter!((
                                (pattern.span, pattern.name, pattern.scope.clone()),
                                None,
                                false,
                            ))]
                        }
                        ast::TypePattern::Default(pattern) => {
                            match (&pattern.type_parameter, &pattern.ty) {
                                (Ok(type_parameter), Ok(ty)) => {
                                    let ty = self.lower_type(ty, ctx, fallback_scope);

                                    match type_parameter {
                                        ast::DefaultTypeParameter::Infer(type_parameter) => {
                                            match &type_parameter.name {
                                                Ok((name_span, name, scope)) => {
                                                    vec![generate_type_parameter!((
                                                        (*name_span, *name, scope.clone()),
                                                        Some(ty),
                                                        true,
                                                    ))]
                                                }
                                                Err(_) => Vec::new(),
                                            }
                                        }
                                        ast::DefaultTypeParameter::Name(type_parameter) => {
                                            vec![generate_type_parameter!((
                                                (
                                                    type_parameter.span,
                                                    type_parameter.name,
                                                    type_parameter.scope.clone(),
                                                ),
                                                Some(ty),
                                                false,
                                            ))]
                                        }
                                    }
                                }
                                _ => Vec::new(),
                            }
                        }
                        ast::TypePattern::Infer(pattern) => match &pattern.name {
                            Ok((name_span, name, scope)) => {
                                vec![generate_type_parameter!((
                                    (*name_span, *name, scope.clone()),
                                    None,
                                    true
                                ))]
                            }
                            _ => Vec::new(),
                        },
                        ast::TypePattern::List(pattern) => pattern
                            .patterns
                            .iter()
                            .filter_map(|pattern| match pattern {
                                Ok(pattern) => match pattern {
                                    ast::TypePattern::Name(pattern) => {
                                        Some(generate_type_parameter!((
                                            (pattern.span, pattern.name, pattern.scope.clone()),
                                            None,
                                            false,
                                        )))
                                    }
                                    ast::TypePattern::Default(pattern) => {
                                        match (&pattern.type_parameter, &pattern.ty) {
                                            (Ok(type_parameter), Ok(ty)) => {
                                                let ty = self.lower_type(ty, ctx, fallback_scope);

                                                match type_parameter {
                                                    ast::DefaultTypeParameter::Infer(
                                                        type_parameter,
                                                    ) => match &type_parameter.name {
                                                        Ok((name_span, name, scope)) => {
                                                            Some(generate_type_parameter!((
                                                                (*name_span, *name, scope.clone()),
                                                                Some(ty),
                                                                true,
                                                            )))
                                                        }
                                                        Err(_) => None,
                                                    },
                                                    ast::DefaultTypeParameter::Name(
                                                        type_parameter,
                                                    ) => Some(generate_type_parameter!((
                                                        (
                                                            type_parameter.span,
                                                            type_parameter.name,
                                                            type_parameter.scope.clone(),
                                                        ),
                                                        Some(ty),
                                                        false,
                                                    ))),
                                                }
                                            }
                                            _ => None,
                                        }
                                    }
                                    ast::TypePattern::Infer(pattern) => match &pattern.name {
                                        Ok((name_span, name, scope)) => {
                                            Some(generate_type_parameter!((
                                                (*name_span, *name, scope.clone()),
                                                None,
                                                true,
                                            )))
                                        }
                                        _ => None,
                                    },
                                    ast::TypePattern::List(pattern) => {
                                        self.compiler.add_error(
                                            "higher-kinded types are not yet supported",
                                            vec![Note::primary(pattern.span, "try removing this")],
                                            "syntax-error",
                                        );

                                        None
                                    }
                                    ast::TypePattern::Where(pattern) => {
                                        self.compiler.add_error(
                                            "syntax error",
                                            vec![Note::primary(
                                                pattern.span(),
                                                "`where` clause is not allowed here",
                                            )],
                                            "syntax-error",
                                        );

                                        None
                                    }
                                },
                                Err(_) => None,
                            })
                            .collect(),
                        ast::TypePattern::Where(lhs) => {
                            self.compiler.add_error(
                                "type function may not have multiple `where` clauses",
                                vec![Note::primary(lhs.span(), "try removing this")],
                                "syntax-error",
                            );

                            Vec::new()
                        }
                    },
                    Err(_) => Vec::new(),
                };

                let bounds = type_pattern
                    .bounds
                    .iter()
                    .filter_map(|bound| match bound {
                        Ok(bound) => {
                            let tr = match self.get(
                                bound.trait_name,
                                bound.trait_span,
                                AnyDeclaration::as_trait,
                                &bound.trait_scope_set,
                            ) {
                                Ok(Some(tr)) => {
                                    self.declarations
                                        .traits
                                        .get_mut(&tr)
                                        .unwrap()
                                        .uses
                                        .insert(bound.trait_span);

                                    tr
                                }
                                Ok(None) => {
                                    let (notes, fix) = self.diagnostic_notes_for_unresolved_name(
                                        bound.trait_span,
                                        bound.trait_name,
                                        ctx,
                                        AnyDeclaration::as_trait,
                                        &bound.trait_scope_set,
                                    );

                                    self.compiler.add_diagnostic(
                                        self.compiler
                                            .error(
                                                format!("cannot find trait `{}`", bound.trait_name),
                                                std::iter::once(Note::primary(
                                                    bound.trait_span,
                                                    "no such trait",
                                                ))
                                                .chain(notes)
                                                .collect(),
                                                "undefined-name",
                                            )
                                            .fix(fix),
                                    );

                                    return None;
                                }
                                Err(candidates) => {
                                    self.compiler.add_error(
                                        format!(
                                            "multiple definitions of trait `{}`",
                                            bound.trait_name
                                        ),
                                        std::iter::once(Note::primary(
                                            bound.trait_span,
                                            format!("`{}` could refer to...", bound.trait_name),
                                        ))
                                        .chain(
                                            self.diagnostic_notes_for_ambiguous_name(
                                                candidates
                                                    .into_iter()
                                                    .map(|candidate| {
                                                        self.declarations
                                                            .traits
                                                            .get(&candidate)
                                                            .unwrap()
                                                            .span
                                                    })
                                                    .collect(),
                                            ),
                                        )
                                        .collect(),
                                        "",
                                    );

                                    return None;
                                }
                            };

                            let parameters = bound
                                .parameters
                                .iter()
                                .map(|ty| match ty {
                                    Ok(ty) => self.lower_type(ty, ctx, fallback_scope),
                                    Err(error) => TypeAnnotation {
                                        span: error.span,
                                        kind: TypeAnnotationKind::error(&self.compiler),
                                    },
                                })
                                .collect();

                            Some(Bound {
                                span: bound.span,
                                tr_span: bound.trait_span,
                                tr,
                                parameters,
                            })
                        }
                        Err(_) => None,
                    })
                    .collect();

                (params, bounds)
            }
            ast::TypePattern::Name(pattern) => {
                let params = vec![generate_type_parameter!((
                    (pattern.span, pattern.name, pattern.scope.clone()),
                    None,
                    false
                ))];
                (params, Vec::new())
            }
            ast::TypePattern::Default(pattern) => match (&pattern.type_parameter, &pattern.ty) {
                (Ok(type_parameter), Ok(ty)) => {
                    let ty = self.lower_type(ty, ctx, fallback_scope);

                    match type_parameter {
                        ast::DefaultTypeParameter::Infer(type_parameter) => {
                            match &type_parameter.name {
                                Ok((name_span, name, scope)) => {
                                    let params = vec![generate_type_parameter!((
                                        (*name_span, *name, scope.clone()),
                                        Some(ty),
                                        true
                                    ))];

                                    (params, Vec::new())
                                }
                                Err(_) => (Vec::new(), Vec::new()),
                            }
                        }
                        ast::DefaultTypeParameter::Name(type_parameter) => {
                            let params = vec![generate_type_parameter!((
                                (
                                    type_parameter.span,
                                    type_parameter.name,
                                    type_parameter.scope.clone(),
                                ),
                                Some(ty),
                                false,
                            ))];

                            (params, Vec::new())
                        }
                    }
                }
                _ => (Vec::new(), Vec::new()),
            },
            ast::TypePattern::Infer(pattern) => match &pattern.name {
                Ok((name_span, name, scope)) => {
                    let params = vec![generate_type_parameter!((
                        (*name_span, *name, scope.clone()),
                        None,
                        true
                    ))];
                    (params, Vec::new())
                }
                _ => (Vec::new(), Vec::new()),
            },
            ast::TypePattern::List(pattern) => {
                let mut scope = None;

                let params =
                    pattern
                        .patterns
                        .iter()
                        .filter_map(|pattern| match pattern {
                            Ok(pattern) => match pattern {
                                ast::TypePattern::Name(pattern) => {
                                    scope.get_or_insert_with(|| pattern.scope.clone());

                                    Some(generate_type_parameter!((
                                        (pattern.span, pattern.name, pattern.scope.clone()),
                                        None,
                                        false,
                                    )))
                                }
                                ast::TypePattern::Default(pattern) => {
                                    match (&pattern.type_parameter, &pattern.ty) {
                                        (Ok(type_parameter), Ok(ty)) => {
                                            let ty = self.lower_type(ty, ctx, fallback_scope);

                                            match type_parameter {
                                                ast::DefaultTypeParameter::Infer(
                                                    type_parameter,
                                                ) => match &type_parameter.name {
                                                    Ok((name_span, name, scope)) => {
                                                        Some(generate_type_parameter!((
                                                            (*name_span, *name, scope.clone()),
                                                            Some(ty),
                                                            true,
                                                        )))
                                                    }
                                                    Err(_) => None,
                                                },
                                                ast::DefaultTypeParameter::Name(type_parameter) => {
                                                    Some(generate_type_parameter!((
                                                        (
                                                            type_parameter.span,
                                                            type_parameter.name,
                                                            type_parameter.scope.clone(),
                                                        ),
                                                        Some(ty),
                                                        false,
                                                    )))
                                                }
                                            }
                                        }
                                        _ => None,
                                    }
                                }
                                ast::TypePattern::Infer(pattern) => match &pattern.name {
                                    Ok((name_span, name, scope)) => Some(generate_type_parameter!(
                                        ((*name_span, *name, scope.clone()), None, true)
                                    )),
                                    _ => None,
                                },
                                ast::TypePattern::List(pattern) => {
                                    self.compiler.add_error(
                                        "higher-kinded types are not yet supported",
                                        vec![Note::primary(pattern.span, "try removing this")],
                                        "syntax-error",
                                    );

                                    None
                                }
                                ast::TypePattern::Where(pattern) => {
                                    self.compiler.add_error(
                                        "syntax error",
                                        vec![Note::primary(
                                            pattern.where_span,
                                            "`where` clause is not allowed here",
                                        )],
                                        "syntax-error",
                                    );

                                    None
                                }
                            },
                            Err(_) => None,
                        })
                        .collect::<Vec<_>>();

                (params, Vec::new())
            }
        };

        (params, bounds, pattern_scope)
    }

    fn lower_decl_attributes(
        &self,
        statement_attributes: &ast::StatementAttributes<Analysis>,
    ) -> DeclarationAttributes {
        // TODO: Raise errors for misused attributes

        DeclarationAttributes {
            help: statement_attributes
                .help
                .clone()
                .into_iter()
                .map(|attribute| attribute.help_text)
                .collect(),
            help_group: statement_attributes
                .help_group
                .as_ref()
                .map(|attribute| attribute.help_group_text),
            help_playground: statement_attributes
                .help_playground
                .as_ref()
                .map(|attribute| attribute.help_playground_text),
            help_template: statement_attributes
                .help_template
                .as_ref()
                .map(|attribute| attribute.help_template_text),
            private: statement_attributes.private.is_some(),
        }
    }

    fn lower_syntax_attributes(
        &mut self,
        statement_attributes: &ast::StatementAttributes<Analysis>,
        _ctx: &Context,
    ) -> SyntaxAttributes {
        SyntaxAttributes {
            decl_attributes: self.lower_decl_attributes(statement_attributes),
        }
    }

    fn lower_type_attributes(
        &mut self,
        attributes: &ast::StatementAttributes<Analysis>,
        ctx: &Context,
        scope: &ScopeSet,
    ) -> TypeAttributes {
        // TODO: Raise errors for misused attributes

        TypeAttributes {
            decl_attributes: self.lower_decl_attributes(attributes),
            on_mismatch: attributes
                .on_mismatch
                .clone()
                .into_iter()
                .filter_map(|attribute| {
                    let param = match attribute.type_parameter {
                        Some((span, name)) => {
                            match self.get(name, span, AnyDeclaration::as_type_parameter, scope) {
                                Ok(Some(param)) => {
                                    self.declarations
                                        .type_parameters
                                        .get_mut(&param)
                                        .unwrap()
                                        .uses
                                        .insert(span);

                                    Some(param)
                                }
                                Ok(None) => {
                                    let (notes, fix) = self.diagnostic_notes_for_unresolved_name(
                                        span,
                                        name,
                                        ctx,
                                        AnyDeclaration::as_type_parameter,
                                        scope,
                                    );

                                    self.compiler.add_diagnostic(
                                        self.compiler
                                            .error(
                                                format!("cannot find type parameter `{name}`"),
                                                std::iter::once(Note::primary(
                                                    span,
                                                    "no such type parameter",
                                                ))
                                                .chain(notes)
                                                .collect(),
                                                "undefined-name",
                                            )
                                            .fix(fix),
                                    );

                                    return None;
                                }
                                Err(candidates) => {
                                    self.compiler.add_error(
                                        format!("multiple definitions of type parameter `{name}`"),
                                        std::iter::once(Note::primary(
                                            span,
                                            format!("`{name}` could refer to..."),
                                        ))
                                        .chain(
                                            self.diagnostic_notes_for_ambiguous_name(
                                                candidates
                                                    .into_iter()
                                                    .map(|candidate| {
                                                        self.declarations
                                                            .type_parameters
                                                            .get(&candidate)
                                                            .unwrap()
                                                            .span
                                                    })
                                                    .collect(),
                                            ),
                                        )
                                        .collect(),
                                        "",
                                    );

                                    return None;
                                }
                            }
                        }
                        None => None,
                    };

                    Some((param, attribute.message))
                })
                .collect::<Vec<_>>(),
            convert_from: attributes
                .convert_from
                .iter()
                .map(|attribute| {
                    let ty = match &attribute.ty {
                        Ok(ty) => self.lower_type(ty, ctx, Some(scope)),
                        Err(error) => TypeAnnotation {
                            span: error.span,
                            kind: TypeAnnotationKind::error(&self.compiler),
                        },
                    };

                    (ty, attribute.replacement.clone())
                })
                .collect(),
            no_reuse: attributes
                .no_reuse
                .as_ref()
                .map(|attribute| Some(attribute.no_reuse_text?.1)),
        }
    }

    fn lower_trait_attributes(
        &mut self,
        attributes: &ast::StatementAttributes<Analysis>,
        ctx: &Context,
        scope: &ScopeSet,
    ) -> TraitAttributes {
        // TODO: Raise errors for misused attributes

        TraitAttributes {
            decl_attributes: self.lower_decl_attributes(attributes),
            on_unimplemented: attributes.on_unimplemented.as_ref().and_then(|attribute| {
                Some((
                    attribute
                        .segments
                        .iter()
                        .cloned()
                        .map(|segment| {
                            let (param_span, param_name) = segment.param.ok()?;

                            let param = match self.get(
                                param_name,
                                param_span,
                                AnyDeclaration::as_type_parameter,
                                scope,
                            ) {
                                Ok(Some(id)) => id,
                                Ok(None) => {
                                    let (notes, fix) = self.diagnostic_notes_for_unresolved_name(
                                        param_span,
                                        param_name,
                                        ctx,
                                        AnyDeclaration::as_type_parameter,
                                        scope,
                                    );

                                    self.compiler.add_diagnostic(
                                        self.compiler
                                            .error(
                                                format!(
                                                    "cannot find type parameter `{param_name}`"
                                                ),
                                                std::iter::once(Note::primary(
                                                    param_span,
                                                    "no such type parameter",
                                                ))
                                                .chain(notes)
                                                .collect(),
                                                "undefined-name",
                                            )
                                            .fix(fix),
                                    );

                                    return None;
                                }
                                Err(candidates) => {
                                    self.compiler.add_error(
                                        format!(
                                            "multiple definitions of type parameter `{param_name}`",
                                        ),
                                        std::iter::once(Note::primary(
                                            param_span,
                                            format!("`{param_name}` could refer to..."),
                                        ))
                                        .chain(
                                            self.diagnostic_notes_for_ambiguous_name(
                                                candidates
                                                    .into_iter()
                                                    .map(|candidate| {
                                                        self.declarations
                                                            .type_parameters
                                                            .get(&candidate)
                                                            .unwrap()
                                                            .span
                                                    })
                                                    .collect(),
                                            ),
                                        )
                                        .collect(),
                                        "",
                                    );

                                    return None;
                                }
                            };

                            Some((segment.string, param))
                        })
                        .collect::<Option<_>>()?,
                    attribute.trailing_segment,
                ))
            }),
            sealed: attributes.sealed.is_some(),
            allow_overlapping_instances: attributes.allow_overlapping_instances.is_some(),
            derive: attributes
                .derive
                .as_ref()
                .map(|attribute| (attribute.span, attribute.path, attribute.name)),
        }
    }

    fn lower_constant_attributes(
        &mut self,
        attributes: &ast::StatementAttributes<Analysis>,
        _ctx: &Context,
    ) -> ConstantAttributes {
        // TODO: Raise errors for misused attributes

        ConstantAttributes {
            decl_attributes: self.lower_decl_attributes(attributes),
            is_contextual: attributes.contextual.is_some(),
            is_specialization: attributes.specialize.is_some(),
        }
    }

    fn get_name_from_assignment<'a>(
        &mut self,
        pattern: &'a ast::AssignmentPattern<Analysis>,
    ) -> Option<(SpanList, InternedString, &'a ScopeSet)> {
        if let ast::AssignmentPattern::Pattern(pattern) = pattern {
            if let ast::Pattern::Name(pattern) = &pattern.pattern {
                return Some((pattern.span, pattern.name, &pattern.scope));
            }
        }

        self.compiler.add_error(
            "syntax error",
            vec![Note::primary(pattern.span(), "expected a name here")],
            "syntax-error",
        );

        None
    }

    fn resolve_value(
        &mut self,
        span: SpanList,
        name: InternedString,
        scope: &ScopeSet,
    ) -> Result<Option<ExpressionKind>, Vec<SpanList>> {
        macro_rules! get {
            ($decls:ident, $transform:expr, $pat:pat, $id:ident) => {
                match self.get(name, span, $transform, scope) {
                    Ok(id) => id,
                    Err(candidates) => {
                        return Err(candidates
                            .into_iter()
                            .filter_map(|id| {
                                #[allow(unreachable_patterns)]
                                match id {
                                    $pat => Some(self.declarations.$decls.get(&$id).unwrap().span),
                                    _ => None,
                                }
                            })
                            .collect());
                    }
                }
            };
            ($decls:ident, $transform:expr) => {
                get!($decls, $transform, id, id)
            };
        }

        if let Some(id) = get!(variables, AnyDeclaration::as_variable) {
            self.declarations
                .variables
                .get_mut(&id)
                .unwrap()
                .uses
                .insert(span);

            Ok(Some(ExpressionKind::Variable(id)))
        } else if let Some((id, None)) =
            get!(constants, AnyDeclaration::as_constant, (id, None), id)
        {
            self.declarations
                .constants
                .get_mut(&id)
                .unwrap()
                .uses
                .insert(span);

            Ok(Some(ExpressionKind::Constant(id)))
        } else if let Some(id) = get!(types, AnyDeclaration::as_type) {
            self.declarations
                .types
                .get_mut(&id)
                .unwrap()
                .uses
                .insert(span);

            match self
                .declarations
                .types
                .get(&id)
                .unwrap()
                .value
                .as_ref()
                .unwrap()
                .kind
            {
                TypeDeclarationKind::Marker => Ok(Some(ExpressionKind::Marker(id))),
                _ => {
                    self.compiler.add_error(
                        "cannot use type as value",
                        vec![Note::primary(span, "try instantiating the type")],
                        "type-as-value",
                    );

                    Ok(Some(ExpressionKind::error(&self.compiler)))
                }
            }
        } else if let Some(id) = get!(traits, AnyDeclaration::as_trait) {
            self.declarations
                .traits
                .get_mut(&id)
                .unwrap()
                .uses
                .insert(span);

            Ok(Some(ExpressionKind::Trait(id)))
        } else if let Some(id) = get!(builtin_types, AnyDeclaration::as_builtin_type) {
            self.declarations
                .builtin_types
                .get_mut(&id)
                .unwrap()
                .uses
                .insert(span);

            self.compiler.add_error(
                "cannot use builtin type as value",
                vec![Note::primary(span, "try using a literal instead")],
                "type-as-value",
            );

            Ok(Some(ExpressionKind::error(&self.compiler)))
        } else if let Some((id, Some(_))) =
            get!(constants, AnyDeclaration::as_constant, (id, Some(_)), id)
        {
            self.declarations
                .constants
                .get_mut(&id)
                .unwrap()
                .uses
                .insert(span);

            Ok(Some(ExpressionKind::Constant(id)))
        } else if let Some(id) = get!(type_parameters, AnyDeclaration::as_type_parameter) {
            self.declarations
                .type_parameters
                .get_mut(&id)
                .unwrap()
                .uses
                .insert(span);

            self.compiler.add_error(
                "cannot use type parameter as value",
                vec![Note::primary(
                    span,
                    "type parameters cannot be instantiated because the actual type is not known here",
                )],
                "type-as-value",
            );

            Ok(Some(ExpressionKind::error(&self.compiler)))
        } else {
            Ok(None)
        }
    }

    fn generate_variant_constructor(
        &mut self,
        id: TypeId,
        owner: Option<ConstantId>,
        name: InternedString,
        span: SpanList,
        index: VariantIndex,
        parameters: &[TypeParameterId],
        tys: &[TypeAnnotation],
    ) -> ConstantId {
        let constructor_id = self.compiler.new_constant_id_in(self.file);

        let constructor_ty = tys.iter().rev().fold(
            TypeAnnotation {
                span,
                kind: TypeAnnotationKind::Named(
                    id,
                    parameters
                        .iter()
                        .map(|param| {
                            let span = self.declarations.type_parameters.get(param).unwrap().span;

                            TypeAnnotation {
                                span,
                                kind: TypeAnnotationKind::Parameter(*param),
                            }
                        })
                        .collect(),
                ),
            },
            |result, next| TypeAnnotation {
                span: SpanList::join(next.span, result.span),
                kind: TypeAnnotationKind::Function(Box::new(next.clone()), Box::new(result)),
            },
        );

        let variables = tys
            .iter()
            .map(|ty| {
                let var = self.compiler.new_variable_id(owner);

                self.declarations.variables.insert(
                    var,
                    Declaration::resolved(None, ty.span, VariableDeclaration),
                );

                (var, ty.span)
            })
            .collect::<Vec<_>>();

        let constructor = variables.iter().enumerate().rev().fold(
            Expression {
                id: self.compiler.new_expression_id(constructor_id),
                span,
                kind: ExpressionKind::Variant(
                    id,
                    index,
                    variables
                        .iter()
                        .map(|&(var, span)| Expression {
                            id: self.compiler.new_expression_id(constructor_id),
                            span,
                            kind: ExpressionKind::Variable(var),
                        })
                        .collect(),
                ),
            },
            |result, (index, (var, span))| Expression {
                id: self.compiler.new_expression_id(constructor_id),
                span: *span,
                kind: ExpressionKind::Function(
                    Pattern {
                        span: *span,
                        kind: PatternKind::Variable(*var),
                    },
                    Box::new(result),
                    variables[..index].to_vec(),
                ),
            },
        );

        self.declarations.constants.insert(
            constructor_id,
            Declaration::resolved(
                Some(name),
                span,
                UnresolvedConstantDeclaration {
                    parameters: parameters.to_vec(),
                    bounds: Vec::new(),
                    ty: constructor_ty,
                    value: Shared::new(Some(constructor)),
                    is_variant: true,
                    attributes: Default::default(),
                },
            )
            .make_unresolved(),
        );

        constructor_id
    }

    fn generate_capture_list(
        &mut self,
        pattern: Option<&Pattern>,
        expr: &mut Expression,
    ) -> CaptureList {
        let mut declared = pattern.map(Pattern::variables).unwrap_or_default();
        let mut captured = CaptureList::new();
        expr.traverse_mut(|expr| match &expr.kind {
            ExpressionKind::Initialize(pattern, _) => {
                declared.extend(pattern.variables());
            }
            ExpressionKind::When(_, arms) => {
                for arm in arms {
                    declared.extend(arm.pattern.variables());
                }
            }
            ExpressionKind::Function(pattern, _, captures) => {
                declared.extend(pattern.variables());
                captured.extend(captures);
            }
            ExpressionKind::Variable(var) => {
                captured.push((*var, expr.span));
            }
            _ => {}
        });

        captured
            .into_iter()
            .filter(|(var, _)| !declared.contains(var))
            .unique_by(|(var, _)| *var)
            .collect()
    }

    fn span_of(&self, decl: AnyDeclaration) -> SpanList {
        match decl {
            AnyDeclaration::Type(id) => self.declarations.types.get(&id).unwrap().span,
            AnyDeclaration::BuiltinType(id) => {
                self.declarations.builtin_types.get(&id).unwrap().span
            }
            AnyDeclaration::Trait(id) => self.declarations.traits.get(&id).unwrap().span,
            AnyDeclaration::TypeParameter(id) => {
                self.declarations.type_parameters.get(&id).unwrap().span
            }
            AnyDeclaration::Constant(id, _) => self.declarations.constants.get(&id).unwrap().span,
            AnyDeclaration::Variable(id) => self.declarations.variables.get(&id).unwrap().span,
        }
    }

    fn attributes_of(&self, decl: AnyDeclaration) -> DeclarationAttributes {
        match decl {
            AnyDeclaration::Type(id) => self
                .declarations
                .types
                .get(&id)
                .unwrap()
                .value
                .as_ref()
                .unwrap()
                .attributes
                .decl_attributes
                .clone(),
            AnyDeclaration::BuiltinType(_) => Default::default(),
            AnyDeclaration::Trait(id) => self
                .declarations
                .traits
                .get(&id)
                .unwrap()
                .value
                .as_ref()
                .unwrap()
                .attributes
                .decl_attributes
                .clone(),
            AnyDeclaration::TypeParameter(_) => Default::default(),
            AnyDeclaration::Constant(id, _) => self
                .declarations
                .constants
                .get(&id)
                .unwrap()
                .value
                .as_ref()
                .unwrap()
                .attributes
                .decl_attributes
                .clone(),
            AnyDeclaration::Variable(_) => Default::default(),
        }
    }

    fn diagnostic_notes_for_ambiguous_name(&self, candidates: Vec<SpanList>) -> Vec<Note> {
        candidates
            .into_iter()
            .map(|span| Note::secondary(span, "...this"))
            .collect()
    }

    fn diagnostic_notes_for_unresolved_name<T>(
        &self,
        span: SpanList,
        name: InternedString,
        ctx: &Context,
        kind: fn(AnyDeclaration) -> Option<T>,
        scope: &ScopeSet,
    ) -> (Vec<Note>, Option<Fix>) {
        self.diagnostic_notes_for_unresolved_name_with(
            span,
            name,
            ctx,
            |decl| kind(decl).is_some(),
            scope,
        )
    }

    fn diagnostic_notes_for_unresolved_name_with(
        &self,
        span: SpanList,
        name: InternedString,
        ctx: &Context,
        kind: impl Fn(AnyDeclaration) -> bool,
        scope: &ScopeSet,
    ) -> (Vec<Note>, Option<Fix>) {
        let mut notes = std::iter::empty()
            .chain(
                self.file_info
                    .diagnostic_aliases
                    .aliases
                    .get(&name)
                    .map(|alias| (
                        Note::secondary(span, format!("did you mean `{alias}`?")),
                        Fix::new(format!("replace with `{alias}`"), FixRange::replace(span.first()), alias),
                    )),
            )
            .chain(ctx.caller_accepts_text.then(|| (
                Note::secondary(
                    span,
                    format!("if you meant to provide text here, try using quotes: `\"{name}\"`"),
                ),
                Fix::new("surround with quotes", FixRange::replace(span.first()), format!("\"{name}\"")),
            )))
            .chain(did_you_mean::math(&name).map(|(lhs, op, rhs)| (
                Note::secondary(
                    span,
                    format!("if you meant to write a mathematical expression, try using whitespace: `{lhs} {op} {rhs}`"),
                ),
                Fix::new(format!("add whitespace around `{op}`"), FixRange::replace(span.first()), format!("{lhs} {op} {rhs}")),
            )))
            .chain(
                did_you_mean::comment(&name).map(|()| (
                    Note::secondary(span, "comments in Wipple begin with `--`"),
                    Fix::new("replace with `--`", FixRange::replace(span.first()), "--"),
                )),
            )
            .collect::<Vec<_>>();

        if notes.is_empty() {
            let candidates = self.collect(kind, scope).map(|name| name.as_str());

            if let Some(suggestion) = did_you_mean::name(&name, candidates) {
                notes.push((
                    Note::secondary(span, format!("did you mean `{suggestion}`?")),
                    Fix::new(
                        format!("replace with `{suggestion}`"),
                        FixRange::replace(span.first()),
                        suggestion,
                    ),
                ));
            }
        }

        let (notes, mut fixes): (Vec<_>, VecDeque<_>) = notes.into_iter().unzip();

        (notes, fixes.pop_front())
    }
}

fn segments(text: &parse::Text<Analysis>) -> (Vec<String>, Option<String>) {
    let (text, escaped_underscores) = text.with_escaped_underscores();
    let text = text.as_str();

    let mut segments = vec![String::new()];
    if !text.is_empty() {
        let is_placeholder =
            |index, ch| ch == '_' && escaped_underscores.binary_search(&index).is_err();

        for (index, ch) in text.char_indices() {
            if is_placeholder(index, ch) {
                segments.push(String::new());
            } else if let Some(segment) = segments.last_mut() {
                segment.push(ch);
            } else {
                segments.push(String::from(ch));
            }
        }
    }

    let trailing_segment = segments.pop();
    (segments, trailing_segment)
}
