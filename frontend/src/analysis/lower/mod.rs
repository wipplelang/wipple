mod builtins;

use crate::{
    analysis::{ast, Analysis, Span, SpanList},
    diagnostics::{Fix, FixRange, Note},
    helpers::{did_you_mean, Backtrace, InternedString, Shared},
    BuiltinSyntaxId, BuiltinTypeId, Compiler, ConstantId, ExpressionId, FieldIndex, FilePath,
    ScopeId, SyntaxId, TraitId, TypeId, TypeParameterId, VariableId, VariantIndex,
};
use serde::{Deserialize, Serialize};
use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque},
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
    pub exported: HashMap<InternedString, HashSet<AnyDeclaration>>,
    scopes: BTreeMap<LoadedScopeId, Scope>,
}

#[derive(Debug, Clone, Default)]
#[non_exhaustive]
pub struct FileAttributes {
    pub imported_by: Shared<Vec<FilePath>>,
    pub help_url: Option<InternedString>,
}

#[derive(Debug, Clone, Default)]
pub struct Declarations {
    pub syntaxes: BTreeMap<SyntaxId, Declaration<SyntaxDeclaration>>,
    pub types: BTreeMap<TypeId, Declaration<TypeDeclaration>>,
    pub type_parameters: BTreeMap<TypeParameterId, Declaration<TypeParameterDeclaration>>,
    pub traits: BTreeMap<TraitId, Declaration<TraitDeclaration>>,
    pub builtin_types: BTreeMap<BuiltinTypeId, Declaration<BuiltinTypeDeclaration>>,
    pub constants: BTreeMap<ConstantId, Declaration<ConstantDeclaration>>,
    pub instances: BTreeMap<ConstantId, Declaration<InstanceDeclaration>>,
    pub variables: BTreeMap<VariableId, Declaration<VariableDeclaration>>,
    pub builtin_syntaxes: BTreeMap<BuiltinSyntaxId, Declaration<BuiltinSyntaxDeclaration>>,
}

#[derive(Debug, Clone, Default)]
struct UnresolvedDeclarations {
    syntaxes: BTreeMap<SyntaxId, Declaration<SyntaxDeclaration>>,
    types: BTreeMap<TypeId, Declaration<Option<TypeDeclaration>>>,
    type_parameters: BTreeMap<TypeParameterId, Declaration<TypeParameterDeclaration>>,
    traits: BTreeMap<TraitId, Declaration<Option<TraitDeclaration>>>,
    builtin_types: BTreeMap<BuiltinTypeId, Declaration<BuiltinTypeDeclaration>>,
    constants: BTreeMap<ConstantId, Declaration<Option<UnresolvedConstantDeclaration>>>,
    instances: BTreeMap<ConstantId, Declaration<Option<InstanceDeclaration>>>,
    variables: BTreeMap<VariableId, Declaration<VariableDeclaration>>,
    builtin_syntaxes: BTreeMap<BuiltinSyntaxId, Declaration<BuiltinSyntaxDeclaration>>,
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
    pub uses: HashSet<SpanList>,
    pub value: T,
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
            uses: HashSet::new(),
            value: None,
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
        }
    }
}

impl<T> Declaration<T> {
    fn resolved(name: Option<InternedString>, span: impl Into<SpanList>, value: T) -> Self {
        Declaration {
            name,
            span: span.into(),
            uses: HashSet::new(),
            value,
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
            value: self.value.into_unique().expect("uninitialized constant"),
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
pub struct InstanceDeclaration {
    pub parameters: Vec<TypeParameterId>,
    pub bounds: Vec<Bound>,
    pub tr_span: SpanList,
    pub tr: TraitId,
    pub tr_parameters: Vec<TypeAnnotation>,
    pub value: Option<InstanceValue>,
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
}

impl DiagnosticItems {
    fn merge(&mut self, other: DiagnosticItems) {
        self.accepts_text.extend(other.accepts_text);
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
    AddInteger,
    SubtractInteger,
    MultiplyInteger,
    DivideInteger,
    ModuloInteger,
    PowerInteger,
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
    AddDouble,
    SubtractDouble,
    MultiplyDouble,
    DivideDouble,
    ModuloDouble,
    PowerDouble,
    FloorDouble,
    CeilDouble,
    SqrtDouble,
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
    TextHeadTail,
    RandomNumber,
    RandomInteger,
    RandomNatural,
    RandomByte,
    RandomSigned,
    RandomUnsigned,
    RandomFloat,
    RandomDouble,
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
        };

        let ctx = Context::default();
        let scope = lowerer.root_scope(file.file.root_scope);

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
                })
                .uses
                .extend(uses);
        }

        for (id, value) in &*file.file.syntax_declarations.lock() {
            if !lowerer.declarations.syntaxes.contains_key(id) {
                let decl = Declaration {
                    name: value.name,
                    span: value.span(),
                    uses: value.uses.iter().copied().collect(),
                    value: SyntaxDeclaration {
                        operator: value.operator_precedence.is_some(),
                        keyword: value.keyword.is_some(),
                        attributes: lowerer.lower_syntax_attributes(
                            &value.attributes,
                            &ctx,
                            &scope,
                        ),
                    },
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
                        for (id, mut decl) in dependency.declarations.$kind.clone() {
                            let mut uses = HashSet::new();
                            let merged_decl = lowerer.declarations.$kind.entry(id).or_insert_with(|| {
                                uses = mem::take(&mut decl.uses);
                                $($transform)?(decl)
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

            lowerer.scopes.extend(dependency.scopes.clone());

            lowerer.extend(dependency.exported.clone(), &scope);
        }

        file_attributes.insert(
            file.file.path,
            lowerer.lower_file_attributes(&file.attributes),
        );

        let statements = lowerer.lower_statements(&file.statements, &scope, &ctx);

        for (&constant_id, constant) in &lowerer.declarations.constants {
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

        for (&instance_id, instance) in &mut lowerer.declarations.instances {
            let tr = instance.value.as_ref().unwrap().tr;
            let tr_decl = lowerer.declarations.traits.get(&tr).unwrap();

            if tr_decl
                .value
                .as_ref()
                .unwrap()
                .attributes
                .allow_overlapping_instances
                && instance.span.first().path != tr_decl.span.first().path
            {
                self.add_error(
                    "instance of trait that allows overlapping instances must occur in the same file as the trait",
                    vec![Note::primary(instance.span, "instance disallowed here")],
                    ""
                );
            }

            let trait_has_value = tr_decl.value.as_ref().unwrap().ty.is_some();
            let instance_value = &instance.value.as_ref().unwrap().value;

            match (trait_has_value, instance_value) {
                (true, None) => {
                    self.add_diagnostic(
                        self.error(
                            "expected instance to have value",
                            vec![Note::primary(
                                instance.span,
                                "try adding `:` after the instance declaration to give it a value",
                            )],
                            "instance-has-no-value",
                        )
                        .fix_with(
                            "give this instance a value",
                            FixRange::after(instance.span.first()),
                            " : (*value*)",
                        ),
                    );

                    instance.value.as_mut().unwrap().value = Some(InstanceValue {
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
                            "instance-has-unexpected-value",
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

        let exported = lowerer.export(&scope);

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
            exported,
            scopes: lowerer.scopes,
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
    scopes: BTreeMap<LoadedScopeId, Scope>,
}

#[derive(Debug, Clone, Default)]
struct Scope {
    parent: Option<LoadedScopeId>,
    children: Vec<LoadedScopeId>,
    values: HashMap<InternedString, HashSet<AnyDeclaration>>,
    declared_variables: BTreeSet<VariableId>,
    used_variables: Shared<CaptureList>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct LoadedScopeId {
    id: ScopeId,
    trace: Backtrace,
}

impl std::borrow::Borrow<ScopeId> for LoadedScopeId {
    fn borrow(&self) -> &ScopeId {
        &self.id
    }
}

impl Lowerer {
    fn root_scope(&mut self, id: ScopeId) -> LoadedScopeId {
        let id = LoadedScopeId {
            id,
            trace: self.compiler.backtrace(),
        };

        self.scopes.insert(id.clone(), Scope::default());

        self.load_builtins(&id);

        id
    }

    fn child_scope(&mut self, id: ScopeId, parent: &LoadedScopeId) -> LoadedScopeId {
        let id = LoadedScopeId {
            id,
            trace: self.compiler.backtrace(),
        };

        assert_ne!(
            &id, parent,
            "cannot make a scope {:?} a child of itself",
            id.id
        );

        assert!(
            !self.scopes.contains_key(&id),
            "scope {:?} already registered at {:#?}",
            id.id,
            id.trace
        );

        let scope = Scope {
            parent: Some(parent.clone()),
            ..Default::default()
        };

        self.scopes.insert(id.clone(), scope);

        self.scopes
            .get_mut(parent)
            .unwrap_or_else(|| panic!("scope {parent:?} loaded out of order"))
            .children
            .push(id.clone());

        id
    }

    fn assert_loaded_scope(&mut self, id: ScopeId) -> LoadedScopeId {
        assert!(
            self.scopes.contains_key(&id),
            "scope {id:?} loaded out of order"
        );

        LoadedScopeId {
            id,
            trace: self.compiler.backtrace(),
        }
    }

    fn insert(&mut self, name: InternedString, value: AnyDeclaration, scope: &LoadedScopeId) {
        let scope = self.scopes.get_mut(scope).unwrap();

        if let AnyDeclaration::Variable(var) = value {
            scope.declared_variables.insert(var);
        }

        let values = scope.values.entry(name).or_default();
        values.remove(&value); // because we use a Hash implementation that only looks at the discriminant
        values.insert(value);
    }

    fn extend(
        &mut self,
        values: impl IntoIterator<Item = (InternedString, HashSet<AnyDeclaration>)>,
        scope: &LoadedScopeId,
    ) {
        for (name, decls) in values {
            for decl in decls {
                self.insert(name, decl, scope);
            }
        }
    }

    fn get<T>(
        &mut self,
        name: InternedString,
        span: SpanList,
        kind: fn(AnyDeclaration) -> Option<T>,
        scope: &LoadedScopeId,
    ) -> Option<T> {
        self.get_inner(name, Some(span), kind, scope)
    }

    fn peek<T>(
        &mut self,
        name: InternedString,
        kind: fn(AnyDeclaration) -> Option<T>,
        scope: &LoadedScopeId,
    ) -> Option<T> {
        self.get_inner(name, None, kind, scope)
    }

    fn get_inner<T>(
        &mut self,
        name: InternedString,
        use_span: Option<SpanList>,
        kind: fn(AnyDeclaration) -> Option<T>,
        scope_id: &LoadedScopeId,
    ) -> Option<T> {
        let mut parent = Some(scope_id);
        let mut results = None;
        let mut used_variables = Vec::new();

        while let Some(scope_id) = parent {
            let scope = self
                .scopes
                .get(scope_id)
                .unwrap_or_else(|| panic!("scope {scope_id:?} loaded out of order"));

            if let Some(value) = scope.values.get(&name) {
                results = Some(value.clone());
                break;
            }

            if use_span.is_some() {
                used_variables.push(&scope.used_variables);
            }

            parent = scope.parent.as_ref();
        }

        if let Some(span) = use_span {
            if let Some(results) = &results {
                if let Some(id) = results
                    .iter()
                    .copied()
                    .find_map(AnyDeclaration::as_variable)
                {
                    for u in used_variables {
                        u.lock().push((id, span));
                    }
                }
            }
        }

        results?.into_iter().find_map(kind)
    }

    fn collect<'a>(
        &'a self,
        kind: impl Fn(AnyDeclaration) -> bool + 'a,
        scope_id: &LoadedScopeId,
    ) -> impl Iterator<Item = InternedString> + 'a {
        let mut parent = Some(scope_id.clone());
        let mut values = None;

        std::iter::from_fn(move || loop {
            if values.as_ref().map_or(true, Vec::is_empty) {
                let scope_id = mem::take(&mut parent)?;

                let scope = self
                    .scopes
                    .get(&scope_id)
                    .unwrap_or_else(|| panic!("scope {scope_id:?} loaded out of order"));

                values = Some(scope.values.clone().into_iter().collect::<Vec<_>>());

                parent = scope.parent.clone();
            }

            if let Some((name, decl)) = values.as_mut()?.pop() {
                if decl.into_iter().any(&kind) {
                    break Some(name);
                }
            }
        })
    }

    fn declares_in(&mut self, var: VariableId, scope_id: &LoadedScopeId) -> bool {
        let scope = self.scopes.get(scope_id).unwrap().clone();

        scope.declared_variables.contains(&var)
            || scope
                .children
                .iter()
                .any(|child| self.declares_in(var, child))
    }

    fn export(
        &mut self,
        scope: &LoadedScopeId,
    ) -> HashMap<InternedString, HashSet<AnyDeclaration>> {
        self.scopes.get_mut(scope).unwrap().values.clone()
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
        Option<(
            LoadedScopeId,
            LoadedScopeId,
            (Vec<TypeParameterId>, Vec<Bound>),
        )>,
        &'a ast::TypeAssignmentValue<Analysis>,
    ),
    Trait(
        TraitId,
        Option<(LoadedScopeId, (Vec<TypeParameterId>, Vec<Bound>))>,
        &'a ast::TraitAssignmentValue<Analysis>,
    ),
    Constant(
        ConstantId,
        (LoadedScopeId, (Vec<TypeParameterId>, Vec<Bound>)),
        &'a ast::Type<Analysis>,
    ),
    Instance(
        ConstantId,
        Option<(LoadedScopeId, (Vec<TypeParameterId>, Vec<Bound>))>,
        (
            SpanList,
            InternedString,
            LoadedScopeId,
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
        scope: &LoadedScopeId,
        ctx: &Context,
    ) -> Vec<Expression> {
        let declarations = statements
            .iter()
            .filter_map(|statement| statement.as_ref().ok())
            .map(|statement| self.lower_statement(statement, scope, ctx))
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
                StatementDeclarationKind::Type(id, ty_pattern, value) => {
                    let (scope, parent_scope, (parameters, bounds)) = ty_pattern
                        .unwrap_or_else(|| (scope.clone(), scope.clone(), Default::default()));

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
                                                .map(|(index, (span, name))| {
                                                    let index = VariantIndex::new(index);

                                                    EnumerationVariant {
                                                        name_span: *span,
                                                        name: *name,
                                                        tys: Vec::new(),
                                                        constructor: self
                                                            .generate_variant_constructor(
                                                                id,
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
                                                        &parent_scope,
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
                                                    Ok(ty) => self.lower_type(ty, &scope, ctx),
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
                                                        Ok(ty) => self.lower_type(ty, &scope, ctx),
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
                                                        variant.name,
                                                        variant.span,
                                                        index,
                                                        &parameters,
                                                        &tys,
                                                    );

                                                variants.push(EnumerationVariant {
                                                    name_span: variant.name_span,
                                                    name: variant.name,
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
                                                    &parent_scope,
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
                                    let ty = self.lower_type(&body.ty, &scope, ctx);
                                    TypeDeclarationKind::Alias(ty)
                                }
                            }
                        }
                        None => TypeDeclarationKind::Marker,
                    };

                    let attributes = self.lower_type_attributes(decl.attributes, ctx, &scope);

                    self.declarations.types.get_mut(&id).unwrap().value = Some(TypeDeclaration {
                        parameters,
                        kind,
                        attributes,
                    });

                    Some(AnyDeclaration::Type(id))
                }
                StatementDeclarationKind::Trait(id, ty_pattern, value) => {
                    let (scope, (parameters, bounds)) =
                        ty_pattern.unwrap_or_else(|| (scope.clone(), Default::default()));

                    if let Some(bound) = bounds.first() {
                        self.compiler.add_error(
                            "`trait` declarations may not have bounds",
                            vec![Note::primary(bound.span, "try removing this")],
                            "syntax-error",
                        );
                    }

                    let ty = value.ty.as_ref().map(|ty| match ty {
                        Ok(ty) => self.lower_type(ty, &scope, ctx),
                        Err(error) => TypeAnnotation {
                            span: error.span,
                            kind: TypeAnnotationKind::error(&self.compiler),
                        },
                    });

                    let attributes = self.lower_trait_attributes(decl.attributes, ctx, &scope);

                    self.declarations.traits.get_mut(&id).unwrap().value = Some(TraitDeclaration {
                        parameters,
                        ty,
                        attributes,
                    });

                    Some(AnyDeclaration::Trait(id))
                }
                StatementDeclarationKind::Constant(id, (scope, (parameters, bounds)), ty) => {
                    let ty = self.lower_type(ty, &scope, ctx);

                    let attributes = self.lower_constant_attributes(decl.attributes, &scope, ctx);

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
                    let (ty_scope, (parameters, bounds)) =
                        ty_pattern.unwrap_or_else(|| (scope.clone(), Default::default()));

                    let tr = match self.get(
                        trait_name,
                        trait_span,
                        AnyDeclaration::as_trait,
                        &trait_scope,
                    ) {
                        Some(tr) => {
                            self.declarations
                                .traits
                                .get_mut(&tr)
                                .unwrap()
                                .uses
                                .insert(trait_span);

                            tr
                        }
                        None => {
                            let (notes, fix) = self.diagnostic_notes_for_unresolved_name(
                                trait_span,
                                trait_name,
                                ctx,
                                AnyDeclaration::as_trait,
                                scope,
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
                    };

                    let trait_params = trait_params
                        .iter()
                        .map(|ty| match ty {
                            Ok(ty) => self.lower_type(ty, &ty_scope, ctx),
                            Err(error) => TypeAnnotation {
                                span: error.span,
                                kind: TypeAnnotationKind::error(&self.compiler),
                            },
                        })
                        .collect::<Vec<_>>();

                    let value = value
                        .map(|(colon_span, value)| InstanceValue {
                            colon_span: Some(colon_span),
                            value: self.lower_expr(value, scope, ctx),
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
                        Some(InstanceDeclaration {
                            parameters,
                            bounds,
                            tr_span: trait_span,
                            tr,
                            tr_parameters: trait_params,
                            value,
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
                            let value = self.lower_expr(expr, scope, ctx);
                            let pattern = self.lower_pattern(pattern, scope, ctx);

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

                                if let Some((id, _)) =
                                    self.peek(decl.name.unwrap(), AnyDeclaration::as_constant, scope)
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

                                for id in associated_parameters {
                                    let parameter =
                                        self.declarations.type_parameters.get(&id).unwrap();

                                    self.insert(
                                        parameter.name.unwrap(),
                                        AnyDeclaration::TypeParameter(id),
                                        &prev_constant_scope,
                                    );
                                }

                                let scope = self.child_scope(
                                    self.compiler.new_scope_id_in(expr.span().first().path),
                                    &prev_constant_scope
                                );

                                let mut value = self.lower_expr(expr, &scope, ctx);

                                let used_variables =
                                    self.generate_capture_list(&mut value, &scope);

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

                    Some(self.lower_expr(&expr, scope, ctx))
                },
            })
            .collect()
    }

    fn lower_statement<'a>(
        &mut self,
        statement: &'a ast::Statement<Analysis>,
        scope: &LoadedScopeId,
        ctx: &Context,
    ) -> Option<StatementDeclaration<'a>> {
        match statement {
            ast::Statement::Annotate(statement) => {
                let id = self.compiler.new_constant_id_in(self.file);

                let (span, name) = match &statement.value {
                    Ok(name) => (name.span, name.name),
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

                let (child_scope, (parameters, bounds), ty) =
                    match statement.annotation.as_ref().ok()? {
                        ast::ConstantTypeAnnotation::Type(annotation) => {
                            (scope.clone(), Default::default(), annotation)
                        }
                        ast::ConstantTypeAnnotation::TypeFunction(annotation) => {
                            let scope = self.child_scope(annotation.scope, scope);

                            let (params, bounds) = annotation
                                .pattern
                                .as_ref()
                                .map(|annotation| self.lower_type_pattern(annotation, &scope, ctx))
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

                            (scope, (params, bounds), ty)
                        }
                    };

                if let Some((existing_id, variant_info)) =
                    self.get(name, span, AnyDeclaration::as_constant, scope)
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
                        self.insert(name, AnyDeclaration::Constant(id, None), scope);
                    }
                } else {
                    self.insert(name, AnyDeclaration::Constant(id, None), scope);
                }

                self.declarations
                    .constants
                    .insert(id, Declaration::unresolved(Some(name), span));

                Some(StatementDeclaration {
                    span: statement.span(),
                    kind: StatementDeclarationKind::Constant(
                        id,
                        (child_scope, (parameters, bounds)),
                        &ty.ty,
                    ),
                    attributes: &statement.attributes,
                })
            }
            ast::Statement::Assign(statement) => {
                let assign_scope = scope; // FIXME: Remove

                match statement.value.as_ref().ok()? {
                    ast::AssignmentValue::Trait(value) => {
                        let (span, name) =
                            self.get_name_from_assignment(statement.pattern.as_ref().ok()?)?;

                        let id = self.compiler.new_trait_id_in(self.file);
                        self.insert(name, AnyDeclaration::Trait(id), scope);

                        self.declarations
                            .traits
                            .insert(id, Declaration::unresolved(Some(name), span));

                        Some(StatementDeclaration {
                            span: statement.span(),
                            kind: StatementDeclarationKind::Trait(id, None, value),
                            attributes: &statement.attributes,
                        })
                    }
                    ast::AssignmentValue::Type(value) => {
                        let (span, name) =
                            self.get_name_from_assignment(statement.pattern.as_ref().ok()?)?;

                        let id = self.compiler.new_type_id_in(self.file);
                        self.insert(name, AnyDeclaration::Type(id), scope);

                        self.declarations
                            .types
                            .insert(id, Declaration::unresolved(Some(name), span));

                        Some(StatementDeclaration {
                            span: statement.span(),
                            kind: StatementDeclarationKind::Type(id, None, value),
                            attributes: &statement.attributes,
                        })
                    }
                    ast::AssignmentValue::Syntax(_) => None,
                    ast::AssignmentValue::TypeFunction(value) => {
                        match value.value.as_deref().ok()? {
                            ast::AssignmentValue::Trait(trait_value) => {
                                let (span, name) = self
                                    .get_name_from_assignment(statement.pattern.as_ref().ok()?)?;

                                let child_scope = self.child_scope(value.scope, assign_scope);

                                let (parameters, bounds) = self.lower_type_pattern(
                                    value.pattern.as_ref().ok()?,
                                    &child_scope,
                                    ctx,
                                );

                                let id = self.compiler.new_trait_id_in(self.file);
                                self.insert(name, AnyDeclaration::Trait(id), scope);

                                self.declarations
                                    .traits
                                    .insert(id, Declaration::unresolved(Some(name), span));

                                Some(StatementDeclaration {
                                    span: statement.span(),
                                    kind: StatementDeclarationKind::Trait(
                                        id,
                                        Some((child_scope, (parameters, bounds))),
                                        trait_value,
                                    ),
                                    attributes: &statement.attributes,
                                })
                            }
                            ast::AssignmentValue::Type(ty_value) => {
                                let (span, name) = self
                                    .get_name_from_assignment(statement.pattern.as_ref().ok()?)?;

                                let child_scope = self.child_scope(value.scope, assign_scope);

                                let (parameters, bounds) = self.lower_type_pattern(
                                    value.pattern.as_ref().ok()?,
                                    &child_scope,
                                    ctx,
                                );

                                let id = self.compiler.new_type_id_in(self.file);
                                self.insert(name, AnyDeclaration::Type(id), scope);

                                self.declarations
                                    .types
                                    .insert(id, Declaration::unresolved(Some(name), span));

                                Some(StatementDeclaration {
                                    span: statement.span(),
                                    kind: StatementDeclarationKind::Type(
                                        id,
                                        Some((child_scope, scope.clone(), (parameters, bounds))),
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
                        }
                    }
                    ast::AssignmentValue::Expression(value) => {
                        match statement.pattern.as_ref().ok()? {
                            ast::AssignmentPattern::Pattern(pattern) => {
                                Some(StatementDeclaration {
                                    span: statement.span(),
                                    kind: StatementDeclarationKind::Queued(
                                        QueuedStatement::Assign(
                                            &pattern.pattern,
                                            &value.expression,
                                        ),
                                    ),
                                    attributes: &statement.attributes,
                                })
                            }
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
                                            self.assert_loaded_scope(pattern.trait_scope),
                                            &pattern.trait_parameters,
                                        ),
                                        Some((statement.colon_span, &value.expression)),
                                    ),
                                    attributes: &statement.attributes,
                                })
                            }
                            ast::AssignmentPattern::TypeFunction(pattern) => {
                                let child_scope = self.child_scope(pattern.scope, assign_scope);

                                let (parameters, bounds) = self.lower_type_pattern(
                                    pattern.type_pattern.as_ref().ok()?,
                                    &child_scope,
                                    ctx,
                                );

                                match pattern.assignment_pattern.as_deref().ok()? {
                                    ast::AssignmentPattern::Instance(pattern) => {
                                        let id = self.compiler.new_constant_id_in(self.file);

                                        self.declarations.instances.insert(
                                            id,
                                            Declaration::unresolved(None, pattern.span()),
                                        );

                                        Some(StatementDeclaration {
                                            span: statement.span(),
                                            kind: StatementDeclarationKind::Instance(
                                                id,
                                                Some((child_scope.clone(), (parameters, bounds))),
                                                (
                                                    pattern.trait_span,
                                                    pattern.trait_name,
                                                    self.assert_loaded_scope(pattern.trait_scope),
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
                }
            }
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
                            self.assert_loaded_scope(statement.trait_scope),
                            &statement.trait_parameters,
                        ),
                        None,
                    ),
                    attributes: &statement.attributes,
                })
            }
            ast::Statement::TypeFunction(statement) => {
                let child_scope = self.child_scope(statement.scope, scope);

                let (parameters, bounds) =
                    self.lower_type_pattern(statement.pattern.as_ref().ok()?, &child_scope, ctx);

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
                                Some((child_scope, (parameters, bounds))),
                                (
                                    statement.trait_span,
                                    statement.trait_name,
                                    self.assert_loaded_scope(statement.trait_scope),
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

    fn lower_expr(
        &mut self,
        expr: &ast::Expression<Analysis>,
        scope: &LoadedScopeId,
        ctx: &Context,
    ) -> Expression {
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
                            Ok(expr) => self.lower_expr(expr, scope, &ctx),
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
                kind: ExpressionKind::Text(expr.text),
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
                let name_scope = self.assert_loaded_scope(expr.scope);
                match self.resolve_value(expr.span, expr.name, &name_scope) {
                    Some(value) => Expression {
                        id: self.compiler.new_expression_id(ctx.owner),
                        span: expr.span,
                        kind: value,
                    },
                    None => {
                        let (notes, fix) = self.diagnostic_notes_for_unresolved_name(
                            expr.span,
                            expr.name,
                            ctx,
                            AnyDeclaration::as_any,
                            scope,
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
                }
            }
            ast::Expression::Block(expr) => {
                let scope = self.child_scope(expr.scope, scope);
                let statements = self.lower_statements(&expr.statements, &scope, ctx);

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

                        let ty_name_scope = self.assert_loaded_scope(ty_name.scope);
                        if let Some(id) = self.get(
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
                                    let scope = self.child_scope(block.scope, scope);

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

                                    let fields = 'parse: {
                                        if block.statements.len() == 1 {
                                            let statement = match block.statements.last().unwrap() {
                                                Ok(statement) => statement,
                                                Err(_) => {
                                                    break 'parse Vec::new();
                                                }
                                            };

                                            if let ast::Statement::Expression(
                                                ast::ExpressionStatement {
                                                    expression: ast::Expression::Call(expr),
                                                    ..
                                                },
                                            ) = statement
                                            {
                                                if let Some(fields) = std::iter::once(
                                                    expr.function.clone().map(|expr| *expr),
                                                )
                                                .chain(expr.inputs.clone())
                                                .map(|expr| match expr {
                                                    Ok(ast::Expression::Name(ref name_expr)) => {
                                                        Some((
                                                            (name_expr.span, name_expr.name),
                                                            expr.clone(),
                                                        ))
                                                    }
                                                    _ => None,
                                                })
                                                .collect::<Option<Vec<_>>>()
                                                {
                                                    break 'parse fields
                                                        .into_iter()
                                                        .filter_map(|(name, expr)| {
                                                            Some((
                                                                name,
                                                                self.lower_expr(
                                                                    &expr.ok()?,
                                                                    &scope,
                                                                    ctx,
                                                                ),
                                                            ))
                                                        })
                                                        .collect();
                                                }
                                            };
                                        }

                                        block.statements
                                                .iter()
                                                .filter_map(|s| Some(match s.as_ref().ok()? {
                                                    ast::Statement::Assign(statement) => {
                                                        match statement.pattern.as_ref().ok()? {
                                                            ast::AssignmentPattern::Pattern(ast::PatternAssignmentPattern { pattern: ast::Pattern::Name(name) }) => {
                                                                let value = match statement.value.as_ref().ok()? {
                                                                    ast::AssignmentValue::Expression(value) => &value.expression,
                                                                    value => {
                                                                        self.compiler.add_error(
                                                                            "syntax error",
                                                                            vec![Note::primary(
                                                                                value.span(),
                                                                                "expected expression",
                                                                            )],
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
                                                            },
                                                        }
                                                    },
                                                    ast::Statement::Expression(ast::ExpressionStatement { expression: expr @ ast::Expression::Name(name), .. }) => {
                                                        ((name.span, name.name), expr.clone())
                                                    },
                                                    // TODO: 'use' inside instantiation
                                                    _ => {
                                                        self.compiler.add_error(
                                                            "structure instantiation may not contain executable statements", vec![Note::primary(
                                                                block.span,
                                                                "try removing this",
                                                            )],
                                                            "syntax-error",
                                                        );

                                                        return None;
                                                    }
                                                }))
                                                .collect::<Vec<_>>()
                                                .into_iter()
                                                .map(|(name, value)| (name, self.lower_expr(&value, &scope, ctx)))
                                                .collect()
                                    };

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
                                    function_call!(
                                        self.lower_expr(function, scope, ctx),
                                        &expr.inputs
                                    )
                                }
                            }
                        } else if let Some(id) = self.get(
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
                        } else if let Some(id) = self.get(
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
                        } else {
                            function_call!(self.lower_expr(function, scope, ctx), &expr.inputs)
                        }
                    }
                    _ => function_call!(self.lower_expr(function, scope, ctx), &expr.inputs),
                }
            }
            ast::Expression::Function(expr) => {
                let scope = self.child_scope(expr.scope, scope);

                let pattern = match &expr.pattern {
                    Ok(pattern) => self.lower_pattern(pattern, &scope, ctx),
                    Err(error) => Pattern {
                        span: error.span,
                        kind: PatternKind::error(&self.compiler),
                    },
                };

                let mut ctx = ctx.clone();
                ctx.in_function = true;

                let mut body = match &expr.body {
                    Ok(expr) => self.lower_expr(expr, &scope, &ctx),
                    Err(error) => Expression {
                        id: self.compiler.new_expression_id(ctx.owner),
                        span: error.span,
                        kind: ExpressionKind::error(&self.compiler),
                    },
                };

                let captures = self.generate_capture_list(&mut body, &scope);

                Expression {
                    id: self.compiler.new_expression_id(ctx.owner),
                    span: expr.span(),
                    kind: ExpressionKind::Function(pattern, Box::new(body), captures),
                }
            }
            ast::Expression::When(expr) => {
                let input = match &expr.input {
                    Ok(expr) => self.lower_expr(expr, scope, ctx),
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

                                let scope = self.child_scope(arm.scope, scope);

                                let (pattern, guard) = match &arm.pattern {
                                    Ok(pattern) => match pattern {
                                        ast::WhenPattern::Where(pattern) => {
                                            let inner_pattern = match pattern.pattern.as_deref() {
                                                Ok(value) => self.lower_pattern(value, &scope, ctx),
                                                Err(error) => Pattern {
                                                    span: error.span,
                                                    kind: PatternKind::error(&self.compiler),
                                                },
                                            };

                                            let condition = match pattern.condition.as_deref() {
                                                Ok(value) => self.lower_expr(value, &scope, ctx),
                                                Err(error) => Expression {
                                                    id: self.compiler.new_expression_id(ctx.owner),
                                                    span: error.span,
                                                    kind: ExpressionKind::error(&self.compiler),
                                                },
                                            };

                                            (inner_pattern, Some(condition))
                                        }
                                        ast::WhenPattern::Pattern(pattern) => (
                                            self.lower_pattern(&pattern.pattern, &scope, ctx),
                                            None,
                                        ),
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
                                    Ok(expr) => self.lower_expr(expr, &scope, ctx),
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
                        Ok(expr) => self.lower_expr(expr, scope, ctx),
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
                        Ok(expr) => self.lower_expr(expr, scope, ctx),
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
                        Ok(expr) => self.lower_expr(expr, scope, ctx),
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
                    Ok(expr) => self.lower_expr(expr, scope, ctx),
                    Err(error) => Expression {
                        id: self.compiler.new_expression_id(ctx.owner),
                        span: error.span,
                        kind: ExpressionKind::error(&self.compiler),
                    },
                };

                let ty = match &expr.ty {
                    Ok(ty) => self.lower_type(ty, scope, ctx),
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
                            Ok(expr) => self.lower_expr(expr, scope, ctx),
                            Err(error) => Expression {
                                id: self.compiler.new_expression_id(ctx.owner),
                                span: error.span,
                                kind: ExpressionKind::error(&self.compiler),
                            },
                        })
                        .collect(),
                ),
            },
            ast::Expression::Format(expr) => Expression {
                id: self.compiler.new_expression_id(ctx.owner),
                span: expr.span(),
                kind: ExpressionKind::Format(
                    expr.segments
                        .iter()
                        .map(|segment| {
                            (
                                segment.string,
                                match &segment.expr {
                                    Ok(expr) => self.lower_expr(expr, scope, ctx),
                                    Err(error) => Expression {
                                        id: self.compiler.new_expression_id(ctx.owner),
                                        span: error.span,
                                        kind: ExpressionKind::error(&self.compiler),
                                    },
                                },
                            )
                        })
                        .collect(),
                    expr.trailing_segment,
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
                            Ok((span, name)) => {
                                match self.get(name, span, AnyDeclaration::as_constant, scope) {
                                    Some((id, _)) => Some(id),
                                    _ => {
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
                                }
                            }
                            Err(_) => None,
                        };

                        let value = match &clause.value {
                            Ok(expr) => self.lower_expr(expr, scope, ctx),
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
                    Ok(expr) => self.lower_expr(expr, scope, ctx),
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
                    Ok(expr) => self.lower_expr(expr, scope, ctx),
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
        }
    }

    fn lower_pattern(
        &mut self,
        pattern: &ast::Pattern<Analysis>,
        scope: &LoadedScopeId,
        ctx: &Context,
    ) -> Pattern {
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
                kind: PatternKind::Text(pattern.text),
            },
            ast::Pattern::Name(pattern) => {
                let scope = self.assert_loaded_scope(pattern.scope);

                match self.peek(pattern.name, AnyDeclaration::as_constant, &scope) {
                    Some((id, Some((ty, variant)))) => {
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
                    _ => {
                        let var = self.compiler.new_variable_id_in(self.file);

                        self.insert(pattern.name, AnyDeclaration::Variable(var), &scope);

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
                                    Ok(pattern) => self.lower_pattern(pattern, scope, ctx),
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
                                        scope: destructuring.scope,
                                    }),
                                    scope,
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
                                                    scope: destructuring.scope,
                                                }),
                                                scope,
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

                if let Some(ty) = self.get(
                    pattern.name,
                    pattern.name_span,
                    AnyDeclaration::as_type,
                    scope,
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
                                    Ok(value) => self.lower_pattern(value, scope, ctx),
                                    Err(error) => Pattern {
                                        span: error.span,
                                        kind: PatternKind::error(&self.compiler),
                                    },
                                })
                                .collect(),
                        ),
                    }
                } else if let Some((id, Some((ty, variant)))) = self.get(
                    pattern.name,
                    pattern.name_span,
                    AnyDeclaration::as_constant,
                    scope,
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
                                    Ok(value) => self.lower_pattern(value, scope, ctx),
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
                        scope,
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
                    Ok(value) => self.lower_pattern(value, scope, ctx),
                    Err(error) => Pattern {
                        span: error.span,
                        kind: PatternKind::error(&self.compiler),
                    },
                };

                let ty = match &pattern.ty {
                    Ok(ty) => self.lower_type(ty, scope, ctx),
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
                    Ok(value) => self.lower_pattern(value, scope, ctx),
                    Err(error) => Pattern {
                        span: error.span,
                        kind: PatternKind::error(&self.compiler),
                    },
                };

                let rhs = match pattern.right.as_deref() {
                    Ok(value) => self.lower_pattern(value, scope, ctx),
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
                            Ok(pattern) => self.lower_pattern(pattern, scope, ctx),
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

    #[allow(clippy::only_used_in_recursion)]
    fn lower_type(
        &mut self,
        ty: &ast::Type<Analysis>,
        scope: &LoadedScopeId,
        ctx: &Context,
    ) -> TypeAnnotation {
        match ty {
            ast::Type::Function(ty) => {
                let input = match &ty.input {
                    Ok(ty) => self.lower_type(ty, scope, ctx),
                    Err(error) => TypeAnnotation {
                        span: error.span,
                        kind: TypeAnnotationKind::Error(error.trace.clone()),
                    },
                };

                let output = match &ty.output {
                    Ok(ty) => self.lower_type(ty, scope, ctx),
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
                            Ok(ty) => self.lower_type(ty, scope, ctx),
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
                        Ok(ty) => self.lower_type(ty, scope, ctx),
                        Err(error) => TypeAnnotation {
                            span: error.span,
                            kind: TypeAnnotationKind::Error(error.trace.clone()),
                        },
                    })
                    .collect();

                if let Some(id) = self.get(ty.name, ty.span, AnyDeclaration::as_type, scope) {
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
                } else if let Some(param) =
                    self.get(ty.name, ty.span, AnyDeclaration::as_type_parameter, scope)
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
                } else if let Some(builtin) =
                    self.get(ty.name, ty.span, AnyDeclaration::as_builtin_type, scope)
                {
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
                } else {
                    let (notes, fix) = self.diagnostic_notes_for_unresolved_name(
                        ty.span,
                        ty.name,
                        ctx,
                        AnyDeclaration::as_type,
                        scope,
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
        scope: &LoadedScopeId,
        ctx: &Context,
    ) -> (Vec<TypeParameterId>, Vec<Bound>) {
        macro_rules! generate_type_parameters {
            ($params:expr) => {
                $params
                    .into_iter()
                    .map(|((name_span, name), default, infer)| {
                        let id = self.compiler.new_type_parameter_id_in(self.file);

                        self.declarations.type_parameters.insert(
                            id,
                            Declaration::resolved(
                                Some(name),
                                name_span,
                                TypeParameterDeclaration { infer, default },
                            ),
                        );

                        self.insert(name, AnyDeclaration::TypeParameter(id), scope);

                        id
                    })
                    .collect()
            };
        }

        match type_pattern {
            ast::TypePattern::Where(type_pattern) => {
                let params = match &type_pattern.pattern {
                    Ok(lhs) => {
                        let params = match lhs.as_ref() {
                            ast::TypePattern::Name(pattern) => {
                                vec![((pattern.span, pattern.name), None, false)]
                            }
                            ast::TypePattern::Default(pattern) => {
                                match (&pattern.name, &pattern.ty) {
                                    (Ok((name_span, name)), Ok(ty)) => {
                                        let ty = self.lower_type(ty, scope, ctx);
                                        vec![((*name_span, *name), Some(ty), false)]
                                    }
                                    _ => Vec::new(),
                                }
                            }
                            ast::TypePattern::Infer(pattern) => match &pattern.name {
                                Ok((name_span, name)) => {
                                    vec![((*name_span, *name), None, true)]
                                }
                                _ => Vec::new(),
                            },
                            ast::TypePattern::List(pattern) => pattern
                                .patterns
                                .iter()
                                .filter_map(|pattern| match pattern {
                                    Ok(pattern) => match pattern {
                                        ast::TypePattern::Name(pattern) => {
                                            Some(((pattern.span, pattern.name), None, false))
                                        }
                                        ast::TypePattern::Default(pattern) => {
                                            match (&pattern.name, &pattern.ty) {
                                                (Ok((name_span, name)), Ok(ty)) => {
                                                    let ty = self.lower_type(ty, scope, ctx);
                                                    Some(((*name_span, *name), Some(ty), false))
                                                }
                                                _ => None,
                                            }
                                        }
                                        ast::TypePattern::Infer(pattern) => match &pattern.name {
                                            Ok((name_span, name)) => {
                                                Some(((*name_span, *name), None, true))
                                            }
                                            _ => None,
                                        },
                                        ast::TypePattern::List(pattern) => {
                                            self.compiler.add_error(
                                                "higher-kinded types are not yet supported",
                                                vec![Note::primary(
                                                    pattern.span,
                                                    "try removing this",
                                                )],
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
                        };

                        generate_type_parameters!(params)
                    }
                    Err(_) => Vec::new(),
                };

                let bounds = type_pattern
                    .bounds
                    .iter()
                    .filter_map(|bound| match bound {
                        Ok(bound) => {
                            let tr = if let Some(tr) = self.get(
                                bound.trait_name,
                                bound.trait_span,
                                AnyDeclaration::as_trait,
                                scope,
                            ) {
                                self.declarations
                                    .traits
                                    .get_mut(&tr)
                                    .unwrap()
                                    .uses
                                    .insert(bound.trait_span);

                                tr
                            } else {
                                let (notes, fix) = self.diagnostic_notes_for_unresolved_name(
                                    bound.trait_span,
                                    bound.trait_name,
                                    ctx,
                                    AnyDeclaration::as_trait,
                                    scope,
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
                            };

                            let parameters = bound
                                .parameters
                                .iter()
                                .map(|ty| match ty {
                                    Ok(ty) => self.lower_type(ty, scope, ctx),
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
                let params =
                    generate_type_parameters!(vec![((pattern.span, pattern.name), None, false)]);
                (params, Vec::new())
            }
            ast::TypePattern::Default(pattern) => match (&pattern.name, &pattern.ty) {
                (Ok((name_span, name)), Ok(ty)) => {
                    let ty = self.lower_type(ty, scope, ctx);
                    let params =
                        generate_type_parameters!(vec![((*name_span, *name), Some(ty), false)]);
                    (params, Vec::new())
                }
                _ => (Vec::new(), Vec::new()),
            },
            ast::TypePattern::Infer(pattern) => match &pattern.name {
                Ok((name_span, name)) => {
                    let params = generate_type_parameters!(vec![((*name_span, *name), None, true)]);
                    (params, Vec::new())
                }
                _ => (Vec::new(), Vec::new()),
            },
            ast::TypePattern::List(pattern) => {
                let params = pattern
                    .patterns
                    .iter()
                    .filter_map(|pattern| match pattern {
                        Ok(pattern) => match pattern {
                            ast::TypePattern::Name(pattern) => {
                                Some(((pattern.span, pattern.name), None, false))
                            }
                            ast::TypePattern::Default(pattern) => {
                                match (&pattern.name, &pattern.ty) {
                                    (Ok((name_span, name)), Ok(ty)) => {
                                        let ty = self.lower_type(ty, scope, ctx);
                                        Some(((*name_span, *name), Some(ty), false))
                                    }
                                    _ => None,
                                }
                            }
                            ast::TypePattern::Infer(pattern) => match &pattern.name {
                                Ok((name_span, name)) => Some(((*name_span, *name), None, true)),
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

                let params = generate_type_parameters!(params);

                (params, Vec::new())
            }
        }
    }

    fn lower_decl_attributes(
        &self,
        statement_attributes: &ast::StatementAttributes<Analysis>,
        _scope: &LoadedScopeId,
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
        }
    }

    fn lower_syntax_attributes(
        &mut self,
        statement_attributes: &ast::StatementAttributes<Analysis>,
        _ctx: &Context,
        scope: &LoadedScopeId,
    ) -> SyntaxAttributes {
        SyntaxAttributes {
            decl_attributes: self.lower_decl_attributes(statement_attributes, scope),
        }
    }

    fn lower_type_attributes(
        &mut self,
        attributes: &ast::StatementAttributes<Analysis>,
        ctx: &Context,
        scope: &LoadedScopeId,
    ) -> TypeAttributes {
        // TODO: Raise errors for misused attributes

        TypeAttributes {
            decl_attributes: self.lower_decl_attributes(attributes, scope),
            on_mismatch: attributes
                .on_mismatch
                .clone()
                .into_iter()
                .filter_map(|attribute| {
                    let param = match attribute.type_parameter {
                        Some((span, name)) => {
                            if let Some(param) =
                                self.get(name, span, AnyDeclaration::as_type_parameter, scope)
                            {
                                self.declarations
                                    .type_parameters
                                    .get_mut(&param)
                                    .unwrap()
                                    .uses
                                    .insert(span);

                                Some(param)
                            } else {
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
                        Ok(ty) => self.lower_type(ty, scope, ctx),
                        Err(error) => TypeAnnotation {
                            span: error.span,
                            kind: TypeAnnotationKind::error(&self.compiler),
                        },
                    };

                    (ty, attribute.replacement.clone())
                })
                .collect(),
        }
    }

    fn lower_trait_attributes(
        &mut self,
        attributes: &ast::StatementAttributes<Analysis>,
        ctx: &Context,
        scope: &LoadedScopeId,
    ) -> TraitAttributes {
        // TODO: Raise errors for misused attributes

        TraitAttributes {
            decl_attributes: self.lower_decl_attributes(attributes, scope),
            on_unimplemented: attributes.on_unimplemented.as_ref().and_then(|attribute| {
                Some((
                    attribute
                        .segments
                        .iter()
                        .cloned()
                        .map(|segment| {
                            let (param_span, param_name) = segment.param.ok()?;

                            let param = if let Some(id) = self.get(
                                param_name,
                                param_span,
                                AnyDeclaration::as_type_parameter,
                                scope,
                            ) {
                                id
                            } else {
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
                                            format!("cannot find type parameter `{param_name}`"),
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
                            };

                            Some((segment.string, param))
                        })
                        .collect::<Option<_>>()?,
                    attribute.trailing_segment,
                ))
            }),
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
        scope: &LoadedScopeId,
        _ctx: &Context,
    ) -> ConstantAttributes {
        // TODO: Raise errors for misused attributes

        ConstantAttributes {
            decl_attributes: self.lower_decl_attributes(attributes, scope),
            is_contextual: attributes.contextual.is_some(),
            is_specialization: attributes.specialize.is_some(),
        }
    }

    fn get_name_from_assignment(
        &mut self,
        pattern: &ast::AssignmentPattern<Analysis>,
    ) -> Option<(SpanList, InternedString)> {
        if let ast::AssignmentPattern::Pattern(pattern) = pattern {
            if let ast::Pattern::Name(pattern) = &pattern.pattern {
                return Some((pattern.span, pattern.name));
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
        scope: &LoadedScopeId,
    ) -> Option<ExpressionKind> {
        if let Some(id) = self.get(name, span, AnyDeclaration::as_variable, scope) {
            self.declarations
                .variables
                .get_mut(&id)
                .unwrap()
                .uses
                .insert(span);

            Some(ExpressionKind::Variable(id))
        } else if let Some((id, None)) = self.get(name, span, AnyDeclaration::as_constant, scope) {
            self.declarations
                .constants
                .get_mut(&id)
                .unwrap()
                .uses
                .insert(span);

            Some(ExpressionKind::Constant(id))
        } else if let Some(id) = self.get(name, span, AnyDeclaration::as_type, scope) {
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
                TypeDeclarationKind::Marker => Some(ExpressionKind::Marker(id)),
                _ => {
                    self.compiler.add_error(
                        "cannot use type as value",
                        vec![Note::primary(span, "try instantiating the type")],
                        "type-as-value",
                    );

                    Some(ExpressionKind::error(&self.compiler))
                }
            }
        } else if let Some(id) = self.get(name, span, AnyDeclaration::as_trait, scope) {
            self.declarations
                .traits
                .get_mut(&id)
                .unwrap()
                .uses
                .insert(span);

            Some(ExpressionKind::Trait(id))
        } else if let Some(id) = self.get(name, span, AnyDeclaration::as_builtin_type, scope) {
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

            Some(ExpressionKind::error(&self.compiler))
        } else if let Some((id, Some(_))) = self.get(name, span, AnyDeclaration::as_constant, scope)
        {
            self.declarations
                .constants
                .get_mut(&id)
                .unwrap()
                .uses
                .insert(span);

            Some(ExpressionKind::Constant(id))
        } else if let Some(id) = self.get(name, span, AnyDeclaration::as_type_parameter, scope) {
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

            Some(ExpressionKind::error(&self.compiler))
        } else {
            None
        }
    }

    fn generate_variant_constructor(
        &mut self,
        id: TypeId,
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
                let var = self.compiler.new_variable_id_in(self.file);

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
        expr: &mut Expression,
        child_scope: &LoadedScopeId,
    ) -> CaptureList {
        let mut captures = CaptureList::new();
        expr.traverse_mut(|expr| {
            if let ExpressionKind::Variable(var) = expr.kind {
                if !self.declares_in(var, child_scope) {
                    captures.push((var, expr.span));
                }
            }
        });

        captures
    }

    fn diagnostic_notes_for_unresolved_name<T>(
        &self,
        span: SpanList,
        name: InternedString,
        ctx: &Context,
        kind: fn(AnyDeclaration) -> Option<T>,
        scope: &LoadedScopeId,
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
        scope: &LoadedScopeId,
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
                Fix::new("add whitespace around operator", FixRange::replace(span.first()), format!("{lhs} {op} {rhs}")),
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
