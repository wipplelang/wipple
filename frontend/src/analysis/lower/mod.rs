#![allow(clippy::type_complexity)]

mod builtins;

pub use expand::SyntaxDeclarationAttributes;

use crate::{
    analysis::{ast, expand},
    diagnostics::*,
    helpers::{InternedString, Shared},
    parse::Span,
    BuiltinTypeId, Compiler, ConstantId, FieldIndex, FilePath, ScopeId, TemplateId, TraitId,
    TypeId, TypeParameterId, VariableId, VariantIndex,
};
use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    hash::Hash,
    mem,
    str::FromStr,
    sync::Arc,
};
use strum::{Display, EnumString};

#[derive(Debug, Clone)]
pub struct File<Decls = Declarations> {
    pub span: Span,
    pub declarations: Decls,
    pub global_attributes: FileAttributes,
    pub exported: ScopeValues,
    pub scopes: BTreeMap<ScopeId, Scope>,
    pub specializations: BTreeMap<ConstantId, Vec<ConstantId>>,
    pub block: Vec<Expression>,
}

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for File {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        use crate::FileIds;

        Ok(File {
            span: Span::arbitrary(u)?,
            // Ensure the program only refers to existing declarations by
            //  generating every possible declaration
            declarations: {
                let template_ids = FileIds::list_arbitrary_template_ids();
                let (constant_ids, instance_ids) = FileIds::split_arbitrary_constant_ids();

                Declarations {
                    syntaxes: template_ids
                        .map(|id| Ok((id, ast::SyntaxDeclaration::arbitrary(u)?)))
                        .collect::<Result<_, _>>()?,
                    types: FileIds::list_arbitrary_type_ids()
                        .map(|id| Ok((id, Declaration::arbitrary(u)?)))
                        .collect::<Result<_, _>>()?,
                    type_parameters: FileIds::list_arbitrary_type_parameter_ids()
                        .map(|id| Ok((id, Declaration::arbitrary(u)?)))
                        .collect::<Result<_, _>>()?,
                    traits: FileIds::list_arbitrary_trait_ids()
                        .map(|id| Ok((id, Declaration::arbitrary(u)?)))
                        .collect::<Result<_, _>>()?,
                    builtin_types: FileIds::list_arbitrary_builtin_type_ids()
                        .map(|id| Ok((id, Declaration::arbitrary(u)?)))
                        .collect::<Result<_, _>>()?,
                    constants: constant_ids
                        .map(|id| Ok((id, Declaration::arbitrary(u)?)))
                        .collect::<Result<_, _>>()?,
                    instances: instance_ids
                        .map(|id| Ok((id, Declaration::arbitrary(u)?)))
                        .collect::<Result<_, _>>()?,
                    variables: FileIds::list_arbitrary_variable_ids()
                        .map(|id| Ok((id, Declaration::arbitrary(u)?)))
                        .collect::<Result<_, _>>()?,
                }
            },
            global_attributes: FileAttributes::arbitrary(u)?,
            exported: ScopeValues::new(),
            scopes: BTreeMap::new(),
            specializations: BTreeMap::arbitrary(u)?,
            block: (0..u.int_in_range(64..=128)?)
                .map(|_| Expression::arbitrary(u))
                .collect::<Result<_, _>>()?,
        })
    }
}

#[derive(Debug, Clone, Default)]
pub struct Declarations {
    pub syntaxes: BTreeMap<TemplateId, ast::SyntaxDeclaration>,
    pub types: BTreeMap<TypeId, Declaration<Type>>,
    pub type_parameters: BTreeMap<TypeParameterId, Declaration<()>>,
    pub traits: BTreeMap<TraitId, Declaration<Trait>>,
    pub builtin_types: BTreeMap<BuiltinTypeId, Declaration<BuiltinType>>,
    pub constants: BTreeMap<ConstantId, Declaration<Constant>>,
    pub instances: BTreeMap<ConstantId, Declaration<Instance>>,
    pub variables: BTreeMap<VariableId, Declaration<()>>,
}

#[derive(Debug, Clone, Default)]
struct UnresolvedDeclarations {
    pub syntaxes: BTreeMap<TemplateId, ast::SyntaxDeclaration>,
    pub types: BTreeMap<TypeId, Declaration<Option<Type>>>,
    pub type_parameters: BTreeMap<TypeParameterId, Declaration<()>>,
    pub traits: BTreeMap<TraitId, Declaration<Option<Trait>>>,
    pub builtin_types: BTreeMap<BuiltinTypeId, Declaration<BuiltinType>>,
    pub constants: BTreeMap<ConstantId, Declaration<Option<UnresolvedConstant>>>,
    pub instances: BTreeMap<ConstantId, Declaration<Option<Instance>>>,
    pub variables: BTreeMap<VariableId, Declaration<()>>,
}

impl UnresolvedDeclarations {
    fn resolve(self) -> Declarations {
        Declarations {
            syntaxes: self.syntaxes,
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
        }
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Declaration<T> {
    pub name: Option<InternedString>,
    pub span: Span,
    pub uses: HashSet<Span>,
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
    fn unresolved(name: Option<InternedString>, span: Span) -> Self {
        Declaration {
            name,
            span,
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
    fn resolved(name: Option<InternedString>, span: Span, value: T) -> Self {
        Declaration {
            name,
            span,
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
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub struct DeclarationAttributes {
    pub help: Vec<InternedString>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Type {
    pub kind: TypeKind,
    pub params: Vec<(Span, TypeParameterId)>,
    pub attributes: TypeAttributes,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub struct TypeAttributes {
    pub decl_attributes: DeclarationAttributes,
    pub on_mismatch: Vec<(Option<TypeParameterId>, InternedString)>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", serde(tag = "type", content = "value"))]
pub enum TypeKind {
    Marker,
    Structure(
        #[cfg_attr(feature = "arbitrary", arbitrary(with = |u: &mut arbitrary::Unstructured| {
            FieldIndex::list_arbitrary()
                .map(|_| TypeField::arbitrary(u))
                .collect::<Result<_, _>>()
        }))]
        Vec<TypeField>,
        HashMap<InternedString, FieldIndex>,
    ),
    Enumeration(
        #[cfg_attr(feature = "arbitrary", arbitrary(with = |u: &mut arbitrary::Unstructured| {
            VariantIndex::list_arbitrary()
                .map(|_| TypeVariant::arbitrary(u))
                .collect::<Result<_, _>>()
        }))]
        Vec<TypeVariant>,
        HashMap<InternedString, VariantIndex>,
    ),
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct TypeField {
    pub ty: TypeAnnotation,
    pub attributes: DeclarationAttributes,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct TypeVariant {
    pub constructor: ConstantId,
    pub tys: Vec<TypeAnnotation>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct BuiltinType {
    pub kind: BuiltinTypeKind,
    pub attributes: DeclarationAttributes,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum BuiltinTypeKind {
    Number,
    Integer,
    Natural,
    Byte,
    Signed,
    Unsigned,
    Float,
    Double,
    Boolean,
    Text,
    List,
    Mutable,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Trait {
    pub parameters: Vec<(Span, TypeParameterId)>,
    pub ty: Option<TypeAnnotation>,
    pub attributes: TraitAttributes,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub struct TraitAttributes {
    pub decl_attributes: DeclarationAttributes,
    pub on_unimplemented: Option<InternedString>,
    pub allow_overlapping_instances: bool,
}

#[derive(Debug, Clone)]
struct UnresolvedConstant {
    pub parameters: Vec<(Span, TypeParameterId)>,
    pub bounds: Vec<Bound>,
    pub ty: TypeAnnotation,
    pub value: Shared<Option<Expression>>,
    pub attributes: ConstantAttributes,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Constant {
    pub parameters: Vec<(Span, TypeParameterId)>,
    pub bounds: Vec<Bound>,
    pub ty: TypeAnnotation,
    pub value: Expression,
    pub attributes: ConstantAttributes,
}

impl From<Constant> for UnresolvedConstant {
    fn from(constant: Constant) -> Self {
        UnresolvedConstant {
            parameters: constant.parameters,
            bounds: constant.bounds,
            ty: constant.ty,
            value: Shared::new(Some(constant.value)),
            attributes: constant.attributes,
        }
    }
}

impl Resolve<Constant> for UnresolvedConstant {
    fn resolve(self) -> Constant {
        Constant {
            parameters: self.parameters,
            bounds: self.bounds,
            ty: self.ty,
            value: self.value.into_unique().expect("uninitialized constant"),
            attributes: self.attributes,
        }
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub struct ConstantAttributes {
    pub decl_attributes: DeclarationAttributes,
    pub is_specialization: bool,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Bound {
    pub span: Span,
    pub tr: TraitId,
    pub parameters: Vec<TypeAnnotation>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Instance {
    pub params: Vec<(Span, TypeParameterId)>,
    pub bounds: Vec<Bound>,
    pub tr: TraitId,
    pub trait_params: Vec<TypeAnnotation>,
    pub value: Option<Expression>,
}

#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub struct FileAttributes {
    pub language_items: LanguageItems,
    pub recursion_limit: Option<usize>,
}

#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub struct LanguageItems {
    pub boolean: Option<TypeId>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Expression {
    pub span: Span,

    #[cfg_attr(feature = "arbitrary", arbitrary(with = |u: &mut arbitrary::Unstructured| {
        Ok(NonErrorExpressionKind::arbitrary(u)?.into())
    }))]
    pub kind: ExpressionKind,
}

non_error_kind! {
    #[derive(Debug, Clone)]
    #[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
    pub enum ExpressionKind {
        Marker(id: TypeId),
        Constant(id: ConstantId),
        Trait(id: TraitId),
        Variable(id: VariableId),
        Text(value: InternedString),
        Number(value: InternedString),
        Block(exprs: Vec<Expression>, top_level: bool),
        End(expr: Box<Expression>),
        Call(func: Box<Expression>, input: Box<Expression>),
        Function(input: Pattern, body: Box<Expression>, captures: CaptureList),
        When(expr: Box<Expression>, arms: Vec<Arm>),
        External(namespace: InternedString, identifier: InternedString, inputs: Vec<Expression>),
        Runtime(func: RuntimeFunction, inputs: Vec<Expression>),
        Annotate(expr: Box<Expression>, ty: TypeAnnotation),
        Initialize(var: Pattern, value: Box<Expression>),
        Instantiate(id: TypeId, fields: Vec<(InternedString, Expression)>),
        Variant(id: TypeId, variant: VariantIndex, values: Vec<Expression>),
        Tuple(exprs: Vec<Expression>),
    }
}

impl Expression {
    pub(crate) fn error(compiler: &Compiler, span: Span) -> Self {
        Expression {
            span,
            kind: ExpressionKind::Error(compiler.backtrace()),
        }
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Arm {
    pub span: Span,
    pub pattern: Pattern,
    pub body: Expression,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Pattern {
    pub span: Span,

    #[cfg_attr(feature = "arbitrary",arbitrary(with = |u: &mut arbitrary::Unstructured| {
        Ok(NonErrorPatternKind::arbitrary(u)?.into())
    }))]
    pub kind: PatternKind,
}

non_error_kind! {
    #[derive(Debug, Clone)]
    #[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
    pub enum PatternKind {
        Wildcard,
        Number(value: InternedString),
        Text(value: InternedString),
        Variable(id: VariableId),
        Destructure(fields: HashMap<InternedString, Pattern>),
        Variant(id: TypeId, variant: VariantIndex, values: Vec<Pattern>),
        Annotate(pat: Box<Pattern>, ty: TypeAnnotation),
        Or(left: Box<Pattern>, right: Box<Pattern>),
        Where(pat: Box<Pattern>, expr: Box<Expression>),
        Tuple(values: Vec<Pattern>),
    }
}

impl PatternKind {
    fn error(compiler: &Compiler) -> Self {
        PatternKind::Error(compiler.backtrace())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct TypeAnnotation {
    pub span: Span,
    pub kind: TypeAnnotationKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum TypeAnnotationKind {
    Error,
    Placeholder,
    Named(TypeId, Vec<TypeAnnotation>),
    Parameter(TypeParameterId),
    Builtin(BuiltinTypeId, Vec<TypeAnnotation>),
    Function(Box<TypeAnnotation>, Box<TypeAnnotation>),
    Tuple(Vec<TypeAnnotation>),
}

impl TypeAnnotation {
    fn error(span: Span) -> Self {
        TypeAnnotation {
            span,
            kind: TypeAnnotationKind::Error,
        }
    }

    pub fn span_of(&self, other: &TypeAnnotationKind) -> Option<Span> {
        if &self.kind == other {
            return Some(self.span);
        }

        match &self.kind {
            TypeAnnotationKind::Error
            | TypeAnnotationKind::Placeholder
            | TypeAnnotationKind::Parameter(_) => None,
            TypeAnnotationKind::Named(_, annotations)
            | TypeAnnotationKind::Builtin(_, annotations)
            | TypeAnnotationKind::Tuple(annotations) => annotations
                .iter()
                .find_map(|annotation| annotation.span_of(other)),
            TypeAnnotationKind::Function(left, right) => {
                left.span_of(other).or_else(|| right.span_of(other))
            }
        }
    }
}

pub type CaptureList = Vec<(VariableId, Span)>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EnumString, Display)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[strum(serialize_all = "kebab-case")]
pub enum RuntimeFunction {
    Crash,
    WriteStdout,
    Format,
    NumberToText,
    IntegerToText,
    NaturalToText,
    ByteToText,
    SignedToText,
    UnsignedToText,
    FloatToText,
    DoubleToText,
    AddNumber,
    SubtractNumber,
    MultiplyNumber,
    DivideNumber,
    PowerNumber,
    FloorNumber,
    CeilNumber,
    SqrtNumber,
    AddInteger,
    SubtractInteger,
    MultiplyInteger,
    DivideInteger,
    PowerInteger,
    AddNatural,
    SubtractNatural,
    MultiplyNatural,
    DivideNatural,
    PowerNatural,
    AddByte,
    SubtractByte,
    MultiplyByte,
    DivideByte,
    PowerByte,
    AddSigned,
    SubtractSigned,
    MultiplySigned,
    DivideSigned,
    PowerSigned,
    AddUnsigned,
    SubtractUnsigned,
    MultiplyUnsigned,
    DivideUnsigned,
    PowerUnsigned,
    AddFloat,
    SubtractFloat,
    MultiplyFloat,
    DivideFloat,
    PowerFloat,
    FloorFloat,
    CeilFloat,
    SqrtFloat,
    AddDouble,
    SubtractDouble,
    MultiplyDouble,
    DivideDouble,
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
    MakeList,
    ListFirst,
    ListLast,
    ListInitial,
    ListTail,
    ListNth,
    ListAppend,
    ListPrepend,
    ListInsert,
    ListRemove,
}

impl Compiler<'_> {
    pub(crate) fn lower(
        &self,
        file: ast::File,
        dependencies: Vec<(Arc<File>, Option<HashMap<InternedString, Span>>)>,
    ) -> File {
        let mut info = Info {
            file: file.path,
            declarations: Default::default(),
            attributes: Default::default(),
            scopes: Default::default(),
            specializations: Default::default(),
        };

        self.root_scope(expand::GLOBAL_ROOT_SCOPE, &mut info);

        for (id, (span, parent)) in file.scopes {
            if let Some(parent) = parent {
                self.child_scope(id, parent, span, &mut info);
            }
        }

        let scope = file.root_scope;

        info.attributes.recursion_limit = file.attributes.recursion_limit;

        info.declarations.syntaxes = file.syntax_declarations;

        for (dependency, imports) in dependencies {
            macro_rules! merge_dependency {
                ($($kind:ident$(($transform:expr))?),* $(,)?) => {
                    $(
                        for (id, mut decl) in dependency.declarations.$kind.clone() {
                            let mut uses = HashSet::new();
                            let merged_decl = info.declarations.$kind.entry(id).or_insert_with(|| {
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

            info.scopes.extend(dependency.scopes.clone());

            info.attributes.merge(&dependency.global_attributes);

            for (&id, specializations) in &dependency.specializations {
                for &specialization in specializations {
                    info.specializations.insert(specialization, id);
                }
            }

            if let Some(imports) = imports {
                for (name, span) in imports {
                    if let Some(value) = dependency.exported.get(&name) {
                        self.insert_into_scope(scope, name, value.clone(), &info);
                    } else {
                        self.add_error(
                            format!("file does not export a value named '{}'", name),
                            vec![Note::primary(span, "no such export")],
                        );
                    }
                }
            } else {
                self.extend_scope(scope, dependency.exported.clone(), &info);
            }
        }

        let block = self.lower_statements(file.statements, scope, &mut info);

        for constant in info.declarations.constants.values() {
            match &mut *constant.value.as_ref().unwrap().value.lock() {
                Some(_) => continue,
                value @ None => {
                    self.add_error(
                        "uninitialized constant",
                        vec![Note::primary(
                            constant.span,
                            format!(
                                "`{}` is never initialized with a value",
                                constant.name.unwrap()
                            ),
                        )],
                    );

                    *value = Some(Expression::error(self, constant.span));
                }
            }
        }

        for instance in info.declarations.instances.values_mut() {
            let tr = instance.value.as_ref().unwrap().tr;
            let tr_decl = info.declarations.traits.get(&tr).unwrap();

            if tr_decl
                .value
                .as_ref()
                .unwrap()
                .attributes
                .allow_overlapping_instances
                && instance.span.path != tr_decl.span.path
            {
                self.add_error(
                    "instance of trait that allows overlapping instances must occur in the same file as the trait",
                    vec![Note::primary(instance.span, "instance disallowed here")],
                );
            }

            let trait_has_value = tr_decl.value.as_ref().unwrap().ty.is_some();
            let instance_has_value = instance.value.as_ref().unwrap().value.is_some();

            match (trait_has_value, instance_has_value) {
                (true, false) => {
                    self.add_error(
                        "expected instance to have value",
                        vec![Note::primary(
                            instance.span,
                            "try adding `:` after the instance declaration to give it a value",
                        )],
                    );

                    instance.value.as_mut().unwrap().value =
                        Some(Expression::error(self, instance.span));
                }
                (false, true) => {
                    self.add_error(
                        "instance has value, but the trait doesn't store a value",
                        vec![Note::primary(
                            instance.span,
                            "try removing this instance's value",
                        )],
                    );
                }
                _ => {}
            }
        }

        let exported = info.scopes.get(&scope).unwrap().values.lock().clone();

        File {
            span: file.span,
            declarations: info.declarations.resolve(),
            global_attributes: info.attributes,
            exported,
            scopes: info.scopes,
            specializations: {
                let mut specializations = BTreeMap::<ConstantId, Vec<ConstantId>>::new();
                for (constant, specialized_constant) in info.specializations {
                    specializations
                        .entry(specialized_constant)
                        .or_default()
                        .push(constant);
                }

                specializations
            },
            block,
        }
    }
}

impl FileAttributes {
    fn merge(&mut self, other: &Self) {
        self.language_items.merge(&other.language_items);

        self.recursion_limit = match (self.recursion_limit, other.recursion_limit) {
            (Some(a), Some(b)) => Some(a.max(b)),
            (Some(l), None) | (None, Some(l)) => Some(l),
            (None, None) => None,
        }
    }
}

impl LanguageItems {
    fn merge(&mut self, other: &Self) {
        if let Some(ty) = other.boolean {
            self.boolean = Some(ty);
        }
    }
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub parent: Option<ScopeId>,
    pub span: Option<Span>,
    pub values: Shared<ScopeValues>,
    pub declared_variables: Shared<BTreeSet<VariableId>>,
    pub used_variables: Shared<CaptureList>,
}

pub type ScopeValues = HashMap<InternedString, ScopeValue>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    Block,
    Function,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", serde(tag = "type", content = "value"))]
pub enum ScopeValue {
    Type(TypeId),
    BuiltinType(BuiltinTypeId),
    Trait(TraitId),
    TypeParameter(TypeParameterId),
    Constant(ConstantId, Option<(TypeId, VariantIndex)>),
    Variable(VariableId),
}

impl<'l> Compiler<'l> {
    fn root_scope(&self, id: ScopeId, info: &mut Info) -> ScopeId {
        let scope = self.create_scope(None);
        self.load_builtins(&scope, info);

        info.scopes.insert(id, scope);

        id
    }

    fn child_scope(
        &self,
        id: ScopeId,
        parent: ScopeId,
        span: Option<Span>,
        info: &mut Info,
    ) -> ScopeId {
        let mut scope = self.create_scope(span);
        scope.parent = Some(parent);

        info.scopes.insert(id, scope);

        id
    }

    fn create_scope(&self, span: Option<Span>) -> Scope {
        Scope {
            parent: None,
            span,
            values: Default::default(),
            declared_variables: Default::default(),
            used_variables: Default::default(),
        }
    }

    fn insert_into_scope(
        &self,
        scope: ScopeId,
        name: InternedString,
        value: ScopeValue,
        info: &Info,
    ) {
        let scope = info.scopes.get(&scope).unwrap();

        if let ScopeValue::Variable(var) = value {
            scope.declared_variables.lock().insert(var);
        }

        scope.values.lock().insert(name, value);
    }

    fn extend_scope(
        &self,
        scope: ScopeId,
        values: impl IntoIterator<Item = (InternedString, ScopeValue)>,
        info: &Info,
    ) {
        for (name, value) in values {
            self.insert_into_scope(scope, name, value, info);
        }
    }

    fn get_in_scope(
        &self,
        scope: ScopeId,
        name: InternedString,
        span: Span,
        info: &Info,
    ) -> Option<ScopeValue> {
        self.get_in_scope_inner(scope, name, span, true, info)
    }

    fn peek_in_scope(
        &self,
        scope: ScopeId,
        name: InternedString,
        span: Span,
        info: &mut Info,
    ) -> Option<ScopeValue> {
        self.get_in_scope_inner(scope, name, span, false, info)
    }

    fn get_in_scope_inner(
        &self,
        scope: ScopeId,
        name: InternedString,
        span: Span,
        track_use: bool,
        info: &Info,
    ) -> Option<ScopeValue> {
        let mut parent = Some(scope);
        let mut result = None;
        let mut used_variables = Vec::new();

        #[cfg(debug_assertions)]
        let mut previous_scopes = Vec::new();

        while let Some(scope) = parent {
            #[cfg(debug_assertions)]
            {
                assert!(
                    !previous_scopes.contains(&scope),
                    "scope cycle detected: {:?}",
                    previous_scopes
                );

                previous_scopes.push(scope);
            }

            let scope = info.scopes.get(&scope).unwrap_or_else(|| {
                panic!(
                    "cannot find scope {:#?}, current file = {:?}",
                    scope, info.file
                )
            });

            if let Some(value) = scope.values.lock().get(&name).cloned() {
                result = Some(value);
                break;
            }

            if track_use {
                used_variables.push(&scope.used_variables);
            }

            parent = scope.parent;
        }

        if track_use {
            if let Some(ScopeValue::Variable(id)) = result {
                for u in used_variables {
                    u.lock().push((id, span));
                }
            }
        }

        result
    }

    fn used_variables_in_scope(&self, scope: ScopeId, info: &mut Info) -> Vec<(VariableId, Span)> {
        let mut parent = Some(scope);
        let mut used_variables = Vec::new();
        let declared_variables = info.scopes.get(&scope).unwrap().declared_variables.lock();
        while let Some(scope) = parent {
            let scope = info.scopes.get(&scope).unwrap();

            used_variables.extend(
                scope
                    .used_variables
                    .lock()
                    .iter()
                    .filter(|(var, _)| !declared_variables.contains(var))
                    .copied(),
            );

            parent = scope.parent;
        }

        used_variables
    }
}

struct Info {
    file: FilePath,
    declarations: UnresolvedDeclarations,
    attributes: FileAttributes,
    scopes: BTreeMap<ScopeId, Scope>,
    specializations: BTreeMap<ConstantId, ConstantId>,
}

#[derive(Debug)]
struct StatementDeclaration {
    span: Span,
    kind: StatementDeclarationKind,
    attributes: ast::StatementAttributes,
}

#[derive(Debug)]
enum StatementDeclarationKind {
    Type(TypeId, ast::TypeDeclaration),
    Trait(TraitId, ast::TraitDeclaration),
    Constant(ConstantId, ast::ConstantDeclaration),
    Instance(ConstantId, ast::Instance),
    Use((Span, InternedString)),
    Queued(QueuedStatement),
}

#[derive(Debug)]
enum QueuedStatement {
    Assign(ast::Pattern, ast::Expression),
    Expression(ast::ExpressionKind),
}

impl Compiler<'_> {
    fn lower_block(
        &self,
        scope: ScopeId,
        statements: Vec<ast::Statement>,
        info: &mut Info,
    ) -> Vec<Expression> {
        self.lower_statements(statements, scope, info)
    }

    fn lower_statements(
        &self,
        statements: Vec<ast::Statement>,
        scope: ScopeId,
        info: &mut Info,
    ) -> Vec<Expression> {
        let declarations = statements
            .into_iter()
            .map(|statement| self.lower_statement(scope, statement, info))
            .collect::<Vec<_>>();

        let mut queue = Vec::new();
        let mut current_constant = None;

        for decl in declarations {
            let mut decl = match decl {
                Some(decl) => decl,
                None => continue,
            };

            if !matches!(decl.kind, StatementDeclarationKind::Queued(_)) {
                current_constant = None;
            }

            let scope_value = match decl.kind {
                StatementDeclarationKind::Type(id, ty) => {
                    let (parameters, _) = self.with_parameters(
                        scope,
                        ty.parameters.map(|params| (params, Vec::new())),
                        info,
                    );

                    let ty = match ty.kind {
                        ast::TypeKind::Marker => Type {
                            kind: TypeKind::Marker,
                            params: parameters,
                            attributes: self.lower_type_attributes(
                                &mut decl.attributes,
                                scope,
                                info,
                            ),
                        },
                        ast::TypeKind::Structure(fields) => {
                            let mut field_tys = Vec::with_capacity(fields.len());
                            let mut field_names = HashMap::with_capacity(fields.len());
                            for (index, mut field) in fields.into_iter().enumerate() {
                                field_tys.push(TypeField {
                                    ty: self.lower_type_annotation(field.ty, scope, info),
                                    attributes: self.lower_decl_attributes(
                                        &mut field.attributes,
                                        scope,
                                        info,
                                    ),
                                });

                                field_names.insert(field.name, FieldIndex::new(index));
                            }

                            Type {
                                kind: TypeKind::Structure(field_tys, field_names),
                                params: parameters,
                                attributes: self.lower_type_attributes(
                                    &mut decl.attributes,
                                    scope,
                                    info,
                                ),
                            }
                        }
                        ast::TypeKind::Enumeration(variants) => {
                            let mut variant_tys = Vec::with_capacity(variants.len());
                            let mut variant_names = HashMap::with_capacity(variants.len());
                            for (index, mut variant) in variants.into_iter().enumerate() {
                                let tys = variant
                                    .values
                                    .into_iter()
                                    .map(|ty| self.lower_type_annotation(ty, scope, info))
                                    .collect::<Vec<_>>();

                                let constructor_id = self.new_constant_id_in(info.file);

                                let constructor_ty = tys.iter().rev().fold(
                                    TypeAnnotation {
                                        span: variant.span,
                                        kind: TypeAnnotationKind::Named(
                                            id,
                                            parameters
                                                .iter()
                                                .map(|&(span, param)| TypeAnnotation {
                                                    span,
                                                    kind: TypeAnnotationKind::Parameter(param),
                                                })
                                                .collect(),
                                        ),
                                    },
                                    |result, next| TypeAnnotation {
                                        span: variant.span,
                                        kind: TypeAnnotationKind::Function(
                                            Box::new(next.clone()),
                                            Box::new(result),
                                        ),
                                    },
                                );

                                let variables = tys
                                    .iter()
                                    .map(|ty| {
                                        let var = self.new_variable_id_in(info.file);

                                        info.declarations
                                            .variables
                                            .insert(var, Declaration::resolved(None, ty.span, ()));

                                        (var, ty.span)
                                    })
                                    .collect::<Vec<_>>();

                                let result = Expression {
                                    span: variant.span,
                                    kind: ExpressionKind::Variant(
                                        id,
                                        VariantIndex::new(index),
                                        variables
                                            .iter()
                                            .map(|(var, span)| Expression {
                                                span: *span,
                                                kind: ExpressionKind::Variable(*var),
                                            })
                                            .collect(),
                                    ),
                                };

                                let constructor = variables.iter().enumerate().rev().fold(
                                    result,
                                    |result, (index, (var, span))| Expression {
                                        span: variant.span,
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

                                let attributes = self.lower_constant_attributes(
                                    &mut variant.attributes,
                                    scope,
                                    info,
                                );

                                info.declarations.constants.insert(
                                    constructor_id,
                                    Declaration::resolved(
                                        Some(variant.name),
                                        variant.span,
                                        UnresolvedConstant {
                                            parameters: parameters.clone(),
                                            bounds: Vec::new(),
                                            ty: constructor_ty,
                                            value: Shared::new(Some(constructor)),
                                            attributes,
                                        },
                                    )
                                    .make_unresolved(),
                                );

                                variant_tys.push(TypeVariant {
                                    constructor: constructor_id,
                                    tys,
                                });

                                variant_names.insert(variant.name, VariantIndex::new(index));
                            }

                            Type {
                                kind: TypeKind::Enumeration(variant_tys, variant_names),
                                params: parameters,
                                attributes: self.lower_type_attributes(
                                    &mut decl.attributes,
                                    scope,
                                    info,
                                ),
                            }
                        }
                    };

                    info.declarations.types.get_mut(&id).unwrap().value = Some(ty);

                    Some(ScopeValue::Type(id))
                }
                StatementDeclarationKind::Trait(id, declaration) => {
                    let (parameters, _) = self.with_parameters(
                        scope,
                        declaration.parameters.map(|params| (params, Vec::new())),
                        info,
                    );

                    let tr = Trait {
                        parameters,
                        ty: declaration
                            .ty
                            .map(|ty| self.lower_type_annotation(ty, scope, info)),
                        attributes: self.lower_trait_attributes(&mut decl.attributes, scope, info),
                    };

                    info.declarations.traits.get_mut(&id).unwrap().value = Some(tr);

                    Some(ScopeValue::Trait(id))
                }
                StatementDeclarationKind::Constant(id, declaration) => {
                    let (parameters, bounds) =
                        self.with_parameters(scope, declaration.parameters, info);

                    let constant = UnresolvedConstant {
                        parameters,
                        bounds,
                        ty: self.lower_type_annotation(declaration.ty, scope, info),
                        value: Default::default(),
                        attributes: self.lower_constant_attributes(
                            &mut decl.attributes,
                            scope,
                            info,
                        ),
                    };

                    info.declarations.constants.get_mut(&id).unwrap().value = Some(constant);
                    current_constant = Some((declaration.name, decl.span, id));

                    Some(ScopeValue::Constant(id, None))
                }
                StatementDeclarationKind::Instance(id, instance) => {
                    let (params, bounds) = self.with_parameters(scope, instance.parameters, info);

                    let tr = match self.get_in_scope(
                        scope,
                        instance.trait_name,
                        instance.trait_span,
                        info,
                    ) {
                        Some(ScopeValue::Trait(tr)) => {
                            info.declarations
                                .traits
                                .get_mut(&tr)
                                .unwrap()
                                .uses
                                .insert(instance.trait_span);

                            tr
                        }
                        Some(_) => {
                            self.add_error(
                                format!("`{}` is not a trait", instance.trait_name),
                                vec![Note::primary(instance.trait_span, "expected a trait here")],
                            );

                            info.declarations.instances.remove(&id);
                            continue;
                        }
                        None => {
                            self.add_error(
                                format!("cannot find `{}`", instance.trait_name),
                                vec![Note::primary(
                                    instance.trait_span,
                                    "this name is not defined",
                                )],
                            );

                            info.declarations.instances.remove(&id);
                            continue;
                        }
                    };

                    let trait_params = instance
                        .trait_parameters
                        .into_iter()
                        .map(|ty| self.lower_type_annotation(ty, scope, info))
                        .collect();

                    let value = instance
                        .value
                        .map(|value| self.lower_expr(value, scope, info));

                    let instance = Instance {
                        params,
                        bounds,
                        tr,
                        trait_params,
                        value,
                    };

                    info.declarations.instances.get_mut(&id).unwrap().value = Some(instance);

                    Some(ScopeValue::Constant(id, None))
                }
                StatementDeclarationKind::Use((span, name)) => {
                    let ty = match self.get_in_scope(scope, name, span, info) {
                        Some(ScopeValue::Type(ty)) => {
                            info.declarations
                                .types
                                .get_mut(&ty)
                                .unwrap()
                                .uses
                                .insert(span);

                            ty
                        }
                        Some(_) => {
                            self.add_error(
                                format!("`{}` is not a type", name),
                                vec![Note::primary(span, "expected a type here")],
                            );

                            continue;
                        }
                        None => {
                            self.add_error(
                                format!("cannot find `{}`", name),
                                vec![Note::primary(span, "this name is not defined")],
                            );

                            continue;
                        }
                    };

                    let (constructors, names) = match &info
                        .declarations
                        .types
                        .get(&ty)
                        .unwrap()
                        .value
                        .as_ref()
                        .unwrap()
                        .kind
                    {
                        TypeKind::Enumeration(constructors, names) => (constructors, names),
                        _ => {
                            self.add_error(
                                "only enumerations may be `use`d",
                                vec![Note::primary(
                                    span,
                                    format!("`{}` is not an enumeration", name),
                                )],
                            );

                            continue;
                        }
                    };

                    for (name, index) in names {
                        let variant = constructors[index.into_inner()].constructor;

                        self.insert_into_scope(
                            scope,
                            *name,
                            ScopeValue::Constant(variant, Some((ty, *index))),
                            info,
                        );
                    }

                    None
                }
                StatementDeclarationKind::Queued(queued) => {
                    queue.push((decl.span, queued, current_constant));
                    current_constant = None;
                    None
                }
            };

            (|| {
                if let Some(language_item) = decl.attributes.language_item {
                    match language_item {
                        expand::LanguageItem::Boolean => {
                            let ty = match scope_value {
                                Some(ScopeValue::Type(id)) => id,
                                _ => {
                                    self.add_error(
                                        "`boolean` language item expects a type",
                                        vec![Note::primary(
                                            decl.span,
                                            "expected type declaration here",
                                        )],
                                    );

                                    return;
                                }
                            };

                            if info.attributes.language_items.boolean.is_some() {
                                self.add_error(
                                    "`language` item may only be defined once",
                                    vec![Note::primary(
                                        decl.span,
                                        "`language` item already defined elsewhere",
                                    )],
                                );

                                return;
                            }

                            info.attributes.language_items.boolean = Some(ty);
                        }
                    }
                }
            })();
        }

        queue
            .into_iter()
            .filter_map(|(span, statement, prev_constant)| (|| match statement {
                QueuedStatement::Assign(pattern, expr) => {
                    macro_rules! assign_pattern {
                        () => {{
                            let value = self.lower_expr(expr, scope, info);
                            let pattern = self.lower_pattern(pattern, scope, info);

                            Some(Expression {
                                span,
                                kind: ExpressionKind::Initialize(pattern, Box::new(value)),
                            })
                        }};
                    }

                    match &pattern.kind {
                        ast::PatternKind::Name(name) => {
                            if let Some((prev_constant_name, prev_constant_span, prev_constant_id)) = prev_constant {
                                if *name != prev_constant_name {
                                    return assign_pattern!();
                                }

                                let decl = info.declarations.constants.get(&prev_constant_id).unwrap();
                                let value = decl.value.as_ref().unwrap();
                                let associated_parameters = value.parameters.clone();
                                let associated_constant = value.value.clone();

                                if let ScopeValue::Constant(id, _) = self.get_in_scope(scope, prev_constant_name, prev_constant_span, info).unwrap() {
                                    if id == prev_constant_id && associated_constant.lock().is_some() {
                                        self.add_error(
                                            format!("constant `{}` already exists in this file", name), vec![
                                                Note::primary(prev_constant_span, "try giving this constant a unique name"),
                                                Note::secondary(prev_constant_span, "other constant declared here")
                                            ],
                                        );

                                        return assign_pattern!();
                                    }
                                }

                                info.declarations
                                    .constants
                                    .get_mut(&prev_constant_id)
                                    .unwrap()
                                    .uses
                                    .insert(pattern.span);

                                // HACK: Insert type parameters temporarily without affecting rest of file
                                let prev_scope_values = info.scopes.get(&scope).unwrap().values.lock().clone();

                                for (_, id) in associated_parameters {
                                    let parameter =
                                        info.declarations.type_parameters.get(&id).unwrap();

                                    self.insert_into_scope(
                                        scope,
                                        parameter.name.unwrap(),
                                        ScopeValue::TypeParameter(id),
                                        info
                                    );
                                }

                                let value = self.lower_expr(expr, scope, info);

                                let used_variables = self.used_variables_in_scope(scope, info);
                                if !used_variables.is_empty() {
                                    self.add_error(
                                        "constant cannot capture outside variables", used_variables
                                            .into_iter()
                                            .map(|(_, span)| {
                                                Note::primary(span, "captured variable")
                                            })
                                            .collect(),
                                    );
                                }

                                // HACK: See above
                                *info.scopes.get(&scope).unwrap().values.lock() = prev_scope_values;

                                *associated_constant.lock() = Some(value);
                                None
                            } else {
                                assign_pattern!()
                            }
                        }
                        _ => assign_pattern!(),
                    }
                },
                QueuedStatement::Expression(expr) => {
                    if let Some((_, span, _)) = prev_constant {
                        self.add_error(
                            "constant must be initialized immediately following its type annotation", vec![Note::primary(span, "try initializing the constant below this")],
                        );
                    }

                    Some(self.lower_expr(ast::Expression { span, kind: expr }, scope, info))
                }
            })())
            .collect()
    }

    fn lower_statement(
        &self,
        scope: ScopeId,
        statement: ast::Statement,
        info: &mut Info,
    ) -> Option<StatementDeclaration> {
        match statement.kind {
            ast::StatementKind::Empty => None,
            ast::StatementKind::Declaration(decl) => match decl {
                ast::Declaration::Type((span, name), ty) => {
                    let id = self.new_type_id_in(info.file);
                    self.insert_into_scope(scope, name, ScopeValue::Type(id), info);

                    info.declarations
                        .types
                        .insert(id, Declaration::unresolved(Some(name), span));

                    Some(StatementDeclaration {
                        span: statement.span,
                        kind: StatementDeclarationKind::Type(id, ty),
                        attributes: statement.attributes,
                    })
                }
                ast::Declaration::Trait((span, name), declaration) => {
                    let id = self.new_trait_id_in(info.file);
                    self.insert_into_scope(scope, name, ScopeValue::Trait(id), info);

                    info.declarations
                        .traits
                        .insert(id, Declaration::unresolved(Some(name), span));

                    Some(StatementDeclaration {
                        span: statement.span,
                        kind: StatementDeclarationKind::Trait(id, declaration),
                        attributes: statement.attributes,
                    })
                }
                ast::Declaration::Constant((span, name), declaration) => {
                    let id = self.new_constant_id_in(info.file);

                    if let Some(ScopeValue::Constant(existing_id, variant_info)) =
                        self.get_in_scope(scope, name, span, info)
                    {
                        if statement.attributes.specialize {
                            if variant_info.is_some() {
                                self.add_error(
                                    "cannot specialize a `type` variant",
                                    vec![Note::primary(span, "cannot specialize this")],
                                );

                                return None;
                            }

                            if info.specializations.contains_key(&existing_id) {
                                self.add_error(
                                    "cannot specialize constant which is a specialization of another constant", vec![Note::primary(span, "cannot specialize this")],
                                );

                                return None;
                            }

                            info.specializations.insert(id, existing_id);
                        } else if existing_id.file == Some(info.file) {
                            let existing_span =
                                info.declarations.constants.get(&existing_id).unwrap().span;

                            self.add_error(
                                format!("constant `{}` already exists in this file", name),
                                vec![
                                    Note::primary(
                                        span,
                                        "try giving this constant a different name",
                                    ),
                                    Note::primary(existing_span, "original constant declared here"),
                                ],
                            );

                            return None;
                        } else {
                            self.insert_into_scope(
                                scope,
                                name,
                                ScopeValue::Constant(id, None),
                                info,
                            );
                        }
                    } else {
                        self.insert_into_scope(scope, name, ScopeValue::Constant(id, None), info);
                    }

                    info.declarations
                        .constants
                        .insert(id, Declaration::unresolved(Some(name), span));

                    Some(StatementDeclaration {
                        span: statement.span,
                        kind: StatementDeclarationKind::Constant(id, declaration),
                        attributes: statement.attributes,
                    })
                }
                ast::Declaration::Instance(instance) => {
                    let id = self.new_constant_id_in(info.file);

                    info.declarations
                        .instances
                        .insert(id, Declaration::unresolved(None, statement.span));

                    Some(StatementDeclaration {
                        span: statement.span,
                        kind: StatementDeclarationKind::Instance(id, instance),
                        attributes: statement.attributes,
                    })
                }
            },
            ast::StatementKind::Assign(pattern, expr) => Some(StatementDeclaration {
                span: statement.span,
                kind: StatementDeclarationKind::Queued(QueuedStatement::Assign(pattern, expr)),
                attributes: statement.attributes,
            }),
            ast::StatementKind::Use((span, name)) => Some(StatementDeclaration {
                span: statement.span,
                kind: StatementDeclarationKind::Use((span, name)),
                attributes: statement.attributes,
            }),
            ast::StatementKind::Expression(expr) => Some(StatementDeclaration {
                span: statement.span,
                kind: StatementDeclarationKind::Queued(QueuedStatement::Expression(expr)),
                attributes: statement.attributes,
            }),
        }
    }

    fn lower_decl_attributes(
        &self,
        statement_attributes: &mut ast::StatementAttributes,
        _scope: ScopeId,
        _info: &mut Info,
    ) -> DeclarationAttributes {
        // TODO: Raise errors for misused attributes

        DeclarationAttributes {
            help: mem::take(&mut statement_attributes.help)
                .into_iter()
                .collect(),
        }
    }

    fn lower_type_attributes(
        &self,
        statement_attributes: &mut ast::StatementAttributes,
        scope: ScopeId,
        info: &mut Info,
    ) -> TypeAttributes {
        // TODO: Raise errors for misused attributes

        TypeAttributes {
            decl_attributes: self.lower_decl_attributes(statement_attributes, scope, info),
            on_mismatch: mem::take(&mut statement_attributes.on_mismatch)
                .into_iter()
                .filter_map(|(parameter, message)| {
                    let param = match parameter {
                        Some(parameter) => {
                            match self.get_in_scope(scope, parameter.name, parameter.span, info) {
                                Some(ScopeValue::TypeParameter(param)) => {
                                    info.declarations
                                        .type_parameters
                                        .get_mut(&param)
                                        .unwrap()
                                        .uses
                                        .insert(parameter.span);

                                    Some(param)
                                }
                                _ => {
                                    self.add_error(
                                        format!("cannot find type parameter `{}`", parameter.name),
                                        vec![Note::primary(parameter.span, "no such type")],
                                    );

                                    return None;
                                }
                            }
                        }
                        None => None,
                    };

                    Some((param, message))
                })
                .collect::<Vec<_>>(),
        }
    }

    fn lower_trait_attributes(
        &self,
        statement_attributes: &mut ast::StatementAttributes,
        scope: ScopeId,
        info: &mut Info,
    ) -> TraitAttributes {
        // TODO: Raise errors for misused attributes

        TraitAttributes {
            decl_attributes: self.lower_decl_attributes(statement_attributes, scope, info),
            on_unimplemented: mem::take(&mut statement_attributes.on_unimplemented),
            allow_overlapping_instances: mem::take(
                &mut statement_attributes.allow_overlapping_instances,
            ),
        }
    }

    fn lower_constant_attributes(
        &self,
        statement_attributes: &mut ast::StatementAttributes,
        scope: ScopeId,
        info: &mut Info,
    ) -> ConstantAttributes {
        // TODO: Raise errors for misused attributes

        ConstantAttributes {
            decl_attributes: self.lower_decl_attributes(statement_attributes, scope, info),
            is_specialization: statement_attributes.specialize,
        }
    }

    fn lower_expr(&self, expr: ast::Expression, scope: ScopeId, info: &mut Info) -> Expression {
        macro_rules! function_call {
            ($function:expr, $inputs:expr) => {
                $inputs
                    .into_iter()
                    .fold($function, |result, next| Expression {
                        span: Span::join(result.span, next.span),
                        kind: ExpressionKind::Call(
                            Box::new(result),
                            Box::new(self.lower_expr(next, scope, info)),
                        ),
                    })
            };
        }

        let kind = match expr.kind {
            ast::ExpressionKind::Error(trace) => ExpressionKind::Error(trace),
            ast::ExpressionKind::Text(text) => ExpressionKind::Text(text),
            ast::ExpressionKind::Number(number) => ExpressionKind::Number(number),
            ast::ExpressionKind::Name(name) => {
                match self.resolve_value(expr.span, name, scope, info) {
                    Some(value) => value,
                    None => {
                        self.add_error(
                            format!("cannot find `{name}`"),
                            vec![Note::primary(expr.span, "this name is not defined")],
                        );

                        return Expression::error(self, expr.span);
                    }
                }
            }
            ast::ExpressionKind::Block(statements) => {
                let block = self.lower_block(scope, statements, info);
                ExpressionKind::Block(block, false)
            }
            ast::ExpressionKind::End(value) => {
                ExpressionKind::End(Box::new(self.lower_expr(*value, scope, info)))
            }
            ast::ExpressionKind::Call(function, inputs) => match &function.kind {
                ast::ExpressionKind::Name(ty_name) => {
                    let input = match inputs.first() {
                        Some(input) => input,
                        None => {
                            self.add_error(
                                "function received no input",
                                vec![Note::primary(
                                    function.span,
                                    "try providing an input to this function",
                                )],
                            );

                            return Expression::error(self, expr.span);
                        }
                    };

                    match self.get_in_scope(scope, *ty_name, function.span, info) {
                        Some(ScopeValue::Type(id)) => {
                            info.declarations
                                .types
                                .get_mut(&id)
                                .unwrap()
                                .uses
                                .insert(function.span);

                            match &input.kind {
                                ast::ExpressionKind::Block(statements) => {
                                    if inputs.len() > 1 {
                                        self.add_error(
                                            "too many inputs in structure instantiation", vec![Note::primary(
                                                Span::join(
                                                    inputs.first().unwrap().span,
                                                    inputs.last().unwrap().span,
                                                ),
                                                "this structure requires a single block containing its fields",
                                            )],
                                        );
                                    }

                                    let fields = 'parse: {
                                        if statements.len() == 1 {
                                            let statement = statements.last().unwrap();

                                            if let ast::StatementKind::Expression(
                                                ast::ExpressionKind::Call(first, rest),
                                            ) = &statement.kind
                                            {
                                                if let Some(fields) =
                                                    std::iter::once(first.as_ref())
                                                        .chain(rest)
                                                        .map(|expr| match &expr.kind {
                                                            ast::ExpressionKind::Name(name) => {
                                                                Some((*name, expr.clone()))
                                                            }
                                                            _ => None,
                                                        })
                                                        .collect::<Option<Vec<_>>>()
                                                {
                                                    break 'parse fields
                                                        .into_iter()
                                                        .map(|(name, expr)| {
                                                            (
                                                                name,
                                                                self.lower_expr(expr, scope, info),
                                                            )
                                                        })
                                                        .collect();
                                                }
                                            };
                                        }

                                        statements
                                            .iter()
                                            .filter_map(|s| match &s.kind {
                                                ast::StatementKind::Assign(pattern, expr) => match &pattern.kind {
                                                    ast::PatternKind::Name(name) => Some((*name, expr.clone())),
                                                    _ => {
                                                        self.add_error(
                                                            "structure instantiation may not contain complex patterns", vec![Note::primary(
                                                                s.span,
                                                                "try splitting this pattern into multiple names",
                                                            )]
                                                        );

                                                        None
                                                    },
                                                },
                                                ast::StatementKind::Expression(expr @ ast::ExpressionKind::Name(name)) => {
                                                    let expr = ast::Expression { span: s.span, kind: expr.clone() };
                                                    Some((*name, expr))
                                                },
                                                // TODO: 'use' inside instantiation
                                                _ => {
                                                    self.add_error(
                                                        "structure instantiation may not contain executable statements", vec![Note::primary(
                                                            s.span,
                                                            "try removing this",
                                                        )]
                                                    );

                                                    None
                                                }
                                            })
                                            .collect::<Vec<_>>()
                                            .into_iter()
                                            .map(|(name, value)| {
                                                (name, self.lower_expr(value, scope, info))
                                            })
                                            .collect()
                                    };

                                    let ty = info
                                        .declarations
                                        .types
                                        .get(&id)
                                        .unwrap()
                                        .value
                                        .as_ref()
                                        .unwrap();

                                    if !matches!(ty.kind, TypeKind::Structure(_, _)) {
                                        self.add_error(
                                            "only structures may be instantiated like this",
                                            vec![Note::primary(function.span, "not a structure")],
                                        );

                                        return Expression::error(self, expr.span);
                                    }

                                    ExpressionKind::Instantiate(id, fields)
                                }
                                ast::ExpressionKind::Name(name) => {
                                    let ty_decl = info.declarations.types.get(&id).unwrap();

                                    let (variant_types, variants) =
                                        match &ty_decl.value.as_ref().unwrap().kind {
                                            TypeKind::Enumeration(types, variants) => {
                                                (types, variants)
                                            }
                                            _ => {
                                                self.add_error(
                                                "only enumerations may be instantiated like this",
                                                vec![Note::primary(
                                                    function.span,
                                                    "not an enumeration",
                                                )],
                                            );

                                                return Expression::error(self, expr.span);
                                            }
                                        };

                                    let index = match variants.get(name) {
                                        Some(index) => *index,
                                        None => {
                                            self.add_error(
                                            format!(
                                                "enumeration `{}` does not declare a variant named `{}`",
                                                ty_name,
                                                name
                                            ), vec![Note::primary(input.span, "no such variant")],
                                        );

                                            return Expression::error(self, expr.span);
                                        }
                                    };

                                    function_call!(
                                        Expression {
                                            span: expr.span,
                                            kind: ExpressionKind::Constant(
                                                variant_types[index.into_inner()].constructor
                                            )
                                        },
                                        inputs.into_iter().skip(1)
                                    )
                                    .kind
                                }
                                _ => {
                                    function_call!(self.lower_expr(*function, scope, info), inputs)
                                        .kind
                                }
                            }
                        }
                        Some(ScopeValue::TypeParameter(id)) => {
                            info.declarations
                                .type_parameters
                                .get_mut(&id)
                                .unwrap()
                                .uses
                                .insert(function.span);

                            self.add_error(
                                "cannot instantiate type parameter",
                                vec![Note::primary(
                                    function.span,
                                    "the actual type this represents is not known here",
                                )],
                            );

                            return Expression::error(self, expr.span);
                        }
                        Some(ScopeValue::BuiltinType(id)) => {
                            info.declarations
                                .builtin_types
                                .get_mut(&id)
                                .unwrap()
                                .uses
                                .insert(function.span);

                            self.add_error(
                                "cannot instantiate builtin type",
                                vec![Note::primary(function.span, "try using a literal instead")],
                            );

                            return Expression::error(self, expr.span);
                        }
                        _ => function_call!(self.lower_expr(*function, scope, info), inputs).kind,
                    }
                }
                _ => function_call!(self.lower_expr(*function, scope, info), inputs).kind,
            },
            ast::ExpressionKind::Function(input, body) => {
                let pattern = self.lower_pattern(input, scope, info);
                let body = self.lower_expr(*body, scope, info);

                let captures = self.used_variables_in_scope(scope, info);

                ExpressionKind::Function(pattern, Box::new(body), captures)
            }
            ast::ExpressionKind::When(input, arms) => ExpressionKind::When(
                Box::new(self.lower_expr(*input, scope, info)),
                arms.into_iter()
                    .map(|arm| Arm {
                        span: arm.span,
                        pattern: self.lower_pattern(arm.pattern, scope, info),
                        body: self.lower_expr(arm.body, scope, info),
                    })
                    .collect(),
            ),
            ast::ExpressionKind::External(lib, identifier, inputs) => (|| {
                let inputs = inputs
                    .into_iter()
                    .map(|expr| self.lower_expr(expr, scope, info))
                    .collect::<Vec<_>>();

                if lib.as_str() == "runtime" {
                    let func = match RuntimeFunction::from_str(identifier.as_str()) {
                        Ok(func) => func,
                        Err(_) => {
                            self.add_error(
                                "unknown runtime function", vec![Note::primary(expr.span, "check the Wipple source code for the latest list of runtime functions")],
                            );

                            return Expression::error(self, expr.span).kind;
                        }
                    };

                    ExpressionKind::Runtime(func, inputs)
                } else {
                    ExpressionKind::External(lib, identifier, inputs)
                }
            })(),
            ast::ExpressionKind::Annotate(expr, ty) => ExpressionKind::Annotate(
                Box::new(self.lower_expr(*expr, scope, info)),
                self.lower_type_annotation(ty, scope, info),
            ),
            ast::ExpressionKind::Tuple(exprs) => ExpressionKind::Tuple(
                exprs
                    .into_iter()
                    .map(|expr| self.lower_expr(expr, scope, info))
                    .collect(),
            ),
        };

        Expression {
            span: expr.span,
            kind,
        }
    }

    fn lower_type_annotation(
        &self,
        ty: ast::TypeAnnotation,
        scope: ScopeId,
        info: &mut Info,
    ) -> TypeAnnotation {
        let kind = match ty.kind {
            ast::TypeAnnotationKind::Error => TypeAnnotationKind::Error,
            ast::TypeAnnotationKind::Placeholder => TypeAnnotationKind::Placeholder,
            ast::TypeAnnotationKind::Named(name, parameters) => {
                let parameters = parameters
                    .into_iter()
                    .map(|parameter| self.lower_type_annotation(parameter, scope, info))
                    .collect();

                match self.get_in_scope(scope, name, ty.span, info) {
                    Some(ScopeValue::Type(id)) => {
                        info.declarations
                            .types
                            .get_mut(&id)
                            .unwrap()
                            .uses
                            .insert(ty.span);

                        TypeAnnotationKind::Named(id, parameters)
                    }
                    Some(ScopeValue::TypeParameter(param)) => {
                        info.declarations
                            .type_parameters
                            .get_mut(&param)
                            .unwrap()
                            .uses
                            .insert(ty.span);

                        if !parameters.is_empty() {
                            // TODO: Higher-kinded types
                            self.add_error(
                                "higher-kinded types are not yet supported",
                                vec![Note::primary(
                                    ty.span,
                                    "try writing this on its own, with no parameters",
                                )],
                            );
                        }

                        TypeAnnotationKind::Parameter(param)
                    }
                    Some(ScopeValue::BuiltinType(builtin)) => {
                        info.declarations
                            .builtin_types
                            .get_mut(&builtin)
                            .unwrap()
                            .uses
                            .insert(ty.span);

                        TypeAnnotationKind::Builtin(builtin, parameters)
                    }
                    _ => {
                        self.add_error(
                            format!("cannot find type `{}`", name),
                            vec![Note::primary(ty.span, "no such type")],
                        );

                        return TypeAnnotation::error(ty.span);
                    }
                }
            }
            ast::TypeAnnotationKind::Function(input, output) => TypeAnnotationKind::Function(
                Box::new(self.lower_type_annotation(*input, scope, info)),
                Box::new(self.lower_type_annotation(*output, scope, info)),
            ),
            ast::TypeAnnotationKind::Tuple(tys) => TypeAnnotationKind::Tuple(
                tys.into_iter()
                    .map(|ty| self.lower_type_annotation(ty, scope, info))
                    .collect(),
            ),
        };

        TypeAnnotation {
            span: ty.span,
            kind,
        }
    }

    fn lower_pattern(&self, pattern: ast::Pattern, scope: ScopeId, info: &mut Info) -> Pattern {
        let kind = (|| match pattern.kind {
            ast::PatternKind::Error(trace) => PatternKind::Error(trace),
            ast::PatternKind::Wildcard => PatternKind::Wildcard,
            ast::PatternKind::Number(number) => PatternKind::Number(number),
            ast::PatternKind::Text(text) => PatternKind::Text(text),
            ast::PatternKind::Name(name) => {
                match self.peek_in_scope(scope, name, pattern.span, info) {
                    Some(ScopeValue::Constant(id, Some((ty, variant)))) => {
                        info.declarations
                            .constants
                            .get_mut(&id)
                            .unwrap()
                            .uses
                            .insert(pattern.span);

                        PatternKind::Variant(ty, variant, Vec::new())
                    }
                    _ => {
                        let var = self.new_variable_id_in(info.file);

                        self.insert_into_scope(scope, name, ScopeValue::Variable(var), info);

                        info.declarations
                            .variables
                            .insert(var, Declaration::resolved(Some(name), pattern.span, ()));

                        PatternKind::Variable(var)
                    }
                }
            }
            ast::PatternKind::Destructure(fields) => PatternKind::Destructure(
                fields
                    .into_iter()
                    .map(|(name, pattern)| (name, self.lower_pattern(pattern, scope, info)))
                    .collect(),
            ),
            ast::PatternKind::Variant((name_span, name), values) => {
                let first = match self.get_in_scope(scope, name, name_span, info) {
                    Some(name) => name,
                    None => {
                        self.add_error(
                            format!("cannot find `{}`", name),
                            vec![Note::primary(name_span, "this name is not defined")],
                        );

                        return PatternKind::error(self);
                    }
                };

                let mut values = values.into_iter();

                match first {
                    ScopeValue::Type(ty) => {
                        info.declarations
                            .types
                            .get_mut(&ty)
                            .unwrap()
                            .uses
                            .insert(name_span);

                        let variants = match &info
                            .declarations
                            .types
                            .get(&ty)
                            .unwrap()
                            .value
                            .as_ref()
                            .unwrap()
                            .kind
                        {
                            TypeKind::Enumeration(_, variants) => variants,
                            _ => {
                                self.add_error(
                                    format!("cannot use `{}` in pattern", name),
                                    vec![Note::primary(
                                        name_span,
                                        "only enumeration types may be used in patterns",
                                    )],
                                );

                                return PatternKind::error(self);
                            }
                        };

                        let second = match values.next() {
                            Some(value) => value,
                            None => {
                                self.add_error(
                                    "incomplete pattern",
                                    vec![Note::primary(
                                        name_span,
                                        "expected a variant name after this",
                                    )],
                                );

                                return PatternKind::error(self);
                            }
                        };

                        let variant_name = match second.kind {
                            ast::PatternKind::Name(name) => name,
                            _ => {
                                self.add_error(
                                    "invalid pattern",
                                    vec![Note::primary(
                                        second.span,
                                        "expected a variant name here",
                                    )],
                                );

                                return PatternKind::error(self);
                            }
                        };

                        let variant = match variants.get(&variant_name) {
                            Some(variant) => *variant,
                            None => {
                                self.add_error(
                                    format!(
                                        "enumeration `{}` does not declare a variant named `{}`",
                                        name, variant_name
                                    ),
                                    vec![Note::primary(second.span, "no such variant")],
                                );

                                return PatternKind::error(self);
                            }
                        };

                        PatternKind::Variant(
                            ty,
                            variant,
                            values
                                .map(|value| self.lower_pattern(value, scope, info))
                                .collect(),
                        )
                    }
                    ScopeValue::Constant(id, Some((ty, variant))) => {
                        info.declarations
                            .constants
                            .get_mut(&id)
                            .unwrap()
                            .uses
                            .insert(name_span);

                        PatternKind::Variant(
                            ty,
                            variant,
                            values
                                .map(|value| self.lower_pattern(value, scope, info))
                                .collect(),
                        )
                    }
                    _ => {
                        self.add_error(
                            format!("cannot use `{}` in pattern", name),
                            vec![Note::primary(name_span, "expected a type or variant here")],
                        );

                        PatternKind::error(self)
                    }
                }
            }
            ast::PatternKind::Annotate(inner_pattern, ty) => PatternKind::Annotate(
                Box::new(self.lower_pattern(*inner_pattern, scope, info)),
                self.lower_type_annotation(ty, scope, info),
            ),
            ast::PatternKind::Or(lhs, rhs) => PatternKind::Or(
                Box::new(self.lower_pattern(*lhs, scope, info)),
                Box::new(self.lower_pattern(*rhs, scope, info)),
            ),
            ast::PatternKind::Where(pattern, condition) => PatternKind::Where(
                Box::new(self.lower_pattern(*pattern, scope, info)),
                Box::new(self.lower_expr(*condition, scope, info)),
            ),
            ast::PatternKind::Tuple(patterns) => PatternKind::Tuple(
                patterns
                    .into_iter()
                    .map(|pattern| self.lower_pattern(pattern, scope, info))
                    .collect(),
            ),
        })();

        Pattern {
            span: pattern.span,
            kind,
        }
    }

    fn resolve_value(
        &self,
        span: Span,
        name: InternedString,
        scope: ScopeId,
        info: &mut Info,
    ) -> Option<ExpressionKind> {
        match self.get_in_scope(scope, name, span, info) {
            Some(ScopeValue::Type(id)) => {
                info.declarations
                    .types
                    .get_mut(&id)
                    .unwrap()
                    .uses
                    .insert(span);

                match info
                    .declarations
                    .types
                    .get(&id)
                    .unwrap()
                    .value
                    .as_ref()
                    .unwrap()
                    .kind
                {
                    TypeKind::Marker => Some(ExpressionKind::Marker(id)),
                    _ => {
                        self.add_error(
                            "cannot use type as value",
                            vec![Note::primary(span, "try instantiating the type")],
                        );

                        Some(Expression::error(self, span).kind)
                    }
                }
            }
            Some(ScopeValue::BuiltinType(id)) => {
                info.declarations
                    .builtin_types
                    .get_mut(&id)
                    .unwrap()
                    .uses
                    .insert(span);

                self.add_error(
                    "cannot use builtin type as value",
                    vec![Note::primary(span, "try using a literal instead")],
                );

                Some(Expression::error(self, span).kind)
            }
            Some(ScopeValue::Trait(id)) => {
                info.declarations
                    .traits
                    .get_mut(&id)
                    .unwrap()
                    .uses
                    .insert(span);

                Some(ExpressionKind::Trait(id))
            }
            Some(ScopeValue::TypeParameter(id)) => {
                info.declarations
                    .type_parameters
                    .get_mut(&id)
                    .unwrap()
                    .uses
                    .insert(span);

                self.add_error(
                    "cannot use type parameter as value", vec![Note::primary(
                        span,
                        "type parameters cannot be instantiated because the actual type is not known here",
                    )],
                );

                Some(Expression::error(self, span).kind)
            }
            Some(ScopeValue::Constant(id, _)) => {
                info.declarations
                    .constants
                    .get_mut(&id)
                    .unwrap()
                    .uses
                    .insert(span);

                Some(ExpressionKind::Constant(id))
            }
            Some(ScopeValue::Variable(id)) => {
                info.declarations
                    .variables
                    .get_mut(&id)
                    .unwrap()
                    .uses
                    .insert(span);

                Some(ExpressionKind::Variable(id))
            }
            None => None,
        }
    }

    fn with_parameters(
        &self,
        scope: ScopeId,
        parameters: Option<(Vec<ast::TypeParameter>, Vec<ast::Bound>)>,
        info: &mut Info,
    ) -> (Vec<(Span, TypeParameterId)>, Vec<Bound>) {
        let (parameters, bounds) = match parameters {
            Some((parameters, bounds)) => (parameters, bounds),
            None => return (Vec::new(), Vec::new()),
        };

        let parameters = parameters
            .into_iter()
            .map(|parameter| {
                let id = self.new_type_parameter_id_in(info.file);

                info.declarations.type_parameters.insert(
                    id,
                    Declaration::resolved(Some(parameter.name), parameter.span, ()),
                );

                self.insert_into_scope(scope, parameter.name, ScopeValue::TypeParameter(id), info);

                (parameter.span, id)
            })
            .collect::<Vec<_>>();

        let bounds = bounds
            .into_iter()
            .filter_map(|bound| {
                let tr = match self.get_in_scope(scope, bound.trait_name, bound.trait_span, info) {
                    Some(ScopeValue::Trait(tr)) => {
                        info.declarations
                            .traits
                            .get_mut(&tr)
                            .unwrap()
                            .uses
                            .insert(bound.trait_span);

                        tr
                    }
                    Some(_) => {
                        self.add_error(
                            format!("`{}` is not a trait", bound.trait_name),
                            vec![Note::primary(bound.trait_span, "expected a trait here")],
                        );

                        return None;
                    }
                    None => {
                        self.add_error(
                            format!("cannot find `{}`", bound.trait_name),
                            vec![Note::primary(bound.trait_span, "this name is not defined")],
                        );

                        return None;
                    }
                };

                let parameters = bound
                    .parameters
                    .into_iter()
                    .map(|ty| self.lower_type_annotation(ty, scope, info))
                    .collect();

                Some(Bound {
                    span: bound.span,
                    tr,
                    parameters,
                })
            })
            .collect::<Vec<_>>();

        (parameters, bounds)
    }
}
