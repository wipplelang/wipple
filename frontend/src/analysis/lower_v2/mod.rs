mod builtins;

use crate::{
    analysis::ast_v2,
    diagnostics::Note,
    helpers::{Backtrace, InternedString, Shared},
    parse::Span,
    BuiltinTypeId, Compiler, ConstantId, FieldIndex, FilePath, ScopeId, TraitId, TypeId,
    TypeParameterId, VariableId, VariantIndex,
};
use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    mem,
    sync::Arc,
};

#[derive(Debug, Clone)]
pub struct File<Decls = Declarations> {
    pub span: Span,
    pub declarations: Decls,
    pub info: FileInfo,
    pub specializations: BTreeMap<ConstantId, Vec<ConstantId>>,
    pub statements: Vec<Expression>,
    pub exported: HashMap<InternedString, AnyDeclaration>,
}

#[derive(Debug, Clone, Default)]
pub struct Declarations {
    pub types: BTreeMap<TypeId, Declaration<TypeDeclaration>>,
    pub type_parameters: BTreeMap<TypeParameterId, Declaration<TypeParameterDeclaration>>,
    pub traits: BTreeMap<TraitId, Declaration<TraitDeclaration>>,
    pub builtin_types: BTreeMap<BuiltinTypeId, Declaration<BuiltinTypeDeclaration>>,
    pub constants: BTreeMap<ConstantId, Declaration<ConstantDeclaration>>,
    pub instances: BTreeMap<ConstantId, Declaration<InstanceDeclaration>>,
    pub variables: BTreeMap<VariableId, Declaration<VariableDeclaration>>,
}

#[derive(Debug, Clone, Default)]
struct UnresolvedDeclarations {
    types: BTreeMap<TypeId, Declaration<Option<TypeDeclaration>>>,
    type_parameters: BTreeMap<TypeParameterId, Declaration<TypeParameterDeclaration>>,
    traits: BTreeMap<TraitId, Declaration<Option<TraitDeclaration>>>,
    builtin_types: BTreeMap<BuiltinTypeId, Declaration<BuiltinTypeDeclaration>>,
    constants: BTreeMap<ConstantId, Declaration<Option<UnresolvedConstantDeclaration>>>,
    instances: BTreeMap<ConstantId, Declaration<Option<InstanceDeclaration>>>,
    variables: BTreeMap<VariableId, Declaration<VariableDeclaration>>,
}

impl UnresolvedDeclarations {
    fn resolve(self) -> Declarations {
        Declarations {
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
#[non_exhaustive]
pub struct DeclarationAttributes {
    pub help: Vec<InternedString>,
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
}

#[derive(Debug, Clone, Default)]
#[non_exhaustive]
pub struct TypeAttributes {
    pub decl_attributes: DeclarationAttributes,
    pub on_mismatch: Vec<(Option<TypeParameterId>, InternedString)>,
}

#[derive(Debug, Clone)]
pub struct StructureField {
    pub name_span: Span,
    pub name: InternedString,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct EnumerationVariant {
    pub name_span: Span,
    pub name: InternedString,
    pub tys: Vec<TypeAnnotation>,
    pub constructor: ConstantId,
}

#[derive(Debug, Clone)]
pub struct TypeParameterDeclaration;

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
    pub on_unimplemented: Option<InternedString>,
    pub allow_overlapping_instances: bool,
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
}

#[derive(Debug, Clone)]
pub struct ConstantDeclaration {
    pub parameters: Vec<TypeParameterId>,
    pub bounds: Vec<Bound>,
    pub ty: TypeAnnotation,
    pub value: Expression,
    pub attributes: ConstantAttributes,
}

#[derive(Debug, Clone)]
pub struct UnresolvedConstantDeclaration {
    pub parameters: Vec<TypeParameterId>,
    pub bounds: Vec<Bound>,
    pub ty: TypeAnnotation,
    pub value: Shared<Option<Expression>>,
    pub attributes: ConstantAttributes,
}

impl From<ConstantDeclaration> for UnresolvedConstantDeclaration {
    fn from(decl: ConstantDeclaration) -> Self {
        UnresolvedConstantDeclaration {
            parameters: decl.parameters,
            bounds: decl.bounds,
            ty: decl.ty,
            value: Shared::new(Some(decl.value)),
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
            attributes: self.attributes,
        }
    }
}

#[derive(Debug, Clone, Default)]
#[non_exhaustive]
pub struct ConstantAttributes {
    pub decl_attributes: DeclarationAttributes,
    pub is_specialization: bool,
}

#[derive(Debug, Clone)]
pub struct InstanceDeclaration {
    pub parameters: Vec<TypeParameterId>,
    pub bounds: Vec<Bound>,
    pub tr_span: Span,
    pub tr: TraitId,
    pub tr_parameters: Vec<TypeAnnotation>,
    pub value: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration;

#[derive(Debug, Clone, Default)]
pub struct FileInfo {
    pub recursion_limit: Option<usize>,
    pub language_items: LanguageItems,
}

impl FileInfo {
    fn merge(&mut self, other: FileInfo) {
        self.recursion_limit = match (self.recursion_limit, other.recursion_limit) {
            (None, None) => None,
            (None, Some(limit)) | (Some(limit), None) => Some(limit),
            (Some(limit), Some(other)) => Some(limit.max(other)),
        };

        self.language_items.merge(other.language_items);
    }
}

#[derive(Debug, Clone, Default)]
pub struct LanguageItems {
    pub boolean: Option<TypeId>,
}

impl LanguageItems {
    fn merge(&mut self, other: LanguageItems) {
        if let Some(boolean) = other.boolean {
            self.boolean.get_or_insert(boolean);
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub span: Span,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Error(Backtrace),
    Marker(TypeId),
    Constant(ConstantId),
    Trait(TraitId),
    Variable(VariableId),
    Text(InternedString),
    Number(InternedString),
    Block(Vec<Expression>, bool),
    End(Box<Expression>),
    Call(Box<Expression>, Box<Expression>),
    Function(Pattern, Box<Expression>, CaptureList),
    When(Box<Expression>, Vec<Arm>),
    External(InternedString, InternedString, Vec<Expression>),
    Runtime(RuntimeFunction, Vec<Expression>),
    Annotate(Box<Expression>, TypeAnnotation),
    Initialize(Pattern, Box<Expression>),
    Instantiate(TypeId, Vec<((Span, InternedString), Expression)>),
    Variant(TypeId, VariantIndex, Vec<Expression>),
    Tuple(Vec<Expression>),
}

impl ExpressionKind {
    pub(crate) fn error(compiler: &Compiler) -> Self {
        ExpressionKind::Error(compiler.backtrace())
    }
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub span: Span,
    pub pattern: Pattern,
    pub body: Expression,
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
    Variable(VariableId),
    Destructure(HashMap<InternedString, Pattern>),
    Variant(TypeId, VariantIndex, Vec<Pattern>),
    Annotate(Box<Pattern>, TypeAnnotation),
    Or(Box<Pattern>, Box<Pattern>),
    Where(Box<Pattern>, Box<Expression>),
    Tuple(Vec<Pattern>),
}

impl PatternKind {
    pub(crate) fn error(compiler: &Compiler) -> Self {
        PatternKind::Error(compiler.backtrace())
    }
}

#[derive(Debug, Clone)]
pub struct TypeAnnotation {
    pub span: Span,
    pub kind: TypeAnnotationKind,
}

#[derive(Debug, Clone)]
pub enum TypeAnnotationKind {
    Error(Backtrace),
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
    pub tr_span: Span,
    pub tr: TraitId,
    pub parameters: Vec<TypeAnnotation>,
}

pub type CaptureList = Vec<(VariableId, Span)>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, strum::EnumString, strum::Display)]
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

#[derive(Debug, Clone, Copy)]
pub enum AnyDeclaration {
    Type(TypeId),
    BuiltinType(BuiltinTypeId),
    Trait(TraitId),
    TypeParameter(TypeParameterId),
    Constant(ConstantId, Option<(TypeId, VariantIndex)>),
    Variable(VariableId),
}

impl Compiler {
    pub(crate) fn lower_v2(&self, file: &ast_v2::File, dependencies: Vec<Arc<File>>) -> File {
        let mut lowerer = Lowerer {
            compiler: self.clone(),
            file: file.span.path,
            file_info: Default::default(),
            declarations: Default::default(),
            specializations: Default::default(),
            scopes: Default::default(),
        };

        let scope = lowerer.root_scope(file.root_scope);

        for dependency in dependencies {
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
                // syntaxes,
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

            lowerer.extend(dependency.exported.clone(), scope);
        }

        let statements = lowerer.lower_statements(&file.statements, scope);

        for constant in lowerer.declarations.constants.values() {
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
                    );

                    Expression {
                        span: constant.span,
                        kind: ExpressionKind::error(self),
                    }
                });
        }

        for instance in lowerer.declarations.instances.values_mut() {
            let tr = instance.value.as_ref().unwrap().tr;
            let tr_decl = lowerer.declarations.traits.get(&tr).unwrap();

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

                    instance.value.as_mut().unwrap().value = Some(Expression {
                        span: instance.span,
                        kind: ExpressionKind::error(self),
                    });
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

        let exported = lowerer.export(scope);

        let mut specializations = BTreeMap::<ConstantId, Vec<ConstantId>>::new();
        for (constant, specialized_constant) in lowerer.specializations {
            specializations
                .entry(specialized_constant)
                .or_default()
                .push(constant);
        }

        File {
            span: file.span,
            declarations: lowerer.declarations.resolve(),
            info: lowerer.file_info,
            specializations,
            statements,
            exported,
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
    scopes: BTreeMap<ScopeId, Scope>,
}

#[derive(Debug, Clone, Default)]
struct Scope {
    parent: Option<ScopeId>,
    values: HashMap<InternedString, AnyDeclaration>,
    declared_variables: BTreeSet<VariableId>,
    used_variables: Shared<CaptureList>,
}

impl Lowerer {
    fn root_scope(&mut self, id: ScopeId) -> ScopeId {
        let mut scope = Scope::default();
        self.load_builtins(&mut scope);

        self.scopes.insert(id, scope);

        id
    }

    fn child_scope(&mut self, id: ScopeId, parent: ScopeId) -> ScopeId {
        let scope = Scope {
            parent: Some(parent),
            ..Default::default()
        };

        self.scopes.insert(id, scope);

        id
    }

    fn insert(&mut self, name: InternedString, value: AnyDeclaration, scope: ScopeId) {
        let scope = self.scopes.get_mut(&scope).unwrap();

        if let AnyDeclaration::Variable(var) = value {
            scope.declared_variables.insert(var);
        }

        scope.values.insert(name, value);
    }

    fn extend(
        &mut self,
        values: impl IntoIterator<Item = (InternedString, AnyDeclaration)>,
        scope: ScopeId,
    ) {
        for (name, value) in values {
            self.insert(name, value, scope);
        }
    }

    fn get(&mut self, name: InternedString, span: Span, scope: ScopeId) -> Option<AnyDeclaration> {
        self.get_inner(name, Some(span), scope)
    }

    fn peek(&mut self, name: InternedString, scope: ScopeId) -> Option<AnyDeclaration> {
        self.get_inner(name, None, scope)
    }

    fn get_inner(
        &mut self,
        name: InternedString,
        use_span: Option<Span>,
        scope_id: ScopeId,
    ) -> Option<AnyDeclaration> {
        let mut parent = Some(scope_id);
        let mut result = None;
        let mut used_variables = Vec::new();

        while let Some(scope_id) = parent {
            let scope = self.scopes.get(&scope_id).unwrap();

            if let Some(value) = scope.values.get(&name) {
                result = Some(*value);
                break;
            }

            if use_span.is_some() {
                used_variables.push(&scope.used_variables);
            }

            parent = scope.parent;
        }

        if let Some(span) = use_span {
            if let Some(AnyDeclaration::Variable(id)) = result {
                for u in used_variables {
                    u.lock().push((id, span));
                }
            }
        }

        result
    }

    fn used_variables(&mut self, scope_id: ScopeId) -> CaptureList {
        let mut parent = Some(scope_id);
        let mut used_variables = CaptureList::new();

        while let Some(scope_id) = parent {
            let scope = self.scopes.get(&scope_id).unwrap();

            used_variables.extend(
                scope
                    .used_variables
                    .lock()
                    .iter()
                    .filter(|(var, _)| !scope.declared_variables.contains(var))
                    .copied(),
            );

            parent = scope.parent;
        }

        used_variables
    }

    fn export(&mut self, scope: ScopeId) -> HashMap<InternedString, AnyDeclaration> {
        mem::take(&mut self.scopes.get_mut(&scope).unwrap().values)
    }
}

#[derive(Debug)]
struct StatementDeclaration<'a> {
    span: Span,
    kind: StatementDeclarationKind<'a>,
    attributes: &'a ast_v2::StatementAttributes,
}

#[derive(Debug)]
enum StatementDeclarationKind<'a> {
    Type(
        TypeId,
        Option<(ScopeId, (Vec<TypeParameterId>, Vec<Bound>))>,
        &'a ast_v2::TypeAssignmentValue,
    ),
    Trait(
        TraitId,
        Option<(ScopeId, (Vec<TypeParameterId>, Vec<Bound>))>,
        &'a ast_v2::TraitAssignmentValue,
    ),
    Constant(
        ConstantId,
        (ScopeId, (Vec<TypeParameterId>, Vec<Bound>)),
        &'a ast_v2::Type,
    ),
    Instance(
        ConstantId,
        Option<(ScopeId, (Vec<TypeParameterId>, Vec<Bound>))>,
        (
            Span,
            InternedString,
            &'a Vec<Result<ast_v2::Type, ast_v2::SyntaxError>>,
        ),
        Option<&'a ast_v2::Expression>,
    ),
    Use(Span, InternedString),
    Queued(QueuedStatement<'a>),
}

#[derive(Debug)]
enum QueuedStatement<'a> {
    Assign(&'a ast_v2::Pattern, &'a ast_v2::Expression),
    Expression(&'a ast_v2::Expression),
}

impl Lowerer {
    fn lower_statements(
        &mut self,
        statements: &[Result<ast_v2::Statement, ast_v2::SyntaxError>],
        scope: ScopeId,
    ) -> Vec<Expression> {
        let declarations = statements
            .iter()
            .filter_map(|statement| statement.as_ref().ok())
            .map(|statement| self.lower_statement(statement, scope))
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
                    let (scope, (parameters, bounds)) =
                        ty_pattern.unwrap_or_else(|| (scope, Default::default()));

                    if let Some(bound) = bounds.first() {
                        self.compiler.add_error(
                            "`type` declarations may not have bounds",
                            vec![Note::primary(bound.tr_span, "try removing this")],
                        );
                    }

                    let kind = match &value.body {
                        Some(ty) => {
                            let ast_v2::TypeBody::Block(ty) = match ty {
                                Ok(ty) => ty,
                                Err(_) => continue,
                            };

                            let mut fields = Vec::new();
                            let mut variants = Vec::new();
                            for (index, member) in ty.members.iter().enumerate() {
                                let member = match member {
                                    Ok(member) => member,
                                    Err(_) => continue,
                                };

                                match member {
                                    ast_v2::TypeMember::Field(field) => {
                                        let ty = match &field.ty {
                                            Ok(ty) => self.lower_type(ty, scope),
                                            Err(error) => TypeAnnotation {
                                                span: error.span,
                                                kind: TypeAnnotationKind::error(&self.compiler),
                                            },
                                        };

                                        fields.push(StructureField {
                                            name_span: field.name_span,
                                            name: field.name,
                                            ty,
                                        });
                                    }
                                    ast_v2::TypeMember::Variant(variant) => {
                                        let index = VariantIndex::new(index);

                                        let tys = variant
                                            .tys
                                            .iter()
                                            .map(|ty| match ty {
                                                Ok(ty) => self.lower_type(ty, scope),
                                                Err(error) => TypeAnnotation {
                                                    span: error.span,
                                                    kind: TypeAnnotationKind::error(&self.compiler),
                                                },
                                            })
                                            .collect::<Vec<_>>();

                                        let constructor = self.generate_variant_constructor(
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
                                );

                                continue;
                            }

                            if !fields.is_empty() {
                                let field_names = fields
                                    .iter()
                                    .enumerate()
                                    .map(|(index, field)| (field.name, FieldIndex::new(index)))
                                    .collect();

                                TypeDeclarationKind::Structure(fields, field_names)
                            } else if !variants.is_empty() {
                                let variant_names = variants
                                    .iter()
                                    .enumerate()
                                    .map(|(index, variant)| {
                                        (variant.name, VariantIndex::new(index))
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
                                );

                                continue;
                            }
                        }
                        None => TypeDeclarationKind::Marker,
                    };

                    let attributes = self.lower_type_attributes(decl.attributes, scope);

                    self.declarations.types.get_mut(&id).unwrap().value = Some(TypeDeclaration {
                        parameters,
                        kind,
                        attributes,
                    });

                    Some(AnyDeclaration::Type(id))
                }
                StatementDeclarationKind::Trait(id, ty_pattern, value) => {
                    let (scope, (parameters, bounds)) =
                        ty_pattern.unwrap_or_else(|| (scope, Default::default()));

                    if let Some(bound) = bounds.first() {
                        self.compiler.add_error(
                            "`trait` declarations may not have bounds",
                            vec![Note::primary(bound.tr_span, "try removing this")],
                        );
                    }

                    let ty = value.ty.as_ref().map(|ty| match ty {
                        Ok(ty) => self.lower_type(ty, scope),
                        Err(error) => TypeAnnotation {
                            span: error.span,
                            kind: TypeAnnotationKind::error(&self.compiler),
                        },
                    });

                    let attributes = self.lower_trait_attributes(decl.attributes, scope);

                    self.declarations.traits.get_mut(&id).unwrap().value = Some(TraitDeclaration {
                        parameters,
                        ty,
                        attributes,
                    });

                    Some(AnyDeclaration::Trait(id))
                }
                StatementDeclarationKind::Constant(id, (scope, (parameters, bounds)), ty) => {
                    let ty = self.lower_type(ty, scope);

                    let attributes = self.lower_constant_attributes(decl.attributes, scope);

                    self.declarations.constants.get_mut(&id).unwrap().value =
                        Some(UnresolvedConstantDeclaration {
                            parameters,
                            bounds,
                            ty,
                            value: Default::default(),
                            attributes,
                        });

                    current_constant = Some((id, scope));

                    Some(AnyDeclaration::Constant(id, None))
                }
                StatementDeclarationKind::Instance(
                    id,
                    ty_pattern,
                    (trait_span, trait_name, trait_params),
                    value,
                ) => {
                    let (scope, (parameters, bounds)) =
                        ty_pattern.unwrap_or_else(|| (scope, Default::default()));

                    let tr = match self.get(trait_name, trait_span, scope) {
                        Some(AnyDeclaration::Trait(tr)) => {
                            self.declarations
                                .traits
                                .get_mut(&tr)
                                .unwrap()
                                .uses
                                .insert(trait_span);

                            tr
                        }
                        Some(_) => {
                            self.compiler.add_error(
                                format!("`{trait_name}` is not a trait"),
                                vec![Note::primary(trait_span, "expected a trait here")],
                            );

                            self.declarations.instances.remove(&id);
                            continue;
                        }
                        None => {
                            self.compiler.add_error(
                                format!("cannot find `{trait_name}`"),
                                vec![Note::primary(trait_span, "this name is not defined")],
                            );

                            self.declarations.instances.remove(&id);
                            continue;
                        }
                    };

                    let trait_params = trait_params
                        .iter()
                        .map(|ty| match ty {
                            Ok(ty) => self.lower_type(ty, scope),
                            Err(error) => TypeAnnotation {
                                span: error.span,
                                kind: TypeAnnotationKind::error(&self.compiler),
                            },
                        })
                        .collect::<Vec<_>>();

                    let value = value.map(|value| self.lower_expr(value, scope));

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
                StatementDeclarationKind::Use(span, name) => {
                    let ty = match self.get(name, span, scope) {
                        Some(AnyDeclaration::Type(ty)) => {
                            self.declarations
                                .types
                                .get_mut(&ty)
                                .unwrap()
                                .uses
                                .insert(span);

                            ty
                        }
                        Some(_) => {
                            self.compiler.add_error(
                                format!("`{name}` is not a type"),
                                vec![Note::primary(span, "expected a type here")],
                            );

                            continue;
                        }
                        None => {
                            self.compiler.add_error(
                                format!("cannot find `{name}`"),
                                vec![Note::primary(span, "this name is not defined")],
                            );

                            continue;
                        }
                    };

                    let (constructors, names) = match &self
                        .declarations
                        .types
                        .get(&ty)
                        .unwrap()
                        .value
                        .as_ref()
                        .unwrap()
                        .kind
                    {
                        TypeDeclarationKind::Enumeration(constructors, names) => {
                            (constructors.clone(), names.clone())
                        }
                        _ => {
                            self.compiler.add_error(
                                "only enumerations may be `use`d",
                                vec![Note::primary(
                                    span,
                                    format!("`{name}` is not an enumeration"),
                                )],
                            );

                            continue;
                        }
                    };

                    for (name, index) in names {
                        let variant = constructors[index.into_inner()].constructor;

                        self.insert(
                            name,
                            AnyDeclaration::Constant(variant, Some((ty, index))),
                            scope,
                        );
                    }

                    None
                }
                StatementDeclarationKind::Queued(statement) => {
                    queue.push((decl.span, statement, mem::take(&mut current_constant)));
                    None
                }
            };

            'language_items: {
                if let Some(language_item) = &decl.attributes.language_item {
                    match &language_item.language_item_kind {
                        ast_v2::LanguageItemStatementAttributeKind::Boolean => {
                            let ty = match scope_value {
                                Some(AnyDeclaration::Type(id)) => id,
                                _ => {
                                    self.compiler.add_error(
                                        "`boolean` language item expects a type",
                                        vec![Note::primary(
                                            decl.span,
                                            "expected type declaration here",
                                        )],
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
                                );

                                break 'language_items;
                            }

                            self.file_info.language_items.boolean = Some(ty);
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
                            let pattern = self.lower_pattern(pattern, scope);
                            let value = self.lower_expr(expr, scope);

                            Some(Expression {
                                span,
                                kind: ExpressionKind::Initialize(pattern, Box::new(value)),
                            })
                        }};
                    }

                    match &pattern {
                        ast_v2::Pattern::Name(pattern) => {
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

                                if let Some(AnyDeclaration::Constant(id, _)) =
                                    self.peek(decl.name.unwrap(), scope)
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
                                        prev_constant_scope,
                                    );
                                }

                                let value = self.lower_expr(expr, prev_constant_scope);

                                let used_variables = self.used_variables(prev_constant_scope);
                                if !used_variables.is_empty() {
                                    self.compiler.add_error(
                                        "constant cannot capture outside variables",
                                        used_variables
                                            .into_iter()
                                            .map(|(_, span)| {
                                                Note::primary(span, "captured variable")
                                            })
                                            .collect(),
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
                        );
                    }

                    Some(self.lower_expr(expr, scope))
                },
            })
            .collect()
    }

    fn lower_statement<'a>(
        &mut self,
        statement: &'a ast_v2::Statement,
        scope: ScopeId,
    ) -> Option<StatementDeclaration<'a>> {
        match statement {
            ast_v2::Statement::Annotate(statement) => {
                let id = self.compiler.new_constant_id_in(self.file);

                let (span, name) = *statement.name.as_ref().ok()?;

                let (child_scope, (parameters, bounds), ty) =
                    match statement.annotation.as_ref().ok()? {
                        ast_v2::ConstantTypeAnnotation::Type(annotation) => {
                            (scope, Default::default(), annotation)
                        }
                        ast_v2::ConstantTypeAnnotation::TypeFunction(annotation) => {
                            let scope = self.child_scope(annotation.scope, scope);

                            let (params, bounds) = annotation
                                .pattern
                                .as_ref()
                                .map(|annotation| self.lower_type_pattern(annotation, scope))
                                .unwrap_or_default();

                            let ty =
                                match annotation.annotation.as_deref().ok()? {
                                    ast_v2::ConstantTypeAnnotation::Type(annotation) => annotation,
                                    ast_v2::ConstantTypeAnnotation::TypeFunction(annotation) => {
                                        self.compiler.add_error(
                                    "type annotation may not contain multiple type functions",
                                    vec![Note::primary(annotation.arrow_span, "try removing this")],
                                );

                                        return None;
                                    }
                                };

                            (scope, (params, bounds), ty)
                        }
                    };

                if let Some(AnyDeclaration::Constant(existing_id, variant_info)) =
                    self.get(name, span, scope)
                {
                    if statement.attributes.specialize.is_some() {
                        if variant_info.is_some() {
                            self.compiler.add_error(
                                "cannot specialize a `type` variant",
                                vec![Note::primary(span, "cannot specialize this")],
                            );

                            return None;
                        }

                        if self.specializations.contains_key(&existing_id) {
                            self.compiler.add_error(
                                "cannot specialize constant which is a specialization of another constant",
                                vec![Note::primary(span, "cannot specialize this")],
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
                    span: statement.colon_span,
                    kind: StatementDeclarationKind::Constant(
                        id,
                        (child_scope, (parameters, bounds)),
                        &ty.ty,
                    ),
                    attributes: &statement.attributes,
                })
            }
            ast_v2::Statement::Assign(statement) => match statement.value.as_ref().ok()? {
                ast_v2::AssignmentValue::Trait(value) => {
                    let (span, name) =
                        self.get_name_from_assignment(statement.pattern.as_ref().ok()?)?;

                    let id = self.compiler.new_trait_id_in(self.file);
                    self.insert(name, AnyDeclaration::Trait(id), scope);

                    self.declarations
                        .traits
                        .insert(id, Declaration::unresolved(Some(name), span));

                    Some(StatementDeclaration {
                        span: statement.colon_span,
                        kind: StatementDeclarationKind::Trait(id, None, value),
                        attributes: &statement.attributes,
                    })
                }
                ast_v2::AssignmentValue::Type(value) => {
                    let (span, name) =
                        self.get_name_from_assignment(statement.pattern.as_ref().ok()?)?;

                    let id = self.compiler.new_type_id_in(self.file);
                    self.insert(name, AnyDeclaration::Type(id), scope);

                    self.declarations
                        .types
                        .insert(id, Declaration::unresolved(Some(name), span));

                    Some(StatementDeclaration {
                        span: statement.colon_span,
                        kind: StatementDeclarationKind::Type(id, None, value),
                        attributes: &statement.attributes,
                    })
                }
                ast_v2::AssignmentValue::Syntax(_) => None,
                ast_v2::AssignmentValue::TypeFunction(value) => {
                    let child_scope = self.child_scope(value.scope, scope);

                    let (parameters, bounds) =
                        self.lower_type_pattern(value.pattern.as_ref().ok()?, child_scope);

                    match value.value.as_deref().ok()? {
                        ast_v2::AssignmentValue::Trait(value) => {
                            let (span, name) =
                                self.get_name_from_assignment(statement.pattern.as_ref().ok()?)?;

                            let id = self.compiler.new_trait_id_in(self.file);
                            self.insert(name, AnyDeclaration::Trait(id), scope);

                            self.declarations
                                .traits
                                .insert(id, Declaration::unresolved(Some(name), span));

                            Some(StatementDeclaration {
                                span: statement.colon_span,
                                kind: StatementDeclarationKind::Trait(
                                    id,
                                    Some((child_scope, (parameters, bounds))),
                                    value,
                                ),
                                attributes: &statement.attributes,
                            })
                        }
                        ast_v2::AssignmentValue::Type(value) => {
                            let (span, name) =
                                self.get_name_from_assignment(statement.pattern.as_ref().ok()?)?;

                            let id = self.compiler.new_type_id_in(self.file);
                            self.insert(name, AnyDeclaration::Type(id), scope);

                            self.declarations
                                .types
                                .insert(id, Declaration::unresolved(Some(name), span));

                            Some(StatementDeclaration {
                                span: statement.colon_span,
                                kind: StatementDeclarationKind::Type(
                                    id,
                                    Some((child_scope, (parameters, bounds))),
                                    value,
                                ),
                                attributes: &statement.attributes,
                            })
                        }
                        _ => {
                            self.compiler.add_error(
                                "syntax error",
                                vec![Note::primary(
                                    statement.colon_span,
                                    "expected a `type` or `trait` definition after this",
                                )],
                            );

                            None
                        }
                    }
                }
                ast_v2::AssignmentValue::Expression(value) => {
                    match statement.pattern.as_ref().ok()? {
                        ast_v2::AssignmentPattern::Pattern(pattern) => Some(StatementDeclaration {
                            span: statement.colon_span,
                            kind: StatementDeclarationKind::Queued(QueuedStatement::Assign(
                                &pattern.pattern,
                                &value.expression,
                            )),
                            attributes: &statement.attributes,
                        }),
                        ast_v2::AssignmentPattern::Instance(pattern) => {
                            let id = self.compiler.new_constant_id_in(self.file);

                            self.declarations
                                .instances
                                .insert(id, Declaration::unresolved(None, pattern.instance_span));

                            Some(StatementDeclaration {
                                span: statement.colon_span,
                                kind: StatementDeclarationKind::Instance(
                                    id,
                                    None,
                                    (
                                        pattern.trait_span,
                                        pattern.trait_name,
                                        &pattern.trait_parameters,
                                    ),
                                    Some(&value.expression),
                                ),
                                attributes: &statement.attributes,
                            })
                        }
                        ast_v2::AssignmentPattern::TypeFunction(pattern) => {
                            let child_scope = self.child_scope(pattern.scope, scope);

                            let (parameters, bounds) = self.lower_type_pattern(
                                pattern.type_pattern.as_ref().ok()?,
                                child_scope,
                            );

                            match pattern.assignment_pattern.as_deref().ok()? {
                                ast_v2::AssignmentPattern::Instance(pattern) => {
                                    let id = self.compiler.new_constant_id_in(self.file);

                                    self.declarations.instances.insert(
                                        id,
                                        Declaration::unresolved(None, pattern.instance_span),
                                    );

                                    Some(StatementDeclaration {
                                        span: statement.colon_span,
                                        kind: StatementDeclarationKind::Instance(
                                            id,
                                            Some((child_scope, (parameters, bounds))),
                                            (
                                                pattern.trait_span,
                                                pattern.trait_name,
                                                &pattern.trait_parameters,
                                            ),
                                            Some(&value.expression),
                                        ),
                                        attributes: &statement.attributes,
                                    })
                                }
                                _ => {
                                    self.compiler.add_error(
                                        "syntax error",
                                        vec![Note::primary(
                                            statement.colon_span,
                                            "expected an `instance` definition after this",
                                        )],
                                    );

                                    None
                                }
                            }
                        }
                    }
                }
            },
            ast_v2::Statement::Instance(statement) => {
                let id = self.compiler.new_constant_id_in(self.file);

                self.declarations
                    .instances
                    .insert(id, Declaration::unresolved(None, statement.instance_span));

                Some(StatementDeclaration {
                    span: statement.instance_span,
                    kind: StatementDeclarationKind::Instance(
                        id,
                        None,
                        (
                            statement.trait_span,
                            statement.trait_name,
                            &statement.trait_parameters,
                        ),
                        None,
                    ),
                    attributes: &statement.attributes,
                })
            }
            ast_v2::Statement::TypeFunction(statement) => {
                let child_scope = self.child_scope(statement.scope, scope);

                let (parameters, bounds) =
                    self.lower_type_pattern(statement.pattern.as_ref().ok()?, child_scope);

                match statement.value.as_deref().ok()? {
                    ast_v2::Statement::Instance(statement) => {
                        let id = self.compiler.new_constant_id_in(self.file);

                        self.declarations
                            .instances
                            .insert(id, Declaration::unresolved(None, statement.instance_span));

                        Some(StatementDeclaration {
                            span: statement.instance_span,
                            kind: StatementDeclarationKind::Instance(
                                id,
                                Some((child_scope, (parameters, bounds))),
                                (
                                    statement.trait_span,
                                    statement.trait_name,
                                    &statement.trait_parameters,
                                ),
                                None,
                            ),
                            attributes: &statement.attributes,
                        })
                    }
                    _ => {
                        self.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(
                                statement.arrow_span,
                                "expected an `instance` declaration after this",
                            )],
                        );

                        None
                    }
                }
            }
            ast_v2::Statement::Use(statement) => match statement.kind.as_ref().ok()? {
                ast_v2::UseStatementKind::File(_, _, _) => None,
                ast_v2::UseStatementKind::Name(name_span, name) => Some(StatementDeclaration {
                    span: statement.use_span,
                    kind: StatementDeclarationKind::Use(*name_span, *name),
                    attributes: &statement.attributes,
                }),
            },
            ast_v2::Statement::Expression(statement) => Some(StatementDeclaration {
                // TODO: Have a `span()` function implemented by all AST nodes instead of this
                span: match &statement.expression {
                    ast_v2::Expression::Function(expr) => expr.arrow_span,
                    ast_v2::Expression::Tuple(expr) => expr.comma_span,
                    ast_v2::Expression::Annotate(expr) => expr.colon_span,
                    ast_v2::Expression::End(expr) => expr.end_span,
                    ast_v2::Expression::External(expr) => expr.external_span,
                    ast_v2::Expression::Format(expr) => expr.format_span,
                    ast_v2::Expression::When(expr) => expr.when_span,
                    ast_v2::Expression::Unit(expr) => expr.span,
                    ast_v2::Expression::Name(expr) => expr.span,
                    ast_v2::Expression::Text(expr) => expr.span,
                    ast_v2::Expression::Number(expr) => expr.span,
                    ast_v2::Expression::Call(expr) => expr.span,
                    ast_v2::Expression::Block(expr) => expr.span,
                },
                kind: StatementDeclarationKind::Queued(QueuedStatement::Expression(
                    &statement.expression,
                )),
                attributes: &statement.attributes,
            }),
        }
    }

    fn lower_expr(&mut self, expr: &ast_v2::Expression, scope: ScopeId) -> Expression {
        macro_rules! function_call {
            ($function:expr, $inputs:expr) => {
                $inputs.into_iter().fold($function, |result, next| {
                    let next = match next {
                        Ok(expr) => self.lower_expr(expr, scope),
                        Err(error) => Expression {
                            span: error.span,
                            kind: ExpressionKind::error(&self.compiler),
                        },
                    };

                    Expression {
                        span: Span::join(result.span, next.span),
                        kind: ExpressionKind::Call(Box::new(result), Box::new(next)),
                    }
                })
            };
        }

        match expr {
            ast_v2::Expression::Text(expr) => Expression {
                span: expr.span,
                kind: ExpressionKind::Text(expr.text),
            },
            ast_v2::Expression::Number(expr) => Expression {
                span: expr.span,
                kind: ExpressionKind::Number(expr.number),
            },
            ast_v2::Expression::Name(expr) => match self.resolve_value(expr.span, expr.name, scope)
            {
                Some(value) => Expression {
                    span: expr.span,
                    kind: value,
                },
                None => {
                    self.compiler.add_error(
                        format!("cannot find `{}`", expr.name),
                        vec![Note::primary(expr.span, "this name is not defined")],
                    );

                    Expression {
                        span: expr.span,
                        kind: ExpressionKind::error(&self.compiler),
                    }
                }
            },
            ast_v2::Expression::Block(expr) => {
                let scope = self.child_scope(expr.scope, scope);
                let statements = self.lower_statements(&expr.statements, scope);

                Expression {
                    span: expr.span,
                    kind: ExpressionKind::Block(statements, false),
                }
            }
            ast_v2::Expression::End(expr) => {
                let value = match expr.value.as_deref() {
                    Ok(expr) => expr,
                    Err(error) => {
                        return Expression {
                            span: error.span,
                            kind: ExpressionKind::error(&self.compiler),
                        };
                    }
                };

                Expression {
                    span: expr.end_span,
                    kind: ExpressionKind::End(Box::new(self.lower_expr(value, scope))),
                }
            }
            ast_v2::Expression::Call(expr) => {
                let function = match expr.function.as_deref() {
                    Ok(expr) => expr,
                    Err(_) => {
                        return Expression {
                            span: expr.span,
                            kind: ExpressionKind::error(&self.compiler),
                        };
                    }
                };

                // TODO: Have a `span()` function implemented by all AST nodes instead of this
                let function_span = match function {
                    ast_v2::Expression::Function(expr) => expr.arrow_span,
                    ast_v2::Expression::Tuple(expr) => expr.comma_span,
                    ast_v2::Expression::Annotate(expr) => expr.colon_span,
                    ast_v2::Expression::End(expr) => expr.end_span,
                    ast_v2::Expression::External(expr) => expr.external_span,
                    ast_v2::Expression::Format(expr) => expr.format_span,
                    ast_v2::Expression::When(expr) => expr.when_span,
                    ast_v2::Expression::Unit(expr) => expr.span,
                    ast_v2::Expression::Name(expr) => expr.span,
                    ast_v2::Expression::Text(expr) => expr.span,
                    ast_v2::Expression::Number(expr) => expr.span,
                    ast_v2::Expression::Call(expr) => expr.span,
                    ast_v2::Expression::Block(expr) => expr.span,
                };

                match function {
                    ast_v2::Expression::Name(ty_name) => {
                        let input = match expr.inputs.first() {
                            Some(input) => input,
                            None => {
                                self.compiler.add_error(
                                    "function received no input",
                                    vec![Note::primary(
                                        function_span,
                                        "try providing an input to this function",
                                    )],
                                );

                                return Expression {
                                    span: expr.span,
                                    kind: ExpressionKind::error(&self.compiler),
                                };
                            }
                        };

                        let input = match input {
                            Ok(expr) => expr,
                            Err(error) => {
                                return Expression {
                                    span: error.span,
                                    kind: ExpressionKind::error(&self.compiler),
                                };
                            }
                        };

                        match self.get(ty_name.name, function_span, scope) {
                            Some(AnyDeclaration::Type(id)) => {
                                self.declarations
                                    .types
                                    .get_mut(&id)
                                    .unwrap()
                                    .uses
                                    .insert(function_span);

                                match input {
                                    ast_v2::Expression::Block(block) => {
                                        if expr.inputs.len() > 1 {
                                            self.compiler.add_error(
                                                "too many inputs in structure instantiation", vec![Note::primary(
                                                    expr.span,
                                                    "this structure requires a single block containing its fields",
                                                )],
                                            );
                                        }

                                        let fields = 'parse: {
                                            if block.statements.len() == 1 {
                                                let statement =
                                                    match block.statements.last().unwrap() {
                                                        Ok(statement) => statement,
                                                        Err(_) => {
                                                            break 'parse Vec::new();
                                                        }
                                                    };

                                                if let ast_v2::Statement::Expression(
                                                    ast_v2::ExpressionStatement {
                                                        expression: ast_v2::Expression::Call(expr),
                                                        ..
                                                    },
                                                ) = statement
                                                {
                                                    if let Some(fields) = std::iter::once(
                                                        expr.function.clone().map(|expr| *expr),
                                                    )
                                                    .chain(expr.inputs.clone())
                                                    .map(|expr| match expr {
                                                        Ok(ast_v2::Expression::Name(
                                                            ref name_expr,
                                                        )) => Some((
                                                            (name_expr.span, name_expr.name),
                                                            expr.clone(),
                                                        )),
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
                                                                        scope,
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
                                                    ast_v2::Statement::Assign(statement) => match statement.pattern.as_ref().ok()? {
                                                        ast_v2::AssignmentPattern::Pattern(ast_v2::PatternAssignmentPattern { pattern: ast_v2::Pattern::Name(name) }) => {
                                                            let value = match statement.value.as_ref().ok()? {
                                                                ast_v2::AssignmentValue::Expression(value) => &value.expression,
                                                                _ => {
                                                                    self.compiler.add_error(
                                                                        "syntax error",
                                                                        vec![Note::primary(
                                                                            statement.colon_span,
                                                                            "expected expression after this",
                                                                        )]
                                                                    );

                                                                    return None;
                                                                }
                                                            };

                                                            ((name.span, name.name), value.clone())
                                                        }
                                                        _ => {
                                                            self.compiler.add_error(
                                                                "structure instantiation may not contain complex patterns", vec![Note::primary(
                                                                    statement.colon_span,
                                                                    "try splitting this pattern into multiple names",
                                                                )]
                                                            );

                                                            return None;
                                                        },
                                                    },
                                                    ast_v2::Statement::Expression(ast_v2::ExpressionStatement { expression: expr @ ast_v2::Expression::Name(name), .. }) => {
                                                        ((name.span, name.name), expr.clone())
                                                    },
                                                    // TODO: 'use' inside instantiation
                                                    _ => {
                                                        self.compiler.add_error(
                                                            "structure instantiation may not contain executable statements", vec![Note::primary(
                                                                block.span,
                                                                "try removing this",
                                                            )]
                                                        );

                                                        return None;
                                                    }
                                                }))
                                                .collect::<Vec<_>>()
                                                .into_iter()
                                                .map(|(name, value)| (name, self.lower_expr(&value, scope)))
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

                                        if !matches!(ty.kind, TypeDeclarationKind::Structure(_, _))
                                        {
                                            self.compiler.add_error(
                                                "only structures may be instantiated like this",
                                                vec![Note::primary(
                                                    function_span,
                                                    "not a structure",
                                                )],
                                            );

                                            return Expression {
                                                span: expr.span,
                                                kind: ExpressionKind::error(&self.compiler),
                                            };
                                        }

                                        Expression {
                                            span: expr.span,
                                            kind: ExpressionKind::Instantiate(id, fields),
                                        }
                                    }
                                    ast_v2::Expression::Name(name) => {
                                        let ty_decl = self.declarations.types.get(&id).unwrap();

                                        let (variant_types, variants) = match &ty_decl
                                            .value
                                            .as_ref()
                                            .unwrap()
                                            .kind
                                        {
                                            TypeDeclarationKind::Enumeration(types, variants) => {
                                                (types, variants)
                                            }
                                            _ => {
                                                self.compiler.add_error(
                                                    "only enumerations may be instantiated like this",
                                                    vec![Note::primary(
                                                        function_span,
                                                        "not an enumeration",
                                                    )],
                                                );

                                                return Expression {
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
                                                    vec![Note::primary(ty_name.span, "no such variant")],
                                                );

                                                return Expression {
                                                    span: expr.span,
                                                    kind: ExpressionKind::error(&self.compiler),
                                                };
                                            }
                                        };

                                        function_call!(
                                            Expression {
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
                                            self.lower_expr(function, scope),
                                            &expr.inputs
                                        )
                                    }
                                }
                            }
                            Some(AnyDeclaration::TypeParameter(id)) => {
                                self.declarations
                                    .type_parameters
                                    .get_mut(&id)
                                    .unwrap()
                                    .uses
                                    .insert(function_span);

                                self.compiler.add_error(
                                    "cannot instantiate type parameter",
                                    vec![Note::primary(
                                        function_span,
                                        "the actual type this represents is not known here",
                                    )],
                                );

                                Expression {
                                    span: expr.span,
                                    kind: ExpressionKind::error(&self.compiler),
                                }
                            }
                            Some(AnyDeclaration::BuiltinType(id)) => {
                                self.declarations
                                    .builtin_types
                                    .get_mut(&id)
                                    .unwrap()
                                    .uses
                                    .insert(function_span);

                                self.compiler.add_error(
                                    "cannot instantiate built-in type",
                                    vec![Note::primary(
                                        function_span,
                                        "try using a literal or built-in function instead",
                                    )],
                                );

                                Expression {
                                    span: expr.span,
                                    kind: ExpressionKind::error(&self.compiler),
                                }
                            }
                            _ => {
                                function_call!(self.lower_expr(function, scope), &expr.inputs)
                            }
                        }
                    }
                    _ => function_call!(self.lower_expr(function, scope), &expr.inputs),
                }
            }
            ast_v2::Expression::Function(expr) => {
                let scope = self.child_scope(expr.scope, scope);

                let pattern = match &expr.pattern {
                    Ok(pattern) => self.lower_pattern(pattern, scope),
                    Err(error) => Pattern {
                        span: error.span,
                        kind: PatternKind::error(&self.compiler),
                    },
                };

                let body = match &expr.body {
                    Ok(expr) => self.lower_expr(expr, scope),
                    Err(error) => Expression {
                        span: error.span,
                        kind: ExpressionKind::error(&self.compiler),
                    },
                };

                let captures = self.used_variables(scope);

                Expression {
                    span: expr.arrow_span,
                    kind: ExpressionKind::Function(pattern, Box::new(body), captures),
                }
            }
            ast_v2::Expression::When(expr) => {
                let input = match &expr.input {
                    Ok(expr) => self.lower_expr(expr, scope),
                    Err(error) => Expression {
                        span: error.span,
                        kind: ExpressionKind::error(&self.compiler),
                    },
                };

                let ast_v2::WhenBody::Block(body) = match &expr.body {
                    Ok(body) => body,
                    Err(error) => {
                        return Expression {
                            span: error.span,
                            kind: ExpressionKind::error(&self.compiler),
                        };
                    }
                };

                Expression {
                    span: expr.when_span,
                    kind: ExpressionKind::When(
                        Box::new(input),
                        body.arms
                            .iter()
                            .filter_map(|arm| {
                                let ast_v2::WhenArm::Function(arm) = arm.as_ref().ok()?;

                                let pattern = match &arm.pattern {
                                    Ok(pattern) => self.lower_pattern(pattern, scope),
                                    Err(error) => Pattern {
                                        span: error.span,
                                        kind: PatternKind::error(&self.compiler),
                                    },
                                };

                                let body = match &arm.body {
                                    Ok(expr) => self.lower_expr(expr, scope),
                                    Err(error) => Expression {
                                        span: error.span,
                                        kind: ExpressionKind::error(&self.compiler),
                                    },
                                };

                                Some(Arm {
                                    span: arm.arrow_span,
                                    pattern,
                                    body,
                                })
                            })
                            .collect(),
                    ),
                }
            }
            ast_v2::Expression::External(expr) => {
                let inputs = expr
                    .inputs
                    .iter()
                    .map(|expr| match expr {
                        Ok(expr) => self.lower_expr(expr, scope),
                        Err(error) => Expression {
                            span: error.span,
                            kind: ExpressionKind::error(&self.compiler),
                        },
                    })
                    .collect::<Vec<_>>();

                if expr.namespace.as_str() == "runtime" {
                    let func = match expr.identifier.as_str().parse::<RuntimeFunction>() {
                        Ok(func) => func,
                        Err(_) => {
                            self.compiler.add_error(
                                "unknown runtime function",
                                vec![Note::primary(
                                    expr.external_span,
                                    "check the Wipple source code for the latest list of runtime functions"
                                )],
                            );

                            return Expression {
                                span: expr.external_span,
                                kind: ExpressionKind::error(&self.compiler),
                            };
                        }
                    };

                    Expression {
                        span: expr.external_span,
                        kind: ExpressionKind::Runtime(func, inputs),
                    }
                } else {
                    Expression {
                        span: expr.external_span,
                        kind: ExpressionKind::External(expr.namespace, expr.identifier, inputs),
                    }
                }
            }
            ast_v2::Expression::Annotate(expr) => {
                let value = match &expr.expr {
                    Ok(expr) => self.lower_expr(expr, scope),
                    Err(error) => Expression {
                        span: error.span,
                        kind: ExpressionKind::error(&self.compiler),
                    },
                };

                let ty = match &expr.ty {
                    Ok(ty) => self.lower_type(ty, scope),
                    Err(error) => TypeAnnotation {
                        span: error.span,
                        kind: TypeAnnotationKind::error(&self.compiler),
                    },
                };

                Expression {
                    span: expr.colon_span,
                    kind: ExpressionKind::Annotate(Box::new(value), ty),
                }
            }
            ast_v2::Expression::Tuple(expr) => Expression {
                span: expr.comma_span,
                kind: ExpressionKind::Tuple(
                    expr.exprs
                        .iter()
                        .map(|expr| match expr {
                            Ok(expr) => self.lower_expr(expr, scope),
                            Err(error) => Expression {
                                span: error.span,
                                kind: ExpressionKind::error(&self.compiler),
                            },
                        })
                        .collect(),
                ),
            },
            ast_v2::Expression::Format(_) => todo!("define `format` as `syntax` instead of here"),
            ast_v2::Expression::Unit(expr) => Expression {
                span: expr.span,
                kind: ExpressionKind::Tuple(Vec::new()),
            },
        }
    }

    fn lower_pattern(&mut self, pattern: &ast_v2::Pattern, scope: ScopeId) -> Pattern {
        match pattern {
            ast_v2::Pattern::Wildcard(pattern) => Pattern {
                span: pattern.span,
                kind: PatternKind::Wildcard,
            },
            ast_v2::Pattern::Number(pattern) => Pattern {
                span: pattern.span,
                kind: PatternKind::Number(pattern.number),
            },
            ast_v2::Pattern::Text(pattern) => Pattern {
                span: pattern.span,
                kind: PatternKind::Text(pattern.text),
            },
            ast_v2::Pattern::Name(pattern) => match self.peek(pattern.name, scope) {
                Some(AnyDeclaration::Constant(id, Some((ty, variant)))) => {
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

                    self.insert(pattern.name, AnyDeclaration::Variable(var), scope);

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
            },
            ast_v2::Pattern::Destructure(pattern) => Pattern {
                span: pattern.span,
                kind: PatternKind::Destructure(
                    pattern
                        .destructurings
                        .iter()
                        .filter_map(|destructuring| match destructuring.as_ref().ok()? {
                            ast_v2::Destructuring::Assign(destructuring) => {
                                let pattern = match &destructuring.pattern {
                                    Ok(pattern) => self.lower_pattern(pattern, scope),
                                    Err(error) => Pattern {
                                        span: error.span,
                                        kind: PatternKind::error(&self.compiler),
                                    },
                                };

                                Some(vec![(destructuring.name, pattern)])
                            }
                            ast_v2::Destructuring::Name(destructuring) => Some(vec![(
                                destructuring.name,
                                self.lower_pattern(
                                    &ast_v2::Pattern::Name(ast_v2::NamePattern {
                                        span: destructuring.span,
                                        name: destructuring.name,
                                    }),
                                    scope,
                                ),
                            )]),
                            ast_v2::Destructuring::List(destructuring) => Some(
                                destructuring
                                    .names
                                    .iter()
                                    .filter_map(|destructuring| {
                                        let destructuring = destructuring.as_ref().ok()?;

                                        Some((
                                            destructuring.name,
                                            self.lower_pattern(
                                                &ast_v2::Pattern::Name(ast_v2::NamePattern {
                                                    span: destructuring.span,
                                                    name: destructuring.name,
                                                }),
                                                scope,
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
            ast_v2::Pattern::Variant(pattern) => {
                let first = match self.get(pattern.name, pattern.name_span, scope) {
                    Some(name) => name,
                    None => {
                        self.compiler.add_error(
                            format!("cannot find `{}`", pattern.name),
                            vec![Note::primary(pattern.name_span, "this name is not defined")],
                        );

                        return Pattern {
                            span: pattern.span,
                            kind: PatternKind::error(&self.compiler),
                        };
                    }
                };

                let mut values = pattern.values.iter();

                match first {
                    AnyDeclaration::Type(ty) => {
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
                                );

                                return Pattern {
                                    span: pattern.span,
                                    kind: PatternKind::error(&self.compiler),
                                };
                            }
                        };

                        let second_span = match second {
                            ast_v2::Pattern::Tuple(pattern) => pattern.comma_span,
                            ast_v2::Pattern::Annotate(pattern) => pattern.colon_span,
                            ast_v2::Pattern::Where(pattern) => pattern.where_span,
                            ast_v2::Pattern::Or(pattern) => pattern.or_span,
                            ast_v2::Pattern::Name(pattern) => pattern.span,
                            ast_v2::Pattern::Text(pattern) => pattern.span,
                            ast_v2::Pattern::Number(pattern) => pattern.span,
                            ast_v2::Pattern::Unit(pattern) => pattern.span,
                            ast_v2::Pattern::Variant(pattern) => pattern.span,
                            ast_v2::Pattern::Destructure(pattern) => pattern.span,
                            ast_v2::Pattern::Wildcard(pattern) => pattern.span,
                        };

                        let variant_name = match second {
                            ast_v2::Pattern::Name(pattern) => pattern.name,
                            _ => {
                                self.compiler.add_error(
                                    "invalid pattern",
                                    vec![Note::primary(
                                        second_span,
                                        "expected a variant name here",
                                    )],
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
                                    vec![Note::primary(second_span, "no such variant")],
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
                                        Ok(value) => self.lower_pattern(value, scope),
                                        Err(error) => Pattern {
                                            span: error.span,
                                            kind: PatternKind::error(&self.compiler),
                                        },
                                    })
                                    .collect(),
                            ),
                        }
                    }
                    AnyDeclaration::Constant(id, Some((ty, variant))) => {
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
                                        Ok(value) => self.lower_pattern(value, scope),
                                        Err(error) => Pattern {
                                            span: error.span,
                                            kind: PatternKind::error(&self.compiler),
                                        },
                                    })
                                    .collect(),
                            ),
                        }
                    }
                    _ => {
                        self.compiler.add_error(
                            format!("cannot use `{}` in pattern", pattern.name),
                            vec![Note::primary(
                                pattern.name_span,
                                "expected a type or variant here",
                            )],
                        );

                        Pattern {
                            span: pattern.span,
                            kind: PatternKind::error(&self.compiler),
                        }
                    }
                }
            }
            ast_v2::Pattern::Annotate(pattern) => {
                let inner_pattern = match pattern.pattern.as_deref() {
                    Ok(value) => self.lower_pattern(value, scope),
                    Err(error) => Pattern {
                        span: error.span,
                        kind: PatternKind::error(&self.compiler),
                    },
                };

                let ty = match &pattern.ty {
                    Ok(ty) => self.lower_type(ty, scope),
                    Err(error) => TypeAnnotation {
                        span: error.span,
                        kind: TypeAnnotationKind::error(&self.compiler),
                    },
                };

                Pattern {
                    span: pattern.colon_span,
                    kind: PatternKind::Annotate(Box::new(inner_pattern), ty),
                }
            }
            ast_v2::Pattern::Or(pattern) => {
                let lhs = match pattern.left.as_deref() {
                    Ok(value) => self.lower_pattern(value, scope),
                    Err(error) => Pattern {
                        span: error.span,
                        kind: PatternKind::error(&self.compiler),
                    },
                };

                let rhs = match pattern.right.as_deref() {
                    Ok(value) => self.lower_pattern(value, scope),
                    Err(error) => Pattern {
                        span: error.span,
                        kind: PatternKind::error(&self.compiler),
                    },
                };

                Pattern {
                    span: pattern.or_span,
                    kind: PatternKind::Or(Box::new(lhs), Box::new(rhs)),
                }
            }
            ast_v2::Pattern::Where(pattern) => {
                let inner_pattern = match pattern.pattern.as_deref() {
                    Ok(value) => self.lower_pattern(value, scope),
                    Err(error) => Pattern {
                        span: error.span,
                        kind: PatternKind::error(&self.compiler),
                    },
                };

                let condition = match pattern.condition.as_deref() {
                    Ok(value) => self.lower_expr(value, scope),
                    Err(error) => Expression {
                        span: error.span,
                        kind: ExpressionKind::error(&self.compiler),
                    },
                };

                Pattern {
                    span: pattern.where_span,
                    kind: PatternKind::Where(Box::new(inner_pattern), Box::new(condition)),
                }
            }
            ast_v2::Pattern::Tuple(pattern) => Pattern {
                span: pattern.comma_span,
                kind: PatternKind::Tuple(
                    pattern
                        .patterns
                        .iter()
                        .map(|pattern| match pattern {
                            Ok(pattern) => self.lower_pattern(pattern, scope),
                            Err(error) => Pattern {
                                span: error.span,
                                kind: PatternKind::error(&self.compiler),
                            },
                        })
                        .collect(),
                ),
            },
            ast_v2::Pattern::Unit(pattern) => Pattern {
                span: pattern.span,
                kind: PatternKind::Tuple(Vec::new()),
            },
        }
    }

    fn lower_type(&mut self, ty: &ast_v2::Type, scope: ScopeId) -> TypeAnnotation {
        match ty {
            ast_v2::Type::Function(ty) => {
                let input = match &ty.input {
                    Ok(ty) => self.lower_type(ty, scope),
                    Err(error) => TypeAnnotation {
                        span: error.span,
                        kind: TypeAnnotationKind::Error(error.trace.clone()),
                    },
                };

                let output = match &ty.output {
                    Ok(ty) => self.lower_type(ty, scope),
                    Err(error) => TypeAnnotation {
                        span: error.span,
                        kind: TypeAnnotationKind::Error(error.trace.clone()),
                    },
                };

                TypeAnnotation {
                    span: ty.arrow_span,
                    kind: TypeAnnotationKind::Function(Box::new(input), Box::new(output)),
                }
            }
            ast_v2::Type::Tuple(ty) => TypeAnnotation {
                span: ty.comma_span,
                kind: TypeAnnotationKind::Tuple(
                    ty.tys
                        .iter()
                        .map(|ty| match ty {
                            Ok(ty) => self.lower_type(ty, scope),
                            Err(error) => TypeAnnotation {
                                span: error.span,
                                kind: TypeAnnotationKind::Error(error.trace.clone()),
                            },
                        })
                        .collect(),
                ),
            },
            ast_v2::Type::Placeholder(ty) => TypeAnnotation {
                span: ty.span,
                kind: TypeAnnotationKind::Placeholder,
            },
            ast_v2::Type::Unit(ty) => TypeAnnotation {
                span: ty.span,
                kind: TypeAnnotationKind::Tuple(Vec::new()),
            },
            ast_v2::Type::Named(ty) => {
                let parameters = ty
                    .parameters
                    .iter()
                    .map(|ty| match ty {
                        Ok(ty) => self.lower_type(ty, scope),
                        Err(error) => TypeAnnotation {
                            span: error.span,
                            kind: TypeAnnotationKind::Error(error.trace.clone()),
                        },
                    })
                    .collect();

                match self.get(ty.name, ty.span, scope) {
                    Some(AnyDeclaration::Type(id)) => {
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
                    }
                    Some(AnyDeclaration::TypeParameter(param)) => {
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
                            );
                        }

                        TypeAnnotation {
                            span: ty.span,
                            kind: TypeAnnotationKind::Parameter(param),
                        }
                    }
                    Some(AnyDeclaration::BuiltinType(builtin)) => {
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
                    }
                    _ => {
                        self.compiler.add_error(
                            format!("cannot find type `{}`", ty.name),
                            vec![Note::primary(ty.span, "no such type")],
                        );

                        TypeAnnotation {
                            span: ty.span,
                            kind: TypeAnnotationKind::error(&self.compiler),
                        }
                    }
                }
            }
        }
    }

    fn lower_type_pattern(
        &mut self,
        type_pattern: &ast_v2::TypePattern,
        scope: ScopeId,
    ) -> (Vec<TypeParameterId>, Vec<Bound>) {
        macro_rules! generate_type_parameters {
            ($params:expr) => {
                $params
                    .into_iter()
                    .map(|(span, name)| {
                        let id = self.compiler.new_type_parameter_id_in(self.file);

                        self.declarations.type_parameters.insert(
                            id,
                            Declaration::resolved(Some(name), span, TypeParameterDeclaration),
                        );

                        self.insert(name, AnyDeclaration::TypeParameter(id), scope);

                        id
                    })
                    .collect()
            };
        }

        match type_pattern {
            ast_v2::TypePattern::Where(type_pattern) => {
                let params = match &type_pattern.pattern {
                    Ok(lhs) => {
                        let params = match lhs.as_ref() {
                            ast_v2::TypePattern::Name(pattern) => {
                                vec![(pattern.span, pattern.name)]
                            }
                            ast_v2::TypePattern::List(pattern) => pattern
                                .patterns
                                .iter()
                                .filter_map(|pattern| match pattern {
                                    Ok(pattern) => match pattern {
                                        ast_v2::TypePattern::Name(pattern) => {
                                            Some((pattern.span, pattern.name))
                                        }
                                        ast_v2::TypePattern::List(pattern) => {
                                            self.compiler.add_error(
                                                "higher-kinded types are not yet supported",
                                                vec![Note::primary(
                                                    pattern.span,
                                                    "try removing this",
                                                )],
                                            );

                                            None
                                        }
                                        ast_v2::TypePattern::Where(pattern) => {
                                            self.compiler.add_error(
                                                "syntax error",
                                                vec![Note::primary(
                                                    pattern.where_span,
                                                    "`where` clause is not allowed here",
                                                )],
                                            );

                                            None
                                        }
                                    },
                                    Err(_) => None,
                                })
                                .collect(),
                            ast_v2::TypePattern::Where(lhs) => {
                                self.compiler.add_error(
                                    "type function may not have multiple `where` clauses",
                                    vec![Note::primary(lhs.where_span, "try removing this")],
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
                            let tr = match self.get(bound.trait_name, bound.trait_span, scope) {
                                Some(AnyDeclaration::Trait(tr)) => {
                                    self.declarations
                                        .traits
                                        .get_mut(&tr)
                                        .unwrap()
                                        .uses
                                        .insert(bound.trait_span);

                                    tr
                                }
                                Some(_) => {
                                    self.compiler.add_error(
                                        format!("`{}` is not a trait", bound.trait_name),
                                        vec![Note::primary(
                                            bound.trait_span,
                                            "expected a trait here",
                                        )],
                                    );

                                    return None;
                                }
                                None => {
                                    self.compiler.add_error(
                                        format!("cannot find `{}`", bound.trait_name),
                                        vec![Note::primary(
                                            bound.trait_span,
                                            "this name is not defined",
                                        )],
                                    );

                                    return None;
                                }
                            };

                            let parameters = bound
                                .parameters
                                .iter()
                                .map(|ty| match ty {
                                    Ok(ty) => self.lower_type(ty, scope),
                                    Err(error) => TypeAnnotation {
                                        span: error.span,
                                        kind: TypeAnnotationKind::error(&self.compiler),
                                    },
                                })
                                .collect();

                            Some(Bound {
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
            ast_v2::TypePattern::Name(pattern) => {
                let params = generate_type_parameters!(vec![(pattern.span, pattern.name)]);
                (params, Vec::new())
            }
            ast_v2::TypePattern::List(pattern) => {
                let params = pattern
                    .patterns
                    .iter()
                    .filter_map(|pattern| match pattern {
                        Ok(pattern) => match pattern {
                            ast_v2::TypePattern::Name(pattern) => {
                                Some((pattern.span, pattern.name))
                            }
                            ast_v2::TypePattern::List(pattern) => {
                                self.compiler.add_error(
                                    "higher-kinded types are not yet supported",
                                    vec![Note::primary(pattern.span, "try removing this")],
                                );

                                None
                            }
                            ast_v2::TypePattern::Where(pattern) => {
                                self.compiler.add_error(
                                    "syntax error",
                                    vec![Note::primary(
                                        pattern.where_span,
                                        "`where` clause is not allowed here",
                                    )],
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
        statement_attributes: &ast_v2::StatementAttributes,
        _scope: ScopeId,
    ) -> DeclarationAttributes {
        // TODO: Raise errors for misused attributes

        DeclarationAttributes {
            help: statement_attributes
                .help
                .clone()
                .into_iter()
                .map(|attribute| attribute.help_text)
                .collect(),
        }
    }

    fn lower_type_attributes(
        &mut self,
        attributes: &ast_v2::StatementAttributes,
        scope: ScopeId,
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
                        Some((span, name)) => match self.get(name, span, scope) {
                            Some(AnyDeclaration::TypeParameter(param)) => {
                                self.declarations
                                    .type_parameters
                                    .get_mut(&param)
                                    .unwrap()
                                    .uses
                                    .insert(span);

                                Some(param)
                            }
                            _ => {
                                self.compiler.add_error(
                                    format!("cannot find type parameter `{name}`"),
                                    vec![Note::primary(span, "no such type")],
                                );

                                return None;
                            }
                        },
                        None => None,
                    };

                    Some((param, attribute.message))
                })
                .collect::<Vec<_>>(),
        }
    }

    fn lower_trait_attributes(
        &mut self,
        attributes: &ast_v2::StatementAttributes,
        scope: ScopeId,
    ) -> TraitAttributes {
        // TODO: Raise errors for misused attributes

        TraitAttributes {
            decl_attributes: self.lower_decl_attributes(attributes, scope),
            on_unimplemented: attributes
                .on_unimplemented
                .as_ref()
                .map(|attribute| attribute.message),
            allow_overlapping_instances: attributes.allow_overlapping_instances.is_some(),
        }
    }

    fn lower_constant_attributes(
        &mut self,
        attributes: &ast_v2::StatementAttributes,
        scope: ScopeId,
    ) -> ConstantAttributes {
        // TODO: Raise errors for misused attributes

        ConstantAttributes {
            decl_attributes: self.lower_decl_attributes(attributes, scope),
            is_specialization: attributes.specialize.is_some(),
        }
    }

    fn get_name_from_assignment(
        &mut self,
        pattern: &ast_v2::AssignmentPattern,
    ) -> Option<(Span, InternedString)> {
        // TODO: Have a `span()` function implemented by all AST nodes instead of this
        let span = match pattern {
            ast_v2::AssignmentPattern::Pattern(pattern) => match &pattern.pattern {
                ast_v2::Pattern::Name(pattern) => return Some((pattern.span, pattern.name)),
                ast_v2::Pattern::Tuple(pattern) => pattern.comma_span,
                ast_v2::Pattern::Annotate(pattern) => pattern.colon_span,
                ast_v2::Pattern::Where(pattern) => pattern.where_span,
                ast_v2::Pattern::Or(pattern) => pattern.or_span,
                ast_v2::Pattern::Text(pattern) => pattern.span,
                ast_v2::Pattern::Number(pattern) => pattern.span,
                ast_v2::Pattern::Unit(pattern) => pattern.span,
                ast_v2::Pattern::Variant(pattern) => pattern.span,
                ast_v2::Pattern::Destructure(pattern) => pattern.span,
                ast_v2::Pattern::Wildcard(pattern) => pattern.span,
            },
            ast_v2::AssignmentPattern::Instance(pattern) => pattern.instance_span,
            ast_v2::AssignmentPattern::TypeFunction(pattern) => pattern.arrow_span,
        };

        self.compiler.add_error(
            "syntax error",
            vec![Note::primary(span, "expected a name here")],
        );

        None
    }

    fn resolve_value(
        &mut self,
        span: Span,
        name: InternedString,
        scope: ScopeId,
    ) -> Option<ExpressionKind> {
        match self.get(name, span, scope) {
            Some(AnyDeclaration::Type(id)) => {
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
                        );

                        Some(ExpressionKind::error(&self.compiler))
                    }
                }
            }
            Some(AnyDeclaration::BuiltinType(id)) => {
                self.declarations
                    .builtin_types
                    .get_mut(&id)
                    .unwrap()
                    .uses
                    .insert(span);

                self.compiler.add_error(
                    "cannot use builtin type as value",
                    vec![Note::primary(span, "try using a literal instead")],
                );

                Some(ExpressionKind::error(&self.compiler))
            }
            Some(AnyDeclaration::Trait(id)) => {
                self.declarations
                    .traits
                    .get_mut(&id)
                    .unwrap()
                    .uses
                    .insert(span);

                Some(ExpressionKind::Trait(id))
            }
            Some(AnyDeclaration::TypeParameter(id)) => {
                self.declarations
                    .type_parameters
                    .get_mut(&id)
                    .unwrap()
                    .uses
                    .insert(span);

                self.compiler.add_error(
                    "cannot use type parameter as value", vec![Note::primary(
                        span,
                        "type parameters cannot be instantiated because the actual type is not known here",
                    )],
                );

                Some(ExpressionKind::error(&self.compiler))
            }
            Some(AnyDeclaration::Constant(id, _)) => {
                self.declarations
                    .constants
                    .get_mut(&id)
                    .unwrap()
                    .uses
                    .insert(span);

                Some(ExpressionKind::Constant(id))
            }
            Some(AnyDeclaration::Variable(id)) => {
                self.declarations
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

    fn generate_variant_constructor(
        &mut self,
        id: TypeId,
        name: InternedString,
        span: Span,
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
                span: Span::join(next.span, result.span),
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
                span,
                kind: ExpressionKind::Variant(
                    id,
                    index,
                    variables
                        .iter()
                        .map(|&(var, span)| Expression {
                            span,
                            kind: ExpressionKind::Variable(var),
                        })
                        .collect(),
                ),
            },
            |result, (index, (var, span))| Expression {
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
                    attributes: Default::default(),
                },
            )
            .make_unresolved(),
        );

        constructor_id
    }
}
