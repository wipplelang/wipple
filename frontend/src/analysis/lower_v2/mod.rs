mod builtins;

use crate::{
    analysis::ast_v2,
    diagnostics::Note,
    helpers::{Backtrace, InternedString, Shared},
    parse::Span,
    BuiltinTypeId, Compiler, ConstantId, FieldIndex, FilePath, ScopeId, TraitId, TypeId,
    TypeParameterId, VariableId, VariantIndex,
};
use im::HashMap;
use std::{
    collections::{BTreeMap, BTreeSet, HashSet},
    sync::Arc,
};

#[derive(Debug, Clone)]
pub struct File<Decls = Declarations> {
    pub span: Span,
    pub declarations: Decls,
    pub language_items: LanguageItems,
    pub specializations: BTreeMap<ConstantId, ConstantId>,
    pub statements: Vec<Expression>,
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
}

#[derive(Debug, Clone, Default)]
#[non_exhaustive]
pub struct TraitAttributes {
    pub decl_attributes: DeclarationAttributes,
    pub on_unimplemented: Option<InternedString>,
    pub allow_overlapping_instances: bool,
}

#[derive(Debug, Clone)]
pub enum BuiltinTypeDeclaration {
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
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration;

#[derive(Debug, Clone)]
pub struct LanguageItems {
    // TODO
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub span: Span,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Error(Backtrace),
    Marker,
    Constant(ConstantId),
    Trait(TraitId),
    Variable(VariableId),
    Text(InternedString),
    Number(InternedString),
    Block(Vec<Expression>),
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
    fn error(compiler: &Compiler) -> Self {
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
    fn error(compiler: &Compiler) -> Self {
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
    fn error(compiler: &Compiler) -> Self {
        TypeAnnotationKind::Error(compiler.backtrace())
    }
}

#[derive(Debug, Clone)]
pub struct Bound {
    pub span: Span,
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

impl Compiler {
    pub(crate) fn lower_v2(&self, file: &ast_v2::File, dependencies: Vec<Arc<File>>) -> File {
        todo!()
    }
}

#[derive(Debug)]
struct Lowerer {
    compiler: Compiler,
    file: FilePath,
    declarations: UnresolvedDeclarations,
    language_items: LanguageItems,
    specializations: BTreeMap<ConstantId, ConstantId>,
    scopes: BTreeMap<ScopeId, Scope>,
}

#[derive(Debug, Clone, Default)]
struct Scope {
    parent: Option<ScopeId>,
    values: HashMap<InternedString, ScopeValue>,
    declared_variables: BTreeSet<VariableId>,
    used_variables: Shared<CaptureList>,
}

#[derive(Debug, Clone, Copy)]
enum ScopeValue {
    Type(TypeId),
    BuiltinType(BuiltinTypeId),
    Trait(TraitId),
    TypeParameter(TypeParameterId),
    Constant(ConstantId, Option<(TypeId, VariantIndex)>),
    Variable(VariableId),
}

impl Lowerer {
    fn root_scope(&mut self) -> ScopeId {
        let mut scope = Scope::default();
        self.load_builtins(&mut scope);

        let id = self.compiler.new_scope_id_in(self.file);
        self.scopes.insert(id, scope);

        id
    }

    fn child_scope(&mut self, parent: ScopeId) -> ScopeId {
        let scope = Scope {
            parent: Some(parent),
            ..Default::default()
        };

        let id = self.compiler.new_scope_id_in(self.file);
        self.scopes.insert(id, scope);

        id
    }

    fn insert(&mut self, name: InternedString, value: ScopeValue, scope: ScopeId) {
        let scope = self.scopes.get_mut(&scope).unwrap();

        if let ScopeValue::Variable(var) = value {
            scope.declared_variables.insert(var);
        }

        scope.values.insert(name, value);
    }

    fn extend(
        &mut self,
        values: impl IntoIterator<Item = (InternedString, ScopeValue)>,
        scope: ScopeId,
    ) {
        for (name, value) in values {
            self.insert(name, value, scope);
        }
    }

    fn get(&mut self, name: InternedString, span: Span, scope: ScopeId) -> Option<ScopeValue> {
        self.get_inner(name, Some(span), scope)
    }

    fn peek(&mut self, name: InternedString, scope: ScopeId) -> Option<ScopeValue> {
        self.get_inner(name, None, scope)
    }

    fn get_inner(
        &mut self,
        name: InternedString,
        use_span: Option<Span>,
        scope_id: ScopeId,
    ) -> Option<ScopeValue> {
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
            if let Some(ScopeValue::Variable(id)) = result {
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
}

#[derive(Debug)]
struct StatementDeclaration {
    span: Span,
    kind: StatementDeclarationKind,
    attributes: ast_v2::StatementAttributes,
}

#[derive(Debug)]
enum StatementDeclarationKind {
    Type(
        TypeId,
        Option<ast_v2::TypePattern>,
        ast_v2::TypeAssignmentValue,
    ),
    Trait(
        TraitId,
        Option<ast_v2::TypePattern>,
        ast_v2::TraitAssignmentValue,
    ),
    Constant(ConstantId, Option<ast_v2::TypePattern>, ast_v2::Type),
    Instance(
        ConstantId,
        Option<ast_v2::TypePattern>,
        ast_v2::InstanceStatement,
    ),
    Use(Span, InternedString),
    Queued(QueuedStatement),
}

#[derive(Debug)]
enum QueuedStatement {
    Assign(ast_v2::Pattern, ast_v2::Expression),
    Expression(ast_v2::Expression),
}

impl Lowerer {
    fn lower_statements(
        &mut self,
        statements: &[ast_v2::Statement],
        scope: ScopeId,
    ) -> Vec<Expression> {
        let declarations = statements
            .iter()
            .map(|statement| self.lower_statement(statement, scope))
            .collect::<Vec<_>>();

        let mut queue = Vec::new();
        let mut current_constant = None;

        for mut decl in declarations {
            let mut decl = match decl {
                Some(decl) => decl,
                None => continue,
            };

            if !matches!(decl.kind, StatementDeclarationKind::Queued(_)) {
                current_constant = None;
            }

            let scope_value = match &decl.kind {
                StatementDeclarationKind::Type(id, ty_pattern, ty) => {
                    let (parameters, bounds) = ty_pattern
                        .as_ref()
                        .map(|ty_pattern| self.lower_type_pattern(ty_pattern, scope))
                        .unwrap_or_default();

                    if let Some(bound) = bounds.first() {
                        self.compiler.add_error(
                            "`type` declarations may not have bounds",
                            vec![Note::primary(bound.span, "try removing this")],
                        );
                    }

                    let kind = match &ty.body {
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
                                            *id,
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

                    self.declarations.types.get_mut(id).unwrap().value = Some(TypeDeclaration {
                        parameters,
                        kind,
                        attributes: self.lower_type_attributes(&mut decl.attributes, scope),
                    });

                    ScopeValue::Type(*id)
                }
                StatementDeclarationKind::Trait(_, _, _) => todo!(),
                StatementDeclarationKind::Constant(_, _, _) => todo!(),
                StatementDeclarationKind::Instance(_, _, _) => todo!(),
                StatementDeclarationKind::Use(_, _) => todo!(),
                StatementDeclarationKind::Queued(_) => todo!(),
            };
        }

        todo!()
    }

    fn lower_statement(
        &mut self,
        statement: &ast_v2::Statement,
        scope: ScopeId,
    ) -> Option<StatementDeclaration> {
        todo!()
    }

    fn lower_type(&mut self, ty: &ast_v2::Type, scope: ScopeId) -> TypeAnnotation {
        todo!()
    }

    fn lower_type_pattern(
        &mut self,
        type_pattern: &ast_v2::TypePattern,
        scope: ScopeId,
    ) -> (Vec<TypeParameterId>, Vec<Bound>) {
        todo!()
    }

    fn lower_type_attributes(
        &mut self,
        attributes: &ast_v2::StatementAttributes,
        scope: ScopeId,
    ) -> TypeAttributes {
        todo!()
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
