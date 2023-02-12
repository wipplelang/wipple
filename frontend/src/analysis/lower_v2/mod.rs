mod builtins;

use crate::{
    analysis::ast_v2,
    helpers::{Backtrace, InternedString, Shared},
    parse::Span,
    BuiltinTypeId, Compiler, ConstantId, FilePath, ScopeId, TraitId, TypeId, TypeParameterId,
    VariableId, VariantIndex,
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
    pub types: BTreeMap<TypeId, TypeDeclaration>,
    pub type_parameters: BTreeMap<TypeParameterId, TypeParameterDeclaration>,
    pub traits: BTreeMap<TraitId, TraitDeclaration>,
    pub builtin_types: BTreeMap<BuiltinTypeId, BuiltinTypeDeclaration>,
    pub constants: BTreeMap<ConstantId, ConstantDeclaration>,
    pub instances: BTreeMap<ConstantId, InstanceDeclaration>,
    pub variables: BTreeMap<VariableId, VariableDeclaration>,
}

#[derive(Debug, Clone, Default)]
struct UnresolvedDeclarations {
    types: BTreeMap<TypeId, Option<TypeDeclaration>>,
    type_parameters: BTreeMap<TypeParameterId, TypeParameterDeclaration>,
    traits: BTreeMap<TraitId, Option<TraitDeclaration>>,
    builtin_types: BTreeMap<BuiltinTypeId, BuiltinTypeDeclaration>,
    constants: BTreeMap<ConstantId, Option<UnresolvedConstantDeclaration>>,
    instances: BTreeMap<ConstantId, Option<InstanceDeclaration>>,
    variables: BTreeMap<VariableId, VariableDeclaration>,
}

impl UnresolvedDeclarations {
    fn resolve(self) -> Declarations {
        Declarations {
            types: self
                .types
                .into_iter()
                .map(|(id, decl)| (id, decl.unwrap()))
                .collect(),
            type_parameters: self.type_parameters,
            traits: self
                .traits
                .into_iter()
                .map(|(id, decl)| (id, decl.unwrap()))
                .collect(),
            builtin_types: self.builtin_types,
            constants: self
                .constants
                .into_iter()
                .map(|(id, decl)| (id, decl.unwrap().resolve()))
                .collect(),
            instances: self
                .instances
                .into_iter()
                .map(|(id, decl)| (id, decl.unwrap()))
                .collect(),
            variables: self.variables,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LanguageItems {
    // TODO
}

#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    pub name_span: Span,
    pub name: InternedString,
    pub parameters: Vec<TypeParameterId>,
    pub kind: TypeDeclarationKind,
    pub uses: HashSet<Span>,
}

#[derive(Debug, Clone)]
pub enum TypeDeclarationKind {
    Structure(Vec<StructureField>),
    Enumeration(Vec<EnumerationVariant>),
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
}

#[derive(Debug, Clone)]
pub struct TypeParameterDeclaration {
    pub span: Span,
    pub name: InternedString,
    pub uses: HashSet<Span>,
}

#[derive(Debug, Clone)]
pub struct TraitDeclaration {
    pub name_span: Span,
    pub name: InternedString,
    pub parameters: Vec<TypeParameterId>,
    pub ty: Option<TypeAnnotation>,
    pub uses: HashSet<Span>,
}

#[derive(Debug, Clone)]
pub struct BuiltinTypeDeclaration {
    pub name: InternedString,
    pub kind: BuiltinTypeDeclarationKind,
    pub uses: HashSet<Span>,
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
    pub name_span: Span,
    pub name: InternedString,
    pub parameters: Vec<TypeParameterId>,
    pub bounds: Vec<Bound>,
    pub ty: TypeAnnotation,
    pub value: Expression,
    pub uses: HashSet<Span>,
}

#[derive(Debug, Clone)]
pub struct UnresolvedConstantDeclaration {
    pub name_span: Span,
    pub name: InternedString,
    pub parameters: Vec<TypeParameterId>,
    pub bounds: Vec<Bound>,
    pub ty: TypeAnnotation,
    pub value: Shared<Option<Expression>>,
    pub uses: HashSet<Span>,
}

impl From<ConstantDeclaration> for UnresolvedConstantDeclaration {
    fn from(decl: ConstantDeclaration) -> Self {
        UnresolvedConstantDeclaration {
            name_span: decl.name_span,
            name: decl.name,
            parameters: decl.parameters,
            bounds: decl.bounds,
            ty: decl.ty,
            value: Shared::new(Some(decl.value)),
            uses: decl.uses,
        }
    }
}

impl UnresolvedConstantDeclaration {
    fn resolve(self) -> ConstantDeclaration {
        ConstantDeclaration {
            name_span: self.name_span,
            name: self.name,
            parameters: self.parameters,
            bounds: self.bounds,
            ty: self.ty,
            value: self.value.into_unique().expect("uninitialized constant"),
            uses: self.uses,
        }
    }
}

#[derive(Debug, Clone)]
pub struct InstanceDeclaration {
    pub instance_span: Span,
    pub parameters: Vec<TypeParameterId>,
    pub bounds: Vec<Bound>,
    pub tr_span: Span,
    pub tr: TraitId,
    pub tr_parameters: Vec<TypeAnnotation>,
    pub value: Expression,
    pub uses: HashSet<Span>,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub name: InternedString,
    pub span: Span,
    pub uses: HashSet<Span>,
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

mod scope {
    use super::*;

    #[derive(Debug, Clone, Default)]
    pub struct Scope {
        parent: Option<ScopeId>,
        values: HashMap<InternedString, ScopeValue>,
        declared_variables: BTreeSet<VariableId>,
        used_variables: Shared<CaptureList>,
    }

    #[derive(Debug, Clone, Copy)]
    pub enum ScopeValue {
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
}

use scope::*;

impl Lowerer {
    fn lower_statements(&mut self, statement: &[ast_v2::Statement]) -> Vec<Expression> {
        todo!()
    }
}
