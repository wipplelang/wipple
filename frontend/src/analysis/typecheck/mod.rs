#![allow(clippy::type_complexity)]

#[macro_use]
mod number;

pub mod display;
mod engine;
mod exhaustiveness;
pub mod format;
mod queries;
pub mod traverse;

pub use engine::{BottomTypeReason, BuiltinType, GenericSubstitutions, Type, TypeStructure};
pub use lower::{Intrinsic, TypeAnnotation, TypeAnnotationKind};

use crate::{
    analysis::{lower, SpanList},
    diagnostics::{Fix, FixRange, Note},
    helpers::{Backtrace, InternedString},
    BuiltinSyntaxId, BuiltinTypeId, Compiler, ConstantId, ExpressionId, FieldIndex, FileKind,
    FilePath, ItemId, PatternId, SyntaxId, TraitId, TypeId, TypeParameterId, VariableId,
    VariantIndex,
};
use async_trait::async_trait;
use itertools::Itertools;
use parking_lot::RwLock;
use serde::Serialize;
use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    mem,
    os::raw::{c_int, c_uint},
};

#[derive(Debug)]
pub enum Progress {
    CollectingTypes,
    ResolvingDeclarations { count: usize, remaining: usize },
}

#[derive(Debug, Default)]
pub struct Program {
    pub items: BTreeMap<ItemId, RwLock<(Option<(Option<TraitId>, ConstantId)>, Expression)>>,
    pub contexts: BTreeMap<ConstantId, ItemId>,
    pub entrypoint: Option<ItemId>,
    pub file_attributes: BTreeMap<FilePath, lower::FileAttributes>,
    pub declarations: Declarations,
    pub exported: HashMap<InternedString, HashSet<lower::AnyDeclaration>>,
}

macro_rules! declarations {
    ($name:ident<$($container:ident)::+>) => {
        #[derive(Debug, Clone, Default)]
        pub struct $name {
            pub syntaxes: $($container)::+<SyntaxId, SyntaxDecl>,
            pub types: $($container)::+<TypeId, TypeDecl>,
            pub traits: $($container)::+<TraitId, TraitDecl>,
            pub constants: $($container)::+<ConstantId, ConstantDecl>,
            pub instances: $($container)::+<TraitId, BTreeMap<ConstantId, InstanceDecl>>,
            pub builtin_types: $($container)::+<BuiltinTypeId, BuiltinTypeDecl>,
            pub type_parameters: $($container)::+<TypeParameterId, TypeParameterDecl>,
            /// NOTE: Not all variables will be listed here, only ones that passed typechecking
            pub variables: $($container)::+<VariableId, VariableDecl>,
            pub builtin_syntaxes: $($container)::+<BuiltinSyntaxId, BuiltinSyntaxDecl>,
        }
    };
}

declarations!(DeclarationsInner<im::HashMap>);
declarations!(Declarations<BTreeMap>);

impl From<DeclarationsInner> for Declarations {
    fn from(decls: DeclarationsInner) -> Self {
        Declarations {
            syntaxes: decls.syntaxes.into_iter().collect(),
            types: decls.types.into_iter().collect(),
            traits: decls.traits.into_iter().collect(),
            constants: decls.constants.into_iter().collect(),
            instances: decls.instances.into_iter().collect(),
            builtin_types: decls.builtin_types.into_iter().collect(),
            type_parameters: decls.type_parameters.into_iter().collect(),
            variables: decls.variables.into_iter().collect(),
            builtin_syntaxes: decls.builtin_syntaxes.into_iter().collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxDecl {
    pub name: InternedString,
    pub span: SpanList,
    pub operator: bool,
    pub keyword: bool,
    pub attributes: lower::SyntaxAttributes,
    pub uses: HashSet<SpanList>,
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub name: InternedString,
    pub span: SpanList,
    pub params: Vec<TypeParameterId>,
    pub kind: TypeDeclKind,
    pub convert_from: Vec<(Type, wipple_syntax::parse::Expr<super::Analysis>)>,
    pub attributes: lower::TypeAttributes,
    pub uses: HashSet<SpanList>,
}

#[derive(Debug, Clone)]
pub enum TypeDeclKind {
    Marker,
    Structure {
        fields: Vec<(TypeAnnotation, engine::Type)>,
        field_names: HashMap<InternedString, FieldIndex>,
    },
    Enumeration {
        variants: Vec<Vec<(TypeAnnotation, engine::Type)>>,
        variant_names: HashMap<InternedString, VariantIndex>,
    },
    Alias(engine::Type),
}

#[derive(Debug, Clone)]
pub struct TraitDecl {
    pub name: InternedString,
    pub span: SpanList,
    pub params: Vec<TypeParameterId>,
    pub ty_annotation: Option<TypeAnnotation>,
    pub ty: Option<engine::Type>,
    pub attributes: lower::TraitAttributes,
    pub uses: HashSet<SpanList>,
}

#[derive(Debug, Clone)]
pub struct ConstantDecl {
    pub name: InternedString,
    pub span: SpanList,
    pub params: Vec<TypeParameterId>,
    pub bounds: Vec<Bound>,
    pub bound_annotations: Vec<(TraitId, Vec<TypeAnnotation>)>,
    pub ty_annotation: TypeAnnotation,
    pub ty: engine::Type,
    pub reduced_ty: Option<engine::Type>,
    pub specializations: Vec<ConstantId>,
    pub is_variant: bool,
    pub attributes: lower::ConstantAttributes,
    pub body: Option<Expression>,
    pub uses: HashSet<SpanList>,
}

#[derive(Debug, Clone)]
pub struct InstanceDecl {
    pub span: SpanList,
    pub params: Vec<TypeParameterId>,
    pub bounds: Vec<Bound>,
    pub bound_annotations: Vec<(TraitId, Vec<TypeAnnotation>)>,
    pub trait_id: TraitId,
    pub trait_params: Vec<engine::Type>,
    pub trait_param_annotations: Vec<lower::TypeAnnotation>,
    pub body: Option<Expression>,
    pub item: ItemId,
}

#[derive(Debug, Clone)]
pub struct Bound {
    pub span: SpanList,
    pub trait_id: TraitId,
    pub params: Vec<engine::UnresolvedType>,
}

#[derive(Debug, Clone)]
pub struct BuiltinTypeDecl {
    pub name: InternedString,
    pub span: SpanList,
    pub attributes: lower::DeclarationAttributes,
    pub uses: HashSet<SpanList>,
}

#[derive(Debug, Clone)]
pub struct TypeParameterDecl {
    pub name: Option<InternedString>,
    pub span: SpanList,
    pub default: Option<engine::Type>,
    pub infer: bool,
    pub uses: HashSet<SpanList>,
}

#[derive(Debug, Clone)]
pub struct VariableDecl {
    pub name: Option<InternedString>,
    pub span: SpanList,
    pub ty: engine::Type,
    pub uses: HashSet<SpanList>,
}

#[derive(Debug, Clone)]
pub struct BuiltinSyntaxDecl {
    pub name: InternedString,
    pub span: SpanList,
    pub definition: wipple_syntax::ast::BuiltinSyntaxDefinition,
    pub uses: HashSet<SpanList>,
}

macro_rules! expr {
    ($vis:vis, $prefix:literal, $type:ty, { $($kinds:tt)* }) => {
        paste::paste! {
            #[derive(Debug, Clone, Serialize)]
            $vis struct [<$prefix Expression>] {
                $vis id: ExpressionId,
                $vis span: SpanList,
                $vis ty: $type,
                $vis kind: [<$prefix ExpressionKind>],
            }

            #[derive(Debug, Clone, Serialize)]
            $vis enum [<$prefix ExpressionKind>] {
                Error(#[serde(skip)] Backtrace),
                Marker,
                Variable(VariableId),
                Text(InternedString),
                Block(Vec<[<$prefix Expression>]>, bool),
                Call(Box<[<$prefix Expression>]>, Box<[<$prefix Expression>]>, bool),
                Function([<$prefix Pattern>], Box<[<$prefix Expression>]>, lower::CaptureList),
                When(Box<[<$prefix Expression>]>, Vec<[<$prefix Arm>]>),
                External(InternedString, InternedString, Vec<[<$prefix Expression>]>),
                Intrinsic(Intrinsic, Vec<[<$prefix Expression>]>),
                Plugin(InternedString, InternedString, Vec<[<$prefix Expression>]>),
                Initialize([<$prefix Pattern>], Box<[<$prefix Expression>]>),
                Structure(Vec<[<$prefix Expression>]>),
                Variant(VariantIndex, Vec<[<$prefix Expression>]>),
                Tuple(Vec<[<$prefix Expression>]>),
                Format(Vec<(InternedString, [<$prefix Expression>])>, Option<InternedString>),
                With((Option<ConstantId>, Box<[<$prefix Expression>]>), Box<[<$prefix Expression>]>),
                ContextualConstant(ConstantId),
                End(Box<[<$prefix Expression>]>),
                $($kinds)*
            }

            #[derive(Debug, Clone, Serialize)]
            $vis struct [<$prefix Arm>] {
                $vis span: SpanList,
                $vis pattern: [<$prefix Pattern>],
                $vis guard: Option<[<$prefix Expression>]>,
                $vis body: [<$prefix Expression>],
            }

            #[allow(unused)]
            impl [<$prefix ExpressionKind>] {
                pub(crate) fn error(compiler: &Compiler) -> Self {
                    [<$prefix ExpressionKind>]::Error(compiler.backtrace())
                }
            }
        }
    };
}

macro_rules! pattern {
    ($vis:vis, $prefix:literal, { $($kinds:tt)* }) => {
        paste::paste! {
            #[derive(Debug, Clone, Serialize)]
            $vis struct [<$prefix Pattern>] {
                $vis id: PatternId,
                $vis span: SpanList,
                $vis kind: [<$prefix PatternKind>],
            }

            #[derive(Debug, Clone, Serialize)]
            $vis enum [<$prefix PatternKind>] {
                Error(#[serde(skip)] Backtrace),
                Wildcard,
                Text(InternedString),
                Variable(VariableId),
                Or(Box<[<$prefix Pattern>]>, Box<[<$prefix Pattern>]>),
                Tuple(Vec<[<$prefix Pattern>]>),
                $($kinds)*
            }

            #[allow(unused)]
            impl [<$prefix PatternKind>] {
                $vis fn error(compiler: &Compiler) -> Self {
                    [<$prefix PatternKind>]::Error(compiler.backtrace())
                }
            }
        }
    };
}

expr!(, "Unresolved", engine::UnresolvedType, {
    Number(InternedString),
    Trait(TraitId),
    Constant(ConstantId),
});

expr!(, "Monomorphized", engine::UnresolvedType, {
    Number(InternedString),
    UnresolvedConstant(ConstantId),
    UnresolvedTrait(TraitId),
    Constant(ItemId),
});

impl From<UnresolvedExpression> for MonomorphizedExpression {
    fn from(expr: UnresolvedExpression) -> Self {
        MonomorphizedExpression {
            id: expr.id,
            span: expr.span,
            ty: expr.ty,
            kind: match expr.kind {
                UnresolvedExpressionKind::Error(trace) => MonomorphizedExpressionKind::Error(trace),
                UnresolvedExpressionKind::Marker => MonomorphizedExpressionKind::Marker,
                UnresolvedExpressionKind::Variable(var) => {
                    MonomorphizedExpressionKind::Variable(var)
                }
                UnresolvedExpressionKind::Text(text) => MonomorphizedExpressionKind::Text(text),
                UnresolvedExpressionKind::Block(statements, top_level) => {
                    MonomorphizedExpressionKind::Block(
                        statements.into_iter().map(From::from).collect(),
                        top_level,
                    )
                }
                UnresolvedExpressionKind::Call(func, input, first) => {
                    MonomorphizedExpressionKind::Call(
                        Box::new((*func).into()),
                        Box::new((*input).into()),
                        first,
                    )
                }
                UnresolvedExpressionKind::Function(pattern, body, captures) => {
                    MonomorphizedExpressionKind::Function(
                        pattern.into(),
                        Box::new((*body).into()),
                        captures,
                    )
                }
                UnresolvedExpressionKind::When(input, arms) => MonomorphizedExpressionKind::When(
                    Box::new((*input).into()),
                    arms.into_iter().map(From::from).collect(),
                ),
                UnresolvedExpressionKind::External(namespace, identifier, inputs) => {
                    MonomorphizedExpressionKind::External(
                        namespace,
                        identifier,
                        inputs.into_iter().map(From::from).collect(),
                    )
                }
                UnresolvedExpressionKind::Intrinsic(func, inputs) => {
                    MonomorphizedExpressionKind::Intrinsic(
                        func,
                        inputs.into_iter().map(From::from).collect(),
                    )
                }
                UnresolvedExpressionKind::Plugin(path, name, inputs) => {
                    MonomorphizedExpressionKind::Plugin(
                        path,
                        name,
                        inputs.into_iter().map(From::from).collect(),
                    )
                }
                UnresolvedExpressionKind::Initialize(pattern, value) => {
                    MonomorphizedExpressionKind::Initialize(
                        pattern.into(),
                        Box::new((*value).into()),
                    )
                }
                UnresolvedExpressionKind::Structure(fields) => {
                    MonomorphizedExpressionKind::Structure(
                        fields.into_iter().map(From::from).collect(),
                    )
                }
                UnresolvedExpressionKind::Variant(index, values) => {
                    MonomorphizedExpressionKind::Variant(
                        index,
                        values.into_iter().map(From::from).collect(),
                    )
                }
                UnresolvedExpressionKind::Tuple(values) => {
                    MonomorphizedExpressionKind::Tuple(values.into_iter().map(From::from).collect())
                }
                UnresolvedExpressionKind::Format(segments, trailing_segment) => {
                    MonomorphizedExpressionKind::Format(
                        segments
                            .into_iter()
                            .map(|(text, value)| (text, value.into()))
                            .collect(),
                        trailing_segment,
                    )
                }
                UnresolvedExpressionKind::Number(number) => {
                    MonomorphizedExpressionKind::Number(number)
                }
                UnresolvedExpressionKind::Trait(id) => {
                    MonomorphizedExpressionKind::UnresolvedTrait(id)
                }
                UnresolvedExpressionKind::Constant(id) => {
                    MonomorphizedExpressionKind::UnresolvedConstant(id)
                }
                UnresolvedExpressionKind::With((id, value), body) => {
                    MonomorphizedExpressionKind::With(
                        (id, Box::new((*value).into())),
                        Box::new((*body).into()),
                    )
                }
                UnresolvedExpressionKind::ContextualConstant(id) => {
                    MonomorphizedExpressionKind::ContextualConstant(id)
                }
                UnresolvedExpressionKind::End(value) => {
                    MonomorphizedExpressionKind::End(Box::new((*value).into()))
                }
            },
        }
    }
}

impl From<UnresolvedArm> for MonomorphizedArm {
    fn from(arm: UnresolvedArm) -> Self {
        MonomorphizedArm {
            span: arm.span,
            pattern: arm.pattern.into(),
            guard: arm.guard.map(From::from),
            body: arm.body.into(),
        }
    }
}

impl From<UnresolvedPattern> for MonomorphizedPattern {
    fn from(pattern: UnresolvedPattern) -> Self {
        MonomorphizedPattern {
            id: pattern.id,
            span: pattern.span,
            kind: match pattern.kind {
                UnresolvedPatternKind::Error(trace) => MonomorphizedPatternKind::Error(trace),
                UnresolvedPatternKind::Wildcard => MonomorphizedPatternKind::Wildcard,
                UnresolvedPatternKind::Text(text) => MonomorphizedPatternKind::Text(text),
                UnresolvedPatternKind::Variable(var) => MonomorphizedPatternKind::Variable(var),
                UnresolvedPatternKind::Or(left, right) => MonomorphizedPatternKind::Or(
                    Box::new((*left).into()),
                    Box::new((*right).into()),
                ),
                UnresolvedPatternKind::Tuple(patterns) => {
                    MonomorphizedPatternKind::Tuple(patterns.into_iter().map(From::from).collect())
                }
                UnresolvedPatternKind::Number(number) => MonomorphizedPatternKind::Number(number),
                UnresolvedPatternKind::Destructure(ty, fields) => {
                    MonomorphizedPatternKind::UnresolvedDestructure(ty, fields)
                }
                UnresolvedPatternKind::Variant(ty, index, values) => {
                    MonomorphizedPatternKind::UnresolvedVariant(ty, index, values)
                }
            },
        }
    }
}

expr!(pub, "", engine::Type, {
    // PluginOutput(#[serde(skip)] lower::Expression),
    Number(rust_decimal::Decimal),
    Integer(i64),
    Natural(u64),
    Byte(u8),
    Signed(c_int),
    Unsigned(c_uint),
    Float(f32),
    Double(f64),
    Constant(ItemId),
    ExpandedConstant(ItemId),
});

impl Expression {
    pub fn contains_error(&self) -> bool {
        let mut contains_error = false;
        self.traverse(|expr| {
            contains_error |= matches!(expr.kind, ExpressionKind::Error(_));

            contains_error |= match &expr.kind {
                ExpressionKind::Error(_) => true,
                ExpressionKind::Function(pattern, _, _)
                | ExpressionKind::Initialize(pattern, _) => pattern.contains_error(),
                ExpressionKind::When(_, arms) => {
                    arms.iter().any(|arm| arm.pattern.contains_error())
                }
                ExpressionKind::With((None, _), _) => true,
                _ => false,
            };
        });

        contains_error
    }

    pub fn find_first_unresolved_plugin(&self) -> Option<ExpressionId> {
        let mut deepest_plugin = None;
        self.traverse(|expr| {
            if let ExpressionKind::Plugin(_, _, inputs) = &expr.kind {
                deepest_plugin = Some(
                    deepest_plugin
                        .or_else(|| {
                            inputs
                                .iter()
                                .find_map(Expression::find_first_unresolved_plugin)
                        })
                        .unwrap_or(expr.id),
                )
            }
        });

        deepest_plugin
    }
}

pattern!(, "Unresolved", {
    Number(InternedString),
    Destructure(
        engine::UnresolvedType,
        HashMap<InternedString, (UnresolvedPattern, engine::UnresolvedType)>,
    ),
    Variant(TypeId, VariantIndex, Vec<UnresolvedPattern>),
});

pattern!(, "Monomorphized", {
    Number(InternedString),
    UnresolvedDestructure(
        engine::UnresolvedType,
        HashMap<InternedString, (UnresolvedPattern, engine::UnresolvedType)>,
    ),
    Destructure(TypeId, BTreeMap<FieldIndex, MonomorphizedPattern>),
    UnresolvedVariant(TypeId, VariantIndex, Vec<UnresolvedPattern>),
    Variant(TypeId, VariantIndex, Vec<MonomorphizedPattern>),
});

pattern!(pub, "", {
    Number(rust_decimal::Decimal),
    Integer(i64),
    Natural(u64),
    Byte(u8),
    Signed(c_int),
    Unsigned(c_uint),
    Float(f32),
    Double(f64),
    Destructure(TypeId, BTreeMap<FieldIndex, Pattern>),
    Variant(TypeId, VariantIndex, Vec<Pattern>)
});

impl Pattern {
    pub fn contains_error(&self) -> bool {
        let mut contains_error = false;
        self.traverse(|pattern| {
            contains_error |= matches!(pattern.kind, PatternKind::Error(_));
        });

        contains_error
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LiteralKind {
    Number,
    Text,
}

impl Expression {
    pub fn literal_kind(&self) -> Option<LiteralKind> {
        match &self.kind {
            ExpressionKind::Number(_)
            | ExpressionKind::Integer(_)
            | ExpressionKind::Natural(_)
            | ExpressionKind::Byte(_)
            | ExpressionKind::Signed(_)
            | ExpressionKind::Unsigned(_)
            | ExpressionKind::Float(_)
            | ExpressionKind::Double(_) => Some(LiteralKind::Number),
            ExpressionKind::Text(_) => Some(LiteralKind::Text),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub error: Box<engine::TypeError>,
    pub expr: Option<ExpressionId>,
    pub span: SpanList,
    pub notes: Vec<Note>,
    pub trace: Backtrace,
}

impl Error {
    pub fn with_note(mut self, note: Note) -> Self {
        self.notes.push(note);
        self
    }
}

impl Compiler {
    pub(crate) async fn typecheck_with_progress(
        &self,
        entrypoint: lower::File,
        complete: bool,
        mut progress: impl FnMut(Progress),
    ) -> Program {
        let mut typechecker = Typechecker::new(self.clone(), entrypoint);

        progress(Progress::CollectingTypes);
        typechecker.collect_types();

        typechecker
            .resolve(complete, |count, remaining| {
                progress(Progress::ResolvingDeclarations { count, remaining });
            })
            .await
    }
}

#[derive(Debug, Clone)]
struct Typechecker {
    compiler: Compiler,
    entrypoint: lower::File,
    exported: HashMap<InternedString, HashSet<lower::AnyDeclaration>>,
    lowered_entrypoint_expr: lower::Expression,
    ctx: engine::Context,
    declarations: RefCell<DeclarationsInner>,
    instances: im::OrdMap<TraitId, Vec<ConstantId>>,
    generic_constants: im::OrdMap<ConstantId, (bool, lower::Expression)>,
    specialized_constants: im::OrdMap<ConstantId, ConstantId>,
    contexts: BTreeMap<ConstantId, ItemId>,
    item_queue: im::Vector<QueuedItem>,
    items: im::OrdMap<ItemId, (Option<(Option<TraitId>, ConstantId)>, Option<Expression>)>,
    entrypoint_expr: Option<UnresolvedExpression>,
    entrypoint_item: Option<ItemId>,
    errors: RefCell<im::Vector<Error>>,
}

#[derive(Debug, Clone)]
struct QueuedItem {
    generic_id: Option<(Option<TraitId>, ConstantId)>,
    id: ItemId,
    expr: UnresolvedExpression,
    info: MonomorphizeInfo,
    contextual: bool,
    entrypoint: bool,
}

impl Typechecker {
    fn error(
        &self,
        error: impl Into<Box<engine::TypeError>>,
        expr: impl Into<Option<ExpressionId>>,
        span: SpanList,
    ) -> Error {
        Error {
            error: error.into(),
            expr: expr.into(),
            span,
            notes: Vec::new(),
            trace: self.compiler.backtrace(),
        }
    }
}

impl Typechecker {
    pub fn new(compiler: Compiler, mut entrypoint_file: lower::File) -> Self {
        let lowered_entrypoint_expr = lower::Expression {
            id: compiler.new_expression_id(None),
            span: entrypoint_file.span,
            kind: lower::ExpressionKind::Block(mem::take(&mut entrypoint_file.statements), true),
        };

        let exported = mem::take(&mut entrypoint_file.exported);

        Typechecker {
            compiler,
            entrypoint: entrypoint_file,
            lowered_entrypoint_expr,
            exported,
            ctx: Default::default(),
            declarations: Default::default(),
            instances: Default::default(),
            generic_constants: Default::default(),
            specialized_constants: Default::default(),
            contexts: Default::default(),
            item_queue: Default::default(),
            items: Default::default(),
            entrypoint_expr: Default::default(),
            entrypoint_item: None,
            errors: Default::default(),
        }
    }

    pub fn add_error(&self, error: Error) {
        self.errors.borrow_mut().push_back(error);
    }

    pub async fn resolve(
        mut self,
        lowering_is_complete: bool,
        mut progress: impl FnMut(usize, usize),
    ) -> Program {
        // Queue the entrypoint

        let entrypoint_item = mem::take(&mut self.entrypoint_expr).map(|entrypoint| {
            let entrypoint_id = self.compiler.new_item_id_in(entrypoint.span.first().path);
            let info = MonomorphizeInfo::default();

            self.item_queue.push_back(QueuedItem {
                generic_id: None,
                id: entrypoint_id,
                expr: entrypoint,
                info,
                contextual: false,
                entrypoint: true,
            });

            entrypoint_id
        });

        self.entrypoint_item = entrypoint_item;

        // Monomorphize constants

        let mut entrypoint_expr = None;
        if lowering_is_complete {
            let mut count = 0;
            let total = self.item_queue.len();
            while let Some(mut item) = self.item_queue.pop_back() {
                count += 1;
                progress(count, total);

                self.items.insert(item.id, (item.generic_id, None));

                let expr = self.repeatedly_monomorphize_expr(item.expr, &mut item.info);

                let expr = self.finalize_expr(expr);

                let expr = self
                    .resolve_plugins(expr, item.generic_id.map(|(_, id)| id), item.info)
                    .await;

                if item.contextual {
                    if let Some((_, id)) = item.generic_id {
                        // NOTE: This relies on the fact that contextual constants
                        // can't be generic, so all monomorphized items will be the
                        // same code. Thus, we can safely replace the ID
                        self.contexts.insert(id, item.id);
                    }
                }

                if item.entrypoint {
                    entrypoint_expr = Some(expr.clone());
                }

                self.items.get_mut(&item.id).unwrap().1 = Some(expr);
            }
        }

        // Ensure specialized constants unify with their generic counterparts

        for (specialized_id, generic_id) in self.specialized_constants.clone() {
            self.typecheck_specialized_constant(specialized_id, generic_id);
        }

        // Typecheck generic constants

        for (id, (instance, body)) in self.generic_constants.clone() {
            self.typecheck_generic_constant_expr(id, instance, body)
                .await;
        }

        // Report errors if needed

        if lowering_is_complete {
            self.report_errors();
        }

        // Consolidate constants based on their type

        let mut cache = HashMap::new();
        let mut cached = BTreeSet::new();
        let mut map = BTreeMap::new();
        let mut generic_id_map = BTreeMap::new();

        let mut items = mem::take(&mut self.items)
            .into_iter()
            .collect::<BTreeMap<_, _>>();

        for (id, (generic_id, expr)) in &items {
            let expr = expr.as_ref().unwrap();

            let cached_id = *cache
                .entry((generic_id, expr.ty.clone()))
                .or_insert_with(|| {
                    cached.insert(*id);
                    *id
                });

            map.insert(*id, cached_id);

            if let Some((_, generic_id)) = generic_id {
                generic_id_map.insert(*generic_id, cached_id);
            }
        }

        for id in items.keys().cloned().collect::<Vec<_>>() {
            if !cached.contains(&id) {
                items.remove(&id);
            }
        }

        for (_, expr) in items.values_mut() {
            let expr = expr.as_mut().unwrap();

            expr.traverse_mut(|expr| {
                if let ExpressionKind::Constant(id) = &mut expr.kind {
                    if let Some(mapped_id) = map.get(id) {
                        *id = *mapped_id;
                    }
                }
            })
        }

        for (generic_id, item_id) in &mut self.contexts {
            if let Some(&id) = generic_id_map.get(generic_id) {
                *item_id = id;
            }
        }

        // Check exhaustiveness

        if !self.compiler.has_errors() {
            for decl in self.declarations.borrow().constants.values() {
                if let Some(expr) = decl.body.as_ref() {
                    self.check_exhaustiveness(expr);
                }
            }

            for decl in self
                .declarations
                .borrow()
                .instances
                .values()
                .flat_map(|instances| instances.values())
            {
                if let Some(expr) = decl.body.as_ref() {
                    self.check_exhaustiveness(expr);
                }
            }

            if let Some(expr) = entrypoint_expr {
                self.check_exhaustiveness(&expr);
            }
        }

        // Build the final program

        Program {
            items: items
                .into_iter()
                .map(|(id, (ids, expr))| (id, RwLock::new((ids, expr.unwrap()))))
                .collect(),
            contexts: self.contexts,
            entrypoint: entrypoint_item,
            file_attributes: self.entrypoint.attributes,
            exported: self.exported,
            declarations: self.declarations.into_inner().into(),
        }
    }

    fn repeatedly_monomorphize_expr(
        &mut self,
        expr: UnresolvedExpression,
        info: &mut MonomorphizeInfo,
    ) -> MonomorphizedExpression {
        let recursion_limit = self
            .entrypoint
            .info
            .recursion_limit
            .unwrap_or(Compiler::DEFAULT_RECURSION_LIMIT);

        let mut expr = MonomorphizedExpression::from(expr);
        let mut substituted_defaults = false;
        loop {
            if info.recursion_count > recursion_limit {
                self.compiler.add_error(
                    "recursion limit reached",
                    vec![Note::primary(expr.span, "while computing this")],
                );

                return MonomorphizedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty: engine::UnresolvedType::Error,
                    kind: MonomorphizedExpressionKind::error(&self.compiler),
                };
            }

            info.has_resolved_trait = false;
            expr = self.monomorphize_expr(expr, info);

            let is_unresolved = || {
                let pattern_is_unresolved = |pattern: &MonomorphizedPattern| {
                    let mut is_unresolved = false;
                    pattern.traverse(|pattern| {
                        is_unresolved |= matches!(
                            pattern.kind,
                            MonomorphizedPatternKind::UnresolvedDestructure(_, _)
                                | MonomorphizedPatternKind::UnresolvedVariant(_, _, _)
                        )
                    });

                    is_unresolved
                };

                let mut is_unresolved = false;
                expr.traverse(|expr| {
                    is_unresolved |= match &expr.kind {
                        MonomorphizedExpressionKind::UnresolvedTrait(_)
                        | MonomorphizedExpressionKind::UnresolvedConstant(_) => true,
                        MonomorphizedExpressionKind::Function(pattern, _, _)
                        | MonomorphizedExpressionKind::Initialize(pattern, _) => {
                            pattern_is_unresolved(pattern)
                        }
                        MonomorphizedExpressionKind::When(_, arms) => {
                            arms.iter().any(|arm| pattern_is_unresolved(&arm.pattern))
                        }

                        _ => false,
                    };
                });

                is_unresolved
            };

            if substituted_defaults && !info.has_resolved_trait || !is_unresolved() {
                break;
            }

            expr.traverse_mut(|expr| {
                expr.ty.substitute_defaults(&self.ctx);
            });

            substituted_defaults = true;

            info.recursion_count += 1;
        }

        expr
    }
}

impl Typechecker {
    pub fn collect_types(&mut self) {
        let mut info = ConvertInfo::new(None);
        let expr = self.convert_expr(self.lowered_entrypoint_expr.clone(), &mut info);
        self.entrypoint_expr = Some(expr);

        macro_rules! declaration {
            ($kind:ident) => {
                paste::paste! {
                    self.entrypoint
                        .declarations
                        .$kind
                        .keys()
                        .cloned()
                        .collect::<Vec<_>>()
                }
            };
        }

        let mut instances = mem::take(&mut self.instances);
        for id in declaration!(instances) {
            self.with_instance_decl(id, |decl| {
                instances.entry(decl.trait_id).or_default().push(id);
            });
        }
        self.instances = instances;

        let mut specialized_constants = mem::take(&mut self.specialized_constants);
        for generic_id in declaration!(constants) {
            self.with_constant_decl(generic_id, |decl| {
                for &specialized_id in &decl.specializations {
                    specialized_constants.insert(specialized_id, generic_id);
                }
            });
        }
        self.specialized_constants = specialized_constants;

        macro_rules! check {
            ($kind:ident) => {
                paste::paste! {
                    check!($kind as [<$kind s>]);
                }
            };
            ($kind:ident as $plural:ident) => {
                paste::paste! {
                    for id in declaration!($plural) {
                        self.[<with_ $kind _decl>](id, |_| {});
                    }
                }
            };
            ($($kind:ident$(as $plural:ident)?),* $(,)?) => {
                $(check!($kind$(as $plural)?);)*
            }
        }

        check!(
            type,
            trait,
            syntax as syntaxes,
            builtin_type,
            type_parameter,
            // variables are handled inside `finalize_pattern`
            builtin_syntax as builtin_syntaxes,
        );
    }

    async fn typecheck_generic_constant_expr(
        &mut self,
        id: ConstantId,
        instance: bool,
        expr: lower::Expression,
    ) {
        let (tr, generic_ty, bounds, span, contextual) = if instance {
            let (tr, trait_params, span, bounds) = self
                .with_instance_decl(id, |decl| {
                    (
                        decl.trait_id,
                        decl.trait_params.clone(),
                        decl.span,
                        decl.bounds.clone(),
                    )
                })
                .expect("instance should have already been accessed at least once");

            let ty = self.substitute_trait_params(
                tr,
                trait_params.into_iter().map(From::from).collect(),
                span,
            );

            let ty = match ty.finalize(&self.ctx) {
                Some(ty) => ty,
                None => {
                    let expr_id = self.compiler.new_expression_id(id);

                    self.add_error(self.error(
                        engine::TypeError::UnresolvedType(ty),
                        expr_id,
                        span,
                    ));

                    let expr = Expression {
                        id: expr_id,
                        span: expr.span,
                        ty: engine::Type::Error,
                        kind: ExpressionKind::error(&self.compiler),
                    };

                    self.declarations
                        .borrow_mut()
                        .instances
                        .get_mut(&tr)
                        .unwrap()
                        .get_mut(&id)
                        .unwrap()
                        .body = Some(expr);

                    return;
                }
            };

            (Some(tr), ty, bounds, span, false)
        } else {
            self.with_constant_decl(id, |decl| {
                (
                    None,
                    decl.ty.clone(),
                    decl.bounds.clone(),
                    decl.span,
                    decl.attributes.is_contextual,
                )
            })
            .expect("constant should have already been accessed at least once")
        };

        let has_type_params = !generic_ty.params().is_empty() || !bounds.is_empty();

        if contextual && has_type_params {
            self.compiler.add_error(
                "contextual constant may not take type parameters",
                vec![Note::primary(span, "try removing these type parameters")],
            );
        }

        // Check if the constant's generic type can be simplified by applying
        // known bounds
        let reduced_ty = (tr.is_none() && has_type_params)
            .then(|| {
                let (generic_ty, generic_bounds) = match self
                    .with_constant_decl(id, |decl| (decl.ty.clone(), decl.bounds.clone()))
                {
                    Some((generic_ty, bounds)) => (generic_ty, bounds),
                    None => return None,
                };

                let mut bounds = generic_bounds.clone();

                let mut info = MonomorphizeInfo::default();

                let mut generic_ty = engine::UnresolvedType::from(generic_ty);

                let mut substitutions = engine::GenericSubstitutions::new();
                self.add_substitutions_skipping_inferred(&mut generic_ty, &mut substitutions);
                for bound in &mut bounds {
                    for param in &mut bound.params {
                        self.add_substitutions_skipping_inferred(param, &mut substitutions);
                    }
                }

                info.param_substitutions = substitutions;

                for (generic_bound, mut bound) in generic_bounds.into_iter().zip(bounds) {
                    let instance_id = self
                        .instance_for_params(
                            bound.trait_id,
                            bound.params.clone(),
                            None,
                            expr.span,
                            Some(bound.clone()),
                            None,
                            generic_bound.params,
                            &mut info,
                        )
                        .ok()
                        .flatten();

                    for param in &mut bound.params {
                        param.apply(&self.ctx);
                    }

                    let instances = info.bound_instances.entry(bound.trait_id).or_default();

                    if !instances.iter().any(|(params, ..)| params == &bound.params) {
                        instances.push((
                            bound.params,
                            instance_id,
                            bound.span,
                            self.compiler.backtrace(),
                        ));
                    }
                }

                generic_ty.finalize(&self.ctx)
            })
            .flatten();

        let expr = self.convert_expr(expr, &mut ConvertInfo::new(id));
        if let Err(error) = self.ctx.unify_generic(expr.ty.clone(), generic_ty.clone()) {
            self.add_error(self.error(error, expr.id, expr.span));
        }

        let mut monomorphize_info = MonomorphizeInfo::default();

        for mut bound in bounds {
            for param in &mut bound.params {
                param.apply(&self.ctx);
            }

            let instances = monomorphize_info
                .bound_instances
                .entry(bound.trait_id)
                .or_default();

            if !instances.iter().any(|(params, ..)| params == &bound.params) {
                instances.push((bound.params, None, bound.span, self.compiler.backtrace()));
            }
        }

        let expr = self.repeatedly_monomorphize_expr(expr, &mut monomorphize_info);

        let expr = self.finalize_expr(expr);

        let expr = self
            .resolve_plugins(expr, Some(id), monomorphize_info)
            .await;

        if let Some(tr) = tr {
            self.declarations
                .borrow_mut()
                .instances
                .get_mut(&tr)
                .unwrap()
                .get_mut(&id)
                .unwrap()
                .body = Some(expr);
        } else {
            let mut decls = self.declarations.borrow_mut();
            let decl = decls.constants.get_mut(&id).unwrap();

            decl.body = Some(expr);
            decl.reduced_ty = reduced_ty;
        }
    }

    fn typecheck_constant_expr(
        &mut self,
        is_instance: bool,
        trait_id: Option<TraitId>,
        id: ConstantId,
        use_id: impl Into<Option<ExpressionId>>,
        use_span: SpanList,
        use_ty: engine::UnresolvedType,
        mut info: MonomorphizeInfo,
    ) -> Result<ItemId, FindInstanceError> {
        let use_id = use_id.into();

        // Cache constant, ignoring variables
        let use_ty_for_caching = {
            let mut use_ty_for_caching = use_ty.clone();
            use_ty_for_caching.apply(&self.ctx);

            // HACK: Replace all variables with the same variable
            let ctx = self.ctx.clone();
            let unused_var = engine::TypeVariable(usize::MAX);
            for var in use_ty_for_caching.all_vars() {
                ctx.substitutions
                    .borrow_mut()
                    .insert(var, engine::UnresolvedType::Variable(unused_var));
            }

            use_ty_for_caching.apply(&ctx);

            use_ty_for_caching
        };

        if let Some(monomorphized_id) = info.cache.get(&(id, use_ty_for_caching.clone())) {
            return Ok(*monomorphized_id);
        }

        let monomorphized_id = self.compiler.new_item_id();

        let mut candidates = Vec::with_capacity(1);
        if !is_instance {
            self.with_constant_decl(id, |decl| {
                candidates.append(&mut decl.specializations.clone());
            });
        }

        let contextual = self
            .with_constant_decl(id, |decl| decl.attributes.is_contextual)
            .unwrap_or(false);

        for &candidate in &candidates {
            self.specialized_constants.insert(candidate, id);
        }

        candidates.push(id);

        let last_index = candidates.len() - 1;
        let is_last_candidate = |index: usize| index == last_index;

        'check: for (index, candidate) in candidates.into_iter().enumerate() {
            info.cache
                .insert((candidate, use_ty_for_caching.clone()), monomorphized_id);

            let mut info = info.clone();

            let (constant_generic_ty, generic_bounds) = if is_instance {
                let (tr, trait_params, span, bounds) = match self.with_instance_decl(id, |decl| {
                    (
                        decl.trait_id,
                        decl.trait_params.clone(),
                        decl.span,
                        decl.bounds.clone(),
                    )
                }) {
                    Some((tr, trait_params, span, bounds)) => (tr, trait_params, span, bounds),
                    None => continue,
                };

                let ty = self.substitute_trait_params(
                    tr,
                    trait_params.clone().into_iter().map(From::from).collect(),
                    span,
                );

                let ty = match ty.finalize(&self.ctx) {
                    Some(ty) => ty,
                    None => {
                        self.add_error(self.error(
                            engine::TypeError::UnresolvedType(ty),
                            None,
                            span,
                        ));
                        continue;
                    }
                };

                (ty, bounds)
            } else {
                match self
                    .with_constant_decl(candidate, |decl| (decl.ty.clone(), decl.bounds.clone()))
                {
                    Some((generic_ty, bounds)) => (generic_ty, bounds),
                    None => continue,
                }
            };

            let mut bounds = generic_bounds.clone();

            let mut substitutions = engine::GenericSubstitutions::new();
            for bound in &mut bounds {
                for param in &mut bound.params {
                    self.add_substitutions(param, &mut substitutions);
                }
            }

            let mut generic_ty = engine::UnresolvedType::from(constant_generic_ty.clone());
            self.add_substitutions(&mut generic_ty, &mut substitutions);

            let (_, body) = self.generic_constants.get(&candidate).unwrap().clone();

            let mut convert_info = ConvertInfo::new(id);

            let prev_ctx = self.ctx.clone();

            if let Err(error) = self.unify(use_id, use_span, use_ty.clone(), generic_ty.clone()) {
                if is_last_candidate(index) {
                    self.add_error(error);
                } else {
                    self.ctx = prev_ctx;
                    continue 'check;
                }
            }

            let mut generic_expr = self.convert_expr(body.clone(), &mut convert_info);

            if let Err(error) = self.unify(
                use_id,
                use_span,
                generic_ty.clone(),
                generic_expr.ty.clone(),
            ) {
                if is_last_candidate(index) {
                    self.add_error(error);
                } else {
                    self.ctx = prev_ctx;
                    continue 'check;
                }
            }

            // If the constant contains type parameters in bounds that are not referenced in the
            // constant's type, instantiate them here
            let generic_ty_params = generic_expr.ty.params();
            generic_expr.traverse_mut(|expr| {
                for param in expr.ty.params() {
                    substitutions.entry(param).or_insert_with(|| {
                        if !generic_ty_params.contains(&param)
                            && bounds.iter().any(|bound| {
                                bound.params.iter().any(|ty| ty.params().contains(&param))
                            })
                        {
                            let default = self.get_default_for_param(param);
                            engine::UnresolvedType::Variable(self.ctx.new_variable(default))
                        } else {
                            engine::UnresolvedType::Parameter(param)
                        }
                    });
                }

                expr.ty.instantiate_with(&self.ctx, &substitutions);
            });

            info.param_substitutions = substitutions;

            for (generic_bound, mut bound) in generic_bounds.into_iter().zip(bounds) {
                let instance_id = match self.instance_for_params(
                    bound.trait_id,
                    bound.params.clone(),
                    use_id,
                    use_span,
                    Some(bound.clone()),
                    Some((constant_generic_ty.clone(), generic_ty.clone())),
                    generic_bound.params,
                    &mut info,
                ) {
                    Ok(id) => id,
                    Err(error) => {
                        if is_last_candidate(index) {
                            return Err(error);
                        } else {
                            self.ctx = prev_ctx;
                            continue 'check;
                        }
                    }
                };

                for param in &mut bound.params {
                    param.apply(&self.ctx);
                }

                let instances = info.bound_instances.entry(bound.trait_id).or_default();

                if !instances.iter().any(|(params, ..)| params == &bound.params) {
                    instances.push((
                        bound.params,
                        instance_id,
                        bound.span,
                        self.compiler.backtrace(),
                    ));
                }
            }

            self.item_queue.push_back(QueuedItem {
                generic_id: Some((trait_id, candidate)),
                id: monomorphized_id,
                expr: generic_expr,
                info,
                contextual,
                entrypoint: false,
            });

            break 'check;
        }

        Ok(monomorphized_id)
    }

    fn typecheck_specialized_constant(
        &mut self,
        specialized_id: ConstantId,
        generic_id: ConstantId,
    ) {
        let prev_ctx = self.ctx.clone();

        let specialized_constant_decl =
            match self.with_constant_decl(specialized_id, |decl| decl.clone()) {
                Some(decl) => decl,
                None => return,
            };

        let mut generic_constant_decl =
            match self.with_constant_decl(generic_id, |decl| decl.clone()) {
                Some(decl) => decl,
                None => return,
            };

        if let Err(error) = self.ctx.unify(
            specialized_constant_decl.ty.clone().into(),
            generic_constant_decl.ty.clone(),
        ) {
            self.add_error(
                self.error(error, None, specialized_constant_decl.span)
                    .with_note(Note::secondary(
                        specialized_constant_decl.span,
                        "this constant must have a more specific type than the original constant",
                    )),
            );

            return;
        }

        let mut substitutions = self.ctx.unify_params(
            specialized_constant_decl.ty.clone().into(),
            generic_constant_decl.ty.clone(),
        );

        let mut monomorphize_info = MonomorphizeInfo::default();

        let generic_bounds = generic_constant_decl.bounds.clone();

        for bound in &mut generic_constant_decl.bounds {
            // Any type parameters not unified with the specialized constant
            // won't appear here, so add them in manually
            for ty in &mut bound.params {
                ty.apply(&self.ctx);

                for param in ty.params() {
                    substitutions.entry(param).or_insert_with(|| {
                        let default = self.get_default_for_param(param);
                        engine::UnresolvedType::Variable(self.ctx.new_variable(default))
                    });
                }

                ty.instantiate_with(&self.ctx, &substitutions);
            }
        }

        monomorphize_info.param_substitutions = substitutions;

        for (generic_bound, mut bound) in generic_bounds
            .into_iter()
            .zip(generic_constant_decl.bounds.clone())
        {
            let instance_id = match self.instance_for_params(
                bound.trait_id,
                bound.params.clone(),
                None,
                specialized_constant_decl.span,
                Some(bound.clone()),
                Some((
                    generic_constant_decl.ty.clone(),
                    specialized_constant_decl.ty.clone().into(),
                )),
                generic_bound.params.clone(),
                &mut monomorphize_info,
            ) {
                Ok(id) => id,
                Err(error) => {
                    let error = match error {
                        FindInstanceError::RecursionLimitReached
                        | FindInstanceError::TraitNotFound => continue,
                        FindInstanceError::TypeError(error) => error,
                        FindInstanceError::MultipleCandidates => {
                            let ty = self.substitute_trait_params(
                                bound.trait_id,
                                bound.params,
                                bound.span,
                            );

                            self.error(engine::TypeError::UnresolvedType(ty), None, bound.span)
                        }
                    };

                    self.add_error(error.with_note(Note::secondary(
                        specialized_constant_decl.span,
                        "this constant must satisfy the bounds of the original constant",
                    )));

                    continue;
                }
            };

            for param in &mut bound.params {
                param.apply(&self.ctx);
            }

            let instances = monomorphize_info
                .bound_instances
                .entry(bound.trait_id)
                .or_default();

            if !instances.iter().any(|(params, ..)| params == &bound.params) {
                instances.push((
                    bound.params,
                    instance_id,
                    bound.span,
                    self.compiler.backtrace(),
                ));
            }
        }

        self.ctx = prev_ctx;
    }
}

struct ConvertInfo {
    id: Option<ConstantId>,
    variables: BTreeMap<VariableId, engine::UnresolvedType>,
    existing_types: BTreeMap<ExpressionId, engine::UnresolvedType>,
    function_end_value: Option<UnresolvedExpression>,
}

impl ConvertInfo {
    fn new(id: impl Into<Option<ConstantId>>) -> Self {
        ConvertInfo {
            id: id.into(),
            variables: Default::default(),
            existing_types: Default::default(),
            function_end_value: Default::default(),
        }
    }
}

impl Typechecker {
    fn convert_expr(
        &mut self,
        expr: lower::Expression,
        info: &mut ConvertInfo,
    ) -> UnresolvedExpression {
        match expr.kind {
            lower::ExpressionKind::Error(trace) => UnresolvedExpression {
                id: expr.id,
                span: expr.span,
                ty: engine::UnresolvedType::Error,
                kind: UnresolvedExpressionKind::Error(trace),
            },
            lower::ExpressionKind::Marker(id) => {
                let mut ty = {
                    self.with_type_decl(id, |ty| ty.params.clone()).map_or(
                        engine::UnresolvedType::Error,
                        |params| {
                            engine::UnresolvedType::Named(
                                id,
                                params
                                    .into_iter()
                                    .map(engine::UnresolvedType::Parameter)
                                    .collect(),
                                engine::TypeStructure::Marker,
                            )
                        },
                    )
                };

                self.instantiate_generics(&mut ty);

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Marker,
                }
            }
            lower::ExpressionKind::Constant(id) => {
                let (mut ty, contextual) = self
                    .with_constant_decl(id, |constant| {
                        (
                            engine::UnresolvedType::from(constant.ty.clone()),
                            constant.attributes.is_contextual,
                        )
                    })
                    .unwrap_or((engine::UnresolvedType::Error, false));

                self.instantiate_generics(&mut ty);

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty: info.existing_types.get(&expr.id).cloned().unwrap_or(ty),
                    kind: if contextual {
                        UnresolvedExpressionKind::ContextualConstant(id)
                    } else {
                        UnresolvedExpressionKind::Constant(id)
                    },
                }
            }
            lower::ExpressionKind::Trait(id) => {
                let ty = if let Some((span, false)) =
                    self.with_trait_decl(id, |decl| (decl.span, decl.ty.is_some()))
                {
                    self.compiler.add_error(
                        "cannot use this trait as a value",
                        vec![
                            Note::primary(
                                expr.span,
                                "this trait does not store a value and may only be used at the type level"
                            ),
                            Note::secondary(span, "trait defined here"),
                        ],
                    );

                    engine::UnresolvedType::Error
                } else {
                    engine::UnresolvedType::Variable(self.ctx.new_variable(None))
                };

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Trait(id),
                }
            }
            lower::ExpressionKind::Variable(var) => {
                let ty = info.variables.get(&var).cloned().unwrap_or_else(|| {
                    engine::UnresolvedType::Variable(self.ctx.new_variable(None))
                });

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Variable(var),
                }
            }
            lower::ExpressionKind::Text(text) => UnresolvedExpression {
                id: expr.id,
                span: expr.span,
                ty: engine::UnresolvedType::Builtin(engine::BuiltinType::Text),
                kind: UnresolvedExpressionKind::Text(text),
            },
            lower::ExpressionKind::Number(number) => UnresolvedExpression {
                id: expr.id,
                span: expr.span,
                ty: engine::UnresolvedType::NumericVariable(self.ctx.new_variable(None)),
                kind: UnresolvedExpressionKind::Number(number),
            },
            lower::ExpressionKind::Block(statements, top_level) => {
                let statements = statements
                    .into_iter()
                    .map(|statement| self.convert_expr(statement, info))
                    .collect::<Vec<_>>();

                let ty = statements
                    .last()
                    .map(|statement| statement.ty.clone())
                    .unwrap_or_else(|| engine::UnresolvedType::Tuple(Vec::new()));

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Block(statements, top_level),
                }
            }
            lower::ExpressionKind::Call(function, input, first) => {
                let input = self.convert_expr(*input, info);
                let function = self.convert_expr(*function, info);

                let output_ty = engine::UnresolvedType::Variable(self.ctx.new_variable(None));

                if let Err(error) = self.unify(
                    function.id,
                    function.span,
                    engine::UnresolvedType::Function(
                        Box::new(input.ty.clone()),
                        Box::new(output_ty.clone()),
                    ),
                    function.ty.clone(),
                ) {
                    self.add_error(error);
                }

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty: output_ty,
                    kind: UnresolvedExpressionKind::Call(
                        Box::new(function),
                        Box::new(input),
                        first,
                    ),
                }
            }
            lower::ExpressionKind::Function(pattern, body, captures) => {
                let input_ty = engine::UnresolvedType::Variable(self.ctx.new_variable(None));
                let pattern = self.convert_pattern(pattern, input_ty.clone(), None, info);

                let prev_function_end_value = info.function_end_value.take();

                let body = self.convert_expr(*body, info);

                if let Some(end_value_expr) =
                    mem::replace(&mut info.function_end_value, prev_function_end_value)
                {
                    if let Err(error) = self.unify(
                        end_value_expr.id,
                        end_value_expr.span,
                        end_value_expr.ty,
                        body.ty.clone(),
                    ) {
                        self.add_error(error);
                    }
                }

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty: engine::UnresolvedType::Function(
                        Box::new(input_ty),
                        Box::new(body.ty.clone()),
                    ),
                    kind: UnresolvedExpressionKind::Function(pattern, Box::new(body), captures),
                }
            }
            lower::ExpressionKind::When(input, arms) => {
                let input = self.convert_expr(*input, info);

                let arms = arms
                    .into_iter()
                    .map(|arm| self.convert_arm(arm, input.ty.clone(), input.span, info))
                    .collect::<Vec<_>>();

                let ty = {
                    if let Some(first_type) = arms.first().map(|arm| arm.body.ty.clone()) {
                        for arm in &arms {
                            if let Err(error) = self.unify(
                                arm.body.id,
                                arm.body.span,
                                arm.body.ty.clone(),
                                first_type.clone(),
                            ) {
                                self.add_error(error);
                            }
                        }

                        first_type
                    } else {
                        engine::UnresolvedType::Variable(self.ctx.new_variable(None))
                    }
                };

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::When(Box::new(input), arms),
                }
            }
            lower::ExpressionKind::External(lib, identifier, inputs) => {
                let inputs = inputs
                    .into_iter()
                    .map(|expr| self.convert_expr(expr, info))
                    .collect();

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty: engine::UnresolvedType::Variable(self.ctx.new_variable(None)),
                    kind: UnresolvedExpressionKind::External(lib, identifier, inputs),
                }
            }
            lower::ExpressionKind::Intrinsic(func, inputs) => {
                let inputs = inputs
                    .into_iter()
                    .map(|expr| self.convert_expr(expr, info))
                    .collect();

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty: engine::UnresolvedType::Variable(self.ctx.new_variable(None)),
                    kind: UnresolvedExpressionKind::Intrinsic(func, inputs),
                }
            }
            lower::ExpressionKind::Plugin(path, name, inputs) => {
                let inputs = inputs
                    .into_iter()
                    .map(|expr| self.convert_expr(expr, info))
                    .collect();

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty: engine::UnresolvedType::Variable(self.ctx.new_variable(None)),
                    kind: UnresolvedExpressionKind::Plugin(path, name, inputs),
                }
            }
            lower::ExpressionKind::Annotate(value, ty) => {
                let ty = self.convert_type_annotation(ty);
                let value = self.convert_expr(*value, info);

                if let Err(error) = self.unify(value.id, value.span, value.ty, ty.clone()) {
                    self.add_error(error);
                }

                UnresolvedExpression {
                    id: expr.id,
                    span: value.span,
                    ty,
                    kind: value.kind,
                }
            }
            lower::ExpressionKind::Initialize(pattern, value) => {
                let value = self.convert_expr(*value, info);
                let pattern =
                    self.convert_pattern(pattern, value.ty.clone(), Some(value.span), info);

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty: engine::UnresolvedType::Tuple(Vec::new()),
                    kind: UnresolvedExpressionKind::Initialize(pattern, Box::new(value)),
                }
            }
            lower::ExpressionKind::Instantiate(id, fields) => {
                let (kind, params) = match self
                    .with_type_decl(id, |decl| (decl.kind.clone(), decl.params.clone()))
                {
                    Some((kind, params)) => (kind, params),
                    None => {
                        return UnresolvedExpression {
                            id: expr.id,
                            span: expr.span,
                            ty: engine::UnresolvedType::Error,
                            kind: UnresolvedExpressionKind::error(&self.compiler),
                        }
                    }
                };

                let (mut structure_field_tys, structure_field_names) = match kind {
                    TypeDeclKind::Structure {
                        fields,
                        field_names,
                    } => (
                        fields
                            .into_iter()
                            .map(|(_, ty)| ty.into())
                            .collect::<Vec<_>>(),
                        field_names,
                    ),
                    _ => {
                        self.compiler.add_error(
                            "cannot instantiate this type as a structure",
                            vec![Note::primary(expr.span, "this is not a structure type")],
                        );

                        return UnresolvedExpression {
                            id: expr.id,
                            span: expr.span,
                            ty: engine::UnresolvedType::Error,
                            kind: UnresolvedExpressionKind::error(&self.compiler),
                        };
                    }
                };

                let mut ty = engine::UnresolvedType::Named(
                    id,
                    params
                        .into_iter()
                        .map(engine::UnresolvedType::Parameter)
                        .collect(),
                    engine::TypeStructure::Structure(structure_field_tys.clone()),
                );

                let mut substitutions = engine::GenericSubstitutions::new();
                self.add_substitutions(&mut ty, &mut substitutions);

                for index in structure_field_names.values() {
                    self.add_substitutions(
                        &mut structure_field_tys[index.into_inner()],
                        &mut substitutions,
                    );
                }

                let mut fields_by_index = structure_field_names.iter().collect::<Vec<_>>();
                fields_by_index.sort_by_key(|(_, index)| *index);

                let mut unpopulated_fields = vec![None; fields_by_index.len()];
                let mut extra_fields = Vec::new();

                for ((_, name), expr) in fields {
                    let (index, ty) = match structure_field_names.get(&name) {
                        Some(index) if index.into_inner() < fields_by_index.len() => {
                            (*index, structure_field_tys[index.into_inner()].clone())
                        }
                        _ => {
                            extra_fields.push(name);
                            continue;
                        }
                    };

                    let mut value = self.convert_expr(expr, info);

                    if let Err(error) =
                        self.unify(value.id, value.span, value.ty.clone(), ty.clone())
                    {
                        self.add_error(error);
                    }

                    value.ty = ty;

                    unpopulated_fields[index.into_inner()] = Some(value);
                }

                if !extra_fields.is_empty() {
                    self.compiler.add_error(
                        "extra fields",
                        vec![Note::primary(
                            expr.span,
                            format!(
                                "try removing {}",
                                extra_fields
                                    .into_iter()
                                    .map(|field| format!("`{field}`"))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                        )],
                    );
                }

                let mut missing_fields = Vec::new();

                let fields = unpopulated_fields
                    .into_iter()
                    .enumerate()
                    .filter_map(|(index, field)| {
                        if field.is_none() {
                            missing_fields.push(*fields_by_index[index].0);
                        }

                        field
                    })
                    .collect::<Vec<_>>();

                if !missing_fields.is_empty() {
                    self.compiler.add_error(
                        "missing fields",
                        vec![Note::primary(
                            expr.span,
                            format!(
                                "try adding {}",
                                missing_fields
                                    .into_iter()
                                    .map(|field| format!("`{field}`"))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                        )],
                    );
                }

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Structure(fields),
                }
            }
            lower::ExpressionKind::Variant(id, index, values) => {
                let (kind, params) = match self
                    .with_type_decl(id, |decl| (decl.kind.clone(), decl.params.clone()))
                {
                    Some((kind, params)) => (kind, params),
                    None => {
                        return UnresolvedExpression {
                            id: expr.id,
                            span: expr.span,
                            ty: engine::UnresolvedType::Error,
                            kind: UnresolvedExpressionKind::error(&self.compiler),
                        };
                    }
                };

                let variants_tys = match kind {
                    TypeDeclKind::Enumeration { variants, .. } => variants
                        .into_iter()
                        .map(|variant| {
                            variant
                                .into_iter()
                                .map(|(_, ty)| ty.into())
                                .collect::<Vec<_>>()
                        })
                        .collect::<Vec<_>>(),
                    _ => {
                        self.compiler.add_error(
                            "cannot instantiate this type as an enumeration",
                            vec![Note::primary(expr.span, "this is not an enumeration type")],
                        );

                        return UnresolvedExpression {
                            id: expr.id,
                            span: expr.span,
                            ty: engine::UnresolvedType::Error,
                            kind: UnresolvedExpressionKind::error(&self.compiler),
                        };
                    }
                };

                let mut ty = engine::UnresolvedType::Named(
                    id,
                    params
                        .into_iter()
                        .map(engine::UnresolvedType::Parameter)
                        .collect(),
                    engine::TypeStructure::Enumeration(variants_tys.clone()),
                );

                let mut substitutions = engine::GenericSubstitutions::new();
                self.add_substitutions(&mut ty, &mut substitutions);

                let mut variant_tys = variants_tys[index.into_inner()].clone();

                for ty in &mut variant_tys {
                    self.add_substitutions(ty, &mut substitutions);
                }

                let values = values
                    .into_iter()
                    .zip(variant_tys)
                    .map(|(expr, ty)| {
                        let mut value = self.convert_expr(expr, info);

                        if let Err(error) =
                            self.unify(value.id, value.span, value.ty.clone(), ty.clone())
                        {
                            self.add_error(error);
                        }

                        value.ty = ty;

                        value
                    })
                    .collect();

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Variant(index, values),
                }
            }
            lower::ExpressionKind::Tuple(exprs) => {
                let exprs = exprs
                    .into_iter()
                    .map(|expr| self.convert_expr(expr, info))
                    .collect::<Vec<_>>();

                let ty = engine::UnresolvedType::Tuple(
                    exprs.iter().map(|expr| expr.ty.clone()).collect(),
                );

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Tuple(exprs),
                }
            }
            lower::ExpressionKind::Format(segments, trailing_segment) => {
                let show_trait = match self.entrypoint.info.language_items.show {
                    Some(show_trait) => show_trait,
                    None => {
                        self.compiler.add_error(
                            "cannot find `show` language item",
                            vec![Note::primary(
                                expr.span,
                                "using `format` requires the `show` language item",
                            )],
                        );

                        return UnresolvedExpression {
                            id: expr.id,
                            span: expr.span,
                            ty: engine::UnresolvedType::Error,
                            kind: UnresolvedExpressionKind::error(&self.compiler),
                        };
                    }
                };

                self.with_trait_decl(show_trait, |_| {});

                let segments = segments
                    .into_iter()
                    .map(|(text, expr)| {
                        let expr = self.convert_expr(expr, info);

                        let ty = engine::UnresolvedType::Variable(self.ctx.new_variable(None));

                        let expr = UnresolvedExpression {
                            id: expr.id,
                            span: expr.span,
                            ty: ty.clone(),
                            kind: UnresolvedExpressionKind::Call(
                                Box::new(UnresolvedExpression {
                                    id: expr.id,
                                    span: expr.span,
                                    ty: engine::UnresolvedType::Function(
                                        Box::new(expr.ty.clone()),
                                        Box::new(ty),
                                    ),
                                    kind: UnresolvedExpressionKind::Trait(show_trait),
                                }),
                                Box::new(expr),
                                true,
                            ),
                        };

                        (text, expr)
                    })
                    .collect::<Vec<_>>();

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty: engine::UnresolvedType::Builtin(engine::BuiltinType::Text),
                    kind: UnresolvedExpressionKind::Format(segments, trailing_segment),
                }
            }
            lower::ExpressionKind::With((id, value), body) => {
                let value = self.convert_expr(*value, info);

                if let Some(id) = id {
                    let mut ty = self
                        .with_constant_decl(id, |constant| {
                            engine::UnresolvedType::from(constant.ty.clone())
                        })
                        .unwrap_or(engine::UnresolvedType::Error);

                    self.instantiate_generics(&mut ty);

                    if let Err(error) = self.unify(value.id, value.span, value.ty.clone(), ty) {
                        self.add_error(error);
                    }
                }

                let body = self.convert_expr(*body, info);

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty: body.ty.clone(),
                    kind: UnresolvedExpressionKind::With((id, Box::new(value)), Box::new(body)),
                }
            }
            lower::ExpressionKind::End(value) => {
                let value = self.convert_expr(*value, info);

                if let Some(existing_value) = info.function_end_value.replace(value.clone()) {
                    if let Err(error) =
                        self.unify(value.id, value.span, value.ty.clone(), existing_value.ty)
                    {
                        self.add_error(error);
                    }
                }

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty: engine::UnresolvedType::Variable(
                        self.ctx.new_variable(Some(Type::Tuple(Vec::new()))),
                    ),
                    kind: UnresolvedExpressionKind::End(Box::new(value)),
                }
            }
        }
    }

    fn convert_arm(
        &mut self,
        arm: lower::Arm,
        input_ty: engine::UnresolvedType,
        input_span: SpanList,
        info: &mut ConvertInfo,
    ) -> UnresolvedArm {
        UnresolvedArm {
            span: arm.span,
            pattern: self.convert_pattern(arm.pattern, input_ty, Some(input_span), info),
            guard: arm.guard.map(|expr| self.convert_expr(expr, info)),
            body: self.convert_expr(arm.body, info),
        }
    }

    fn convert_pattern(
        &mut self,
        pattern: lower::Pattern,
        ty: engine::UnresolvedType,
        input_span: Option<SpanList>,
        info: &mut ConvertInfo,
    ) -> UnresolvedPattern {
        UnresolvedPattern {
            id: self.compiler.new_pattern_id(info.id),
            span: pattern.span,
            kind: (|| match pattern.kind {
                lower::PatternKind::Error(trace) => UnresolvedPatternKind::Error(trace),
                lower::PatternKind::Wildcard => UnresolvedPatternKind::Wildcard,
                lower::PatternKind::Number(number) => {
                    let numeric_ty =
                        engine::UnresolvedType::NumericVariable(self.ctx.new_variable(None));

                    if let Err(error) = self.unify(None, pattern.span, ty, numeric_ty) {
                        self.add_error(error);
                    }

                    UnresolvedPatternKind::Number(number)
                }
                lower::PatternKind::Text(text) => {
                    if let Err(error) = self.unify(
                        None,
                        pattern.span,
                        ty,
                        engine::UnresolvedType::Builtin(engine::BuiltinType::Text),
                    ) {
                        self.add_error(error);
                    }

                    UnresolvedPatternKind::Text(text)
                }
                lower::PatternKind::Variable(var) => {
                    info.variables.insert(var, ty);
                    UnresolvedPatternKind::Variable(var)
                }
                lower::PatternKind::Destructure(fields) => UnresolvedPatternKind::Destructure(
                    ty,
                    fields
                        .into_iter()
                        .map(|(name, pattern)| {
                            let ty = engine::UnresolvedType::Variable(self.ctx.new_variable(None));
                            (
                                name,
                                (
                                    self.convert_pattern(pattern, ty.clone(), input_span, info),
                                    ty,
                                ),
                            )
                        })
                        .collect(),
                ),
                lower::PatternKind::Variant(id, variant, values) => {
                    let (params, variants_tys) = match self.with_type_decl(id, |decl| {
                        (
                            decl.params.clone(),
                            match &decl.kind {
                                TypeDeclKind::Enumeration { variants, .. } => {
                                    Some(variants.clone())
                                }
                                _ => None,
                            },
                        )
                    }) {
                        Some((params, variants_tys)) => (params, variants_tys),
                        None => return UnresolvedPatternKind::error(&self.compiler),
                    };

                    let variants_tys = match variants_tys {
                        Some(tys) => tys,
                        None => {
                            self.compiler.add_error(
                                "cannot use variant pattern here",
                                match input_span {
                                    Some(span) => vec![
                                        Note::primary(pattern.span, "incorrect pattern"),
                                        Note::secondary(span, "this is not a variant"),
                                    ],
                                    None => vec![Note::primary(
                                        pattern.span,
                                        "the input to this function is not a variant",
                                    )],
                                },
                            );

                            return UnresolvedPatternKind::error(&self.compiler);
                        }
                    };

                    let mut substitutions = engine::GenericSubstitutions::new();

                    let mut variant_tys = variants_tys[variant.into_inner()]
                        .clone()
                        .into_iter()
                        .map(|(_, ty)| engine::UnresolvedType::from(ty))
                        .collect::<Vec<_>>();

                    let enumeration_ty = engine::UnresolvedType::Named(
                        id,
                        params
                            .into_iter()
                            .map(|param| {
                                let mut ty = engine::UnresolvedType::Parameter(param);
                                self.add_substitutions(&mut ty, &mut substitutions);
                                ty
                            })
                            .collect(),
                        engine::TypeStructure::Enumeration(
                            variants_tys
                                .into_iter()
                                .map(|tys| {
                                    tys.into_iter()
                                        .map(|(_, ty)| {
                                            let mut ty = engine::UnresolvedType::from(ty);
                                            ty.instantiate_with(&self.ctx, &substitutions);
                                            ty
                                        })
                                        .collect()
                                })
                                .collect(),
                        ),
                    );

                    if let Err(error) = self.unify(None, pattern.span, ty, enumeration_ty) {
                        self.add_error(error);
                    }

                    for ty in &mut variant_tys {
                        ty.instantiate_with(&self.ctx, &substitutions);
                    }

                    UnresolvedPatternKind::Variant(
                        id,
                        variant,
                        values
                            .into_iter()
                            .zip(variant_tys)
                            .map(|(pattern, ty)| {
                                self.convert_pattern(pattern, ty, input_span, info)
                            })
                            .collect(),
                    )
                }
                lower::PatternKind::Annotate(inner, target_ty) => {
                    let target_ty = self.convert_type_annotation(target_ty);

                    if let Err(error) = self.unify(None, pattern.span, ty, target_ty.clone()) {
                        self.add_error(error);
                    }

                    self.convert_pattern(*inner, target_ty, input_span, info)
                        .kind
                }
                lower::PatternKind::Or(lhs, rhs) => UnresolvedPatternKind::Or(
                    Box::new(self.convert_pattern(*lhs, ty.clone(), input_span, info)),
                    Box::new(self.convert_pattern(*rhs, ty, input_span, info)),
                ),
                lower::PatternKind::Tuple(patterns) => {
                    let tuple_tys = patterns
                        .iter()
                        .map(|_| engine::UnresolvedType::Variable(self.ctx.new_variable(None)))
                        .collect::<Vec<_>>();

                    if let Err(error) = self.unify(
                        None,
                        pattern.span,
                        ty,
                        engine::UnresolvedType::Tuple(tuple_tys.clone()),
                    ) {
                        self.add_error(error);
                    }

                    UnresolvedPatternKind::Tuple(
                        patterns
                            .into_iter()
                            .zip(tuple_tys)
                            .map(|(pattern, ty)| {
                                self.convert_pattern(pattern, ty, input_span, info)
                            })
                            .collect(),
                    )
                }
            })(),
        }
    }
}

#[derive(Debug, Clone, Default)]
struct MonomorphizeInfo {
    param_substitutions: engine::GenericSubstitutions,
    cache: HashMap<(ConstantId, engine::UnresolvedType), ItemId>,
    bound_instances: BTreeMap<
        TraitId,
        Vec<(
            Vec<engine::UnresolvedType>,
            Option<ConstantId>,
            SpanList,
            Backtrace,
        )>,
    >,
    instance_stack: BTreeMap<TraitId, Vec<(ConstantId, Vec<engine::UnresolvedType>)>>,
    recursion_count: usize,
    has_resolved_trait: bool,
}

impl Typechecker {
    fn monomorphize_expr(
        &mut self,
        expr: impl Into<MonomorphizedExpression>,
        info: &mut MonomorphizeInfo,
    ) -> MonomorphizedExpression {
        let mut expr = expr.into();

        expr.ty.apply(&self.ctx);

        let expr_for_error =
            move |typechecker: &Self, id: Result<TraitId, ConstantId>, error: FindInstanceError| {
                match error {
                    FindInstanceError::RecursionLimitReached | FindInstanceError::TraitNotFound => {
                        MonomorphizedExpressionKind::error(&typechecker.compiler)
                    }
                    FindInstanceError::TypeError(error) => {
                        typechecker.add_error(error);
                        MonomorphizedExpressionKind::error(&typechecker.compiler)
                    }
                    FindInstanceError::MultipleCandidates => match id {
                        Ok(trait_id) => MonomorphizedExpressionKind::UnresolvedTrait(trait_id),
                        Err(constant_id) => {
                            MonomorphizedExpressionKind::UnresolvedConstant(constant_id)
                        }
                    },
                }
            };

        MonomorphizedExpression {
            id: expr.id,
            span: expr.span,
            ty: expr.ty.clone(),
            kind: (|| match expr.kind {
                MonomorphizedExpressionKind::Error(trace) => {
                    MonomorphizedExpressionKind::Error(trace)
                }
                MonomorphizedExpressionKind::Marker => MonomorphizedExpressionKind::Marker,
                MonomorphizedExpressionKind::UnresolvedConstant(generic_id) => {
                    match self.typecheck_constant_expr(
                        false,
                        None,
                        generic_id,
                        expr.id,
                        expr.span,
                        expr.ty.clone(),
                        info.clone(),
                    ) {
                        Ok(id) => {
                            info.has_resolved_trait = true;
                            MonomorphizedExpressionKind::Constant(id)
                        }
                        Err(error) => expr_for_error(self, Err(generic_id), error),
                    }
                }
                MonomorphizedExpressionKind::Constant(id) => {
                    MonomorphizedExpressionKind::Constant(id)
                }
                MonomorphizedExpressionKind::Variable(var) => {
                    MonomorphizedExpressionKind::Variable(var)
                }
                MonomorphizedExpressionKind::Text(text) => MonomorphizedExpressionKind::Text(text),
                MonomorphizedExpressionKind::Number(number) => {
                    MonomorphizedExpressionKind::Number(number)
                }
                MonomorphizedExpressionKind::Block(statements, top_level) => {
                    MonomorphizedExpressionKind::Block(
                        statements
                            .into_iter()
                            .map(|expr| self.monomorphize_expr(expr, info))
                            .collect::<Vec<_>>(),
                        top_level,
                    )
                }
                MonomorphizedExpressionKind::Call(func, input, first) => {
                    let input = self.monomorphize_expr(*input, info);
                    let func = self.monomorphize_expr(*func, info);

                    MonomorphizedExpressionKind::Call(Box::new(func), Box::new(input), first)
                }
                MonomorphizedExpressionKind::Function(pattern, body, captures) => {
                    let pattern = match expr.ty {
                        engine::UnresolvedType::Function(input_ty, _) => {
                            let mut input_ty = *input_ty;
                            input_ty.apply(&self.ctx);

                            self.monomorphize_pattern(pattern, input_ty.clone())
                        }
                        _ => self.monomorphize_pattern(pattern, engine::UnresolvedType::Error),
                    };

                    let body = self.monomorphize_expr(*body, info);

                    MonomorphizedExpressionKind::Function(pattern, Box::new(body), captures)
                }
                MonomorphizedExpressionKind::When(input, arms) => {
                    let mut input = self.monomorphize_expr(*input, info);
                    input.ty.apply(&self.ctx);

                    let arms = arms
                        .into_iter()
                        .map(|arm| self.monomorphize_arm(arm, input.ty.clone(), info))
                        .collect();

                    MonomorphizedExpressionKind::When(Box::new(input), arms)
                }
                MonomorphizedExpressionKind::External(lib, identifier, inputs) => {
                    MonomorphizedExpressionKind::External(
                        lib,
                        identifier,
                        inputs
                            .into_iter()
                            .map(|expr| self.monomorphize_expr(expr, info))
                            .collect(),
                    )
                }
                MonomorphizedExpressionKind::Intrinsic(func, inputs) => {
                    MonomorphizedExpressionKind::Intrinsic(
                        func,
                        inputs
                            .into_iter()
                            .map(|expr| self.monomorphize_expr(expr, info))
                            .collect(),
                    )
                }
                MonomorphizedExpressionKind::Plugin(path, name, inputs) => {
                    MonomorphizedExpressionKind::Plugin(
                        path,
                        name,
                        inputs
                            .into_iter()
                            .map(|expr| self.monomorphize_expr(expr, info))
                            .collect(),
                    )
                }
                MonomorphizedExpressionKind::Initialize(pattern, value) => {
                    // Resolve the right-hand side first
                    let mut value = self.monomorphize_expr(*value, info);
                    value.ty.apply(&self.ctx);

                    let pattern = self.monomorphize_pattern(pattern, value.ty.clone());

                    MonomorphizedExpressionKind::Initialize(pattern, Box::new(value))
                }
                MonomorphizedExpressionKind::Structure(fields) => {
                    MonomorphizedExpressionKind::Structure(
                        fields
                            .into_iter()
                            .map(|expr| self.monomorphize_expr(expr, info))
                            .collect(),
                    )
                }
                MonomorphizedExpressionKind::Variant(index, values) => {
                    MonomorphizedExpressionKind::Variant(
                        index,
                        values
                            .into_iter()
                            .map(|expr| self.monomorphize_expr(expr, info))
                            .collect(),
                    )
                }
                MonomorphizedExpressionKind::UnresolvedTrait(tr) => {
                    let instance_id =
                        match self.instance_for_ty(tr, expr.ty.clone(), expr.id, expr.span, info) {
                            Ok(instance) => {
                                info.has_resolved_trait = true;
                                instance
                            }
                            Err(error) => match error {
                                FindInstanceError::TraitNotFound
                                | FindInstanceError::RecursionLimitReached => {
                                    return MonomorphizedExpressionKind::error(&self.compiler);
                                }
                                FindInstanceError::TypeError(error) => {
                                    self.add_error(error);
                                    return MonomorphizedExpressionKind::error(&self.compiler);
                                }
                                FindInstanceError::MultipleCandidates => {
                                    return MonomorphizedExpressionKind::UnresolvedTrait(tr);
                                }
                            },
                        };

                    let monomorphized_id = match instance_id {
                        Some(id) => self.typecheck_constant_expr(
                            true,
                            Some(tr),
                            id,
                            expr.id,
                            expr.span,
                            expr.ty.clone(),
                            info.clone(),
                        ),
                        None => return MonomorphizedExpressionKind::error(&self.compiler),
                    };

                    match monomorphized_id {
                        Ok(id) => MonomorphizedExpressionKind::Constant(id),
                        Err(error) => expr_for_error(self, Ok(tr), error),
                    }
                }
                MonomorphizedExpressionKind::Tuple(exprs) => MonomorphizedExpressionKind::Tuple(
                    exprs
                        .into_iter()
                        .map(|expr| self.monomorphize_expr(expr, info))
                        .collect(),
                ),
                MonomorphizedExpressionKind::Format(segments, trailing_segment) => {
                    MonomorphizedExpressionKind::Format(
                        segments
                            .into_iter()
                            .map(|(text, expr)| (text, self.monomorphize_expr(expr, info)))
                            .collect(),
                        trailing_segment,
                    )
                }
                MonomorphizedExpressionKind::With((id, value), body) => {
                    MonomorphizedExpressionKind::With(
                        (id, Box::new(self.monomorphize_expr(*value, info))),
                        Box::new(self.monomorphize_expr(*body, info)),
                    )
                }
                MonomorphizedExpressionKind::ContextualConstant(id) => {
                    match self.typecheck_constant_expr(
                        false,
                        None,
                        id,
                        expr.id,
                        expr.span,
                        expr.ty.clone(),
                        info.clone(),
                    ) {
                        Ok(_) => MonomorphizedExpressionKind::ContextualConstant(id),
                        Err(error) => expr_for_error(self, Err(id), error),
                    }
                }
                MonomorphizedExpressionKind::End(value) => {
                    MonomorphizedExpressionKind::End(Box::new(self.monomorphize_expr(*value, info)))
                }
            })(),
        }
    }

    fn monomorphize_arm(
        &mut self,
        arm: impl Into<MonomorphizedArm>,
        ty: engine::UnresolvedType,
        info: &mut MonomorphizeInfo,
    ) -> MonomorphizedArm {
        let arm = arm.into();

        MonomorphizedArm {
            span: arm.span,
            pattern: self.monomorphize_pattern(arm.pattern, ty),
            guard: arm.guard.map(|guard| {
                let guard = self.monomorphize_expr(guard, info);

                if let Some(boolean_ty) = self.entrypoint.info.language_items.boolean {
                    if let Err(error) = self.unify(
                        guard.id,
                        guard.span,
                        guard.ty.clone(),
                        engine::UnresolvedType::Named(
                            boolean_ty,
                            Vec::new(),
                            // HACK: Optimization because unification doesn't take structure into
                            // account -- the structure can be applied during finalization
                            engine::TypeStructure::Marker,
                        ),
                    ) {
                        self.add_error(error);
                    }
                } else {
                    self.compiler.add_error(
                        "cannot find `boolean` language item",
                        vec![Note::primary(
                            guard.span,
                            "typechecking this condition requires the `boolean` language item",
                        )],
                    )
                }

                guard
            }),
            body: self.monomorphize_expr(arm.body, info),
        }
    }

    fn monomorphize_pattern(
        &mut self,
        pattern: impl Into<MonomorphizedPattern>,
        mut ty: engine::UnresolvedType,
    ) -> MonomorphizedPattern {
        let pattern = pattern.into();

        ty.apply(&self.ctx);

        let kind = (|| match pattern.kind {
            MonomorphizedPatternKind::Error(trace) => MonomorphizedPatternKind::Error(trace),
            MonomorphizedPatternKind::Number(number) => {
                let numeric_ty =
                    engine::UnresolvedType::NumericVariable(self.ctx.new_variable(None));

                if let Err(error) = self.unify(None, pattern.span, ty, numeric_ty) {
                    self.add_error(error);
                }

                MonomorphizedPatternKind::Number(number)
            }
            MonomorphizedPatternKind::Text(text) => {
                if let Err(error) = self.unify(
                    None,
                    pattern.span,
                    ty,
                    engine::UnresolvedType::Builtin(engine::BuiltinType::Text),
                ) {
                    self.add_error(error);
                }

                MonomorphizedPatternKind::Text(text)
            }
            MonomorphizedPatternKind::Wildcard => MonomorphizedPatternKind::Wildcard,
            MonomorphizedPatternKind::Variable(var) => MonomorphizedPatternKind::Variable(var),
            MonomorphizedPatternKind::UnresolvedDestructure(structure_ty, fields) => {
                if let Err(error) = self.unify(None, pattern.span, ty.clone(), structure_ty.clone())
                {
                    self.add_error(error);
                }

                ty.apply(&self.ctx);

                let (id, params) = match ty.clone() {
                    engine::UnresolvedType::Named(id, params, _) => (id, params),
                    engine::UnresolvedType::Variable(_) => {
                        return MonomorphizedPatternKind::UnresolvedDestructure(
                            structure_ty,
                            fields,
                        );
                    }
                    _ => {
                        self.compiler.add_error(
                            "cannot destructure this value",
                            vec![Note::primary(pattern.span, "value is not a structure")],
                        );

                        return MonomorphizedPatternKind::error(&self.compiler);
                    }
                };

                let structure = self
                    .with_type_decl(id, Clone::clone)
                    .expect("structure should have already been accessed at least once");

                let (structure_field_tys, structure_field_names) = match &structure.kind {
                    TypeDeclKind::Structure {
                        fields,
                        field_names,
                    } => (fields.clone(), field_names.clone()),
                    _ => {
                        self.compiler.add_error(
                            "cannot destructure this value",
                            vec![Note::primary(pattern.span, "value is not a structure")],
                        );

                        return MonomorphizedPatternKind::error(&self.compiler);
                    }
                };

                let substitutions = structure
                    .params
                    .iter()
                    .copied()
                    .zip(params)
                    .collect::<BTreeMap<_, _>>();

                let fields = fields
                    .into_iter()
                    .filter_map(|(name, (pattern, ty))| {
                        let index = match structure_field_names.get(&name) {
                            Some(index) => *index,
                            None => {
                                self.compiler.add_error(
                                    format!("value has no member named '{name}'"),
                                    vec![Note::primary(pattern.span, "no such member")],
                                );

                                return None;
                            }
                        };

                        let mut member_ty = engine::UnresolvedType::from(
                            structure_field_tys[index.into_inner()].1.clone(),
                        );

                        member_ty.instantiate_with(&self.ctx, &substitutions);

                        if let Err(error) = self.unify(None, pattern.span, ty, member_ty.clone()) {
                            self.add_error(error);
                        }

                        let pattern = self.monomorphize_pattern(pattern, member_ty);

                        Some((index, pattern))
                    })
                    .collect();

                MonomorphizedPatternKind::Destructure(id, fields)
            }
            MonomorphizedPatternKind::Destructure(id, fields) => {
                MonomorphizedPatternKind::Destructure(id, fields)
            }
            MonomorphizedPatternKind::UnresolvedVariant(variant_ty, variant, values) => {
                let (id, params) = match &ty {
                    engine::UnresolvedType::Named(id, params, _) => (*id, params),
                    _ => return MonomorphizedPatternKind::error(&self.compiler),
                };

                let enumeration = self
                    .with_type_decl(id, Clone::clone)
                    .expect("enumeration should have already been accessed at least once");

                let mut variant_tys = match enumeration.kind {
                    TypeDeclKind::Enumeration { mut variants, .. } => variants
                        .swap_remove(variant.into_inner())
                        .into_iter()
                        .map(|(_, ty)| engine::UnresolvedType::from(ty))
                        .collect::<Vec<_>>(),
                    _ => {
                        self.compiler.add_error(
                            "cannot match a variant on this value",
                            vec![Note::primary(pattern.span, "value is not an enumeration")],
                        );

                        return MonomorphizedPatternKind::error(&self.compiler);
                    }
                };

                let substitutions = enumeration
                    .params
                    .iter()
                    .copied()
                    .zip(params.iter().cloned())
                    .collect::<BTreeMap<_, _>>();

                for ty in &mut variant_tys {
                    ty.instantiate_with(&self.ctx, &substitutions);
                }

                if let Err(error) = self.unify(
                    None,
                    pattern.span,
                    ty.clone(),
                    engine::UnresolvedType::Named(
                        variant_ty,
                        enumeration
                            .params
                            .iter()
                            .map(|param| substitutions.get(param).unwrap().clone())
                            .collect(),
                        // HACK: Optimization because unification doesn't take structure into
                        // account -- the structure can be applied during finalization
                        engine::TypeStructure::Marker,
                    ),
                ) {
                    self.add_error(error);
                }

                let values = values
                    .into_iter()
                    .zip(variant_tys)
                    .map(|(pattern, variant_ty)| self.monomorphize_pattern(pattern, variant_ty))
                    .collect();

                MonomorphizedPatternKind::Variant(id, variant, values)
            }
            MonomorphizedPatternKind::Variant(id, variant, values) => {
                MonomorphizedPatternKind::Variant(id, variant, values)
            }
            MonomorphizedPatternKind::Or(lhs, rhs) => MonomorphizedPatternKind::Or(
                Box::new(self.monomorphize_pattern(*lhs, ty.clone())),
                Box::new(self.monomorphize_pattern(*rhs, ty)),
            ),
            MonomorphizedPatternKind::Tuple(patterns) => {
                let tys = patterns
                    .iter()
                    .map(|_| engine::UnresolvedType::Variable(self.ctx.new_variable(None)))
                    .collect::<Vec<_>>();

                if let Err(error) = self.unify(
                    None,
                    pattern.span,
                    ty.clone(),
                    engine::UnresolvedType::Tuple(tys.clone()),
                ) {
                    self.add_error(error);
                }

                MonomorphizedPatternKind::Tuple(
                    patterns
                        .into_iter()
                        .zip(tys)
                        .map(|(pattern, ty)| self.monomorphize_pattern(pattern, ty))
                        .collect(),
                )
            }
        })();

        MonomorphizedPattern {
            id: pattern.id,
            span: pattern.span,
            kind,
        }
    }
}

#[derive(Debug)]
enum FindInstanceError {
    RecursionLimitReached,
    TypeError(Error),
    TraitNotFound,
    MultipleCandidates,
}

impl Typechecker {
    fn instance_for_ty(
        &mut self,
        tr: TraitId,
        ty: engine::UnresolvedType,
        use_id: impl Into<Option<ExpressionId>>,
        use_span: SpanList,
        info: &mut MonomorphizeInfo,
    ) -> Result<Option<ConstantId>, FindInstanceError> {
        let use_id = use_id.into();

        let tr_decl = match self.with_trait_decl(tr, Clone::clone) {
            Some(decl) => decl,
            None => return Err(FindInstanceError::TraitNotFound),
        };

        let trait_ty = tr_decl
            .ty
            .clone()
            .expect("`instance_for_ty` may only be used with traits that have values");

        let mut fresh_parameters = tr_decl
            .params
            .iter()
            .map(|&param| {
                (
                    param,
                    engine::UnresolvedType::Parameter(
                        self.compiler
                            .new_type_parameter_id_in(use_span.first().path),
                    ),
                )
            })
            .collect::<engine::GenericSubstitutions>();

        let mut generic_trait_ty = trait_ty.clone().into();
        self.add_substitutions(&mut generic_trait_ty, &mut fresh_parameters);
        let generic_trait_ty = generic_trait_ty.finalize(&self.ctx).unwrap();

        let mut trait_ty = generic_trait_ty.clone().into();
        self.instantiate_generics(&mut trait_ty);

        if let Err(error) = self.unify(use_id, use_span, ty.clone(), trait_ty.clone()) {
            return Err(FindInstanceError::TypeError(error));
        }

        self.instance_for_inner(
            tr,
            tr_decl,
            Ok(ty.clone()),
            use_id,
            use_span,
            None,
            Some((generic_trait_ty, ty)),
            fresh_parameters.into_values().collect(), // NOTE: relies on the parameter IDs being in order
            info,
        )
    }

    fn instance_for_params(
        &mut self,
        tr: TraitId,
        params: Vec<engine::UnresolvedType>,
        use_id: impl Into<Option<ExpressionId>>,
        use_span: SpanList,
        bound: Option<Bound>,
        original_ty: Option<(engine::Type, engine::UnresolvedType)>,
        original_bound_params: Vec<engine::UnresolvedType>,
        info: &mut MonomorphizeInfo,
    ) -> Result<Option<ConstantId>, FindInstanceError> {
        let tr_decl = match self.with_trait_decl(tr, Clone::clone) {
            Some(decl) => decl,
            None => return Err(FindInstanceError::TraitNotFound),
        };

        self.instance_for_inner(
            tr,
            tr_decl,
            Err(params),
            use_id.into(),
            use_span,
            bound,
            original_ty,
            original_bound_params,
            info,
        )
    }

    fn instance_for_inner(
        &mut self,
        tr: TraitId,
        tr_decl: TraitDecl,
        ty_or_params: Result<engine::UnresolvedType, Vec<engine::UnresolvedType>>,
        use_id: Option<ExpressionId>,
        use_span: SpanList,
        bound: Option<Bound>,
        original_ty: Option<(engine::Type, engine::UnresolvedType)>,
        original_bound_params: Vec<engine::UnresolvedType>,
        info: &mut MonomorphizeInfo,
    ) -> Result<Option<ConstantId>, FindInstanceError> {
        let recursion_limit = self
            .entrypoint
            .info
            .recursion_limit
            .unwrap_or(Compiler::DEFAULT_RECURSION_LIMIT);

        if info.recursion_count > recursion_limit {
            self.compiler.add_error(
                "recursion limit reached",
                vec![Note::primary(use_span, "while computing this")],
            );

            return Err(FindInstanceError::RecursionLimitReached);
        }

        type Params = Vec<(engine::UnresolvedType, (TypeParameterId, bool))>;

        let mut params = match ty_or_params.clone() {
            Ok(ty) => self.extract_params(tr, ty),
            Err(params) => params
                .into_iter()
                .map(Into::into)
                .zip(
                    self.with_trait_decl(tr, |decl| {
                        decl.params
                            .iter()
                            .map(|&param| {
                                let inferred = self
                                    .with_type_parameter_decl(param, |param| param.infer)
                                    .unwrap_or(false);

                                (param, inferred)
                            })
                            .collect::<Vec<_>>()
                    })
                    .expect("trait should have already been accessed at least once"),
                )
                .collect::<Params>(),
        };

        for (param, _) in &mut params {
            param.apply(&self.ctx);
        }

        // If a bound refers to itself, assume that the bound is satisfied
        if let Some(id) =
            info.instance_stack
                .entry(tr)
                .or_default()
                .iter()
                .find_map(|(id, stack_params)| {
                    let ctx = self.ctx.clone();
                    let mut error = false;
                    for ((param, _), stack_param) in
                        params.clone().into_iter().zip(stack_params.clone())
                    {
                        if ctx.unify_generic(param, stack_param).is_err() {
                            error = true;
                            break;
                        }
                    }

                    (!error).then_some(*id)
                })
        {
            return Ok(Some(id));
        }

        let mut error_candidates = Vec::new();

        macro_rules! find_instance {
            ($resolve:expr) => {{
                // First try with numeric variables...
                match find_instance!(@find params.clone(), $resolve) {
                    // ...if there is a single candidate, return it.
                    Some(Ok(candidate)) => return Ok(candidate),
                    // ...if there are multiple candiates, try again finalizing numeric variables.
                    Some(Err(FindInstanceError::MultipleCandidates)) => {
                        let params = params
                            .clone()
                            .into_iter()
                            .map(|(mut ty, infer)| {
                                ty.finalize_numeric_variables(&self.ctx);
                                (ty, infer)
                            })
                            .collect::<Vec<_>>();

                        match find_instance!(@find params, $resolve) {
                            Some(result) => return result,
                            None => {}
                        }
                    }
                    Some(Err(error)) => return Err(error),
                    // ...if there are no candidates, continue the search.
                    None => {}
                }
            }};
            (@find $params:expr, $resolve:expr) => {{
                let params = $params;
                let mut candidates = $resolve(params.clone())?
                    .into_iter()
                    .unique_by(|(_, id, _)| *id)
                    .collect::<Vec<_>>();

                match candidates.len() {
                    0 => None,
                    1 => {
                        let (ctx, id, _) = candidates.pop().unwrap();
                        self.ctx = ctx;

                        Some(Ok(id))
                    }
                    _ => Some(Err(FindInstanceError::MultipleCandidates)),
                }
            }};
        }

        macro_rules! unify_instance_params {
            ($candidates:expr, $params:expr, $instance_params:expr, $substitutions:expr, $prev_ctx:expr) => {{
                let mut error = None;
                for ((param_ty, (_, inferred)), instance_param_ty) in
                    $params.clone().into_iter().zip($instance_params.clone())
                {
                    if self
                        .ctx
                        .unify_generic(param_ty.clone(), instance_param_ty.clone())
                        .is_err()
                    {
                        error = Some(error.unwrap_or(inferred) || inferred);
                        break;
                    }
                }

                if let Some(inferred) = error {
                    if inferred {
                        if let Some((original_generic_ty, original_ty)) = original_ty.clone() {
                            let original_bound_params = original_bound_params.clone();

                            let trait_params = self
                                .with_trait_decl(tr, |decl| decl.params.clone())
                                .unwrap();

                            let mut bound_substitutions = trait_params
                                .into_iter()
                                .zip(original_bound_params)
                                .collect::<engine::GenericSubstitutions>();

                            let inferred_params = bound_substitutions
                                .keys()
                                .into_iter()
                                .chain(info.param_substitutions.keys())
                                .copied()
                                .filter(|&param| {
                                    self.with_type_parameter_decl(param, |decl| decl.infer)
                                        .unwrap_or(false)
                                })
                                .collect::<Vec<_>>();

                            let mut param_substitutions = info.param_substitutions.clone();
                            for &param in &inferred_params {
                                param_substitutions
                                    .insert(param, engine::UnresolvedType::Parameter(param));
                            }

                            let mut trait_ty = engine::UnresolvedType::from(
                                self.with_trait_decl(tr, |decl| {
                                    decl.ty.clone().expect("trait must have value")
                                })
                                .unwrap(),
                            );

                            self.add_substitutions(&mut trait_ty, &mut bound_substitutions);
                            self.add_substitutions(&mut trait_ty, &mut param_substitutions);
                            self.add_substitutions(&mut trait_ty, &mut bound_substitutions);

                            let inferred_trait_ty = self.substitute_trait_params(
                                tr,
                                $instance_params.clone(),
                                use_span,
                            );

                            let _ = self.ctx.unify(trait_ty.clone(), inferred_trait_ty);

                            let mut inferred_original_ty = original_generic_ty.clone().into();

                            self.add_substitutions(
                                &mut inferred_original_ty,
                                &mut param_substitutions,
                            );

                            self.add_substitutions(
                                &mut inferred_original_ty,
                                &mut bound_substitutions,
                            );

                            let result = if ty_or_params.is_ok() {
                                self.ctx.unify(inferred_original_ty, original_ty)
                            } else {
                                self.ctx.unify(original_ty, inferred_original_ty)
                            };

                            if let Err(error) = result {
                                if $candidates.is_empty() {
                                    self.ctx = $prev_ctx;

                                    return Err(FindInstanceError::TypeError(
                                        self.error(error, use_id, use_span),
                                    ));
                                }
                            }
                        }
                    }

                    self.ctx = $prev_ctx;
                    continue;
                }
            }};
        }

        let bound_instances = info.bound_instances.get(&tr).cloned().unwrap_or_default();
        find_instance!(|params: Params| {
            let mut candidates = Vec::new();
            for (mut instance_params, instance_id, span, _) in bound_instances.clone() {
                let mut substitutions = engine::GenericSubstitutions::new();
                let prev_ctx = self.ctx.clone();

                for ty in &mut instance_params {
                    self.add_substitutions_skipping_inferred(ty, &mut substitutions);
                }

                unify_instance_params!(
                    candidates,
                    params,
                    instance_params,
                    substitutions,
                    prev_ctx
                );

                let ctx = mem::replace(&mut self.ctx, prev_ctx);
                candidates.push((ctx, instance_id, span));
            }

            Ok(candidates)
        });

        let declared_instances = self.instances.get(&tr).cloned().unwrap_or_default();
        find_instance!(|params: Params| {
            let mut candidates = Vec::new();
            'check: for id in declared_instances.clone() {
                let mut substitutions = engine::GenericSubstitutions::new();
                let prev_ctx = self.ctx.clone();

                let (mut instance_params, instance_span, generic_bounds) = match self
                    .with_instance_decl(id, |instance| {
                        (
                            instance
                                .trait_params
                                .clone()
                                .into_iter()
                                .map(engine::UnresolvedType::from)
                                .collect::<Vec<_>>(),
                            instance.span,
                            instance.bounds.clone(),
                        )
                    }) {
                    Some((instance_params, instance_span, bounds)) => {
                        (instance_params, instance_span, bounds)
                    }
                    None => {
                        self.ctx = prev_ctx;
                        continue 'check;
                    }
                };

                for ty in &mut instance_params {
                    self.add_substitutions_skipping_inferred(ty, &mut substitutions);
                }

                unify_instance_params!(
                    candidates,
                    params,
                    instance_params,
                    substitutions,
                    prev_ctx
                );

                for generic_bound in generic_bounds {
                    info.recursion_count += 1;

                    info.instance_stack
                        .entry(tr)
                        .or_default()
                        .push((id, generic_bound.params.clone()));

                    let mut bound = generic_bound.clone();
                    for ty in &mut bound.params {
                        self.add_substitutions(ty, &mut substitutions);
                    }

                    let result = self.instance_for_params(
                        bound.trait_id,
                        bound.params.clone(),
                        None,
                        bound.span,
                        Some(bound),
                        original_ty.clone(),
                        generic_bound.params,
                        info,
                    );

                    info.instance_stack.entry(tr).or_default().pop();
                    info.recursion_count -= 1;

                    if result.is_err() {
                        self.ctx = prev_ctx;
                        error_candidates.push(instance_span);

                        continue 'check;
                    }
                }

                let ctx = mem::replace(&mut self.ctx, prev_ctx);
                candidates.push((ctx, Some(id), instance_span));

                if tr_decl.attributes.allow_overlapping_instances {
                    break 'check; // use the first available instance
                }
            }

            Ok(candidates)
        });

        let params = params
            .into_iter()
            .map(|(param, _)| param)
            .collect::<Vec<_>>();

        Err(FindInstanceError::TypeError(self.error(
            engine::TypeError::MissingInstance(
                tr,
                params,
                bound.map(|bound| bound.span),
                error_candidates,
            ),
            use_id,
            use_span,
        )))
    }

    fn extract_params(
        &mut self,
        tr: TraitId,
        ty: engine::UnresolvedType,
    ) -> Vec<(engine::UnresolvedType, (TypeParameterId, bool))> {
        let (tr_ty, tr_params) = self
            .with_trait_decl(tr, |decl| {
                (
                    decl.ty
                        .as_ref()
                        .expect("`extract_params` may only be used with traits that have values")
                        .clone(),
                    decl.params.clone(),
                )
            })
            .expect("trait should have already been accessed at least once");

        let params = self.ctx.unify_params(ty, tr_ty);

        tr_params
            .into_iter()
            .map(|param| {
                let inferred = self
                    .with_type_parameter_decl(param, |param| param.infer)
                    .unwrap_or(false);

                let ty = params.get(&param).cloned().unwrap_or_else(|| {
                    engine::UnresolvedType::Variable(self.ctx.new_variable(None))
                });

                (ty, (param, inferred))
            })
            .collect()
    }
}

impl Typechecker {
    fn finalize_expr(&mut self, expr: MonomorphizedExpression) -> Expression {
        let ty = expr.ty.finalize(&self.ctx).unwrap_or_else(|| {
            self.add_error(self.error(
                engine::TypeError::UnresolvedType(expr.ty.clone()),
                expr.id,
                expr.span,
            ));

            engine::Type::Error
        });

        let kind = (|| match expr.kind {
            MonomorphizedExpressionKind::Error(trace) => ExpressionKind::Error(trace),
            MonomorphizedExpressionKind::Marker => ExpressionKind::Marker,
            MonomorphizedExpressionKind::UnresolvedTrait(_)
            | MonomorphizedExpressionKind::UnresolvedConstant(_) => {
                self.add_error(self.error(
                    engine::TypeError::UnresolvedType(expr.ty),
                    expr.id,
                    expr.span,
                ));

                ExpressionKind::error(&self.compiler)
            }
            MonomorphizedExpressionKind::Constant(id) => ExpressionKind::Constant(id),
            MonomorphizedExpressionKind::Variable(var) => ExpressionKind::Variable(var),
            MonomorphizedExpressionKind::Text(text) => ExpressionKind::Text(text),
            MonomorphizedExpressionKind::Number(number) => {
                match parse_number!(number, ExpressionKind, &ty, Type) {
                    Some(Ok(number)) => number,
                    Some(Err(error)) => {
                        self.add_error(self.error(error, expr.id, expr.span));
                        ExpressionKind::error(&self.compiler)
                    }
                    None => {
                        self.add_error(self.error(
                            engine::TypeError::Mismatch(
                                engine::UnresolvedType::Builtin(engine::BuiltinType::Number),
                                expr.ty,
                            ),
                            expr.id,
                            expr.span,
                        ));

                        ExpressionKind::error(&self.compiler)
                    }
                }
            }
            MonomorphizedExpressionKind::Block(statements, top_level) => ExpressionKind::Block(
                statements
                    .into_iter()
                    .map(|expr| self.finalize_expr(expr))
                    .collect(),
                top_level,
            ),
            MonomorphizedExpressionKind::Call(func, input, first) => ExpressionKind::Call(
                Box::new(self.finalize_expr(*func)),
                Box::new(self.finalize_expr(*input)),
                first,
            ),
            MonomorphizedExpressionKind::Function(pattern, body, captures) => {
                let input_ty = match &ty {
                    engine::Type::Function(input, _) => input.clone(),
                    _ => return ExpressionKind::error(&self.compiler),
                };

                ExpressionKind::Function(
                    self.finalize_pattern(pattern, &input_ty),
                    Box::new(self.finalize_expr(*body)),
                    captures,
                )
            }
            MonomorphizedExpressionKind::When(input, arms) => {
                let input = self.finalize_expr(*input);

                let arms = arms
                    .into_iter()
                    .map(|arm| Arm {
                        span: arm.span,
                        pattern: self.finalize_pattern(arm.pattern, &input.ty),
                        guard: arm.guard.map(|expr| self.finalize_expr(expr)),
                        body: self.finalize_expr(arm.body),
                    })
                    .collect::<Vec<_>>();

                ExpressionKind::When(Box::new(input), arms)
            }
            MonomorphizedExpressionKind::External(lib, identifier, inputs) => {
                ExpressionKind::External(
                    lib,
                    identifier,
                    inputs
                        .into_iter()
                        .map(|expr| self.finalize_expr(expr))
                        .collect(),
                )
            }
            MonomorphizedExpressionKind::Intrinsic(func, inputs) => ExpressionKind::Intrinsic(
                func,
                inputs
                    .into_iter()
                    .map(|expr| self.finalize_expr(expr))
                    .collect(),
            ),
            MonomorphizedExpressionKind::Plugin(path, name, inputs) => ExpressionKind::Plugin(
                path,
                name,
                inputs
                    .into_iter()
                    .map(|expr| self.finalize_expr(expr))
                    .collect(),
            ),
            MonomorphizedExpressionKind::Initialize(pattern, value) => {
                let value = self.finalize_expr(*value);

                ExpressionKind::Initialize(
                    self.finalize_pattern(pattern, &value.ty),
                    Box::new(value),
                )
            }
            MonomorphizedExpressionKind::Structure(fields) => ExpressionKind::Structure(
                fields
                    .into_iter()
                    .map(|expr| self.finalize_expr(expr))
                    .collect(),
            ),
            MonomorphizedExpressionKind::Variant(index, values) => ExpressionKind::Variant(
                index,
                values
                    .into_iter()
                    .map(|expr| self.finalize_expr(expr))
                    .collect(),
            ),
            MonomorphizedExpressionKind::Tuple(exprs) => ExpressionKind::Tuple(
                exprs
                    .into_iter()
                    .map(|expr| self.finalize_expr(expr))
                    .collect(),
            ),
            MonomorphizedExpressionKind::Format(segments, trailing_segment) => {
                ExpressionKind::Format(
                    segments
                        .into_iter()
                        .map(|(text, expr)| (text, self.finalize_expr(expr)))
                        .collect(),
                    trailing_segment,
                )
            }
            MonomorphizedExpressionKind::With((id, value), body) => ExpressionKind::With(
                (id, Box::new(self.finalize_expr(*value))),
                Box::new(self.finalize_expr(*body)),
            ),
            MonomorphizedExpressionKind::ContextualConstant(id) => {
                ExpressionKind::ContextualConstant(id)
            }
            MonomorphizedExpressionKind::End(value) => {
                ExpressionKind::End(Box::new(self.finalize_expr(*value)))
            }
        })();

        Expression {
            id: expr.id,
            span: expr.span,
            ty,
            kind,
        }
    }

    fn finalize_pattern(
        &mut self,
        pattern: MonomorphizedPattern,
        input_ty: &engine::Type,
    ) -> Pattern {
        Pattern {
            id: pattern.id,
            span: pattern.span,
            kind: (|| match pattern.kind {
                MonomorphizedPatternKind::Error(trace) => PatternKind::Error(trace),
                MonomorphizedPatternKind::Wildcard => PatternKind::Wildcard,
                MonomorphizedPatternKind::Number(number) => {
                    match parse_number!(number, PatternKind, input_ty, Type) {
                        Some(Ok(number)) => number,
                        Some(Err(error)) => {
                            self.add_error(self.error(error, None, pattern.span));
                            PatternKind::error(&self.compiler)
                        }
                        None => {
                            self.add_error(self.error(
                                engine::TypeError::Mismatch(
                                    engine::UnresolvedType::Builtin(engine::BuiltinType::Number),
                                    input_ty.clone().into(),
                                ),
                                None,
                                pattern.span,
                            ));

                            PatternKind::error(&self.compiler)
                        }
                    }
                }
                MonomorphizedPatternKind::Text(text) => PatternKind::Text(text),
                MonomorphizedPatternKind::Variable(var) => {
                    self.with_variable_decl(var, input_ty.clone(), |_| {});
                    PatternKind::Variable(var)
                }
                MonomorphizedPatternKind::UnresolvedDestructure(_, _) => unreachable!(),
                MonomorphizedPatternKind::Destructure(id, fields) => {
                    let input_tys = match input_ty {
                        engine::Type::Named(_, _, engine::TypeStructure::Structure(fields)) => {
                            fields.clone()
                        }
                        engine::Type::Named(_, _, engine::TypeStructure::Recursive(id)) => {
                            match self
                                .with_type_decl(*id, |decl| match &decl.kind {
                                    TypeDeclKind::Structure { fields, .. } => {
                                        Some(fields.iter().map(|(_, ty)| ty.clone()).collect())
                                    }
                                    _ => None,
                                })
                                .unwrap()
                            {
                                Some(fields) => fields,
                                None => return PatternKind::error(&self.compiler),
                            }
                        }
                        _ => return PatternKind::error(&self.compiler),
                    };

                    PatternKind::Destructure(
                        id,
                        fields
                            .into_iter()
                            .zip(input_tys)
                            .map(|((index, field), ty)| (index, self.finalize_pattern(field, &ty)))
                            .collect(),
                    )
                }
                MonomorphizedPatternKind::UnresolvedVariant(_, _, _) => unreachable!(),
                MonomorphizedPatternKind::Variant(id, index, values) => {
                    let input_tys = match input_ty {
                        engine::Type::Named(_, _, engine::TypeStructure::Enumeration(variants)) => {
                            variants[index.into_inner()].clone()
                        }
                        engine::Type::Named(_, _, engine::TypeStructure::Recursive(id)) => {
                            match self
                                .with_type_decl(*id, |decl| match &decl.kind {
                                    TypeDeclKind::Enumeration { variants, .. } => Some(
                                        variants[index.into_inner()]
                                            .iter()
                                            .map(|(_, ty)| ty.clone())
                                            .collect(),
                                    ),
                                    _ => None,
                                })
                                .unwrap()
                            {
                                Some(fields) => fields,
                                None => return PatternKind::error(&self.compiler),
                            }
                        }
                        _ => return PatternKind::error(&self.compiler),
                    };

                    PatternKind::Variant(
                        id,
                        index,
                        values
                            .into_iter()
                            .zip(input_tys)
                            .map(|(value, ty)| self.finalize_pattern(value, &ty))
                            .collect(),
                    )
                }
                MonomorphizedPatternKind::Or(lhs, rhs) => PatternKind::Or(
                    Box::new(self.finalize_pattern(*lhs, input_ty)),
                    Box::new(self.finalize_pattern(*rhs, input_ty)),
                ),
                MonomorphizedPatternKind::Tuple(patterns) => {
                    let input_tys = match input_ty {
                        engine::Type::Tuple(tys) => tys,
                        _ => return PatternKind::error(&self.compiler),
                    };

                    PatternKind::Tuple(
                        patterns
                            .into_iter()
                            .zip(input_tys)
                            .map(|(pattern, ty)| self.finalize_pattern(pattern, ty))
                            .collect(),
                    )
                }
            })(),
        }
    }

    async fn resolve_plugins(
        &mut self,
        expr: Expression,
        constant_id: Option<ConstantId>,
        mut info: MonomorphizeInfo,
    ) -> Expression {
        let mut final_expr = expr;

        loop {
            let mut resolved_plugin = false;

            if let Some(id) = final_expr.find_first_unresolved_plugin() {
                resolved_plugin = true;

                // Find the plugin call
                let expr = final_expr.as_root_query(id).unwrap().clone();
                let ty = expr.ty;
                let (path, name, inputs) = match expr.kind {
                    ExpressionKind::Plugin(path, name, inputs) => (path, name, inputs),
                    _ => unreachable!(),
                };

                // Run the plugin
                let expr = self
                    .resolve_plugin(path, name, expr.id, expr.span, ty.clone(), inputs)
                    .await;

                // Convert it to a typecheckable expression
                let mut convert_info = ConvertInfo::new(constant_id);
                let expr = self.convert_expr(expr, &mut convert_info);

                // Make sure the type unifies with the type of the original expression
                if let Err(error) = self.unify(expr.id, expr.span, expr.ty.clone(), ty) {
                    self.add_error(error);
                }

                let expr = self.repeatedly_monomorphize_expr(expr, &mut info);
                let expr = self.finalize_expr(expr);

                let replaced = final_expr.as_root_replace(id, expr);
                assert!(replaced);
            }

            if !resolved_plugin {
                break;
            }
        }

        final_expr
    }
}

impl Typechecker {
    fn with_syntax_decl<T>(&self, id: SyntaxId, f: impl FnOnce(&SyntaxDecl) -> T) -> Option<T> {
        if let Some(decl) = self.declarations.borrow().syntaxes.get(&id) {
            return Some(f(decl));
        }

        let decl = self.entrypoint.declarations.syntaxes.get(&id)?.clone();

        let decl = SyntaxDecl {
            name: decl
                .name
                .unwrap_or_else(|| InternedString::new("<unknown>")),
            span: decl.span,
            operator: decl.value.operator,
            keyword: decl.value.keyword,
            uses: decl.uses,
            attributes: decl.value.attributes,
        };

        Some(f(self
            .declarations
            .borrow_mut()
            .syntaxes
            .entry(id)
            .or_insert(decl)))
    }

    fn with_type_decl<T>(&self, id: TypeId, f: impl FnOnce(&TypeDecl) -> T) -> Option<T> {
        if let Some(decl) = self.declarations.borrow().types.get(&id) {
            return Some(f(decl));
        }

        let decl = self.entrypoint.declarations.types.get(&id)?.clone();

        let decl = TypeDecl {
            name: decl
                .name
                .unwrap_or_else(|| InternedString::new("<unknown>")),
            span: decl.span,
            params: decl.value.parameters,
            kind: match decl.value.kind {
                lower::TypeDeclarationKind::Marker => TypeDeclKind::Marker,
                lower::TypeDeclarationKind::Structure(fields, field_names) => {
                    TypeDeclKind::Structure {
                        fields: fields
                            .into_iter()
                            .map(|field| {
                                (
                                    field.ty.clone(),
                                    self.convert_finalized_type_annotation(field.ty),
                                )
                            })
                            .collect(),
                        field_names,
                    }
                }
                lower::TypeDeclarationKind::Enumeration(variants, variant_names) => {
                    TypeDeclKind::Enumeration {
                        variants: variants
                            .into_iter()
                            .map(|variant| {
                                variant
                                    .tys
                                    .into_iter()
                                    .map(|ty| {
                                        (ty.clone(), self.convert_finalized_type_annotation(ty))
                                    })
                                    .collect()
                            })
                            .collect(),
                        variant_names,
                    }
                }
                lower::TypeDeclarationKind::Alias(ty) => {
                    TypeDeclKind::Alias(self.convert_finalized_type_annotation(ty))
                }
            },
            convert_from: decl
                .value
                .attributes
                .convert_from
                .iter()
                .cloned()
                .map(|(annotation, replacement)| {
                    (
                        self.convert_finalized_type_annotation(annotation),
                        replacement,
                    )
                })
                .collect(),
            attributes: decl.value.attributes,
            uses: decl.uses,
        };

        Some(f(self
            .declarations
            .borrow_mut()
            .types
            .entry(id)
            .or_insert(decl)))
    }

    fn with_trait_decl<T>(&self, id: TraitId, f: impl FnOnce(&TraitDecl) -> T) -> Option<T> {
        if let Some(decl) = self.declarations.borrow().traits.get(&id) {
            return Some(f(decl));
        }

        let decl = self.entrypoint.declarations.traits.get(&id)?.clone();

        let decl = TraitDecl {
            name: decl
                .name
                .unwrap_or_else(|| InternedString::new("<unknown>")),
            span: decl.span,
            params: decl.value.parameters,
            ty_annotation: decl.value.ty.clone(),
            ty: decl
                .value
                .ty
                .map(|ty| self.convert_finalized_type_annotation(ty)),
            attributes: decl.value.attributes,
            uses: decl.uses,
        };

        Some(f(self
            .declarations
            .borrow_mut()
            .traits
            .entry(id)
            .or_insert(decl)))
    }

    fn with_constant_decl<T>(
        &mut self,
        id: ConstantId,
        f: impl FnOnce(&ConstantDecl) -> T,
    ) -> Option<T> {
        if let Some(decl) = self.declarations.borrow_mut().constants.get(&id) {
            return Some(f(decl));
        }

        let decl = self.entrypoint.declarations.constants.get(&id)?.clone();

        self.generic_constants.insert(id, (false, decl.value.value));

        let ty = self.convert_generic_type_annotation(decl.value.ty.clone());

        let bounds = decl
            .value
            .bounds
            .clone()
            .into_iter()
            .map(|bound| Bound {
                span: bound.span,
                trait_id: bound.tr,
                params: bound
                    .parameters
                    .into_iter()
                    .map(|ty| self.convert_generic_type_annotation(ty).into())
                    .collect(),
            })
            .collect::<Vec<_>>();

        let decl = ConstantDecl {
            name: decl
                .name
                .unwrap_or_else(|| InternedString::new("<unknown>")),
            span: decl.span,
            params: decl.value.parameters,
            bounds,
            bound_annotations: decl
                .value
                .bounds
                .into_iter()
                .map(|bound| (bound.tr, bound.parameters))
                .collect(),
            ty_annotation: decl.value.ty,
            ty,
            reduced_ty: None,
            specializations: self
                .entrypoint
                .specializations
                .get(&id)
                .cloned()
                .unwrap_or_default(),
            is_variant: decl.value.is_variant,
            attributes: decl.value.attributes,
            body: None,
            uses: decl.uses,
        };

        Some(f(self
            .declarations
            .borrow_mut()
            .constants
            .entry(id)
            .or_insert(decl)))
    }

    fn with_instance_decl<T>(
        &mut self,
        id: ConstantId,
        f: impl FnOnce(&InstanceDecl) -> T,
    ) -> Option<T> {
        let decl = self.entrypoint.declarations.instances.get(&id)?.clone();

        let trait_id = decl.value.tr;

        if let Some(decl) = self
            .declarations
            .borrow_mut()
            .instances
            .entry(trait_id)
            .or_default()
            .get(&id)
        {
            return Some(f(decl));
        }

        let tr = self.with_trait_decl(trait_id, Clone::clone)?;

        if tr.ty.is_some() {
            self.generic_constants.insert(
                id,
                (
                    true,
                    decl.value
                        .value
                        .map(|instance_value| instance_value.value)
                        .unwrap_or_else(|| lower::Expression {
                            id: self.compiler.new_expression_id(id),
                            span: decl.span,
                            kind: lower::ExpressionKind::error(&self.compiler),
                        }),
                ),
            );
        }

        let mut params = decl
            .value
            .tr_parameters
            .clone()
            .into_iter()
            .map(|ty| self.convert_generic_type_annotation(ty))
            .zip(tr.params.iter().map(|&param| {
                self.with_type_parameter_decl(param, |param| param.infer.then_some(param.span))
                    .flatten()
            }))
            .collect::<Vec<_>>();

        for param in tr.params.iter().skip(params.len()) {
            let name = match self.with_type_parameter_decl(*param, |decl| decl.name) {
                Some(name) => name,
                None => {
                    params.push((engine::Type::Error, None));
                    continue;
                }
            };

            self.compiler.add_error(
                format!(
                    "missing type for trait parameter `{}`",
                    name.as_deref().unwrap_or("_")
                ),
                vec![Note::primary(
                    decl.span,
                    "try adding another type after the trait provided to `instance`",
                )],
            );

            params.push((engine::Type::Error, None));
        }

        let has_bounds = !decl.value.bounds.is_empty();

        let bounds = decl
            .value
            .bounds
            .clone()
            .into_iter()
            .map(|bound| Bound {
                span: bound.span,
                trait_id: bound.tr,
                params: bound
                    .parameters
                    .into_iter()
                    .map(|ty| self.convert_generic_type_annotation(ty).into())
                    .collect(),
            })
            .collect::<Vec<_>>();

        let item = self.compiler.new_item_id();

        if !tr.attributes.allow_overlapping_instances {
            // Check if the instance collides with any other instances -- there's no
            // need to check the bounds because there's no way to ensure a type
            // doesn't satisfy the bounds specified in both instances

            let other_instances = self
                .declarations
                .borrow_mut()
                .instances
                .entry(trait_id)
                .or_default()
                .clone();

            let colliding_instances = other_instances
                .into_values()
                .filter_map(|other| {
                    let prev_ctx = self.ctx.clone();

                    let mut substitutions = engine::GenericSubstitutions::new();

                    let mut all_unify = true;
                    for ((instance_param_ty, infer_span), other_param_ty) in
                        params.clone().into_iter().zip(other.trait_params)
                    {
                        if infer_span.is_some() {
                            continue;
                        }

                        let mut instance_param_ty = engine::UnresolvedType::from(instance_param_ty);
                        self.add_substitutions(&mut instance_param_ty, &mut substitutions);

                        let mut other_param_ty = engine::UnresolvedType::from(other_param_ty);
                        self.add_substitutions(&mut other_param_ty, &mut substitutions);

                        if self.ctx.unify(instance_param_ty, other_param_ty).is_err() {
                            all_unify = false;
                            break;
                        }
                    }

                    self.ctx = prev_ctx;
                    all_unify.then_some(other.span)
                })
                .collect::<Vec<_>>();

            if !colliding_instances.is_empty() {
                self.compiler.add_error(
                    format!(
                        "this instance collides with {} other instances",
                        colliding_instances.len()
                    ), std::iter::once(Note::primary(
                        decl.span,
                        if has_bounds {
                            "this instance may have different bounds than the others, but one type could satisfy the bounds on more than one of these instances simultaneously"
                        } else {
                            "try making this instance more specific"
                        },
                    ))
                    .chain(params.iter().filter_map(|(_, infer_span)| *infer_span).map(|infer_span| {
                        Note::secondary(infer_span, "this type parameter is inferred and cannot be different across instances with otherwise the same types")
                    }))
                    .chain(colliding_instances.into_iter().map(|span| {
                        Note::secondary(span, "this instance could apply to the same type(s)")
                    }))
                    .collect(),
                );
            }
        }

        let decl = InstanceDecl {
            span: decl.span,
            params: decl.value.parameters,
            bounds,
            bound_annotations: decl
                .value
                .bounds
                .into_iter()
                .map(|bound| (bound.tr, bound.parameters))
                .collect(),
            trait_id,
            trait_params: params.into_iter().map(|(param, _)| param).collect(),
            trait_param_annotations: decl.value.tr_parameters,
            body: None,
            item,
        };

        Some(f(self
            .declarations
            .borrow_mut()
            .instances
            .entry(trait_id)
            .or_default()
            .entry(id)
            .or_insert(decl)))
    }

    fn with_builtin_type_decl<T>(
        &self,
        id: BuiltinTypeId,
        f: impl FnOnce(&BuiltinTypeDecl) -> T,
    ) -> Option<T> {
        if let Some(decl) = self.declarations.borrow().builtin_types.get(&id) {
            return Some(f(decl));
        }

        let decl = self.entrypoint.declarations.builtin_types.get(&id)?.clone();

        let decl = BuiltinTypeDecl {
            name: decl
                .name
                .unwrap_or_else(|| InternedString::new("<unknown>")),
            span: decl.span,
            attributes: decl.value.attributes,
            uses: decl.uses,
        };

        Some(f(self
            .declarations
            .borrow_mut()
            .builtin_types
            .entry(id)
            .or_insert(decl)))
    }

    fn with_type_parameter_decl<T>(
        &self,
        id: TypeParameterId,
        f: impl FnOnce(&TypeParameterDecl) -> T,
    ) -> Option<T> {
        if let Some(decl) = self.declarations.borrow().type_parameters.get(&id) {
            return Some(f(decl));
        }

        let decl = self
            .entrypoint
            .declarations
            .type_parameters
            .get(&id)?
            .clone();

        let decl = TypeParameterDecl {
            name: decl.name,
            span: decl.span,
            default: decl
                .value
                .default
                .map(|ty| self.convert_generic_type_annotation(ty)),
            infer: decl.value.infer,
            uses: decl.uses,
        };

        Some(f(self
            .declarations
            .borrow_mut()
            .type_parameters
            .entry(id)
            .or_insert(decl)))
    }

    fn with_variable_decl<T>(
        &self,
        id: VariableId,
        ty: engine::Type,
        f: impl FnOnce(&VariableDecl) -> T,
    ) -> Option<T> {
        if let Some(decl) = self.declarations.borrow().variables.get(&id) {
            return Some(f(decl));
        }

        let decl = self.entrypoint.declarations.variables.get(&id)?.clone();

        let decl = VariableDecl {
            name: decl.name,
            span: decl.span,
            ty,
            uses: decl.uses,
        };

        Some(f(self
            .declarations
            .borrow_mut()
            .variables
            .entry(id)
            .or_insert(decl)))
    }

    fn with_builtin_syntax_decl<T>(
        &self,
        id: BuiltinSyntaxId,
        f: impl FnOnce(&BuiltinSyntaxDecl) -> T,
    ) -> Option<T> {
        if let Some(decl) = self.declarations.borrow().builtin_syntaxes.get(&id) {
            return Some(f(decl));
        }

        let decl = self
            .entrypoint
            .declarations
            .builtin_syntaxes
            .get(&id)?
            .clone();

        let decl = BuiltinSyntaxDecl {
            name: decl
                .name
                .unwrap_or_else(|| InternedString::new("<unknown>")),
            span: decl.span,
            definition: decl.value.definition,
            uses: decl.uses,
        };

        Some(f(self
            .declarations
            .borrow_mut()
            .builtin_syntaxes
            .entry(id)
            .or_insert(decl)))
    }
}

impl Typechecker {
    fn unify(
        &mut self,
        id: impl Into<Option<ExpressionId>>,
        span: impl Into<SpanList>,
        actual: engine::UnresolvedType,
        expected: impl Into<engine::UnresolvedType>,
    ) -> Result<(), Error> {
        self.ctx
            .unify(actual, expected)
            .map_err(|e| self.error(e, id, span.into()))
    }

    fn add_substitutions(
        &self,
        ty: &mut engine::UnresolvedType,
        substitutions: &mut engine::GenericSubstitutions,
    ) {
        ty.apply(&self.ctx);

        for param in ty.params() {
            substitutions.entry(param).or_insert_with(|| {
                let default = self.get_default_for_param(param);
                engine::UnresolvedType::Variable(self.ctx.new_variable(default))
            });
        }

        ty.instantiate_with(&self.ctx, substitutions);
    }

    fn add_substitutions_skipping_inferred(
        &self,
        ty: &mut engine::UnresolvedType,
        substitutions: &mut engine::GenericSubstitutions,
    ) {
        ty.apply(&self.ctx);

        for param in ty.params() {
            substitutions.entry(param).or_insert_with(|| {
                let inferred = self
                    .with_type_parameter_decl(param, |decl| decl.infer)
                    .unwrap_or(false);

                if inferred {
                    engine::UnresolvedType::Parameter(param)
                } else {
                    let default = self.get_default_for_param(param);
                    engine::UnresolvedType::Variable(self.ctx.new_variable(default))
                }
            });
        }

        ty.instantiate_with(&self.ctx, substitutions);
    }

    fn instantiate_generics(&mut self, ty: &mut engine::UnresolvedType) {
        self.add_substitutions(ty, &mut GenericSubstitutions::new());
    }

    fn convert_type_annotation(&self, annotation: TypeAnnotation) -> engine::UnresolvedType {
        self.convert_type_annotation_inner(
            annotation,
            &|typechecker, _| {
                Some(engine::UnresolvedType::Variable(
                    typechecker.ctx.new_variable(None),
                ))
            },
            &mut Vec::new(),
        )
    }

    fn convert_generic_type_annotation(&self, annotation: TypeAnnotation) -> engine::Type {
        let span = annotation.span;

        let ty = self.convert_type_annotation_inner(
            annotation,
            &|typechecker, span| {
                let param = typechecker.compiler.new_type_parameter_id();

                typechecker
                    .declarations
                    .borrow_mut()
                    .type_parameters
                    .insert(
                        param,
                        TypeParameterDecl {
                            name: None,
                            span,
                            default: None,
                            infer: false,
                            uses: HashSet::from([span]),
                        },
                    );

                Some(engine::UnresolvedType::Parameter(param))
            },
            &mut Vec::new(),
        );

        ty.finalize(&self.ctx).unwrap_or_else(|| {
            self.add_error(self.error(engine::TypeError::UnresolvedType(ty), None, span));
            engine::Type::Error
        })
    }

    fn convert_finalized_type_annotation(&self, annotation: TypeAnnotation) -> engine::Type {
        let span = annotation.span;

        let ty = self.convert_type_annotation_inner(annotation, &|_, _| None, &mut Vec::new());

        ty.finalize(&self.ctx).unwrap_or_else(|| {
            self.add_error(self.error(engine::TypeError::UnresolvedType(ty), None, span));
            engine::Type::Error
        })
    }

    fn convert_type_annotation_inner(
        &self,
        annotation: TypeAnnotation,
        convert_placeholder: &impl Fn(&Self, SpanList) -> Option<engine::UnresolvedType>,
        stack: &mut Vec<TypeId>,
    ) -> engine::UnresolvedType {
        match annotation.kind {
            TypeAnnotationKind::Error(_) => engine::UnresolvedType::Error,
            TypeAnnotationKind::Placeholder => {
                if let Some(ty) = convert_placeholder(self, annotation.span) {
                    ty
                } else {
                    self.compiler.add_error(
                        "type placeholder is not allowed here",
                        vec![Note::primary(
                            annotation.span,
                            "try providing an actual type in place of `_`",
                        )],
                    );

                    engine::UnresolvedType::Error
                }
            }
            TypeAnnotationKind::Named(id, params) => {
                let mut params = params
                    .into_iter()
                    .map(|param| {
                        self.convert_type_annotation_inner(param, convert_placeholder, stack)
                    })
                    .collect::<Vec<_>>();

                let ty = self.entrypoint.declarations.types.get(&id).unwrap().clone();

                for param in ty.value.parameters.iter().skip(params.len()) {
                    let name = match self.with_type_parameter_decl(*param, |decl| decl.name) {
                        Some(name) => name,
                        None => {
                            params.push(engine::UnresolvedType::Error);
                            continue;
                        }
                    };

                    self.compiler.add_error(
                        format!(
                            "missing type for type parameter `{}`",
                            name.as_deref().unwrap_or("<unknown>")
                        ),
                        vec![Note::primary(
                            annotation.span,
                            "try adding another type after this",
                        )],
                    );

                    params.push(engine::UnresolvedType::Error);
                }

                if stack.contains(&id) {
                    return engine::UnresolvedType::Named(
                        id,
                        params,
                        engine::TypeStructure::Recursive(id),
                    );
                }

                stack.push(id);

                let substitutions = ty
                    .value
                    .parameters
                    .iter()
                    .copied()
                    .zip(params.iter().cloned())
                    .collect::<engine::GenericSubstitutions>();

                macro_rules! convert_and_instantiate {
                    ($ty:expr) => {{
                        let mut ty =
                            self.convert_type_annotation_inner($ty, convert_placeholder, stack);

                        ty.instantiate_with(&self.ctx, &substitutions);

                        ty
                    }};
                }

                let structure = match &ty.value.kind {
                    lower::TypeDeclarationKind::Marker => engine::TypeStructure::Marker,
                    lower::TypeDeclarationKind::Structure(fields, _) => {
                        engine::TypeStructure::Structure(
                            fields
                                .iter()
                                .map(|field| convert_and_instantiate!(field.ty.clone()))
                                .collect(),
                        )
                    }
                    lower::TypeDeclarationKind::Enumeration(variants, _) => {
                        engine::TypeStructure::Enumeration(
                            variants
                                .iter()
                                .map(|variant| {
                                    variant
                                        .tys
                                        .iter()
                                        .map(|ty| convert_and_instantiate!(ty.clone()))
                                        .collect()
                                })
                                .collect(),
                        )
                    }
                    lower::TypeDeclarationKind::Alias(ty) => {
                        stack.pop();

                        return convert_and_instantiate!(ty.clone());
                    }
                };

                stack.pop();

                engine::UnresolvedType::Named(id, params, structure)
            }
            TypeAnnotationKind::Parameter(id) => engine::UnresolvedType::Parameter(id),
            TypeAnnotationKind::Builtin(id, mut parameters) => {
                let builtin_ty = self
                    .entrypoint
                    .declarations
                    .builtin_types
                    .get(&id)
                    .unwrap()
                    .clone();

                match builtin_ty.value.kind {
                    lower::BuiltinTypeDeclarationKind::Number => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                "`Number` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            );
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Number)
                    }
                    lower::BuiltinTypeDeclarationKind::Integer => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                "`Integer` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            );
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Integer)
                    }
                    lower::BuiltinTypeDeclarationKind::Natural => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                "`Natural` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            );
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Natural)
                    }
                    lower::BuiltinTypeDeclarationKind::Byte => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                "`Byte` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            );
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Byte)
                    }
                    lower::BuiltinTypeDeclarationKind::Signed => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                "`Signed` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            );
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Signed)
                    }
                    lower::BuiltinTypeDeclarationKind::Unsigned => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                "`Unsigned` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            );
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Unsigned)
                    }
                    lower::BuiltinTypeDeclarationKind::Float => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                "`Float` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            );
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Float)
                    }
                    lower::BuiltinTypeDeclarationKind::Double => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                "`Double` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            );
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Double)
                    }
                    lower::BuiltinTypeDeclarationKind::Text => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                "`Text` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            );
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Text)
                    }
                    lower::BuiltinTypeDeclarationKind::List => {
                        if parameters.is_empty() {
                            self.compiler.add_error(
                                "`List` accepts 1 parameter, but none were provided",
                                vec![Note::primary(
                                    annotation.span,
                                    "try adding `_` here to infer the type of `Element`",
                                )],
                            );

                            engine::UnresolvedType::Builtin(engine::BuiltinType::List(Box::new(
                                engine::UnresolvedType::Error,
                            )))
                        } else {
                            if parameters.len() > 1 {
                                self.compiler.add_error(
                                    format!(
                                        "`List` accepts 1 parameter, but {} were provided",
                                        parameters.len()
                                    ),
                                    vec![Note::primary(
                                        annotation.span,
                                        "try removing some of these",
                                    )],
                                );
                            }

                            engine::UnresolvedType::Builtin(engine::BuiltinType::List(Box::new(
                                self.convert_type_annotation_inner(
                                    parameters.pop().unwrap(),
                                    convert_placeholder,
                                    stack,
                                ),
                            )))
                        }
                    }
                    lower::BuiltinTypeDeclarationKind::Mutable => {
                        if parameters.is_empty() {
                            self.compiler.add_error(
                                "`Mutable` accepts 1 parameter, but none were provided",
                                vec![Note::primary(
                                    annotation.span,
                                    "try adding `_` here to infer the type of `Value`",
                                )],
                            );

                            engine::UnresolvedType::Builtin(engine::BuiltinType::Mutable(Box::new(
                                engine::UnresolvedType::Error,
                            )))
                        } else {
                            if parameters.len() > 1 {
                                self.compiler.add_error(
                                    format!(
                                        "`Mutable` accepts 1 parameter, but {} were provided",
                                        parameters.len()
                                    ),
                                    vec![Note::primary(
                                        annotation.span,
                                        "try removing some of these",
                                    )],
                                );
                            }

                            engine::UnresolvedType::Builtin(engine::BuiltinType::Mutable(Box::new(
                                self.convert_type_annotation_inner(
                                    parameters.pop().unwrap(),
                                    convert_placeholder,
                                    stack,
                                ),
                            )))
                        }
                    }
                    lower::BuiltinTypeDeclarationKind::Ui => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                "`UI` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            );
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Ui)
                    }
                    lower::BuiltinTypeDeclarationKind::TaskGroup => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                "`Task-Group` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            );
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::TaskGroup)
                    }
                }
            }
            TypeAnnotationKind::Function(input, output) => engine::UnresolvedType::Function(
                Box::new(self.convert_type_annotation_inner(*input, convert_placeholder, stack)),
                Box::new(self.convert_type_annotation_inner(*output, convert_placeholder, stack)),
            ),
            TypeAnnotationKind::Tuple(tys) => engine::UnresolvedType::Tuple(
                tys.into_iter()
                    .map(|ty| self.convert_type_annotation_inner(ty, convert_placeholder, stack))
                    .collect(),
            ),
        }
    }

    fn substitute_trait_params(
        &mut self,
        trait_id: TraitId,
        params: Vec<engine::UnresolvedType>,
        span: SpanList,
    ) -> engine::UnresolvedType {
        let (trait_span, trait_ty, trait_params) = self
            .with_trait_decl(trait_id, |decl| {
                (
                decl.span,
                decl.ty
                    .as_ref()
                    .expect(
                        "`substitute_trait_params` may only be used with traits that have values",
                    )
                    .clone(),
                decl.params.clone(),
            )
            })
            .expect("instance should have already been accessed at least once");

        if trait_params.len() != params.len() {
            self.compiler.add_error(
                "wrong number of parameters provided to trait",
                vec![
                    Note::primary(span, "try providing the correct number of parameters here"),
                    Note::secondary(trait_span, "trait defined here"),
                ],
            );
            return engine::UnresolvedType::Error;
        }

        let substitutions = trait_params
            .into_iter()
            .zip(params)
            .collect::<GenericSubstitutions>();

        let mut instance_ty = engine::UnresolvedType::from(trait_ty);
        instance_ty.instantiate_with(&self.ctx, &substitutions);

        instance_ty
    }

    fn get_default_for_param(&self, param: TypeParameterId) -> Option<Type> {
        self.with_type_parameter_decl(param, |decl| decl.default.clone())
            .flatten()
    }
}

impl Typechecker {
    async fn resolve_plugin(
        &mut self,
        path: InternedString,
        name: InternedString,
        id: ExpressionId,
        span: SpanList,
        ty: Type,
        inputs: Vec<Expression>,
    ) -> lower::Expression {
        struct PluginApi;

        #[async_trait]
        impl crate::PluginApi for PluginApi {
            // TODO
        }

        let path = match self.compiler.loader.resolve(
            FilePath::Path(path),
            FileKind::Plugin,
            id.owner.and_then(|id| id.file),
        ) {
            Ok(path) => path,
            Err(error) => {
                self.compiler.add_error(
                    format!("cannot load file `{}`: {}", path, error),
                    vec![Note::primary(span, "while resolving this plugin")],
                );

                return lower::Expression {
                    id,
                    span,
                    kind: lower::ExpressionKind::error(&self.compiler),
                };
            }
        };

        let output = self
            .compiler
            .loader
            .plugin(
                path,
                name,
                crate::PluginInput {
                    id,
                    span,
                    ty: ty.clone(),
                    inputs,
                },
                &PluginApi,
            )
            .await;

        match output {
            Ok(output) => output.expr,
            Err(error) => {
                self.compiler.add_error(
                    format!("error while resolving plugin: {error}"),
                    vec![Note::primary(span, "see plugin documentation for details")],
                );

                lower::Expression {
                    id,
                    span,
                    kind: lower::ExpressionKind::error(&self.compiler),
                }
            }
        }
    }
}

impl Typechecker {
    fn format_type(
        &mut self,
        ty: impl Into<format::FormattableType>,
        format: format::Format,
    ) -> String {
        let typechecker = RefCell::new(self);

        macro_rules! getter {
            ($x:ident, $f:expr, $default:expr) => {
                paste::paste!(|id| {
                    match typechecker
                        .borrow_mut()
                        .[<with_ $x _decl>](id, |decl| $f(decl.name))
                    {
                        Some(name) => name,
                        None => $default,
                    }
                })
            };
        }

        let type_names = getter!(
            type,
            |name: InternedString| name.to_string(),
            String::from("<unknown>")
        );

        let trait_names = getter!(
            trait,
            |name: InternedString| name.to_string(),
            String::from("<unknown>")
        );

        let param_names = getter!(
            type_parameter,
            |name: Option<_>| name.as_ref().map(ToString::to_string),
            Some(String::from("<unknown>"))
        );

        format::format_type(ty, type_names, trait_names, param_names, format)
    }

    pub fn report_errors(&mut self) {
        if self.compiler.diagnostics.contains_errors() {
            return;
        }

        // TODO: Group by constant ID instead (store constant ID in error)
        for errors in self
            .errors
            .take()
            .into_iter()
            .into_group_map_by(|error| error.span.first().path)
            .into_values()
        {
            let (unresolved_type_errors, other_errors): (Vec<_>, Vec<_>) =
                errors.into_iter().partition_map(|mut e| {
                    use itertools::Either;

                    match &mut *e.error {
                        engine::TypeError::UnresolvedType(ty) => {
                            ty.apply(&self.ctx);
                            Either::Left((ty.all_vars(), e))
                        }
                        _ => Either::Right(e),
                    }
                });

            let should_report_unresolved_type_errors = other_errors.is_empty();

            for error in other_errors {
                self.report_error(error);
            }

            if should_report_unresolved_type_errors {
                let mut reported_type_variables = BTreeSet::new();

                for (vars, error) in unresolved_type_errors {
                    // Prevent unresolved type errors from cascading throughout the
                    // entire program -- just report the first instance of a value
                    // whose type cannot be determined
                    if vars
                        .iter()
                        .any(|var| !reported_type_variables.contains(var))
                    {
                        self.report_error(error);
                    }

                    reported_type_variables.extend(vars);
                }
            }
        }
    }

    fn report_error(&mut self, mut error: Error) {
        let multi_var_format = format::Format {
            type_function: format::TypeFunctionFormat::Description,
            type_variable: format::TypeVariableFormat::Description,
            surround_in_backticks: true,
        };

        let single_var_format = format::Format {
            type_function: format::TypeFunctionFormat::Description,
            type_variable: format::TypeVariableFormat::None,
            surround_in_backticks: true,
        };

        // Adjust error for more accurate diagnostics
        if let engine::TypeError::Mismatch(actual, expected) = &mut *error.error {
            actual.apply(&self.ctx);
            expected.apply(&self.ctx);

            // If a function's type doesn't match but its inputs do, then
            // display the error at the start of the call chain instead of the
            // function itself
            loop {
                if let engine::UnresolvedType::Function(actual_input, actual_output) = &actual {
                    if let engine::UnresolvedType::Function(expected_input, expected_output) =
                        &expected
                    {
                        let actual_input = actual_input.as_ref().clone();
                        let actual_output = actual_output.as_ref().clone();
                        let expected_input = expected_input.as_ref().clone();
                        let expected_output = expected_output.as_ref().clone();

                        if let Some(id) = error.expr {
                            if let Some(root) = self.root_for(id.owner) {
                                if self
                                    .ctx
                                    .clone()
                                    .unify(actual_output.clone(), expected_output.clone())
                                    .is_err()
                                {
                                    if let Some(call) = root.as_root_query_parent_of(id) {
                                        *actual = actual_output;
                                        *expected = expected_output;
                                        error.expr = Some(call.id);
                                        error.span = call.span;
                                        continue;
                                    } else {
                                        break;
                                    }
                                } else if self
                                    .ctx
                                    .clone()
                                    .unify(actual_input.clone(), expected_input.clone())
                                    .is_err()
                                {
                                    if let ExpressionKind::Call(_, input, _) =
                                        &root.as_root_query_parent_of(id).unwrap().kind
                                    {
                                        *actual = actual_input;
                                        *expected = expected_input;
                                        error.expr = Some(input.id);
                                        error.span = input.span;
                                    }

                                    break;
                                }
                            }
                        }
                    }
                }

                break;
            }
        }

        let mut diagnostic = match *error.error {
            engine::TypeError::ErrorExpression => return,
            engine::TypeError::Recursive(_) => self.compiler.error(
                "recursive type",
                vec![Note::primary(
                    error.span,
                    "the type of this references itself",
                )],
            ),
            engine::TypeError::Mismatch(mut actual, mut expected) => {
                actual.apply(&self.ctx);
                expected.apply(&self.ctx);

                let actual_ty = match &actual {
                    engine::UnresolvedType::Named(id, params, _) => Some((
                        self.declarations.borrow().types.get(id).unwrap().clone(),
                        params.clone(),
                    )),
                    _ => None,
                };

                let format = if expected.vars().len() + actual.vars().len() <= 1 {
                    single_var_format
                } else {
                    multi_var_format
                };

                let message = format!(
                    "expected {}, but found {}",
                    self.format_type(expected.clone(), format),
                    self.format_type(actual.clone(), format)
                );

                let mut notes = vec![Note::primary(error.span, message)];
                let mut fix = None;

                {
                    let mut output = actual.clone();
                    let mut num_inputs = 0usize;
                    while let engine::UnresolvedType::Function(_, ty) = output {
                        output = *ty;
                        num_inputs += 1;
                    }

                    if self.ctx.clone().unify(output, expected.clone()).is_ok() {
                        notes.push(Note::secondary(
                            error.span,
                            "try providing an input to this function",
                        ));

                        if let Some(source_code) =
                            self.compiler.source_code_for_span(error.span.first())
                        {
                            let (description, replacement) = if num_inputs == 1 {
                                (
                                    String::from("provide input"),
                                    format!("({source_code} (*input*))"),
                                )
                            } else {
                                (
                                    format!("provide {num_inputs} inputs"),
                                    format!(
                                        "({}{})",
                                        source_code,
                                        (0..num_inputs)
                                            .map(|n| format!(" (*input {}*)", n + 1)) // TODO: Use parameter name defined in function
                                            .collect::<String>(),
                                    ),
                                )
                            };

                            fix = Some(Fix::new(
                                description,
                                FixRange::replace(error.span.first()),
                                replacement,
                            ));
                        }
                    }
                }

                if let engine::UnresolvedType::Function(actual_input, actual_output) = &actual {
                    if let engine::UnresolvedType::Function(expected_input, expected_output) =
                        &expected
                    {
                        let mut actual_output = actual_output.as_ref().clone();
                        let mut expected_output = expected_output.as_ref().clone();

                        while let (
                            engine::UnresolvedType::Function(_, actual_output_inner),
                            engine::UnresolvedType::Function(_, expected_output_inner),
                        ) = (&actual_output, &expected_output)
                        {
                            actual_output = actual_output_inner.as_ref().clone();
                            expected_output = expected_output_inner.as_ref().clone();

                            if self
                                .ctx
                                .clone()
                                .unify(actual_output.clone(), expected_output.clone())
                                .is_err()
                            {
                                break;
                            }
                        }

                        if self
                            .ctx
                            .clone()
                            .unify(actual_output.clone(), expected_output.clone())
                            .is_err()
                        {
                            notes.push(Note::secondary(
                                error.span,
                                format!(
                                    "this function returns {}, but it should return {}",
                                    self.format_type(actual_output, format),
                                    self.format_type(expected_output, format)
                                ),
                            ));
                        } else if self
                            .ctx
                            .clone()
                            .unify(
                                actual_input.as_ref().clone(),
                                expected_input.as_ref().clone(),
                            )
                            .is_err()
                        {
                            notes.push(Note::secondary(
                                error.span,
                                format!(
                                    "this function accepts {}, but it should accept {}",
                                    self.format_type(actual_input.as_ref().clone(), format),
                                    self.format_type(expected_input.as_ref().clone(), format)
                                ),
                            ));
                        }
                    }
                }

                if let Some((actual_ty, actual_params)) = actual_ty {
                    if let Some(note) =
                        actual_ty
                            .attributes
                            .on_mismatch
                            .iter()
                            .find_map(|(param, message)| {
                                param
                                    .as_ref()
                                    .map_or(true, |param| {
                                        let param = actual_ty
                                            .params
                                            .iter()
                                            .position(|p| p == param)
                                            .expect("type parameter associated with wrong type");

                                        let inner_ty = actual_params[param].clone();

                                        self.ctx.clone().unify(inner_ty, expected.clone()).is_ok()
                                    })
                                    .then(|| Note::secondary(error.span, message))
                            })
                    {
                        notes.push(note);
                    }
                }

                if let engine::UnresolvedType::Named(id, _, _) = &expected {
                    let ty = self.declarations.borrow().types.get(id).unwrap().clone();

                    if let Some(source_code) =
                        self.compiler.source_code_for_span(error.span.first())
                    {
                        if let Some(replacement) =
                            ty.convert_from.iter().find_map(|(ty, replacement)| {
                                self.ctx
                                    .clone()
                                    .unify(actual.clone(), ty.clone())
                                    .is_ok()
                                    .then_some(replacement)
                            })
                        {
                            let mut replacement = replacement.clone();
                            wipple_syntax::parse::substitute(
                                &mut replacement,
                                InternedString::new("value"),
                                wipple_syntax::parse::Expr::new(
                                    error.span,
                                    wipple_syntax::parse::ExprKind::SourceCode(source_code),
                                ),
                            );

                            fix = Some(Fix::new(
                                format!(
                                    "convert the {} into {}",
                                    self.format_type(actual.clone(), format),
                                    self.format_type(expected.clone(), format)
                                ),
                                FixRange::replace(error.span.first()),
                                replacement,
                            ));
                        }
                    }
                }

                let mut var_counts = actual.vars().into_iter().counts().into_iter();
                if let Some((var, count)) = var_counts.next() {
                    if var_counts.next().is_none() && count > 1 {
                        notes.push(Note::secondary(
                            error.span,
                            format!(
                                "all uses of {} must resolve to the same type",
                                self.format_type(
                                    engine::UnresolvedType::Variable(var),
                                    format::Format {
                                        surround_in_backticks: true,
                                        type_function: format::TypeFunctionFormat::None,
                                        type_variable: format::TypeVariableFormat::Standalone,
                                    }
                                )
                            ),
                        ));
                    }
                }

                self.compiler
                    .error_with_trace("mismatched types", notes, error.trace)
                    .fix(fix)
            }
            engine::TypeError::MissingInstance(id, mut params, bound_span, error_candidates) => {
                for param in &mut params {
                    param.apply(&self.ctx);
                }

                let trait_attributes = self
                    .declarations
                    .borrow()
                    .traits
                    .get(&id)
                    .unwrap()
                    .attributes
                    .clone();

                let error_message = trait_attributes.on_unimplemented.map_or_else(
                    || String::from("missing instance"),
                    |(segments, trailing_segment)| {
                        let trait_params = self
                            .declarations
                            .borrow()
                            .traits
                            .get(&id)
                            .unwrap()
                            .params
                            .clone()
                            .into_iter()
                            .zip(params.clone())
                            .collect::<engine::GenericSubstitutions>();

                        segments
                            .iter()
                            .map(|(text, param)| {
                                let ty = trait_params.get(param).unwrap().clone();
                                text.to_string() + &self.format_type(ty, Default::default())
                            })
                            .chain(trailing_segment.map(|text| text.to_string()))
                            .collect()
                    },
                );

                let format = if params
                    .iter()
                    .map(|ty| ty.visible_vars().len())
                    .sum::<usize>()
                    == params
                        .iter()
                        .map(|ty| ty.visible_vars().into_iter().unique().count())
                        .sum()
                {
                    single_var_format
                } else {
                    multi_var_format
                };

                let note_message = format!(
                    "could not find instance {}",
                    self.format_type(format::FormattableType::r#trait(id, params), format)
                );

                self.compiler.error_with_trace(
                    error_message,
                    std::iter::once(
                        Note::primary(error.span, note_message).use_caller_if_available(),
                    )
                    .chain(
                        bound_span.map(|span| Note::secondary(span, "required by this bound here")),
                    )
                    .chain(error_candidates.into_iter().map(|span| {
                        Note::secondary(
                            span,
                            "this instance could apply, but its bounds weren't satisfied",
                        )
                    }))
                    .collect(),
                    error.trace,
                )
            }
            engine::TypeError::UnresolvedType(mut ty) => {
                ty.apply(&self.ctx);

                let format =
                    if ty.visible_vars().len() == ty.visible_vars().into_iter().unique().count() {
                        single_var_format
                    } else {
                        multi_var_format
                    };

                let note = (!matches!(ty, engine::UnresolvedType::Variable(_))).then(|| {
                    Note::primary(
                        error.span,
                        format!("this has type {}", self.format_type(ty, format)),
                    )
                });

                self.compiler.error_with_trace(
                    "could not determine the type of this expression",
                    std::iter::once(Note::primary(
                        error.span,
                        "try annotating the type with `::`",
                    ))
                    .chain(note)
                    .collect(),
                    error.trace,
                )
            }
            engine::TypeError::InvalidNumericLiteral(ty) => {
                let format =
                    if ty.visible_vars().len() == ty.visible_vars().into_iter().unique().count() {
                        single_var_format
                    } else {
                        multi_var_format
                    };

                let message = format!(
                    "number does not fit into a {}",
                    self.format_type(ty, format)
                );

                self.compiler.error_with_trace(
                    message,
                    vec![Note::primary(error.span, "invalid numeric literal")],
                    error.trace,
                )
            }
        };

        diagnostic.notes.append(&mut error.notes);

        self.compiler.diagnostics.add(diagnostic);
    }
}

impl Typechecker {
    fn root_for(&mut self, id: impl Into<Option<ConstantId>>) -> Option<Expression> {
        match id.into() {
            Some(id) => self
                .with_constant_decl(id, |decl| decl.body.clone())
                .flatten(),
            None => self
                .items
                .get(&self.entrypoint_item?)
                .and_then(|(_, expr)| expr.clone()),
        }
    }
}
