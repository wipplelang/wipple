#![allow(clippy::type_complexity)]

#[macro_use]
mod number;

mod access;
pub mod display;
mod engine;
mod exhaustiveness;
pub mod format;
mod queries;
pub mod traverse;

pub use engine::{
    BottomTypeReason, BuiltinType, GenericSubstitutions, Type, TypeKind, TypeStructure,
};
pub use lower::{Intrinsic, Semantics, TypeAnnotation, TypeAnnotationKind};

use crate::{
    analysis::{lower, Analysis, SpanList},
    diagnostics::{DiagnosticLocation, Fix, FixRange, Note},
    helpers::{Backtrace, InternedString},
    BuiltinSyntaxId, BuiltinTypeId, Compiler, ConstantId, ExpressionId, FieldIndex, FileKind,
    FilePath, ItemId, PatternId, SnippetId, SyntaxId, TraitId, TypeId, TypeParameterId, VariableId,
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
    ops::ControlFlow,
    os::raw::{c_int, c_uint},
};
use wipple_util::Shared;

#[derive(Debug)]
pub enum Progress {
    CollectingTypes,
    ResolvingDeclarations { count: usize, remaining: usize },
}

#[derive(Debug, Default)]
pub struct Program {
    pub items: BTreeMap<ItemId, RwLock<(Option<(Option<TraitId>, ConstantId)>, Expression)>>,
    pub generic_items: BTreeMap<ConstantId, Expression>,
    pub contexts: BTreeMap<ConstantId, ItemId>,
    pub top_level: Option<ItemId>,
    pub entrypoint_wrapper: Option<ItemId>,
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
            pub snippets: $($container)::+<SnippetId, SnippetDecl>,
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
            snippets: decls.snippets.into_iter().collect(),
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
    pub enumeration_ty: Option<TypeId>,
    pub attributes: lower::ConstantAttributes,
    pub uses: HashSet<SpanList>,
    body: lower::Expression,
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
    body: Option<lower::Expression>,
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
    pub infer: bool,
    pub default: Option<engine::Type>,
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

#[derive(Debug, Clone)]
pub struct SnippetDecl {
    pub name: InternedString,
    pub span: SpanList,
    pub expr: wipple_syntax::parse::Expr<Analysis>,
    pub wrap: bool,
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
                Function([<$prefix Pattern>], Box<[<$prefix Expression>]>, Vec<(VariableId, $type)>),
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
                Semantics(Semantics, Box<[<$prefix Expression>]>),
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
    Extend(Box<UnresolvedExpression>, Vec<((SpanList, InternedString), UnresolvedExpression)>),
});

expr!(, "Monomorphized", engine::UnresolvedType, {
    Number(InternedString),
    BoundInstance(TraitId),
    ErrorConstant(ConstantId),
    UnresolvedConstant(ConstantId, Option<(Option<SpanList>, Vec<SpanList>)>),
    UnresolvedTrait(TraitId, Option<(Option<SpanList>, Vec<SpanList>)>),
    Constant(ItemId),
    UnresolvedExtend(Box<MonomorphizedExpression>, Vec<((SpanList, InternedString), MonomorphizedExpression)>),
    Extend(Box<MonomorphizedExpression>, BTreeMap<FieldIndex, MonomorphizedExpression>),
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
                    MonomorphizedExpressionKind::UnresolvedTrait(id, None)
                }
                UnresolvedExpressionKind::Constant(id) => {
                    MonomorphizedExpressionKind::UnresolvedConstant(id, None)
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
                UnresolvedExpressionKind::Extend(value, fields) => {
                    MonomorphizedExpressionKind::UnresolvedExtend(
                        Box::new((*value).into()),
                        fields
                            .into_iter()
                            .map(|(field, expr)| (field, expr.into()))
                            .collect(),
                    )
                }
                UnresolvedExpressionKind::Semantics(semantics, value) => {
                    MonomorphizedExpressionKind::Semantics(semantics, Box::new((*value).into()))
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
    BoundInstance(TraitId),
    ExpandedConstant(ItemId),
    UnresolvedConstant(ConstantId),
    ErrorConstant(ConstantId),
    UnresolvedTrait(TraitId),
    UnresolvedExtend,
    Extend(Box<Expression>, BTreeMap<FieldIndex, Expression>),
});

impl Expression {
    pub fn contains_error(&self) -> bool {
        let mut contains_error = false;
        self.traverse(
            |expr| {
                contains_error |= match &expr.kind {
                    ExpressionKind::Error(_) | ExpressionKind::ErrorConstant(_) => true,
                    ExpressionKind::Function(pattern, _, _)
                    | ExpressionKind::Initialize(pattern, _) => pattern.contains_error(),
                    ExpressionKind::When(_, arms) => {
                        arms.iter().any(|arm| arm.pattern.contains_error())
                    }
                    ExpressionKind::With((None, _), _) => true,
                    _ => false,
                };

                if contains_error {
                    ControlFlow::Break(false)
                } else {
                    ControlFlow::Continue(())
                }
            },
            |_| ControlFlow::Continue(()),
        );

        contains_error
    }

    pub fn find_first_unresolved_plugin(&self) -> Option<ExpressionId> {
        let mut deepest_plugin = None;
        self.traverse(
            |expr| {
                if let ExpressionKind::Plugin(_, _, inputs) = &expr.kind {
                    deepest_plugin = Some(
                        deepest_plugin
                            .or_else(|| {
                                inputs
                                    .iter()
                                    .find_map(Expression::find_first_unresolved_plugin)
                            })
                            .unwrap_or(expr.id),
                    );
                }

                ControlFlow::Continue(())
            },
            |_| ControlFlow::Continue(()),
        );

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
    UnresolvedDestructure,
    Variant(TypeId, VariantIndex, Vec<Pattern>),
    UnresolvedVariant,
});

impl Pattern {
    pub fn contains_error(&self) -> bool {
        let mut contains_error = false;
        self.traverse(|pattern| {
            contains_error |= matches!(
                pattern.kind,
                PatternKind::Error(_)
                    | PatternKind::UnresolvedDestructure
                    | PatternKind::UnresolvedVariant
            );
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
        top_level: lower::File,
        complete: bool,
        mut progress: impl FnMut(Progress),
    ) -> Program {
        let mut typechecker = Typechecker::new(self.clone(), top_level);

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
    top_level: lower::File,
    exported: HashMap<InternedString, HashSet<lower::AnyDeclaration>>,
    lowered_top_level_expr: lower::Expression,
    ctx: engine::Context,
    declarations: RefCell<DeclarationsInner>,
    instances: im::OrdMap<TraitId, Vec<ConstantId>>,
    specialized_constants: im::OrdMap<ConstantId, ConstantId>,
    contexts: BTreeMap<ConstantId, ItemId>,
    monomorphization_cache: im::OrdMap<ConstantId, Vec<(engine::UnresolvedType, ItemId)>>,
    item_queue: im::Vector<QueuedItem>,
    items: im::OrdMap<ItemId, (Option<(Option<TraitId>, ConstantId)>, Option<Expression>)>,
    generic_items: im::OrdMap<ConstantId, Expression>,
    top_level_expr: Option<UnresolvedExpression>,
    top_level_item: Option<ItemId>,
    errors: RefCell<im::Vector<Error>>,
}

#[derive(Debug, Clone)]
struct QueuedItem {
    generic_id: Option<(Option<TraitId>, ConstantId)>,
    id: ItemId,
    expr: UnresolvedExpression,
    info: MonomorphizeInfo,
    contextual: bool,
    top_level: bool,
}

impl Typechecker {
    fn unresolved_ty(
        &self,
        kind: engine::UnresolvedTypeKind,
        span: impl Into<Option<SpanList>>,
    ) -> engine::UnresolvedType {
        engine::UnresolvedType::from_parts(
            kind,
            engine::TypeInfo {
                span: span.into(),
                reason: None,
                trace: self.compiler.backtrace(),
            },
        )
    }

    fn ty(&self, kind: engine::TypeKind, span: impl Into<Option<SpanList>>) -> engine::Type {
        engine::Type::from_parts(
            kind,
            engine::TypeInfo {
                span: span.into(),
                reason: None,
                trace: self.compiler.backtrace(),
            },
        )
    }

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
    pub fn new(compiler: Compiler, mut top_level_file: lower::File) -> Self {
        let lowered_top_level_expr = lower::Expression {
            id: compiler.new_expression_id(None),
            span: top_level_file.span,
            kind: lower::ExpressionKind::Block(mem::take(&mut top_level_file.statements), true),
        };

        let exported = mem::take(&mut top_level_file.exported)
            .into_iter()
            .map(|(name, values)| (name, values.into_iter().map(|(_, value)| value).collect()))
            .collect::<HashMap<_, _>>();

        Typechecker {
            compiler,
            top_level: top_level_file,
            lowered_top_level_expr,
            exported,
            ctx: Default::default(),
            declarations: Default::default(),
            instances: Default::default(),
            specialized_constants: Default::default(),
            contexts: Default::default(),
            monomorphization_cache: Default::default(),
            item_queue: Default::default(),
            items: Default::default(),
            generic_items: Default::default(),
            top_level_expr: Default::default(),
            top_level_item: None,
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
        macro_rules! process_queue {
            () => {{
                let mut count = 0;
                let total = self.item_queue.len();
                while let Some(mut item) = self.item_queue.pop_back() {
                    count += 1;
                    progress(count, total);

                    let is_generic = item.info.is_generic;
                    let is_top_level_or_generic = item.top_level || is_generic;

                    if !is_generic {
                        self.items.insert(item.id, (item.generic_id, None));
                    }

                    let expr = self.repeatedly_monomorphize_expr(item.expr, &mut item.info);

                    let expr = self.finalize_expr(expr, is_top_level_or_generic);

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

                    if is_top_level_or_generic && !expr.contains_error() {
                        self.check_exhaustiveness(&expr);
                        self.check_access(&expr);
                    }

                    if let Some((_, id)) = item.generic_id {
                        self.generic_items.insert(id, expr.clone());
                    }

                    if !is_generic {
                        self.items.get_mut(&item.id).unwrap().1 = Some(expr);
                    }
                }
            }};
        }

        // Typecheck generic constants

        for (tr, id) in (self
            .top_level
            .declarations
            .constants
            .keys()
            .copied()
            .map(|id| (None, id)))
        .chain(
            self.top_level
                .declarations
                .instances
                .iter()
                .map(|(id, decl)| (Some(decl.value.tr), *id)),
        )
        .collect::<Vec<_>>()
        {
            let prev_ctx = self.ctx.clone();

            if let Err(Some(error)) =
                self.typecheck_constant_expr(tr, id, None, None, None, Default::default())
            {
                self.add_error(error);
            }

            process_queue!();

            self.ctx = prev_ctx;
            self.monomorphization_cache.clear();
            self.items.clear();
            self.contexts.clear();
        }

        // Typecheck the top level

        let top_level_item = mem::take(&mut self.top_level_expr).map(|top_level| {
            let top_level_id = self.compiler.new_item_id_in(top_level.span.first().path);

            self.item_queue.push_back(QueuedItem {
                generic_id: None,
                id: top_level_id,
                expr: top_level,
                info: MonomorphizeInfo::default(),
                contextual: false,
                top_level: true,
            });

            top_level_id
        });

        self.top_level_item = top_level_item;

        if let Some(entrypoint) = self.top_level.info.entrypoint {
            if let Some(decl) = self.with_constant_decl(entrypoint, |decl| decl.clone()) {
                if decl.ty
                    != self.ty(
                        engine::TypeKind::Function(
                            Box::new(self.ty(
                                engine::TypeKind::Function(
                                    Box::new(self.ty(engine::TypeKind::Tuple(Vec::new()), None)),
                                    Box::new(self.ty(engine::TypeKind::Tuple(Vec::new()), None)),
                                ),
                                None,
                            )),
                            Box::new(self.ty(engine::TypeKind::Tuple(Vec::new()), None)),
                        ),
                        None,
                    )
                {
                    self.compiler.add_error(
                        decl.span,
                        "entrypoint must have type `Entrypoint`",
                        "",
                    );
                }

                if !decl.params.is_empty() || !decl.bounds.is_empty() {
                    self.compiler.add_error(
                        decl.span,
                        "entrypoint may not have type parameters or bounds",
                        "",
                    );
                }

                self.typecheck_constant_expr(
                    None,
                    entrypoint,
                    None,
                    decl.span,
                    Some(decl.ty.into()),
                    Default::default(),
                )
                .expect("entrypoint must not contain bounds");
            }
        }

        process_queue!();

        // Replace constants with specialized versions as needed

        fn specialize(typechecker: &mut Typechecker, id: ItemId, progress: &mut bool) {
            let mut expr = typechecker
                .items
                .get(&id)
                .unwrap()
                .1
                .as_ref()
                .unwrap()
                .clone();

            expr.traverse_mut(
                |expr| {
                    if let ExpressionKind::Constant(id) = &mut expr.kind {
                        if let (Some((_, generic_id)), _) = typechecker
                            .items
                            .get(id)
                            .unwrap_or_else(|| panic!("{id:?} at {:?}", expr.span))
                        {
                            if let Some(specialized_id) = typechecker.specialized_constant_for(
                                *generic_id,
                                expr.id,
                                expr.span,
                                &expr.ty,
                            ) {
                                *id = specialized_id;
                                *progress = true;
                            }
                        }
                    }

                    ControlFlow::Continue(())
                },
                |_| ControlFlow::Continue(()),
            );

            *typechecker.items.get_mut(&id).unwrap().1.as_mut().unwrap() = expr;
        }

        let mut specialized = BTreeSet::new();
        loop {
            let mut progress = false;
            for id in self.items.keys().cloned().collect::<Vec<_>>() {
                if !specialized.contains(&id) {
                    specialize(&mut self, id, &mut progress);
                    specialized.insert(id);
                }
            }

            if !progress {
                break;
            }
        }

        process_queue!();

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

            expr.traverse_mut(
                |expr| {
                    if let ExpressionKind::Constant(id) = &mut expr.kind {
                        if let Some(mapped_id) = map.get(id) {
                            *id = *mapped_id;
                        }
                    }

                    ControlFlow::Continue(())
                },
                |_| ControlFlow::Continue(()),
            )
        }

        for (generic_id, item_id) in &mut self.contexts {
            if let Some(&id) = generic_id_map.get(generic_id) {
                *item_id = id;
            }
        }

        // Build the final program

        Program {
            items: items
                .into_iter()
                .map(|(id, (ids, expr))| (id, RwLock::new((ids, expr.unwrap()))))
                .collect(),
            contexts: self.contexts,
            generic_items: self.generic_items.into_iter().collect(),
            top_level: top_level_item,
            entrypoint_wrapper: self
                .top_level
                .info
                .entrypoint
                .and_then(|id| generic_id_map.get(&id).copied()),
            file_attributes: self.top_level.attributes,
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
            .top_level
            .info
            .recursion_limit
            .unwrap_or(Compiler::DEFAULT_RECURSION_LIMIT);

        let mut expr = MonomorphizedExpression::from(expr);
        let mut substituted_defaults = false;
        loop {
            if info.recursion_stack.len() > recursion_limit {
                self.report_recursion_limit_reached(expr.span, &info.recursion_stack);

                return MonomorphizedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty: self.unresolved_ty(engine::UnresolvedTypeKind::Error, expr.span),
                    kind: MonomorphizedExpressionKind::error(&self.compiler),
                };
            }

            *info.has_resolved_expr.lock() = false;
            expr = self.monomorphize_expr(expr, info);

            let is_resolved = || {
                let pattern_is_resolved = |pattern: &MonomorphizedPattern| {
                    let mut is_resolved = true;
                    pattern.traverse(|pattern| {
                        is_resolved &= !matches!(
                            pattern.kind,
                            MonomorphizedPatternKind::UnresolvedDestructure(_, _)
                                | MonomorphizedPatternKind::UnresolvedVariant(_, _, _)
                        )
                    });

                    is_resolved
                };

                let mut is_resolved = true;
                expr.traverse(
                    |expr| {
                        is_resolved &= match &expr.kind {
                            MonomorphizedExpressionKind::UnresolvedTrait(_, _)
                            | MonomorphizedExpressionKind::UnresolvedConstant(_, _)
                            | MonomorphizedExpressionKind::UnresolvedExtend(_, _) => false,
                            MonomorphizedExpressionKind::Function(pattern, _, _)
                            | MonomorphizedExpressionKind::Initialize(pattern, _) => {
                                pattern_is_resolved(pattern)
                            }
                            MonomorphizedExpressionKind::When(_, arms) => {
                                arms.iter().all(|arm| pattern_is_resolved(&arm.pattern))
                            }
                            _ => true,
                        };

                        ControlFlow::Continue(())
                    },
                    |_| ControlFlow::Continue(()),
                );

                is_resolved
            };

            // Stop if we've resolved everything
            if is_resolved() {
                break;
            }

            // Stop if we've made no progress and we've already substituted defaults
            if !*info.has_resolved_expr.lock() && substituted_defaults {
                break;
            }

            // Substitute defaults one expression at a time, working from the
            // innermost expression out
            expr.traverse_mut(
                |_| ControlFlow::Continue(()),
                |expr| {
                    if expr.ty.substitute_defaults(&self.ctx, false) {
                        ControlFlow::Break(false)
                    } else {
                        ControlFlow::Continue(())
                    }
                },
            );

            substituted_defaults = true;

            info.recursion_stack.push(expr.span);
        }

        expr
    }

    fn report_recursion_limit_reached(&self, span: SpanList, stack: &[SpanList]) {
        self.compiler.add_diagnostic(
            self.compiler
                .error(
                    span,
                    "recursion limit reached while computing the type of this expression",
                    "recursion-limit",
                )
                .notes(
                    stack
                        .iter()
                        .copied()
                        .unique()
                        .map(|span| Note::secondary(span, "while computing this")),
                ),
        );
    }
}

impl Typechecker {
    pub fn collect_types(&mut self) {
        let mut info = ConvertInfo::new(None);
        let expr = self.convert_expr(self.lowered_top_level_expr.clone(), &mut info);
        self.top_level_expr = Some(expr);

        macro_rules! declaration {
            ($kind:ident) => {
                paste::paste! {
                    self.top_level
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
            snippet,
        );
    }

    fn typecheck_constant_expr(
        &mut self,
        trait_id: Option<TraitId>,
        id: ConstantId,
        use_id: impl Into<Option<ExpressionId>>,
        use_span: impl Into<Option<SpanList>>,
        use_ty: impl Into<Option<engine::UnresolvedType>>,
        known_bounds: BoundInstances,
    ) -> Result<Option<ItemId>, Option<Error>> {
        let use_id = use_id.into();
        let use_span = use_span.into();
        let use_ty = use_ty.into();

        let mut info = MonomorphizeInfo {
            is_generic: use_ty.is_none(),
            bound_instances: known_bounds.clone(),
            ..Default::default()
        };

        let (generic_span, params, constant_generic_ty, generic_bounds) = if trait_id.is_some() {
            let (tr, params, trait_params, span, bounds) = self
                .with_instance_decl(id, |decl| {
                    (
                        decl.trait_id,
                        decl.params.clone(),
                        decl.trait_params.clone(),
                        decl.span,
                        decl.bounds.clone(),
                    )
                })
                .unwrap();

            let ty = self.substitute_trait_params(
                tr,
                trait_params.clone().into_iter().map(From::from).collect(),
                span,
            );

            let (finalized_ty, resolved) = ty.finalize(&self.ctx);
            assert!(resolved);

            (span, params, finalized_ty, bounds)
        } else {
            self.with_constant_decl(id, |decl| {
                (
                    decl.span,
                    decl.params.clone(),
                    decl.ty.clone(),
                    decl.bounds.clone(),
                )
            })
            .unwrap()
        };

        let use_span = use_span.unwrap_or(generic_span);

        let mut use_ty = use_ty.unwrap_or_else(|| {
            let mut ty = constant_generic_ty.clone().into();
            self.instantiate_generics(&mut ty);
            ty
        });

        use_ty.apply(&self.ctx);
        assert!(!use_ty.contains_opaque());

        let contextual = self
            .with_constant_decl(id, |decl| decl.attributes.is_contextual)
            .unwrap_or(false);

        // Unify instantiated constant type with type at use site

        let prev_ctx = self.ctx.clone();

        let mut substitutions = engine::GenericSubstitutions::new();
        for param in params {
            let inferred = self
                .with_type_parameter_decl(param, |decl| decl.infer)
                .unwrap_or(false);

            let mut default = self
                .with_type_parameter_decl(param, |decl| decl.default.clone())
                .flatten()
                .map(engine::UnresolvedType::from);

            if let Some(default) = &mut default {
                default.instantiate_with(&self.ctx, &substitutions);
            }

            let kind = if inferred {
                engine::UnresolvedTypeKind::Opaque(self.ctx.new_variable(default))
            } else {
                engine::UnresolvedTypeKind::Variable(self.ctx.new_variable(default))
            };

            substitutions.insert(param, self.unresolved_ty(kind, None));
        }

        let mut instantiated_bounds = generic_bounds.clone();

        for bound in &mut instantiated_bounds {
            for ty in &mut bound.params {
                ty.instantiate_with(&self.ctx, &substitutions);
            }
        }

        let mut instantiated_generic_ty = engine::UnresolvedType::from(constant_generic_ty.clone());

        instantiated_generic_ty.instantiate_with(&self.ctx, &substitutions);

        if info.is_generic {
            let mut constant_generic_ty = engine::UnresolvedType::from(constant_generic_ty.clone());
            self.instantiate_generics(&mut constant_generic_ty);

            if let Err(error) = self
                .ctx
                .unify(instantiated_generic_ty.clone(), constant_generic_ty)
            {
                self.ctx = prev_ctx;
                return Err(Some(self.error(error, use_id, use_span)));
            }
        }

        if let Err(error) = self
            .ctx
            .unify(instantiated_generic_ty.clone(), use_ty.clone())
        {
            self.ctx = prev_ctx.clone();
            return Err(Some(self.error(error, use_id, use_span)));
        }

        // Check bounds

        for (bound_index, bound) in instantiated_bounds.clone().into_iter().enumerate() {
            if let Err(error) = self.instance_for_params(
                bound.trait_id,
                bound.params.clone(),
                use_id,
                use_span,
                Some((id, bound_index, bound.clone())),
                &mut info,
            ) {
                match error {
                    FindInstanceError::RecursionLimitReached => {
                        return Err(None);
                    }
                    FindInstanceError::TypeError(error) => {
                        if !matches!(error.error.as_ref(), engine::TypeError::MissingInstance(..)) {
                            if let Err(error) = self
                                .ctx
                                .unify(instantiated_generic_ty.clone(), use_ty.clone())
                            {
                                self.ctx = prev_ctx;
                                return Err(Some(self.error(error, use_id, use_span)));
                            } else {
                                self.ctx = prev_ctx;
                                return Err(Some(error));
                            }
                        }
                    }
                    FindInstanceError::MultipleCandidates(_) => {}
                };
            }
        }

        // Turn the opaque type variables into regular type variables

        instantiated_generic_ty.instantiate_opaque(&self.ctx);
        instantiated_generic_ty.apply(&self.ctx);
        assert!(!instantiated_generic_ty.contains_opaque());

        for bound in &mut instantiated_bounds {
            for ty in &mut bound.params {
                ty.instantiate_opaque(&self.ctx);
            }
        }

        use_ty.instantiate_opaque(&self.ctx);
        use_ty.apply(&self.ctx);
        assert!(!use_ty.contains_opaque());

        // Now that we've determined the types of the non-inferred parameters from the bounds,
        // determine the types of the inferred parameters by re-evaluating the bounds

        for (bound_index, bound) in instantiated_bounds.clone().into_iter().enumerate() {
            if let Err(error) = self.instance_for_params(
                bound.trait_id,
                bound.params.clone(),
                use_id,
                use_span,
                Some((id, bound_index, bound.clone())),
                &mut info,
            ) {
                match error {
                    FindInstanceError::RecursionLimitReached => {
                        return Err(None);
                    }
                    FindInstanceError::TypeError(error) => {
                        if !matches!(error.error.as_ref(), engine::TypeError::MissingInstance(..)) {
                            if let Err(error) = self.unify(
                                use_id,
                                use_span,
                                instantiated_generic_ty.clone(),
                                use_ty.clone(),
                            ) {
                                self.ctx = prev_ctx;
                                return Err(Some(error));
                            } else {
                                self.ctx = prev_ctx;
                                return Err(Some(error));
                            }
                        }
                    }
                    FindInstanceError::MultipleCandidates(_) => {}
                };
            }
        }

        if let Err(error) = self.unify(
            use_id,
            use_span,
            instantiated_generic_ty.clone(),
            use_ty.clone(),
        ) {
            self.ctx = prev_ctx;
            return Err(Some(error));
        }

        instantiated_generic_ty.apply(&self.ctx);
        assert!(!instantiated_generic_ty.contains_opaque());

        use_ty.apply(&self.ctx);
        assert!(!use_ty.contains_opaque());

        // Apply bounds

        if info.is_generic {
            if let Err(error) = self.unify(
                use_id,
                use_span,
                instantiated_generic_ty.clone(),
                constant_generic_ty.clone(),
            ) {
                self.ctx = prev_ctx;
                return Err(Some(error));
            }

            for (instantiated_bound, generic_bound) in
                instantiated_bounds.clone().into_iter().zip(generic_bounds)
            {
                for (instantiated_ty, generic_ty) in instantiated_bound
                    .params
                    .into_iter()
                    .zip(generic_bound.params)
                {
                    if let Err(error) =
                        self.unify(use_id, instantiated_bound.span, instantiated_ty, generic_ty)
                    {
                        self.ctx = prev_ctx;
                        return Err(Some(error));
                    }
                }
            }

            instantiated_generic_ty.apply(&self.ctx);
            assert!(!instantiated_generic_ty.contains_opaque());

            use_ty.apply(&self.ctx);
            assert!(!use_ty.contains_opaque());

            mem::take(&mut info.bound_instances);
            for bound in &mut instantiated_bounds {
                for param in &mut bound.params {
                    param.apply(&self.ctx);
                }

                info.bound_instances
                    .entry(bound.trait_id)
                    .or_default()
                    .push((
                        bound.params.clone(),
                        None,
                        bound.span,
                        self.compiler.backtrace(),
                    ));
            }
        } else {
            let resolved_bounds = instantiated_bounds
                .into_iter()
                .enumerate()
                .map(|(bound_index, bound)| {
                    let instance = match self.instance_for_params(
                        bound.trait_id,
                        bound.params.clone(),
                        use_id,
                        use_span,
                        Some((id, bound_index, bound.clone())),
                        &mut info,
                    ) {
                        Ok(instance) => instance,
                        Err(error) => match error {
                            FindInstanceError::RecursionLimitReached => {
                                return Err(None);
                            }
                            FindInstanceError::TypeError(error) => {
                                self.ctx = prev_ctx.clone();
                                return Err(Some(error));
                            }
                            FindInstanceError::MultipleCandidates(_) => {
                                return Err(None);
                            }
                        },
                    };

                    *info.has_resolved_expr.lock() = true;

                    Ok((instance, bound))
                })
                .collect::<Result<Vec<_>, _>>()?;

            mem::take(&mut info.bound_instances);
            for (instance, mut bound) in resolved_bounds {
                for param in &mut bound.params {
                    param.apply(&self.ctx);
                }

                info.bound_instances
                    .entry(bound.trait_id)
                    .or_default()
                    .push((
                        bound.params,
                        instance,
                        bound.span,
                        self.compiler.backtrace(),
                    ));
            }
        }

        // Convert the constant's body

        let body = if trait_id.is_some() {
            self.with_instance_decl(id, |decl| decl.body.clone())
                .unwrap()
        } else {
            self.with_constant_decl(id, |decl| Some(decl.body.clone()))
                .unwrap()
        };

        let body = match body {
            Some(body) => {
                let mut convert_info = ConvertInfo::new(id);
                let body = self.convert_expr(body, &mut convert_info);

                if let Err(error) = self.unify(
                    use_id,
                    body.span,
                    body.ty.clone(),
                    instantiated_generic_ty.clone(),
                ) {
                    self.ctx = prev_ctx;
                    return Err(Some(error));
                }

                instantiated_generic_ty.apply(&self.ctx);
                assert!(!instantiated_generic_ty.contains_opaque());

                use_ty.apply(&self.ctx);
                assert!(!use_ty.contains_opaque());

                Some(body)
            }
            None => None,
        };

        if let Err(error) = self.unify(
            use_id,
            use_span,
            instantiated_generic_ty.clone(),
            use_ty.clone(),
        ) {
            self.ctx = prev_ctx;
            return Err(Some(error));
        }

        instantiated_generic_ty.apply(&self.ctx);
        assert!(!instantiated_generic_ty.contains_opaque());

        use_ty.apply(&self.ctx);
        assert!(!use_ty.contains_opaque());

        // Register the monomorphized constant, or use an existing one from the
        // cache

        let ty_for_caching = self.use_ty_for_caching(instantiated_generic_ty.clone());

        if let Some(candidates) = self.monomorphization_cache.get(&id) {
            for (ty, monomorphized_id) in candidates {
                let monomorphized_id = *monomorphized_id;
                let prev_ctx = self.ctx.clone();

                if self.ctx.unify(ty.clone(), ty_for_caching.clone()).is_ok() {
                    return Ok(Some(monomorphized_id));
                }

                self.ctx = prev_ctx;
            }
        }

        if let Some(body) = body {
            let monomorphized_id = self.compiler.new_item_id_with(id.file);

            self.monomorphization_cache
                .entry(id)
                .or_default()
                .push((ty_for_caching, monomorphized_id));

            self.item_queue.push_back(QueuedItem {
                generic_id: Some((trait_id, id)),
                id: monomorphized_id,
                expr: body,
                info,
                contextual,
                top_level: false,
            });

            Ok(Some(monomorphized_id))
        } else {
            Ok(None)
        }
    }

    fn specialized_constant_for(
        &mut self,
        generic_id: ConstantId,
        use_id: ExpressionId,
        use_span: SpanList,
        use_ty: &engine::Type,
    ) -> Option<ItemId> {
        let (generic_ty, candidates) = self.with_constant_decl(generic_id, |decl| {
            (decl.ty.clone(), decl.specializations.clone())
        })?;

        candidates.into_iter().find_map(|candidate| {
            let (candidate_span, candidate_ty) = self
                .with_constant_decl(candidate, |decl| (decl.span, decl.ty.clone()))
                .unwrap();

            let prev_ctx = self.ctx.clone();

            if let Err(error) = self
                .ctx
                .unify(candidate_ty.clone().into(), generic_ty.clone())
            {
                self.add_error(
                    self.error(error, None, candidate_span)
                        .with_note(Note::secondary(
                        candidate_span,
                        "this constant must have a more specific type than the original constant",
                    )),
                );

                self.ctx = prev_ctx;
                return None;
            }

            if self
                .unify(
                    use_id,
                    use_span,
                    use_ty.clone().into(),
                    candidate_ty.clone(),
                )
                .is_err()
            {
                self.ctx = prev_ctx;
                return None;
            };

            self.specialized_constants.insert(candidate, generic_id);

            let id = self
                .typecheck_constant_expr(
                    None,
                    candidate,
                    use_id,
                    use_span,
                    Some(use_ty.clone().into()),
                    Default::default(),
                )
                .expect("specialized constants may not contain bounds")
                .unwrap();

            Some(id)
        })
    }

    fn use_ty_for_caching(
        &self,
        use_ty: impl Into<engine::UnresolvedType>,
    ) -> engine::UnresolvedType {
        let mut use_ty_for_caching = use_ty.into();
        use_ty_for_caching.apply(&self.ctx);

        let ctx = self.ctx.clone();

        // HACK: Replace all variables with the same variable
        let caching_var = engine::TypeVariable(usize::MAX);
        for var in use_ty_for_caching.all_vars() {
            ctx.substitutions.borrow_mut().insert(
                var,
                self.unresolved_ty(engine::UnresolvedTypeKind::Opaque(caching_var), None),
            );
        }

        use_ty_for_caching.apply(&ctx);

        use_ty_for_caching
    }
}

struct ConvertInfo {
    id: Option<ConstantId>,
    substitutions: engine::GenericSubstitutions,
    variables: BTreeMap<VariableId, engine::UnresolvedType>,
    existing_types: BTreeMap<ExpressionId, engine::UnresolvedType>,
    function_end_value: Option<UnresolvedExpression>,
}

impl ConvertInfo {
    fn new(id: impl Into<Option<ConstantId>>) -> Self {
        ConvertInfo {
            id: id.into(),
            substitutions: Default::default(),
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
                ty: self.unresolved_ty(engine::UnresolvedTypeKind::Error, expr.span),
                kind: UnresolvedExpressionKind::Error(trace),
            },
            lower::ExpressionKind::Marker(id) => {
                let params = self.with_type_decl(id, |ty| ty.params.clone()).unwrap();

                let mut ty = self.unresolved_ty(
                    engine::UnresolvedTypeKind::Named(
                        id,
                        params
                            .clone()
                            .into_iter()
                            .map(|param| {
                                self.unresolved_ty(
                                    engine::UnresolvedTypeKind::Parameter(param),
                                    None,
                                )
                            })
                            .collect(),
                        engine::TypeStructure::Marker,
                    ),
                    expr.span,
                );

                self.instantiate_generics(&mut ty);

                ty.info.span = Some(expr.span);

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
                    .unwrap_or((
                        self.unresolved_ty(engine::UnresolvedTypeKind::Error, expr.span),
                        false,
                    ));

                self.instantiate_generics(&mut ty);

                ty.info.span = Some(expr.span);

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
                let (span, ty) = self
                    .with_trait_decl(id, |decl| (decl.span, decl.ty.clone()))
                    .unwrap();

                match ty {
                    Some(ty) => {
                        let mut ty = engine::UnresolvedType::from(ty);
                        self.instantiate_generics(&mut ty);
                        ty.info.span = Some(expr.span);

                        UnresolvedExpression {
                            id: expr.id,
                            span: expr.span,
                            ty,
                            kind: UnresolvedExpressionKind::Trait(id),
                        }
                    }
                    None => {
                        self.compiler.add_diagnostic(
                            self.compiler.error(
                                expr.span,
                                "this trait does not store a value and may only be used at the type level",
                                "trait-does-not-contain-value",
                            )
                            .note(Note::secondary(span, "trait defined here")),
                        );

                        UnresolvedExpression {
                            id: expr.id,
                            span: expr.span,
                            ty: self.unresolved_ty(engine::UnresolvedTypeKind::Error, expr.span),
                            kind: UnresolvedExpressionKind::error(&self.compiler),
                        }
                    }
                }
            }
            lower::ExpressionKind::Variable(var) => {
                let mut ty = info.variables.get(&var).cloned().unwrap_or_else(|| {
                    self.unresolved_ty(
                        engine::UnresolvedTypeKind::Variable(self.ctx.new_variable(None)),
                        None,
                    )
                });

                ty.info.span = Some(expr.span);

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
                ty: self.unresolved_ty(
                    engine::UnresolvedTypeKind::Builtin(engine::BuiltinType::Text),
                    expr.span,
                ),
                kind: UnresolvedExpressionKind::Text(text),
            },
            lower::ExpressionKind::Number(number) => UnresolvedExpression {
                id: expr.id,
                span: expr.span,
                ty: self.unresolved_ty(
                    engine::UnresolvedTypeKind::NumericVariable(self.ctx.new_variable(None)),
                    expr.span,
                ),
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
                    .unwrap_or_else(|| {
                        self.unresolved_ty(engine::UnresolvedTypeKind::Tuple(Vec::new()), expr.span)
                    });

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

                let output_ty = self
                    .unresolved_ty(
                        engine::UnresolvedTypeKind::Variable(self.ctx.new_variable(None)),
                        expr.span,
                    )
                    .with_reason(Some(engine::TypeReason::FunctionOutput(function.span)));

                if let Err(error) = self.unify(
                    function.id,
                    function.span,
                    function.ty.clone(),
                    self.unresolved_ty(
                        engine::UnresolvedTypeKind::Function(
                            Box::new(input.ty.clone()),
                            Box::new(output_ty.clone()),
                        ),
                        function.span,
                    ),
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
                let input_ty = self.unresolved_ty(
                    engine::UnresolvedTypeKind::Variable(self.ctx.new_variable(None)),
                    pattern.span,
                );
                let pattern = self.convert_pattern(pattern, input_ty.clone(), info);

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
                    ty: self.unresolved_ty(
                        engine::UnresolvedTypeKind::Function(
                            Box::new(input_ty.with_reason(Some(
                                engine::TypeReason::FunctionInput(pattern.span),
                            ))),
                            Box::new(
                                body.ty.clone().with_reason(Some(
                                    engine::TypeReason::FunctionOutput(body.span),
                                )),
                            ),
                        ),
                        expr.span,
                    ),
                    kind: UnresolvedExpressionKind::Function(
                        pattern,
                        Box::new(body),
                        captures
                            .into_iter()
                            .filter_map(|(var, _)| {
                                let ty = info.variables.get(&var)?.clone();
                                Some((var, ty))
                            })
                            .collect(),
                    ),
                }
            }
            lower::ExpressionKind::When(input, arms) => {
                let input = self.convert_expr(*input, info);

                let arms = arms
                    .into_iter()
                    .map(|arm| self.convert_arm(arm, input.ty.clone(), info))
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
                        self.unresolved_ty(
                            engine::UnresolvedTypeKind::Variable(self.ctx.new_variable(None)),
                            expr.span,
                        )
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
                    ty: self.unresolved_ty(
                        engine::UnresolvedTypeKind::Variable(self.ctx.new_variable(None)),
                        expr.span,
                    ),
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
                    ty: self.unresolved_ty(
                        engine::UnresolvedTypeKind::Variable(self.ctx.new_variable(None)),
                        expr.span,
                    ),
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
                    ty: self.unresolved_ty(
                        engine::UnresolvedTypeKind::Variable(self.ctx.new_variable(None)),
                        expr.span,
                    ),
                    kind: UnresolvedExpressionKind::Plugin(path, name, inputs),
                }
            }
            lower::ExpressionKind::Annotate(value, ty) => {
                let mut ty =
                    self.convert_type_annotation(Some(engine::TypeReason::Annotation(ty.span)), ty);

                self.add_substitutions(&mut ty, &mut info.substitutions);

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
                let pattern = self.convert_pattern(pattern, value.ty.clone(), info);

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty: self
                        .unresolved_ty(engine::UnresolvedTypeKind::Tuple(Vec::new()), expr.span),
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
                            ty: self.unresolved_ty(engine::UnresolvedTypeKind::Error, expr.span),
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
                        if let Some(code) = self
                            .compiler
                            .single_line_source_code_for_span(expr.span.first())
                        {
                            self.compiler.add_error(
                                expr.span,
                                format!("`{code}` is not a structure type"),
                                "syntax-error",
                            );
                        } else {
                            self.compiler.add_error(
                                expr.span,
                                "expected a structure type",
                                "syntax-error",
                            );
                        }

                        return UnresolvedExpression {
                            id: expr.id,
                            span: expr.span,
                            ty: self.unresolved_ty(engine::UnresolvedTypeKind::Error, expr.span),
                            kind: UnresolvedExpressionKind::error(&self.compiler),
                        };
                    }
                };

                let mut ty = self.unresolved_ty(
                    engine::UnresolvedTypeKind::Named(
                        id,
                        params
                            .into_iter()
                            .map(|param| {
                                self.unresolved_ty(
                                    engine::UnresolvedTypeKind::Parameter(param),
                                    None,
                                )
                            })
                            .collect(),
                        engine::TypeStructure::Structure(structure_field_tys.clone()),
                    ),
                    expr.span,
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

                for ((span, name), expr) in fields {
                    let (index, ty) = match structure_field_names.get(&name) {
                        Some(index) if index.into_inner() < fields_by_index.len() => {
                            (*index, structure_field_tys[index.into_inner()].clone())
                        }
                        _ => {
                            extra_fields.push((span, name));
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

                for (span, name) in extra_fields {
                    self.compiler.add_diagnostic(
                        self.compiler
                            .error(
                                span,
                                format!("extra field `{name}`"),
                                "extra-missing-fields",
                            )
                            .fix_with(
                                "remove the extra field",
                                FixRange::replace(span.first()),
                                "",
                            ),
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
                        expr.span,
                        match missing_fields.len() {
                            0 => unreachable!(),
                            1 => format!("missing field `{}`", missing_fields.pop().unwrap()),
                            _ => {
                                let last_missing_field = missing_fields.pop().unwrap();

                                format!(
                                    "missing fields {} and `{}`",
                                    missing_fields
                                        .into_iter()
                                        .map(|name| format!("`{name}`, "))
                                        .collect::<String>(),
                                    last_missing_field
                                )
                            }
                        },
                        "extra-missing-fields",
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
                            ty: self.unresolved_ty(engine::UnresolvedTypeKind::Error, expr.span),
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
                        if let Some(code) = self
                            .compiler
                            .single_line_source_code_for_span(expr.span.first())
                        {
                            self.compiler.add_error(
                                expr.span,
                                format!("`{code}` is not an enumeration type"),
                                "syntax-error",
                            );
                        } else {
                            self.compiler.add_error(
                                expr.span,
                                "expected an enumeration type",
                                "syntax-error",
                            );
                        }

                        return UnresolvedExpression {
                            id: expr.id,
                            span: expr.span,
                            ty: self.unresolved_ty(engine::UnresolvedTypeKind::Error, expr.span),
                            kind: UnresolvedExpressionKind::error(&self.compiler),
                        };
                    }
                };

                let mut ty = self.unresolved_ty(
                    engine::UnresolvedTypeKind::Named(
                        id,
                        params
                            .into_iter()
                            .map(|param| {
                                self.unresolved_ty(
                                    engine::UnresolvedTypeKind::Parameter(param),
                                    None,
                                )
                            })
                            .collect(),
                        engine::TypeStructure::Enumeration(variants_tys.clone()),
                    ),
                    expr.span,
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

                let ty = self.unresolved_ty(
                    engine::UnresolvedTypeKind::Tuple(
                        exprs.iter().map(|expr| expr.ty.clone()).collect(),
                    ),
                    expr.span,
                );

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Tuple(exprs),
                }
            }
            lower::ExpressionKind::Format(segments, trailing_segment) => {
                let show_trait = match self.top_level.info.language_items.show {
                    Some(show_trait) => show_trait,
                    None => {
                        self.compiler.add_error(
                            expr.span,
                            "using placeholder text requires the `show` language item",
                            "",
                        );

                        return UnresolvedExpression {
                            id: expr.id,
                            span: expr.span,
                            ty: self.unresolved_ty(engine::UnresolvedTypeKind::Error, expr.span),
                            kind: UnresolvedExpressionKind::error(&self.compiler),
                        };
                    }
                };

                self.with_trait_decl(show_trait, |_| {});

                let segments = segments
                    .into_iter()
                    .map(|(text, expr)| {
                        let expr = self.convert_expr(expr, info);

                        let ty = self.unresolved_ty(
                            engine::UnresolvedTypeKind::Variable(self.ctx.new_variable(None)),
                            expr.span,
                        );

                        let expr = UnresolvedExpression {
                            id: expr.id,
                            span: expr.span,
                            ty: ty.clone(),
                            kind: UnresolvedExpressionKind::Call(
                                Box::new(UnresolvedExpression {
                                    id: expr.id,
                                    span: expr.span,
                                    ty: self.unresolved_ty(
                                        engine::UnresolvedTypeKind::Function(
                                            Box::new(expr.ty.clone()),
                                            Box::new(ty),
                                        ),
                                        expr.span,
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
                    ty: self.unresolved_ty(
                        engine::UnresolvedTypeKind::Builtin(engine::BuiltinType::Text),
                        expr.span,
                    ),
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
                        .unwrap_or(self.unresolved_ty(engine::UnresolvedTypeKind::Error, None));

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
                    ty: self.unresolved_ty(
                        engine::UnresolvedTypeKind::Variable(self.ctx.new_variable(Some(
                            self.unresolved_ty(
                                engine::UnresolvedTypeKind::Tuple(Vec::new()),
                                expr.span,
                            ),
                        ))),
                        expr.span,
                    ),
                    kind: UnresolvedExpressionKind::End(Box::new(value)),
                }
            }
            lower::ExpressionKind::Extend(value, fields) => {
                let value = self.convert_expr(*value, info);

                let fields = fields
                    .into_iter()
                    .map(|(field, expr)| (field, self.convert_expr(expr, info)))
                    .collect::<Vec<_>>();

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty: value.ty.clone(),
                    kind: UnresolvedExpressionKind::Extend(Box::new(value), fields),
                }
            }
            lower::ExpressionKind::Semantics(semantics, expr) => {
                let expr = self.convert_expr(*expr, info);

                UnresolvedExpression {
                    id: expr.id,
                    span: expr.span,
                    ty: expr.ty.clone(),
                    kind: UnresolvedExpressionKind::Semantics(semantics, Box::new(expr)),
                }
            }
        }
    }

    fn convert_arm(
        &mut self,
        arm: lower::Arm,
        input_ty: engine::UnresolvedType,
        info: &mut ConvertInfo,
    ) -> UnresolvedArm {
        UnresolvedArm {
            span: arm.span,
            pattern: self.convert_pattern(arm.pattern, input_ty, info),
            guard: arm.guard.map(|expr| self.convert_expr(expr, info)),
            body: self.convert_expr(arm.body, info),
        }
    }

    fn convert_pattern(
        &mut self,
        pattern: lower::Pattern,
        ty: engine::UnresolvedType,
        info: &mut ConvertInfo,
    ) -> UnresolvedPattern {
        UnresolvedPattern {
            id: self.compiler.new_pattern_id(info.id),
            span: pattern.span,
            kind: (|| match pattern.kind {
                lower::PatternKind::Error(trace) => UnresolvedPatternKind::Error(trace),
                lower::PatternKind::Wildcard => UnresolvedPatternKind::Wildcard,
                lower::PatternKind::Number(number) => {
                    let numeric_ty = self.unresolved_ty(
                        engine::UnresolvedTypeKind::NumericVariable(self.ctx.new_variable(None)),
                        pattern.span,
                    );

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
                        self.unresolved_ty(
                            engine::UnresolvedTypeKind::Builtin(engine::BuiltinType::Text),
                            pattern.span,
                        ),
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
                            let ty = self.unresolved_ty(
                                engine::UnresolvedTypeKind::Variable(self.ctx.new_variable(None)),
                                pattern.span,
                            );

                            (name, (self.convert_pattern(pattern, ty.clone(), info), ty))
                        })
                        .collect(),
                ),
                lower::PatternKind::Variant(id, variant, values) => {
                    let (name, params, variants_tys) = match self.with_type_decl(id, |decl| {
                        (
                            decl.name,
                            decl.params.clone(),
                            match &decl.kind {
                                TypeDeclKind::Enumeration { variants, .. } => {
                                    Some(variants.clone())
                                }
                                _ => None,
                            },
                        )
                    }) {
                        Some((name, params, variants_tys)) => (name, params, variants_tys),
                        None => return UnresolvedPatternKind::error(&self.compiler),
                    };

                    let variants_tys = match variants_tys {
                        Some(tys) => tys,
                        None => {
                            self.compiler.add_error(
                                pattern.span,
                                format!("pattern must match a variant of `{name}`"),
                                "syntax-error",
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

                    let enumeration_ty = self.unresolved_ty(
                        engine::UnresolvedTypeKind::Named(
                            id,
                            params
                                .into_iter()
                                .map(|param| {
                                    let mut ty = self.unresolved_ty(
                                        engine::UnresolvedTypeKind::Parameter(param),
                                        None,
                                    );
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
                        ),
                        pattern.span,
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
                            .map(|(pattern, ty)| self.convert_pattern(pattern, ty, info))
                            .collect(),
                    )
                }
                lower::PatternKind::Annotate(inner, target_ty) => {
                    let target_ty = self.convert_type_annotation(
                        Some(engine::TypeReason::Annotation(target_ty.span)),
                        target_ty,
                    );

                    if let Err(error) = self.unify(None, pattern.span, ty, target_ty.clone()) {
                        self.add_error(error);
                    }

                    self.convert_pattern(*inner, target_ty, info).kind
                }
                lower::PatternKind::Or(lhs, rhs) => UnresolvedPatternKind::Or(
                    Box::new(self.convert_pattern(*lhs, ty.clone(), info)),
                    Box::new(self.convert_pattern(*rhs, ty, info)),
                ),
                lower::PatternKind::Tuple(patterns) => {
                    let tuple_tys = patterns
                        .iter()
                        .map(|pattern| {
                            self.unresolved_ty(
                                engine::UnresolvedTypeKind::Variable(self.ctx.new_variable(None)),
                                pattern.span,
                            )
                        })
                        .collect::<Vec<_>>();

                    if let Err(error) = self.unify(
                        None,
                        pattern.span,
                        ty,
                        self.unresolved_ty(
                            engine::UnresolvedTypeKind::Tuple(tuple_tys.clone()),
                            pattern.span,
                        ),
                    ) {
                        self.add_error(error);
                    }

                    UnresolvedPatternKind::Tuple(
                        patterns
                            .into_iter()
                            .zip(tuple_tys)
                            .map(|(pattern, ty)| self.convert_pattern(pattern, ty, info))
                            .collect(),
                    )
                }
            })(),
        }
    }
}

type BoundInstances = im::OrdMap<
    TraitId,
    Vec<(
        Vec<engine::UnresolvedType>,
        Option<(ConstantId, Vec<Bound>)>,
        SpanList,
        Backtrace,
    )>,
>;

#[derive(Debug, Clone, Default)]
struct MonomorphizeInfo {
    is_generic: bool,
    bound_instances: BoundInstances,
    instance_stack:
        BTreeMap<TraitId, Vec<(ConstantId, usize, Vec<engine::UnresolvedType>, Vec<Bound>)>>,
    recursion_stack: Vec<SpanList>,
    has_resolved_expr: Shared<bool>,
}

impl Typechecker {
    fn monomorphize_expr(
        &mut self,
        expr: impl Into<MonomorphizedExpression>,
        info: &mut MonomorphizeInfo,
    ) -> MonomorphizedExpression {
        let mut expr = expr.into();

        expr.ty.apply(&self.ctx);

        MonomorphizedExpression {
            id: expr.id,
            span: expr.span,
            ty: expr.ty.clone(),
            kind: (|| match expr.kind {
                MonomorphizedExpressionKind::Error(trace) => {
                    MonomorphizedExpressionKind::Error(trace)
                }
                MonomorphizedExpressionKind::Marker => MonomorphizedExpressionKind::Marker,
                MonomorphizedExpressionKind::BoundInstance(id) => {
                    MonomorphizedExpressionKind::BoundInstance(id)
                }
                MonomorphizedExpressionKind::ErrorConstant(id) => {
                    MonomorphizedExpressionKind::ErrorConstant(id)
                }
                MonomorphizedExpressionKind::UnresolvedConstant(generic_id, _) => {
                    let kind = match self.typecheck_constant_expr(
                        None,
                        generic_id,
                        expr.id,
                        expr.span,
                        expr.ty.clone(),
                        info.bound_instances.clone(),
                    ) {
                        Ok(id) => {
                            *info.has_resolved_expr.lock() = true;

                            MonomorphizedExpressionKind::Constant(id.unwrap())
                        }
                        Err(error) => {
                            if let Some(error) = error {
                                self.add_error(error);

                                *info.has_resolved_expr.lock() = true;

                                MonomorphizedExpressionKind::ErrorConstant(generic_id)
                            } else {
                                MonomorphizedExpressionKind::UnresolvedConstant(generic_id, None)
                            }
                        }
                    };

                    expr.ty.apply(&self.ctx);
                    assert!(!expr.ty.contains_opaque());

                    kind
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
                    let pattern = match expr.ty.kind {
                        engine::UnresolvedTypeKind::Function(input_ty, _) => {
                            let mut input_ty = *input_ty;
                            input_ty.apply(&self.ctx);

                            self.monomorphize_pattern(pattern, input_ty.clone())
                        }
                        _ => self.monomorphize_pattern(
                            pattern,
                            self.unresolved_ty(engine::UnresolvedTypeKind::Error, expr.span),
                        ),
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
                MonomorphizedExpressionKind::UnresolvedTrait(tr, _) => {
                    let prev_bound_instances = info.bound_instances.clone();

                    let instance_id =
                        match self.instance_for_ty(tr, expr.ty.clone(), expr.id, expr.span, info) {
                            Ok(Some(instance)) => instance,
                            Ok(None) => {
                                // This is not an error, it just means that the instance is from a
                                // bound in a generic constant -- return a placeholder since generic
                                // constants aren't lowered to IR anyway

                                info.bound_instances = prev_bound_instances;

                                expr.ty.apply(&self.ctx);
                                assert!(!expr.ty.contains_opaque());

                                *info.has_resolved_expr.lock() = true;

                                return MonomorphizedExpressionKind::BoundInstance(tr);
                            }
                            Err(error) => {
                                info.bound_instances = prev_bound_instances;

                                match error {
                                    FindInstanceError::RecursionLimitReached => {
                                        return MonomorphizedExpressionKind::error(&self.compiler);
                                    }
                                    FindInstanceError::TypeError(error) => {
                                        self.add_error(error);
                                        return MonomorphizedExpressionKind::error(&self.compiler);
                                    }
                                    FindInstanceError::MultipleCandidates(candidates) => {
                                        return MonomorphizedExpressionKind::UnresolvedTrait(
                                            tr, candidates,
                                        );
                                    }
                                }
                            }
                        };

                    expr.ty.apply(&self.ctx);
                    assert!(!expr.ty.contains_opaque());

                    info.bound_instances = prev_bound_instances;

                    let monomorphized_id = self.typecheck_constant_expr(
                        Some(tr),
                        instance_id,
                        expr.id,
                        expr.span,
                        expr.ty.clone(),
                        info.bound_instances.clone(),
                    );

                    *info.has_resolved_expr.lock() = true;

                    match monomorphized_id {
                        Ok(id) => MonomorphizedExpressionKind::Constant(id.unwrap()),
                        Err(error) => {
                            if let Some(error) = error {
                                self.add_error(error);
                            }

                            MonomorphizedExpressionKind::error(&self.compiler)
                        }
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
                        None,
                        id,
                        expr.id,
                        expr.span,
                        expr.ty.clone(),
                        info.bound_instances.clone(),
                    ) {
                        Ok(_) => {
                            *info.has_resolved_expr.lock() = true;
                            MonomorphizedExpressionKind::ContextualConstant(id)
                        }
                        Err(error) => {
                            *info.has_resolved_expr.lock() = true;

                            if let Some(error) = error {
                                self.add_error(error);
                            }

                            MonomorphizedExpressionKind::error(&self.compiler)
                        }
                    }
                }
                MonomorphizedExpressionKind::End(value) => {
                    MonomorphizedExpressionKind::End(Box::new(self.monomorphize_expr(*value, info)))
                }
                MonomorphizedExpressionKind::UnresolvedExtend(value, fields) => {
                    let value = self.monomorphize_expr(*value, info);

                    let (id, structure_field_tys) = match &value.ty.kind {
                        engine::UnresolvedTypeKind::Named(
                            id,
                            _,
                            engine::TypeStructure::Structure(fields),
                        ) => (*id, fields.clone()),
                        _ => {
                            if let Some(code) = self
                                .compiler
                                .single_line_source_code_for_span(expr.span.first())
                            {
                                self.compiler.add_error(
                                    expr.span,
                                    format!("`{code}` is not a structure type"),
                                    "syntax-error",
                                );
                            } else {
                                self.compiler.add_error(
                                    expr.span,
                                    "expected a structure type",
                                    "syntax-error",
                                );
                            }

                            return MonomorphizedExpressionKind::error(&self.compiler);
                        }
                    };

                    let structure_field_names = match self
                        .with_type_decl(id, |decl| decl.kind.clone())
                        .expect("type should have already been accessed at least once")
                    {
                        TypeDeclKind::Structure { field_names, .. } => field_names,
                        _ => {
                            if let Some(code) = self
                                .compiler
                                .single_line_source_code_for_span(expr.span.first())
                            {
                                self.compiler.add_error(
                                    expr.span,
                                    format!("`{code}` is not a structure type"),
                                    "syntax-error",
                                );
                            } else {
                                self.compiler.add_error(
                                    expr.span,
                                    "expected a structure type",
                                    "syntax-error",
                                );
                            }

                            return MonomorphizedExpressionKind::error(&self.compiler);
                        }
                    };

                    let mut extra_fields = Vec::new();

                    let fields = fields
                        .into_iter()
                        .filter_map(|((span, name), mut expr)| {
                            let index = match structure_field_names.get(&name) {
                                Some(index) => *index,
                                None => {
                                    extra_fields.push((span, name));
                                    return None;
                                }
                            };

                            let ty = structure_field_tys[index.into_inner()].clone();

                            if let Err(error) =
                                self.unify(value.id, value.span, expr.ty, ty.clone())
                            {
                                self.add_error(error);
                            }

                            expr.ty = ty;

                            Some((index, expr))
                        })
                        .collect();

                    MonomorphizedExpressionKind::Extend(Box::new(value), fields)
                }
                MonomorphizedExpressionKind::Extend(value, fields) => {
                    MonomorphizedExpressionKind::Extend(value, fields)
                }
                MonomorphizedExpressionKind::Semantics(semantics, expr) => {
                    MonomorphizedExpressionKind::Semantics(
                        semantics,
                        Box::new(self.monomorphize_expr(*expr, info)),
                    )
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

                if let Some(boolean_ty) = self.top_level.info.language_items.boolean {
                    if let Err(error) = self.unify(
                        guard.id,
                        guard.span,
                        guard.ty.clone(),
                        self.unresolved_ty(
                            engine::UnresolvedTypeKind::Named(
                                boolean_ty,
                                Vec::new(),
                                // HACK: Optimization because unification doesn't take structure into
                                // account -- the structure can be applied during finalization
                                engine::TypeStructure::Marker,
                            ),
                            guard.span,
                        ),
                    ) {
                        self.add_error(error);
                    }
                } else {
                    self.compiler.add_error(
                        guard.span,
                        "typechecking this condition requires the `boolean` language item",
                        "",
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
        let pattern: MonomorphizedPattern = pattern.into();

        ty.apply(&self.ctx);

        let kind = (|| match pattern.kind {
            MonomorphizedPatternKind::Error(trace) => MonomorphizedPatternKind::Error(trace),
            MonomorphizedPatternKind::Number(number) => {
                let numeric_ty = self.unresolved_ty(
                    engine::UnresolvedTypeKind::NumericVariable(self.ctx.new_variable(None)),
                    pattern.span,
                );

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
                    self.unresolved_ty(
                        engine::UnresolvedTypeKind::Builtin(engine::BuiltinType::Text),
                        pattern.span,
                    ),
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

                let (id, params) = match ty.kind.clone() {
                    engine::UnresolvedTypeKind::Named(id, params, _) => (id, params),
                    engine::UnresolvedTypeKind::Variable(_) => {
                        return MonomorphizedPatternKind::UnresolvedDestructure(
                            structure_ty,
                            fields,
                        );
                    }
                    _ => {
                        let ty = self.format_type(
                            ty,
                            format::Format {
                                surround_in_backticks: true,
                                ..Default::default()
                            },
                        );

                        if let Some(code) = self
                            .compiler
                            .single_line_source_code_for_span(pattern.span.first())
                        {
                            self.compiler.add_error(
                                pattern.span,
                                format!("`{code}` cannot match {ty} values"),
                                "syntax-error",
                            );
                        } else {
                            self.compiler.add_error(
                                pattern.span,
                                format!("pattern cannot match {ty} values"),
                                "syntax-error",
                            );
                        }

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
                        let ty = self.format_type(
                            ty,
                            format::Format {
                                surround_in_backticks: true,
                                ..Default::default()
                            },
                        );

                        if let Some(code) = self
                            .compiler
                            .single_line_source_code_for_span(pattern.span.first())
                        {
                            self.compiler.add_error(
                                pattern.span,
                                format!("`{code}` cannot match {ty} values"),
                                "syntax-error",
                            );
                        } else {
                            self.compiler.add_error(
                                pattern.span,
                                format!("pattern cannot match {ty} values"),
                                "syntax-error",
                            );
                        }

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
                                let ty = self.format_type(
                                    ty,
                                    format::Format {
                                        surround_in_backticks: true,
                                        ..Default::default()
                                    },
                                );

                                self.compiler.add_error(
                                    pattern.span,
                                    format!("{ty} has no field `{name}`"),
                                    "undefined-name",
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
                let (id, params) = match &ty.kind {
                    engine::UnresolvedTypeKind::Named(id, params, _) => (*id, params),
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
                        let ty = self.format_type(
                            ty,
                            format::Format {
                                surround_in_backticks: true,
                                ..Default::default()
                            },
                        );

                        if let Some(code) = self
                            .compiler
                            .single_line_source_code_for_span(pattern.span.first())
                        {
                            self.compiler.add_error(
                                pattern.span,
                                format!("`{code}` cannot match {ty} values"),
                                "syntax-error",
                            );
                        } else {
                            self.compiler.add_error(
                                pattern.span,
                                format!("pattern cannot match {ty} values"),
                                "syntax-error",
                            );
                        }

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
                    self.unresolved_ty(
                        engine::UnresolvedTypeKind::Named(
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
                        pattern.span,
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
                    .map(|_| {
                        self.unresolved_ty(
                            engine::UnresolvedTypeKind::Variable(self.ctx.new_variable(None)),
                            None,
                        )
                    })
                    .collect::<Vec<_>>();

                if let Err(error) = self.unify(
                    None,
                    pattern.span,
                    ty.clone(),
                    self.unresolved_ty(
                        engine::UnresolvedTypeKind::Tuple(tys.clone()),
                        pattern.span,
                    ),
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
    MultipleCandidates(Option<(Option<SpanList>, Vec<SpanList>)>),
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
        let instance =
            self.instance_for_inner(tr, Ok(ty.clone()), use_id.into(), use_span, None, info)?;

        Ok(instance.map(|(id, _)| id))
    }

    fn instance_for_params(
        &mut self,
        tr: TraitId,
        params: Vec<engine::UnresolvedType>,
        use_id: impl Into<Option<ExpressionId>>,
        use_span: SpanList,
        bound: Option<(ConstantId, usize, Bound)>,
        info: &mut MonomorphizeInfo,
    ) -> Result<Option<(ConstantId, Vec<Bound>)>, FindInstanceError> {
        self.instance_for_inner(tr, Err(params), use_id.into(), use_span, bound, info)
    }

    fn instance_for_inner(
        &mut self,
        tr: TraitId,
        ty_or_params: Result<engine::UnresolvedType, Vec<engine::UnresolvedType>>,
        use_id: Option<ExpressionId>,
        use_span: SpanList,
        bound: Option<(ConstantId, usize, Bound)>,
        info: &mut MonomorphizeInfo,
    ) -> Result<Option<(ConstantId, Vec<Bound>)>, FindInstanceError> {
        let recursion_limit = self
            .top_level
            .info
            .recursion_limit
            .unwrap_or(Compiler::DEFAULT_RECURSION_LIMIT);

        if info.recursion_stack.len() > recursion_limit {
            self.report_recursion_limit_reached(use_span, &info.recursion_stack);
            return Err(FindInstanceError::RecursionLimitReached);
        }

        let tr_decl = self.with_trait_decl(tr, Clone::clone).unwrap();

        let mut params = match ty_or_params {
            Ok(ty) => {
                let generic_trait_ty = tr_decl
                    .ty
                    .clone()
                    .expect("`instance_for_ty` requires a valued trait");

                let mut substitutions = engine::GenericSubstitutions::new();

                let mut trait_ty = engine::UnresolvedType::from(generic_trait_ty);
                self.add_substitutions(&mut trait_ty, &mut substitutions);

                if let Err(error) = self.unify(use_id, use_span, ty.clone(), trait_ty.clone()) {
                    return Err(FindInstanceError::TypeError(error));
                }

                tr_decl
                    .params
                    .into_iter()
                    .map(|param| substitutions.get(&param).unwrap().clone())
                    .collect()
            }
            Err(params) => params,
        };

        for param in &mut params {
            param.apply(&self.ctx);
        }

        // If a bound refers to itself, assume that the bound is satisfied
        if let Some((instance_id, bound_index, _)) = bound {
            if let Some((id, _, instance_params, bounds)) = info
                .instance_stack
                .entry(tr)
                .or_default()
                .iter()
                .rev()
                .find(|(stack_instance_id, stack_bound_index, _, _)| {
                    instance_id == *stack_instance_id && bound_index == *stack_bound_index
                })
            {
                let prev_ctx = self.ctx.clone();

                let mut error = false;
                for (instance_param_ty, param_ty) in
                    instance_params.clone().into_iter().zip(params.clone())
                {
                    if self
                        .ctx
                        .unify_generic(
                            self.use_ty_for_caching(instance_param_ty.clone()),
                            self.use_ty_for_caching(param_ty.clone()),
                        )
                        .is_err()
                    {
                        error = true;
                        break;
                    }
                }

                if !error {
                    if params.clone().into_iter().zip(params.clone()).any(
                        |(param, param_for_caching)| {
                            // Ensure that applying the instance recursively
                            // actually makes progress on determining the type
                            self.use_ty_for_caching(param)
                                != self.use_ty_for_caching(param_for_caching)
                        },
                    ) {
                        return Ok(Some((*id, bounds.clone())));
                    } else {
                        self.ctx = prev_ctx;
                        return Err(FindInstanceError::MultipleCandidates(None));
                    }
                } else {
                    self.ctx = prev_ctx;
                }
            }
        }

        let mut error_candidates = Vec::new();

        macro_rules! find_instance {
            ($resolve:expr) => {{
                // First try with numeric variables...
                match find_instance!(@find params.clone(), $resolve) {
                    // ...if there is a single candidate, return it.
                    Some(Ok(candidate)) => return Ok(candidate),
                    // ...if there are multiple candidates, try again finalizing numeric variables.
                    Some(Err(FindInstanceError::MultipleCandidates(_))) => {
                        let params = params
                            .clone()
                            .into_iter()
                            .map(|mut ty| {
                                ty.substitute_defaults(&self.ctx, true);
                                ty
                            })
                            .collect::<Vec<_>>();

                        match find_instance!(@find params, $resolve) {
                            Some(Ok(candidate)) => return Ok(candidate),
                            Some(Err(error)) => return Err(error),
                            None => {}
                        }
                    }
                    Some(Err(error)) => return Err(error),
                    // ...if there are no candidates, continue the search.
                    None => {}
                }
            }};
            (@find $params:expr, $resolve:expr) => {{
                let mut candidates = $resolve($params)?;

                match candidates.len() {
                    0 => None,
                    1 => {
                        let (ctx, candidate, _) = candidates.pop().unwrap();
                        self.ctx = ctx;
                        Some(Ok(candidate))
                    }
                    _ => Some(Err(FindInstanceError::MultipleCandidates(
                        Some((bound.as_ref().map(|(_, _, bound)| bound.span), candidates.into_iter().map(|(.., span)| span).collect())),
                    ))),
                }
            }};
        }

        macro_rules! unify_instance_params {
            ($label:lifetime, $candidates:expr, $unify:expr, $params:expr, $instance_params:expr, $prev_ctx:expr $(,)?) => {{
                for (instance_param_ty, param_ty) in
                    $instance_params.clone().into_iter().zip($params.clone())
                {
                    if $unify(instance_param_ty.clone(), param_ty.clone()).is_err() {
                        self.ctx = $prev_ctx.clone();
                        continue $label;
                    }
                }
            }};
        }

        let bound_instances = info.bound_instances.get(&tr).cloned().unwrap_or_default();

        find_instance!(|params: Vec<engine::UnresolvedType>| {
            let mut candidates = Vec::new();
            'check: for (instance_params, candidate, span, _backtrace) in bound_instances.clone() {
                let prev_ctx = self.ctx.clone();

                unify_instance_params!(
                    'check,
                    candidates,
                    |left, right| self.ctx.unify_generic(left, right),
                    params,
                    instance_params,
                    prev_ctx,
                );

                let ctx = mem::replace(&mut self.ctx, prev_ctx);
                candidates.push((ctx, candidate, span));
            }

            Ok(candidates)
        });

        let declared_instances = self
            .instances
            .get(&tr)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .map(|id| {
                self.with_instance_decl(id, |instance| {
                    (
                        id,
                        instance
                            .trait_params
                            .clone()
                            .into_iter()
                            .map(engine::UnresolvedType::from)
                            .collect::<Vec<_>>(),
                        instance.span,
                        instance.bounds.clone(),
                    )
                })
                .unwrap()
            })
            .collect::<Vec<_>>();

        find_instance!(|params: Vec<engine::UnresolvedType>| {
            let mut candidates = Vec::new();
            'check: for (id, mut instance_params, instance_span, instance_bounds) in
                declared_instances.clone()
            {
                let prev_ctx = self.ctx.clone();

                let mut substitutions = engine::GenericSubstitutions::new();
                for ty in &mut instance_params {
                    self.add_substitutions(ty, &mut substitutions);
                }

                unify_instance_params!(
                    'check,
                    candidates,
                    |left, right| self.ctx.unify(left, right),
                    params,
                    instance_params,
                    prev_ctx,
                );

                if let Some((id, bound_index, _)) = bound {
                    info.instance_stack.entry(tr).or_default().push((
                        id,
                        bound_index,
                        instance_params.clone(),
                        instance_bounds.clone(),
                    ));
                }

                for (index, mut instance_bound) in instance_bounds.clone().into_iter().enumerate() {
                    for ty in &mut instance_bound.params {
                        self.add_substitutions(ty, &mut substitutions);
                    }

                    info.recursion_stack.push(instance_bound.span);

                    let result = self.instance_for_params(
                        instance_bound.trait_id,
                        instance_bound.params.clone(),
                        None,
                        instance_bound.span,
                        Some((id, index, instance_bound.clone())),
                        info,
                    );

                    info.recursion_stack.pop();

                    if let Err(error) = result {
                        self.ctx = prev_ctx;

                        if bound.is_some() {
                            info.instance_stack.entry(tr).or_default().pop();
                        }

                        let unsatisfied_bounds = match error {
                            // TODO: Display multiple bounds
                            FindInstanceError::TypeError(error) => match *error.error {
                                engine::TypeError::MissingInstance(tr, params, _, _) => {
                                    vec![(tr, params)]
                                }
                                _ => return Err(FindInstanceError::TypeError(error)),
                            },
                            FindInstanceError::MultipleCandidates(_) => Vec::new(),
                            _ => return Err(error),
                        };

                        error_candidates.push((instance_span, unsatisfied_bounds));
                        continue 'check;
                    }
                }

                if bound.is_some() {
                    info.instance_stack.entry(tr).or_default().pop();
                }

                let ctx = mem::replace(&mut self.ctx, prev_ctx);
                candidates.push((ctx, Some((id, instance_bounds)), instance_span));

                if tr_decl.attributes.allow_overlapping_instances {
                    break 'check; // use the first available instance
                }
            }

            Ok(candidates)
        });

        Err(FindInstanceError::TypeError(self.error(
            engine::TypeError::MissingInstance(
                tr,
                params,
                bound.as_ref().map(|(_, _, bound)| bound.span),
                error_candidates,
            ),
            use_id,
            use_span,
        )))
    }

    fn extract_params(
        &mut self,
        actual_ty: engine::UnresolvedType,
        params: Vec<TypeParameterId>,
        generic_ty: engine::Type,
    ) -> engine::GenericSubstitutions {
        let mut substitutions = engine::GenericSubstitutions::new();
        for param in params {
            let mut default = self.get_default_for_param(param, Some(&mut substitutions));
            if let Some(ty) = &mut default {
                self.add_substitutions(ty, &mut substitutions);
            }

            substitutions.insert(
                param,
                self.unresolved_ty(
                    engine::UnresolvedTypeKind::Variable(self.ctx.new_variable(default)),
                    None,
                ),
            );
        }

        let prev_ctx = self.ctx.clone();

        let mut generic_ty = engine::UnresolvedType::from(generic_ty.clone());
        generic_ty.instantiate_with(&self.ctx, &substitutions);

        let _ = self.ctx.unify(actual_ty, generic_ty);
        for ty in substitutions.values_mut() {
            ty.apply(&self.ctx);
        }

        self.ctx = prev_ctx;

        substitutions
    }
}

impl Typechecker {
    fn finalize_expr(
        &mut self,
        expr: MonomorphizedExpression,
        report_error_if_unresolved: bool,
    ) -> Expression {
        let expr = self.finalize_expr_inner(expr, report_error_if_unresolved);

        self.check_exhaustiveness(&expr);
        self.check_access(&expr);

        expr
    }

    fn finalize_expr_inner(
        &mut self,
        expr: MonomorphizedExpression,
        report_error_if_unresolved: bool,
    ) -> Expression {
        let (ty, resolved) = expr.ty.finalize(&self.ctx);
        if !resolved && report_error_if_unresolved {
            self.add_error(self.error(
                engine::TypeError::UnresolvedType(expr.ty.clone(), None),
                expr.id,
                expr.span,
            ));
        }

        let kind = (|| match expr.kind {
            MonomorphizedExpressionKind::Error(trace) => ExpressionKind::Error(trace),
            MonomorphizedExpressionKind::Marker => ExpressionKind::Marker,
            MonomorphizedExpressionKind::BoundInstance(id) => ExpressionKind::BoundInstance(id),
            MonomorphizedExpressionKind::ErrorConstant(id) => ExpressionKind::ErrorConstant(id),
            MonomorphizedExpressionKind::UnresolvedTrait(id, candidates) => {
                self.add_error(self.error(
                    engine::TypeError::UnresolvedType(expr.ty, candidates),
                    expr.id,
                    expr.span,
                ));

                ExpressionKind::UnresolvedTrait(id)
            }
            MonomorphizedExpressionKind::UnresolvedConstant(id, candidates) => {
                self.add_error(self.error(
                    engine::TypeError::UnresolvedType(expr.ty, candidates),
                    expr.id,
                    expr.span,
                ));

                ExpressionKind::UnresolvedConstant(id)
            }
            MonomorphizedExpressionKind::Constant(id) => ExpressionKind::Constant(id),
            MonomorphizedExpressionKind::Variable(var) => ExpressionKind::Variable(var),
            MonomorphizedExpressionKind::Text(text) => ExpressionKind::Text(text),
            MonomorphizedExpressionKind::Number(number) => {
                match parse_number!(number, ExpressionKind, &ty, TypeKind) {
                    Some(Ok(number)) => number,
                    Some(Err(error)) => {
                        self.add_error(self.error(error, expr.id, expr.span));
                        ExpressionKind::error(&self.compiler)
                    }
                    None => {
                        self.add_error(self.error(
                            engine::TypeError::Mismatch(
                                self.unresolved_ty(
                                    engine::UnresolvedTypeKind::Builtin(
                                        engine::BuiltinType::Number,
                                    ),
                                    expr.span,
                                ),
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
                    .map(|expr| self.finalize_expr_inner(expr, report_error_if_unresolved))
                    .collect(),
                top_level,
            ),
            MonomorphizedExpressionKind::Call(func, input, first) => ExpressionKind::Call(
                Box::new(self.finalize_expr_inner(*func, report_error_if_unresolved)),
                Box::new(self.finalize_expr_inner(*input, report_error_if_unresolved)),
                first,
            ),
            MonomorphizedExpressionKind::Function(pattern, body, captures) => {
                let input_ty = match &ty.kind {
                    engine::TypeKind::Function(input, _) => input.clone(),
                    _ => return ExpressionKind::error(&self.compiler),
                };

                ExpressionKind::Function(
                    self.finalize_pattern(pattern, &input_ty),
                    Box::new(self.finalize_expr_inner(*body, report_error_if_unresolved)),
                    captures
                        .into_iter()
                        .map(|(var, ty)| {
                            // We don't care about raising an error here because
                            // it will be raised during the variable's definition.
                            let (ty, _) = ty.finalize(&self.ctx);
                            (var, ty)
                        })
                        .collect(),
                )
            }
            MonomorphizedExpressionKind::When(input, arms) => {
                let input = self.finalize_expr_inner(*input, report_error_if_unresolved);

                let arms = arms
                    .into_iter()
                    .map(|arm| Arm {
                        span: arm.span,
                        pattern: self.finalize_pattern(arm.pattern, &input.ty),
                        guard: arm
                            .guard
                            .map(|expr| self.finalize_expr_inner(expr, report_error_if_unresolved)),
                        body: self.finalize_expr_inner(arm.body, report_error_if_unresolved),
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
                        .map(|expr| self.finalize_expr_inner(expr, report_error_if_unresolved))
                        .collect(),
                )
            }
            MonomorphizedExpressionKind::Intrinsic(func, inputs) => ExpressionKind::Intrinsic(
                func,
                inputs
                    .into_iter()
                    .map(|expr| self.finalize_expr_inner(expr, report_error_if_unresolved))
                    .collect(),
            ),
            MonomorphizedExpressionKind::Plugin(path, name, inputs) => ExpressionKind::Plugin(
                path,
                name,
                inputs
                    .into_iter()
                    .map(|expr| self.finalize_expr_inner(expr, report_error_if_unresolved))
                    .collect(),
            ),
            MonomorphizedExpressionKind::Initialize(pattern, value) => {
                let value = self.finalize_expr_inner(*value, report_error_if_unresolved);

                ExpressionKind::Initialize(
                    self.finalize_pattern(pattern, &value.ty),
                    Box::new(value),
                )
            }
            MonomorphizedExpressionKind::Structure(fields) => ExpressionKind::Structure(
                fields
                    .into_iter()
                    .map(|expr| self.finalize_expr_inner(expr, report_error_if_unresolved))
                    .collect(),
            ),
            MonomorphizedExpressionKind::Variant(index, values) => ExpressionKind::Variant(
                index,
                values
                    .into_iter()
                    .map(|expr| self.finalize_expr_inner(expr, report_error_if_unresolved))
                    .collect(),
            ),
            MonomorphizedExpressionKind::Tuple(exprs) => ExpressionKind::Tuple(
                exprs
                    .into_iter()
                    .map(|expr| self.finalize_expr_inner(expr, report_error_if_unresolved))
                    .collect(),
            ),
            MonomorphizedExpressionKind::Format(segments, trailing_segment) => {
                ExpressionKind::Format(
                    segments
                        .into_iter()
                        .map(|(text, expr)| {
                            (
                                text,
                                self.finalize_expr_inner(expr, report_error_if_unresolved),
                            )
                        })
                        .collect(),
                    trailing_segment,
                )
            }
            MonomorphizedExpressionKind::With((id, value), body) => ExpressionKind::With(
                (
                    id,
                    Box::new(self.finalize_expr_inner(*value, report_error_if_unresolved)),
                ),
                Box::new(self.finalize_expr_inner(*body, report_error_if_unresolved)),
            ),
            MonomorphizedExpressionKind::ContextualConstant(id) => {
                ExpressionKind::ContextualConstant(id)
            }
            MonomorphizedExpressionKind::End(value) => ExpressionKind::End(Box::new(
                self.finalize_expr_inner(*value, report_error_if_unresolved),
            )),
            MonomorphizedExpressionKind::UnresolvedExtend(_, _) => {
                self.add_error(self.error(
                    engine::TypeError::UnresolvedType(expr.ty, None),
                    expr.id,
                    expr.span,
                ));

                ExpressionKind::UnresolvedExtend
            }
            MonomorphizedExpressionKind::Extend(value, fields) => ExpressionKind::Extend(
                Box::new(self.finalize_expr_inner(*value, report_error_if_unresolved)),
                fields
                    .into_iter()
                    .map(|(index, field)| {
                        (
                            index,
                            self.finalize_expr_inner(field, report_error_if_unresolved),
                        )
                    })
                    .collect(),
            ),
            MonomorphizedExpressionKind::Semantics(semantics, expr) => ExpressionKind::Semantics(
                semantics,
                Box::new(self.finalize_expr_inner(*expr, report_error_if_unresolved)),
            ),
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
                    match parse_number!(number, PatternKind, input_ty, TypeKind) {
                        Some(Ok(number)) => number,
                        Some(Err(error)) => {
                            self.add_error(self.error(error, None, pattern.span));
                            PatternKind::error(&self.compiler)
                        }
                        None => {
                            self.add_error(self.error(
                                engine::TypeError::Mismatch(
                                    self.unresolved_ty(
                                        engine::UnresolvedTypeKind::Builtin(
                                            engine::BuiltinType::Number,
                                        ),
                                        pattern.span,
                                    ),
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
                MonomorphizedPatternKind::UnresolvedDestructure(_, _) => {
                    self.add_error(self.error(
                        engine::TypeError::UnresolvedType(input_ty.clone().into(), None),
                        None,
                        pattern.span,
                    ));

                    PatternKind::UnresolvedDestructure
                }
                MonomorphizedPatternKind::Destructure(id, fields) => {
                    let input_tys = match &input_ty.kind {
                        engine::TypeKind::Named(_, _, engine::TypeStructure::Structure(fields)) => {
                            fields.clone()
                        }
                        engine::TypeKind::Named(_, _, engine::TypeStructure::Recursive(id)) => {
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
                MonomorphizedPatternKind::UnresolvedVariant(_, _, _) => {
                    self.add_error(self.error(
                        engine::TypeError::UnresolvedType(input_ty.clone().into(), None),
                        None,
                        pattern.span,
                    ));

                    PatternKind::UnresolvedVariant
                }
                MonomorphizedPatternKind::Variant(id, index, values) => {
                    let input_tys = match &input_ty.kind {
                        engine::TypeKind::Named(
                            _,
                            _,
                            engine::TypeStructure::Enumeration(variants),
                        ) => variants[index.into_inner()].clone(),
                        engine::TypeKind::Named(_, _, engine::TypeStructure::Recursive(id)) => {
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
                    let input_tys = match &input_ty.kind {
                        engine::TypeKind::Tuple(tys) => tys,
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
                let expr = self.finalize_expr(expr, true);

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

        let decl = self.top_level.declarations.syntaxes.get(&id)?.clone();

        let decl = SyntaxDecl {
            name: decl
                .name
                .unwrap_or_else(|| InternedString::new("<unknown>")),
            span: decl.span,
            operator: decl.value.operator,
            keyword: decl.value.keyword,
            uses: decl.uses.into_iter().collect(),
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

        let decl = self.top_level.declarations.types.get(&id)?.clone();

        for &param in &decl.value.parameters {
            let (span, inferred, has_default) = self
                .with_type_parameter_decl(param, |decl| {
                    (decl.span, decl.infer, decl.default.is_some())
                })
                .unwrap();

            if inferred {
                self.compiler
                    .add_error(span, "inferred type parameters are only allowed in constants and instance definitions", "");
            }

            if has_default {
                self.compiler.add_error(
                    span,
                    "default types are only allowed in constants and instance definitions",
                    "",
                );
            }
        }

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
                                    self.convert_finalized_type_annotation(
                                        Some(engine::TypeReason::StructureField(field.ty.span)),
                                        field.ty,
                                    ),
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
                                        (
                                            ty.clone(),
                                            self.convert_finalized_type_annotation(
                                                Some(engine::TypeReason::VariantElement(ty.span)),
                                                ty,
                                            ),
                                        )
                                    })
                                    .collect()
                            })
                            .collect(),
                        variant_names,
                    }
                }
                lower::TypeDeclarationKind::Alias(ty) => {
                    TypeDeclKind::Alias(self.convert_finalized_type_annotation(
                        Some(engine::TypeReason::TypeAlias(ty.span)),
                        ty,
                    ))
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
                        self.convert_finalized_type_annotation(None, annotation),
                        replacement,
                    )
                })
                .collect(),
            attributes: decl.value.attributes,
            uses: decl.uses.into_iter().collect(),
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

        let decl = self.top_level.declarations.traits.get(&id)?.clone();

        for &param in &decl.value.parameters {
            let (span, inferred, has_default) = self
                .with_type_parameter_decl(param, |decl| {
                    (decl.span, decl.infer, decl.default.is_some())
                })
                .unwrap();

            if inferred {
                self.compiler
                    .add_error(span, "inferred type parameters are only allowed in constants and instance definitions", "");
            }

            if has_default {
                self.compiler.add_error(
                    span,
                    "default types are only allowed in constants and instance definitions",
                    "",
                );
            }
        }

        let decl = TraitDecl {
            name: decl
                .name
                .unwrap_or_else(|| InternedString::new("<unknown>")),
            span: decl.span,
            params: decl.value.parameters,
            ty_annotation: decl.value.ty.clone(),
            ty: decl.value.ty.map(|ty| {
                self.convert_finalized_type_annotation(Some(engine::TypeReason::Trait(ty.span)), ty)
            }),
            attributes: decl.value.attributes,
            uses: decl.uses.into_iter().collect(),
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

        let decl = self.top_level.declarations.constants.get(&id)?.clone();

        let mut params = decl.value.parameters;

        let ty = self.convert_generic_type_annotation(
            Some(engine::TypeReason::Annotation(decl.span)),
            decl.value.ty.clone(),
            &mut params,
        );

        macro_rules! convert_bounds {
            ($bounds:expr) => {
                $bounds
                    .clone()
                    .into_iter()
                    .map(|bound| Bound {
                        span: bound.span,
                        trait_id: bound.tr,
                        params: bound
                            .parameters
                            .into_iter()
                            .map(|ty| {
                                self.convert_generic_type_annotation(
                                    Some(engine::TypeReason::Bound(ty.span)),
                                    ty,
                                    &mut params,
                                )
                                .into()
                            })
                            .collect(),
                    })
                    .collect::<Vec<_>>()
            };
        }

        let bounds = convert_bounds!(decl.value.bounds);

        let decl = ConstantDecl {
            name: decl
                .name
                .unwrap_or_else(|| InternedString::new("<unknown>")),
            span: decl.span,
            params,
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
                .top_level
                .specializations
                .get(&id)
                .cloned()
                .unwrap_or_default(),
            enumeration_ty: decl.value.enumeration_ty,
            attributes: decl.value.attributes,
            body: decl.value.value,
            uses: decl.uses.into_iter().collect(),
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
        let decl = self.top_level.declarations.instances.get(&id)?.clone();

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

        let mut decl_params = decl.value.parameters;

        let mut params = decl
            .value
            .tr_parameters
            .clone()
            .into_iter()
            .map(|ty| {
                self.convert_generic_type_annotation(
                    Some(engine::TypeReason::Instance(ty.span)),
                    ty,
                    &mut decl_params,
                )
            })
            .collect::<Vec<_>>();

        for param in tr.params.iter().skip(params.len()) {
            let name = match self.with_type_parameter_decl(*param, |decl| decl.name) {
                Some(name) => name,
                None => {
                    params.push(self.ty(engine::TypeKind::Error, None));
                    continue;
                }
            };

            self.compiler.add_diagnostic(
                self.compiler
                    .error(
                        decl.span,
                        format!(
                            "missing type for trait parameter `{}`",
                            name.as_deref().unwrap_or("_")
                        ),
                        "syntax-error",
                    )
                    .fix_with("add a type", FixRange::after(decl.span.first()), "{%type%}"),
            );

            params.push(self.ty(engine::TypeKind::Error, None));
        }

        let has_bounds = !decl.value.bounds.is_empty();

        macro_rules! convert_bounds {
            ($bounds:expr) => {
                $bounds
                    .clone()
                    .into_iter()
                    .map(|bound| Bound {
                        span: bound.span,
                        trait_id: bound.tr,
                        params: bound
                            .parameters
                            .into_iter()
                            .map(|ty| {
                                self.convert_generic_type_annotation(
                                    Some(engine::TypeReason::Bound(ty.span)),
                                    ty,
                                    &mut decl_params,
                                )
                                .into()
                            })
                            .collect(),
                    })
                    .collect::<Vec<_>>()
            };
        }

        let bounds = convert_bounds!(decl.value.bounds);

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
                    for (instance_param_ty, other_param_ty) in
                        params.clone().into_iter().zip(other.trait_params)
                    {
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
                let mut error = self.compiler.error(
                    decl.span,
                    format!(
                        "this instance overlaps with {} other instances",
                        colliding_instances.len()
                    ),
                    "colliding-instances",
                );

                if has_bounds {
                    error = error.note(Note::secondary(decl.span, "this instance may have different bounds than the others, but one type could satisfy the bounds on more than one of these instances simultaneously"));
                }

                for span in colliding_instances {
                    error = error.note(Note::secondary(
                        span,
                        "this instance could apply to the same type(s)",
                    ));
                }

                self.compiler.add_diagnostic(error);
            }
        }

        let decl = InstanceDecl {
            span: decl.span,
            params: decl_params,
            bounds,
            bound_annotations: decl
                .value
                .bounds
                .into_iter()
                .map(|bound| (bound.tr, bound.parameters))
                .collect(),
            trait_id,
            trait_params: params,
            trait_param_annotations: decl.value.tr_parameters,
            body: decl.value.value.map(|value| value.value),
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

        let decl = self.top_level.declarations.builtin_types.get(&id)?.clone();

        let decl = BuiltinTypeDecl {
            name: decl
                .name
                .unwrap_or_else(|| InternedString::new("<unknown>")),
            span: decl.span,
            attributes: decl.value.attributes,
            uses: decl.uses.into_iter().collect(),
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
            .top_level
            .declarations
            .type_parameters
            .get(&id)?
            .clone();

        let decl = TypeParameterDecl {
            name: decl.name,
            span: decl.span,
            infer: decl.value.infer,
            default: decl.value.default.map(|ty| {
                self.convert_finalized_type_annotation(
                    Some(engine::TypeReason::DefaultType(ty.span)),
                    ty,
                )
            }),
            uses: decl.uses.into_iter().collect(),
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
        // HACK: We do not retrieve the cached value because `ty` changes
        // depending on the expression currently being monomorphized, and we
        // always want the most up-to-date type.

        let decl = self.top_level.declarations.variables.get(&id)?.clone();

        let decl = VariableDecl {
            name: decl.name,
            span: decl.span,
            ty,
            uses: decl.uses.into_iter().collect(),
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
            .top_level
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
            uses: decl.uses.into_iter().collect(),
        };

        Some(f(self
            .declarations
            .borrow_mut()
            .builtin_syntaxes
            .entry(id)
            .or_insert(decl)))
    }

    fn with_snippet_decl<T>(&self, id: SnippetId, f: impl FnOnce(&SnippetDecl) -> T) -> Option<T> {
        if let Some(decl) = self.declarations.borrow().snippets.get(&id) {
            return Some(f(decl));
        }

        let decl = self.top_level.declarations.snippets.get(&id)?.clone();

        let decl = SnippetDecl {
            name: decl
                .name
                .unwrap_or_else(|| InternedString::new("<unknown>")),
            span: decl.span,
            expr: decl.value.expr,
            wrap: decl.value.wrap,
        };

        Some(f(self
            .declarations
            .borrow_mut()
            .snippets
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
            if !substitutions.contains_key(&param) {
                let mut default = self.get_default_for_param(param, Some(substitutions));
                if let Some(ty) = &mut default {
                    self.add_substitutions(ty, substitutions);
                }

                let ty = self.unresolved_ty(
                    engine::UnresolvedTypeKind::Variable(self.ctx.new_variable(default)),
                    None,
                );

                substitutions.insert(param, ty);
            }
        }

        ty.instantiate_with(&self.ctx, substitutions);
    }

    fn instantiate_generics(&mut self, ty: &mut engine::UnresolvedType) {
        ty.apply(&self.ctx);

        let mut substitutions = GenericSubstitutions::new();
        for param in ty.params() {
            let mut default = self.get_default_for_param(param, Some(&mut substitutions));
            if let Some(ty) = &mut default {
                self.add_substitutions(ty, &mut substitutions);
            }

            let ty = self.unresolved_ty(
                engine::UnresolvedTypeKind::Variable(self.ctx.new_variable(default)),
                None,
            );

            substitutions.insert(param, ty);
        }

        ty.instantiate_with(&self.ctx, &substitutions);
    }

    fn convert_type_annotation(
        &self,
        reason: Option<engine::TypeReason>,
        annotation: TypeAnnotation,
    ) -> engine::UnresolvedType {
        self.convert_type_annotation_inner(
            reason,
            annotation,
            &mut |typechecker, span| {
                Some(self.unresolved_ty(
                    engine::UnresolvedTypeKind::Variable(typechecker.ctx.new_variable(None)),
                    span,
                ))
            },
            &mut Vec::new(),
        )
    }

    fn convert_generic_type_annotation(
        &self,
        reason: Option<engine::TypeReason>,
        annotation: TypeAnnotation,
        params: &mut Vec<TypeParameterId>,
    ) -> engine::Type {
        let span = annotation.span;

        let ty = self.convert_type_annotation_inner(
            reason,
            annotation,
            &mut |typechecker, span| {
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
                            infer: false,
                            default: None,
                            uses: HashSet::from([span]),
                        },
                    );

                params.push(param);

                Some(self.unresolved_ty(engine::UnresolvedTypeKind::Parameter(param), span))
            },
            &mut Vec::new(),
        );

        let (finalized_ty, resolved) = ty.finalize(&self.ctx);
        if !resolved {
            self.add_error(self.error(engine::TypeError::UnresolvedType(ty, None), None, span));
        }

        finalized_ty
    }

    fn convert_finalized_type_annotation(
        &self,
        reason: Option<engine::TypeReason>,
        annotation: TypeAnnotation,
    ) -> engine::Type {
        let span = annotation.span;

        let ty = self.convert_type_annotation_inner(
            reason,
            annotation,
            &mut |_, _| None,
            &mut Vec::new(),
        );

        let (finalized_ty, resolved) = ty.finalize(&self.ctx);
        if !resolved {
            self.add_error(self.error(engine::TypeError::UnresolvedType(ty, None), None, span));
        }

        finalized_ty
    }

    fn convert_type_annotation_inner(
        &self,
        reason: Option<engine::TypeReason>,
        annotation: TypeAnnotation,
        convert_placeholder: &mut impl FnMut(&Self, SpanList) -> Option<engine::UnresolvedType>,
        stack: &mut Vec<TypeId>,
    ) -> engine::UnresolvedType {
        let ty = (|| match annotation.kind {
            TypeAnnotationKind::Error(_) => {
                self.unresolved_ty(engine::UnresolvedTypeKind::Error, annotation.span)
            }
            TypeAnnotationKind::Placeholder => {
                if let Some(ty) = convert_placeholder(self, annotation.span) {
                    ty
                } else {
                    self.compiler.add_diagnostic(
                        self.compiler
                            .error(
                                annotation.span,
                                "type placeholder is not allowed here",
                                "unexpected-type-placeholder",
                            )
                            .fix_with(
                                "provide a type in place of the placeholder",
                                FixRange::replace(annotation.span.first()),
                                "{%type%}",
                            ),
                    );

                    self.unresolved_ty(engine::UnresolvedTypeKind::Error, annotation.span)
                }
            }
            TypeAnnotationKind::Named(id, params) => {
                let mut params = params
                    .into_iter()
                    .map(|param| {
                        self.convert_type_annotation_inner(None, param, convert_placeholder, stack)
                    })
                    .collect::<Vec<_>>();

                let ty = self.top_level.declarations.types.get(&id).unwrap().clone();

                for param in ty.value.parameters.iter().skip(params.len()) {
                    let name =
                        match self.with_type_parameter_decl(*param, |decl| decl.name) {
                            Some(name) => name,
                            None => {
                                params.push(self.unresolved_ty(
                                    engine::UnresolvedTypeKind::Error,
                                    annotation.span,
                                ));
                                continue;
                            }
                        };

                    self.compiler.add_error(
                        annotation.span,
                        format!(
                            "missing type for type parameter `{}`",
                            name.as_deref().unwrap_or("<unknown>")
                        ),
                        "syntax-error",
                    );

                    params.push(
                        self.unresolved_ty(engine::UnresolvedTypeKind::Error, annotation.span),
                    );
                }

                if stack.contains(&id) {
                    return self.unresolved_ty(
                        engine::UnresolvedTypeKind::Named(
                            id,
                            params,
                            engine::TypeStructure::Recursive(id),
                        ),
                        annotation.span,
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
                        let mut ty = self.convert_type_annotation_inner(
                            None,
                            $ty,
                            convert_placeholder,
                            stack,
                        );

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

                self.unresolved_ty(
                    engine::UnresolvedTypeKind::Named(id, params, structure),
                    annotation.span,
                )
            }
            TypeAnnotationKind::Parameter(id) => {
                self.unresolved_ty(engine::UnresolvedTypeKind::Parameter(id), annotation.span)
            }
            TypeAnnotationKind::Builtin(id, mut parameters) => {
                let builtin_ty = self
                    .top_level
                    .declarations
                    .builtin_types
                    .get(&id)
                    .unwrap()
                    .clone();

                match builtin_ty.value.kind {
                    lower::BuiltinTypeDeclarationKind::Number => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                annotation.span,
                                "`Number` does not accept parameters",
                                "syntax-error",
                            );
                        }

                        self.unresolved_ty(
                            engine::UnresolvedTypeKind::Builtin(engine::BuiltinType::Number),
                            annotation.span,
                        )
                    }
                    lower::BuiltinTypeDeclarationKind::Integer => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                annotation.span,
                                "`Integer` does not accept parameters",
                                "syntax-error",
                            );
                        }

                        self.unresolved_ty(
                            engine::UnresolvedTypeKind::Builtin(engine::BuiltinType::Integer),
                            annotation.span,
                        )
                    }
                    lower::BuiltinTypeDeclarationKind::Natural => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                annotation.span,
                                "`Natural` does not accept parameters",
                                "syntax-error",
                            );
                        }
                        self.unresolved_ty(
                            engine::UnresolvedTypeKind::Builtin(engine::BuiltinType::Natural),
                            annotation.span,
                        )
                    }
                    lower::BuiltinTypeDeclarationKind::Byte => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                annotation.span,
                                "`Byte` does not accept parameters",
                                "syntax-error",
                            );
                        }

                        self.unresolved_ty(
                            engine::UnresolvedTypeKind::Builtin(engine::BuiltinType::Byte),
                            annotation.span,
                        )
                    }
                    lower::BuiltinTypeDeclarationKind::Signed => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                annotation.span,
                                "`Signed` does not accept parameters",
                                "syntax-error",
                            );
                        }

                        self.unresolved_ty(
                            engine::UnresolvedTypeKind::Builtin(engine::BuiltinType::Signed),
                            annotation.span,
                        )
                    }
                    lower::BuiltinTypeDeclarationKind::Unsigned => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                annotation.span,
                                "`Unsigned` does not accept parameters",
                                "syntax-error",
                            );
                        }

                        self.unresolved_ty(
                            engine::UnresolvedTypeKind::Builtin(engine::BuiltinType::Unsigned),
                            annotation.span,
                        )
                    }
                    lower::BuiltinTypeDeclarationKind::Float => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                annotation.span,
                                "`Float` does not accept parameters",
                                "syntax-error",
                            );
                        }

                        self.unresolved_ty(
                            engine::UnresolvedTypeKind::Builtin(engine::BuiltinType::Float),
                            annotation.span,
                        )
                    }
                    lower::BuiltinTypeDeclarationKind::Double => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                annotation.span,
                                "`Double` does not accept parameters",
                                "syntax-error",
                            );
                        }

                        self.unresolved_ty(
                            engine::UnresolvedTypeKind::Builtin(engine::BuiltinType::Double),
                            annotation.span,
                        )
                    }
                    lower::BuiltinTypeDeclarationKind::Text => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                annotation.span,
                                "`Text` does not accept parameters",
                                "syntax-error",
                            );
                        }

                        self.unresolved_ty(
                            engine::UnresolvedTypeKind::Builtin(engine::BuiltinType::Text),
                            annotation.span,
                        )
                    }
                    lower::BuiltinTypeDeclarationKind::List => {
                        if parameters.is_empty() {
                            self.compiler.add_error(
                                annotation.span,
                                "`List` accepts 1 parameter, but none were provided",
                                "syntax-error",
                            );

                            self.unresolved_ty(
                                engine::UnresolvedTypeKind::Builtin(engine::BuiltinType::List(
                                    Box::new(
                                        self.unresolved_ty(engine::UnresolvedTypeKind::Error, None),
                                    ),
                                )),
                                annotation.span,
                            )
                        } else {
                            if parameters.len() > 1 {
                                self.compiler.add_error(
                                    annotation.span,
                                    format!(
                                        "`List` accepts 1 parameter, but {} were provided",
                                        parameters.len()
                                    ),
                                    "syntax-error",
                                );
                            }

                            self.unresolved_ty(
                                engine::UnresolvedTypeKind::Builtin(engine::BuiltinType::List(
                                    Box::new(self.convert_type_annotation_inner(
                                        None,
                                        parameters.pop().unwrap(),
                                        convert_placeholder,
                                        stack,
                                    )),
                                )),
                                annotation.span,
                            )
                        }
                    }
                    lower::BuiltinTypeDeclarationKind::Reference => {
                        if parameters.is_empty() {
                            self.compiler.add_error(
                                annotation.span,
                                "`Reference` accepts 1 parameter, but none were provided",
                                "syntax-error",
                            );

                            self.unresolved_ty(
                                engine::UnresolvedTypeKind::Builtin(
                                    engine::BuiltinType::Reference(Box::new(
                                        self.unresolved_ty(engine::UnresolvedTypeKind::Error, None),
                                    )),
                                ),
                                annotation.span,
                            )
                        } else {
                            if parameters.len() > 1 {
                                self.compiler.add_error(
                                    annotation.span,
                                    format!(
                                        "`Reference` accepts 1 parameter, but {} were provided",
                                        parameters.len()
                                    ),
                                    "syntax-error",
                                );
                            }

                            self.unresolved_ty(
                                engine::UnresolvedTypeKind::Builtin(
                                    engine::BuiltinType::Reference(Box::new(
                                        self.convert_type_annotation_inner(
                                            None,
                                            parameters.pop().unwrap(),
                                            convert_placeholder,
                                            stack,
                                        ),
                                    )),
                                ),
                                annotation.span,
                            )
                        }
                    }
                    lower::BuiltinTypeDeclarationKind::Ui => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                annotation.span,
                                "`UI` does not accept parameters",
                                "syntax-error",
                            );
                        }

                        self.unresolved_ty(
                            engine::UnresolvedTypeKind::Builtin(engine::BuiltinType::Ui),
                            annotation.span,
                        )
                    }
                    lower::BuiltinTypeDeclarationKind::TaskGroup => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                annotation.span,
                                "`Task-Group` does not accept parameters",
                                "syntax-error",
                            );
                        }

                        self.unresolved_ty(
                            engine::UnresolvedTypeKind::Builtin(engine::BuiltinType::TaskGroup),
                            annotation.span,
                        )
                    }
                    lower::BuiltinTypeDeclarationKind::Hasher => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                annotation.span,
                                "`Hasher` does not accept parameters",
                                "syntax-error",
                            );
                        }

                        self.unresolved_ty(
                            engine::UnresolvedTypeKind::Builtin(engine::BuiltinType::Hasher),
                            annotation.span,
                        )
                    }
                }
            }
            TypeAnnotationKind::Function(input, output) => self.unresolved_ty(
                engine::UnresolvedTypeKind::Function(
                    Box::new(self.convert_type_annotation_inner(
                        Some(engine::TypeReason::FunctionInput(input.span)),
                        *input,
                        convert_placeholder,
                        stack,
                    )),
                    Box::new(self.convert_type_annotation_inner(
                        Some(engine::TypeReason::FunctionInput(output.span)),
                        *output,
                        convert_placeholder,
                        stack,
                    )),
                ),
                annotation.span,
            ),
            TypeAnnotationKind::Tuple(tys) => self.unresolved_ty(
                engine::UnresolvedTypeKind::Tuple(
                    tys.into_iter()
                        .map(|ty| {
                            self.convert_type_annotation_inner(None, ty, convert_placeholder, stack)
                        })
                        .collect(),
                ),
                annotation.span,
            ),
        })();

        ty.with_reason(reason)
    }

    fn substitute_trait_params(
        &mut self,
        trait_id: TraitId,
        params: Vec<engine::UnresolvedType>,
        span: SpanList,
    ) -> engine::UnresolvedType {
        let (trait_ty, trait_params) = self
            .with_trait_decl(trait_id, |decl| {
                (
                    decl.ty
                        .clone()
                        .unwrap_or(self.ty(engine::TypeKind::Error, decl.span)),
                    decl.params.clone(),
                )
            })
            .expect("instance should have already been accessed at least once");

        if trait_params.len() != params.len() {
            self.compiler.add_error(
                span,
                "wrong number of parameters provided to trait",
                "syntax-error",
            );

            return self.unresolved_ty(engine::UnresolvedTypeKind::Error, span);
        }

        let substitutions = trait_params
            .into_iter()
            .zip(params)
            .collect::<GenericSubstitutions>();

        let mut instance_ty = engine::UnresolvedType::from(trait_ty);
        instance_ty.instantiate_with(&self.ctx, &substitutions);

        instance_ty
    }

    fn get_default_for_param(
        &self,
        param: TypeParameterId,
        substitutions: Option<&mut engine::GenericSubstitutions>,
    ) -> Option<engine::UnresolvedType> {
        let mut default = self
            .with_type_parameter_decl(param, |decl| {
                decl.default.clone().map(engine::UnresolvedType::from)
            })
            .flatten()?;

        if let Some(substitutions) = substitutions {
            self.add_substitutions(&mut default, substitutions);
            default.apply(&self.ctx);
        }

        Some(default)
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
                    span,
                    format!("cannot load file `{}`: {}", path, error),
                    "missing-file",
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
                self.compiler
                    .add_error(span, format!("error while resolving plugin: {error}"), "");

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
            ($x:ident, $f:expr, $default:expr $(,)?) => {
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
            String::from("<unknown>"),
        );

        let trait_names = getter!(
            trait,
            |name: InternedString| name.to_string(),
            String::from("<unknown>"),
        );

        let param_names = getter!(
            type_parameter,
            |name: Option<_>| name
                .as_ref()
                .map(ToString::to_string)
                .unwrap_or_else(|| String::from("_")),
            String::from("<unknown>"),
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
                        engine::TypeError::UnresolvedType(ty, _candidates) => {
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
                    if vars.is_empty()
                        || vars
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
        match &mut *error.error {
            engine::TypeError::Mismatch(actual, expected) => {
                actual.apply(&self.ctx);
                expected.apply(&self.ctx);

                // If a function's type doesn't match but its inputs do, then
                // display the error at the start of the call chain instead of the
                // function itself
                loop {
                    if let engine::UnresolvedTypeKind::Function(actual_input, actual_output) =
                        &actual.kind
                    {
                        if let engine::UnresolvedTypeKind::Function(
                            expected_input,
                            expected_output,
                        ) = &expected.kind
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
                                        if let Some(call) = root.as_root_query_parent_of(id) {
                                            if let ExpressionKind::Call(_, input, _) = &call.kind {
                                                *actual = expected_input;
                                                *expected = actual_input;
                                                error.expr = Some(input.id);
                                                error.span = input.span;
                                            }
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
            engine::TypeError::UnresolvedType(ty, _) => {
                ty.apply(&self.ctx);

                loop {
                    let mut modified = false;

                    if let Some(expr) = error.expr.and_then(|id| self.expr_for(id)) {
                        match expr.kind {
                            ExpressionKind::Block(mut statements, _) => {
                                if let Some(expr) = statements.pop() {
                                    modified = true;
                                    error.expr = Some(expr.id);
                                    error.span = expr.span;
                                }
                            }
                            ExpressionKind::Function(_, expr, _) => {
                                if let engine::UnresolvedTypeKind::Function(input, output) =
                                    &ty.kind
                                {
                                    if !input.contains_vars() && output.contains_vars() {
                                        modified = true;
                                        error.expr = Some(expr.id);
                                        error.span = expr.span;
                                        *ty = output.as_ref().clone();
                                    }
                                }
                            }
                            _ => {}
                        }
                    }

                    if !modified {
                        break;
                    }
                }
            }
            _ => {}
        }

        let operator_notes = || {
            error
                .span
                .first()
                .expanded_from_operator
                .map(|(name, left, right)| {
                    std::iter::once(
                        Note::secondary(
                            DiagnosticLocation::from(error.span).use_caller_if_available(),
                            format!("`{name}` consumes all expressions on either side; you may be missing parentheses"),
                        )
                    )
                    .chain(left.as_deref().map(|(span, _)| Note::secondary(
                        *span,
                        format!("this is parsed as one single input to `{name}`"),
                    )))
                    .chain(right.as_deref().map(|(span, _)| Note::secondary(
                        *span,
                        format!("this is parsed as one single input to `{name}`"),
                    )))
                    .collect::<Vec<_>>()
                })
        };

        let operator_fix = |typechecker: &Self| {
            let (_, left, right) = error.span.first().expanded_from_operator?;

            match (left.as_deref(), right.as_deref()) {
                (None, None) | (Some(_), Some(_)) => None,
                (Some((_, left)), None) => {
                    let span = SpanList::join(*left.last().unwrap(), error.span).first();
                    let code = typechecker.compiler.source_code_for_span(span)?;

                    Some(Fix::new(
                        format!("add parentheses around `{code}`"),
                        FixRange::replace(span),
                        format!("({code})"),
                    ))
                }
                (None, Some((_, right))) => {
                    let span = SpanList::join(error.span, *right.first().unwrap()).first();
                    let code = typechecker.compiler.source_code_for_span(span)?;

                    Some(Fix::new(
                        format!("add parentheses around `{code}`"),
                        FixRange::replace(span),
                        format!("({code})"),
                    ))
                }
            }
        };

        let message_from_segments = |typechecker: &mut Self,
                                     (segments, trailing_segment): (
            Vec<(InternedString, TypeParameterId)>,
            Option<InternedString>,
        ),
                                     params: engine::GenericSubstitutions,
                                     show_code: bool,
                                     format: format::Format| {
            segments
                .iter()
                .map(|(text, param)| {
                    let mut ty = params.get(param).unwrap().clone();
                    ty.apply(&typechecker.ctx);

                    let code = ty
                        .info
                        .span
                        .and_then(|span| {
                            if !show_code {
                                return None;
                            }

                            let code = typechecker
                                .compiler
                                .single_line_source_code_for_span(span.first())?;

                            Some(format!("`{code}`"))
                        })
                        .unwrap_or_else(|| typechecker.format_type(ty, format));

                    text.to_string() + &code
                })
                .chain(trailing_segment.map(|text| text.to_string()))
                .collect()
        };

        let mut diagnostic = match *error.error {
            engine::TypeError::ErrorExpression => return,
            engine::TypeError::Recursive(_) => self.compiler.error(
                error.span,
                "could not determine the type of this expression because it references itself recursively",
                "recursive-type",
            ),
            engine::TypeError::Mismatch(mut actual, mut expected) => {
                actual.apply(&self.ctx);
                expected.apply(&self.ctx);

                let actual_ty = match &actual.kind {
                    engine::UnresolvedTypeKind::Named(id, params, _) => Some((
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

                let mut message = None;

                let mut notes = Vec::new();
                let mut fix = None;

                if let Some(reason) = expected.info.reason {
                    let note = match reason {
                        engine::TypeReason::Pattern(span) => Note::secondary(
                            span,
                            format!(
                                "expected because this pattern has type {}",
                                self.format_type(expected.clone(), format)
                            ),
                        ),
                        engine::TypeReason::Annotation(span) => {
                            Note::secondary(span, "expected because the type was annotated here")
                        }
                        engine::TypeReason::TypeAlias(span) => Note::secondary(
                            span,
                            format!(
                                "expected because the type was defined to be {} here",
                                self.format_type(expected.clone(), format)
                            ),
                        ),
                        engine::TypeReason::Trait(span) => Note::secondary(
                            span,
                            format!(
                                "expected because the trait was defined to be a value of type {}",
                                self.format_type(expected.clone(), format)
                            ),
                        ),
                        engine::TypeReason::Instance(span) => Note::secondary(
                            span,
                            format!(
                                "expected because this type in the instance is {}",
                                self.format_type(expected.clone(), format)
                            ),
                        ),
                        engine::TypeReason::StructureField(span) => Note::secondary(
                            span,
                            format!(
                                "expected because this field in the structure has type {}",
                                self.format_type(expected.clone(), format)
                            ),
                        ),
                        engine::TypeReason::VariantElement(span) => Note::secondary(
                            span,
                            format!(
                                "expected because this element of the variant has type {}",
                                self.format_type(expected.clone(), format)
                            ),
                        ),
                        engine::TypeReason::FunctionInput(span) => Note::secondary(
                            span,
                            format!(
                                "expected because the input to the function has type {}",
                                self.format_type(expected.clone(), format)
                            ),
                        ),
                        engine::TypeReason::FunctionOutput(span) => Note::secondary(
                            span,
                            format!(
                                "expected because the output of the function has type {}",
                                self.format_type(expected.clone(), format)
                            ),
                        ),
                        engine::TypeReason::Bound(span) => Note::secondary(
                            span,
                            format!(
                                "expected because this type in the bound is {}",
                                self.format_type(expected.clone(), format)
                            ),
                        ),
                        engine::TypeReason::DefaultType(span) => Note::secondary(
                            span,
                            format!(
                                "expected because the type defaults to {}",
                                self.format_type(expected.clone(), format)
                            ),
                        ),
                    };

                    notes.push(note);
                }

                {
                    let mut output = actual.clone();
                    let mut num_inputs = 0usize;
                    while let engine::UnresolvedTypeKind::Function(_, ty) = output.kind {
                        output = *ty;
                        num_inputs += 1;
                    }

                    if self.ctx.clone().unify(output, expected.clone()).is_ok() {
                        notes.push(Note::secondary(
                            error.span,
                            "try providing an input to this function",
                        ));

                        if let Some(source_code) = self
                            .compiler
                            .single_line_source_code_for_span(error.span.first())
                        {
                            let (description, replacement) = if num_inputs == 1 {
                                (
                                    String::from("provide input"),
                                    format!("({source_code} {{%input%}})"),
                                )
                            } else {
                                (
                                    format!("provide {num_inputs} inputs"),
                                    format!(
                                        "({}{})",
                                        source_code,
                                        (0..num_inputs)
                                            .map(|n| format!(" {{%input {}%}}", n + 1)) // TODO: Use parameter name defined in function
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

                if let engine::UnresolvedTypeKind::Function(expected_input, expected_output) =
                    &expected.kind
                {
                    if let engine::UnresolvedTypeKind::Function(actual_input, actual_output) =
                        &actual.kind
                    {
                        let mut actual_output = actual_output.as_ref().clone();
                        let mut expected_output = expected_output.as_ref().clone();

                        while let (
                            engine::UnresolvedTypeKind::Function(_, actual_output_inner),
                            engine::UnresolvedTypeKind::Function(_, expected_output_inner),
                        ) = (&actual_output.kind, &expected_output.kind)
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
                            message = Some(format!(
                                "function returns {}, but it should return {}",
                                self.format_type(actual_output, format),
                                self.format_type(expected_output, format)
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
                            message = Some(format!(
                                "function accepts {}, but it should accept {}",
                                self.format_type(actual_input.as_ref().clone(), format),
                                self.format_type(expected_input.as_ref().clone(), format)
                            ));
                        }
                    } else if let Some(id) = error.expr {
                        if let Some(root) = self.root_for(id.owner) {
                            if let Some(expr) = root.as_root_query(id) {
                                if matches!(expr.kind, ExpressionKind::Call(..)) {
                                    message = Some(String::from("too many inputs provided to function"));
                                } else {
                                    message = Some(format!(
                                        "cannot call {} value because it is not a function",
                                        self.format_type(actual.clone(), format),
                                    ));
                                }
                            }
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

                if let engine::UnresolvedTypeKind::Named(id, _, _) = &expected.kind {
                    let ty = self.declarations.borrow().types.get(id).unwrap().clone();

                    if let Some(source_code) = self
                        .compiler
                        .single_line_source_code_for_span(error.span.first())
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
                                    self.unresolved_ty(
                                        engine::UnresolvedTypeKind::Variable(var),
                                        None
                                    ),
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

                if let Some(operator_notes) = operator_notes() {
                    notes.extend(operator_notes);
                }

                if let Some(operator_fix) = operator_fix(self) {
                    fix = Some(operator_fix);
                }

                if let Some(id) = error.expr {
                    if let Some((func, _)) = self.start_of_call_chain_for(id) {
                        let mut constant_id = None;
                        func.traverse(
                            |expr| {
                                if let ExpressionKind::ErrorConstant(id) = &expr.kind  {
                                    constant_id = Some(id);
                                }

                                ControlFlow::Continue(())
                            },
                            |_| ControlFlow::Continue(()),
                        );

                        if let Some(id) = constant_id {
                            if self
                                .top_level
                                .info
                                .diagnostic_items
                                .collection_elements
                                .contains(id)
                            {
                                notes.push(Note::secondary(
                                    error.span,
                                    "this element must have the same type as the other elements",
                                ));
                            }
                        }
                    }
                }

                if let engine::UnresolvedTypeKind::Builtin(engine::BuiltinType::Text) = actual.kind
                {
                    if let engine::UnresolvedTypeKind::Function(_, _) = expected.kind {
                        if let Some(id) = error.expr {
                            if self.start_of_call_chain_for(id).is_some() {
                                message = Some(String::from("formatting only works on text literals"));
                            }
                        }
                    }
                }

                let default_message = format!(
                    "expected {}, but found {}",
                    self.format_type(expected.clone(), format),
                    self.format_type(actual.clone(), format)
                );

                let message = match message {
                    Some(message) => {
                        notes.push(Note::secondary(error.span, default_message));
                        message
                    }
                    None => default_message,
                };

                let mut error = self.compiler
                    .error_with_trace(error.span, message,  "mismatched-types", error.trace)
                    .fix(fix);

                error.notes = notes;

                error
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

                let trait_params = self
                    .declarations
                    .borrow()
                    .traits
                    .get(&id)
                    .unwrap()
                    .params
                    .clone();

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

                let mut notes = trait_params
                    .clone()
                    .into_iter()
                    .zip(params.clone())
                    .filter_map(|(_, ty)| {
                        let span = ty.info.span?;

                        if span == error.span
                            || !span.first().is_subspan_of(error.span.first())
                            || matches!(ty.kind, engine::UnresolvedTypeKind::Variable(_))
                        {
                            return None;
                        }

                        Some(Note::secondary(
                            span,
                            format!("this has type {}", self.format_type(ty, format)),
                        ))
                    })
                    .collect::<Vec<_>>()
                    .into_iter()
                    .chain(operator_notes().into_iter().flatten())
                    .chain(bound_span.map(|span| Note::secondary(span, "required by this bound here")))
                    .chain(error_candidates.into_iter().filter_map(|(span, unsatisfied_bounds)| {
                        if unsatisfied_bounds.is_empty() {
                            return None;
                        }

                        let format = format::Format {
                            surround_in_backticks: true,
                            ..Default::default()
                        };

                        Some(Note::secondary(
                            span,
                            format!(
                                "this instance could apply, but {}",
                                match unsatisfied_bounds.as_slice() {
                                    [] => unreachable!(),
                                    [(tr, params)] => format!("the bound {} wasn't satisfied", self.format_type(format::FormattableType::r#trait(*tr, params.clone()), format)),
                                    [(tr, params), rest @ ..] => format!(
                                        "the bounds {} and {} weren't satisfied",
                                        self.format_type(format::FormattableType::r#trait(*tr, params.clone()), format),
                                        rest.iter()
                                            .map(|(tr, params)| self.format_type(format::FormattableType::r#trait(*tr, params.clone()), format))
                                            .join(", "),
                                    )
                                }
                            ),
                        ))
                    }))
                    .collect::<Vec<_>>();

                let default_error_message = format!(
                    "could not find instance {}",
                    self.format_type(format::FormattableType::r#trait(id, params.clone()), format)
                );

                let error_message = match trait_attributes.on_unimplemented {
                    Some((segments, trailing_segment)) =>  {
                        notes.push(Note::secondary(DiagnosticLocation::from(error.span).use_caller_if_available(), default_error_message));

                        message_from_segments(
                            self,
                            (segments, trailing_segment),
                            trait_params
                                .into_iter()
                                .zip(params)
                                .collect(),
                            trait_attributes.decl_attributes.help_show_code,
                            format,
                        )
                    }
                    None => default_error_message,
                };

                let mut error = self.compiler
                    .error_with_trace(DiagnosticLocation::from(error.span).use_caller_if_available(), error_message, "missing-instance", error.trace)
                    .fix(operator_fix(self));

                error.notes = notes;

                error
            }
            engine::TypeError::UnresolvedType(mut ty, _candidates) => {
                ty.apply(&self.ctx);

                let format =
                    if ty.visible_vars().len() == ty.visible_vars().into_iter().unique().count() {
                        single_var_format
                    } else {
                        multi_var_format
                    };

                let note =
                    (!matches!(ty.kind, engine::UnresolvedTypeKind::Variable(_))).then(|| {
                        Note::primary(
                            error.span,
                            format!("this has type {}", self.format_type(ty, format)),
                        )
                    });

                let (error_message, fix) = error
                    .expr
                    .and_then(|id| self.expr_for(id))
                    .and_then(|expr| {
                        let constant = match expr.kind {
                            ExpressionKind::Constant(item) => {
                                let (_, constant) = self.items.get(&item).unwrap().0?;
                                constant
                            }
                            ExpressionKind::ErrorConstant(id)
                            | ExpressionKind::UnresolvedConstant(id) => id,
                            _ => return None,
                        };

                        let decl = self.with_constant_decl(constant, |decl| decl.clone())?;

                        let (segments, trailing_segment) = decl.attributes.on_unresolved?;

                        let params = self.extract_params(expr.ty.into(), decl.params, decl.ty);

                        let mut fix = None;

                        if let Some(source_code) = self
                            .compiler
                            .single_line_source_code_for_span(error.span.first())
                        {
                            if let Some((message, mut replacement)) = decl.attributes.resolve {
                                wipple_syntax::parse::substitute(
                                    &mut replacement,
                                    InternedString::new("value"),
                                    wipple_syntax::parse::Expr::new(
                                        error.span,
                                        wipple_syntax::parse::ExprKind::SourceCode(source_code),
                                    ),
                                );

                                fix = Some(Fix::new(
                                    message,
                                    FixRange::replace(error.span.first()),
                                    replacement,
                                ));
                            }
                        }

                        Some((
                            Some(message_from_segments(
                                self,
                                (segments, trailing_segment),
                                params,
                                decl.attributes.decl_attributes.help_show_code,
                                format,
                            )),
                            fix,
                        ))
                    })
                    .unwrap_or_default();

                let mut notes = note.into_iter().collect::<Vec<_>>();

                // TODO: List candidates in a smarter way to not overwhelm the user
                if let Some((bound, candidates)) = _candidates {
                    if let Some(bound) = bound {
                        notes.push(Note::secondary(bound, "...as required by this bound"));
                    }

                    for candidate in candidates {
                        notes.push(Note::secondary(candidate, if bound.is_some() { "this instance could potentially apply..." } else { "this instance could potentially apply" }));
                    }
                }

                let default_error_message = String::from("could not determine the type of this expression");

                let error_message = match error_message {
                    Some(message) => {
                        notes.push(Note::secondary(error.span, default_error_message));
                        message
                    }
                    None => default_error_message,
                };

                self.compiler
                    .error_with_trace(
                        error.span,
                        error_message,
                        "unknown-type",
                        error.trace,
                    )
                    .notes(notes)
                    .fix(fix.unwrap_or_else(|| {
                        Fix::new("annotate the type with `::`", FixRange::after(error.span.first()), ":: {%type%}")
                    }))
            }
            engine::TypeError::InvalidNumericLiteral(ty) => {
                let format =
                    if ty.visible_vars().len() == ty.visible_vars().into_iter().unique().count() {
                        single_var_format
                    } else {
                        multi_var_format
                    };

                let message = format!(
                    "{} does not fit into a {}",
                    self.compiler
                        .single_line_source_code_for_span(error.span.first())
                        .map_or_else(|| String::from("number"), |code| format!("`{code}`")),
                    self.format_type(ty, format)
                );

                self.compiler.error_with_trace(
                    error.span,
                    message,
                    "invalid-number",
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
            Some(id) => self.generic_items.get(&id).cloned(),
            None => self
                .items
                .get(&self.top_level_item?)
                .and_then(|(_, expr)| expr.clone()),
        }
    }

    fn expr_for(&mut self, id: ExpressionId) -> Option<Expression> {
        self.root_for(id.owner)
            .and_then(|expr| expr.as_root_query(id).cloned())
    }

    fn start_of_call_chain_for(&mut self, id: ExpressionId) -> Option<(Expression, Expression)> {
        self.root_for(id.owner).and_then(|expr| {
            expr.as_root_query_start_of_call_chain(id)
                .map(|(func, input)| (func.clone(), input.clone()))
        })
    }
}

#[allow(dead_code)]
impl Typechecker {
    fn debug_format() -> format::Format<'static> {
        format::Format {
            type_variable: format::TypeVariableFormat::Numeric,
            ..Default::default()
        }
    }

    fn debug_string_for_ty(&mut self, ty: impl Into<engine::UnresolvedType>) -> String {
        let mut ty = ty.into();
        ty.apply(&self.ctx);
        self.format_type(ty, Self::debug_format())
    }

    fn debug_string_for_bounds(&mut self, bounds: impl IntoIterator<Item = Bound>) -> String {
        bounds
            .into_iter()
            .map(|bound| {
                format!(
                    "({})",
                    self.debug_string_for_bound(bound.trait_id, bound.params),
                )
            })
            .join(" ")
    }

    fn debug_string_for_bound(
        &mut self,
        trait_id: TraitId,
        mut params: Vec<engine::UnresolvedType>,
    ) -> String {
        for ty in &mut params {
            ty.apply(&self.ctx);
        }

        self.format_type(
            format::FormattableType::r#trait(trait_id, params),
            Self::debug_format(),
        )
    }
}
