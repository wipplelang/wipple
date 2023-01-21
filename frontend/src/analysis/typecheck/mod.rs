#![allow(clippy::type_complexity)]

#[macro_use]
mod number;

pub mod display;
mod engine;
pub mod format;
pub mod traverse;

pub use engine::{BottomTypeReason, BuiltinType, GenericSubstitutions, Type, TypeStructure};
pub use lower::{RuntimeFunction, TypeAnnotation, TypeAnnotationKind};

use crate::{
    analysis::{expand, lower},
    diagnostics::Note,
    helpers::{Backtrace, InternedString},
    parse::Span,
    BuiltinTypeId, Compiler, ConstantId, FieldIndex, ItemId, TemplateId, TraitId, TypeId,
    TypeParameterId, VariableId, VariantIndex,
};
use itertools::Itertools;
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

#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct Program {
    pub items: BTreeMap<ItemId, (Option<ConstantId>, Expression)>,
    pub entrypoint: Option<ItemId>,
    pub declarations: Declarations,
    pub exported: lower::ScopeValues,
    pub scopes: Vec<(Span, lower::ScopeValues)>,
}

macro_rules! declarations {
    ($name:ident<$($container:ident)::+>) => {
        #[derive(Debug, Clone, Default)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize))]
        pub struct $name {
            pub types: $($container)::+<TypeId, TypeDecl>,
            pub traits: $($container)::+<TraitId, TraitDecl>,
            pub constants: $($container)::+<ConstantId, ConstantDecl>,
            pub instances: $($container)::+<TraitId, BTreeMap<ConstantId, InstanceDecl>>,
            pub operators: $($container)::+<TemplateId, OperatorDecl>,
            pub templates: $($container)::+<TemplateId, TemplateDecl>,
            pub builtin_types: $($container)::+<BuiltinTypeId, BuiltinTypeDecl>,
            pub type_parameters: $($container)::+<TypeParameterId, TypeParameterDecl>,
            /// NOTE: Not all variables will be listed here, only ones that passed typechecking
            pub variables: $($container)::+<VariableId, VariableDecl>,
        }
    };
}

declarations!(DeclarationsInner<im::HashMap>);
declarations!(Declarations<BTreeMap>);

impl From<DeclarationsInner> for Declarations {
    fn from(decls: DeclarationsInner) -> Self {
        Declarations {
            types: decls.types.into_iter().collect(),
            traits: decls.traits.into_iter().collect(),
            constants: decls.constants.into_iter().collect(),
            instances: decls.instances.into_iter().collect(),
            operators: decls.operators.into_iter().collect(),
            templates: decls.templates.into_iter().collect(),
            builtin_types: decls.builtin_types.into_iter().collect(),
            type_parameters: decls.type_parameters.into_iter().collect(),
            variables: decls.variables.into_iter().collect(),
        }
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct TypeDecl {
    pub name: InternedString,
    pub span: Span,
    pub params: Vec<(Span, TypeParameterId)>,
    pub kind: TypeDeclKind,
    pub attributes: lower::TypeAttributes,
    pub uses: HashSet<Span>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
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
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct TraitDecl {
    pub name: InternedString,
    pub span: Span,
    pub params: Vec<(Span, TypeParameterId)>,
    pub ty_annotation: Option<TypeAnnotation>,
    pub ty: Option<engine::Type>,
    pub attributes: lower::TraitAttributes,
    pub uses: HashSet<Span>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct ConstantDecl {
    pub name: InternedString,
    pub span: Span,
    pub params: Vec<(Span, TypeParameterId)>,
    pub bounds: Vec<Bound>,
    pub bound_annotations: Vec<(TraitId, Vec<TypeAnnotation>)>,
    pub ty_annotation: TypeAnnotation,
    pub ty: engine::Type,
    pub specializations: Vec<ConstantId>,
    pub attributes: lower::ConstantAttributes,
    pub body: Option<Expression>,
    pub uses: HashSet<Span>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct InstanceDecl {
    pub span: Span,
    pub params: Vec<(Span, TypeParameterId)>,
    pub bounds: Vec<Bound>,
    pub bound_annotations: Vec<(TraitId, Vec<TypeAnnotation>)>,
    pub trait_id: TraitId,
    pub trait_params: Vec<engine::Type>,
    pub trait_param_annotations: Vec<lower::TypeAnnotation>,
    pub body: Option<Expression>,
    pub item: ItemId,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct Bound {
    pub span: Span,
    pub trait_id: TraitId,
    pub params: Vec<engine::UnresolvedType>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct OperatorDecl {
    pub name: InternedString,
    pub span: Span,
    pub uses: HashSet<Span>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct TemplateDecl {
    pub name: InternedString,
    pub span: Span,
    pub attributes: expand::TemplateAttributes,
    pub uses: HashSet<Span>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct BuiltinTypeDecl {
    pub name: InternedString,
    pub span: Span,
    pub attributes: lower::DeclarationAttributes,
    pub uses: HashSet<Span>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct TypeParameterDecl {
    pub name: Option<InternedString>,
    pub span: Span,
    pub uses: HashSet<Span>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct VariableDecl {
    pub name: Option<InternedString>,
    pub span: Span,
    pub ty: engine::Type,
    pub uses: HashSet<Span>,
}

macro_rules! expr {
    ($vis:vis, $prefix:literal, $type:ty, { $($kinds:tt)* }) => {
        paste::paste! {
            #[derive(Debug, Clone)]
            #[cfg_attr(feature = "serde", derive(serde::Serialize))]
            $vis struct [<$prefix Expression>] {
                $vis span: Span,
                $vis ty: $type,
                $vis kind: [<$prefix ExpressionKind>],
            }

            #[derive(Debug, Clone)]
            #[cfg_attr(feature = "serde", derive(serde::Serialize))]
            #[cfg_attr(feature = "serde", serde(tag = "type", content = "value"))]
            $vis enum [<$prefix ExpressionKind>] {
                Error(Backtrace),
                Marker,
                Variable(VariableId),
                Text(InternedString),
                Block(Vec<[<$prefix Expression>]>, bool),
                End(Box<[<$prefix Expression>]>),
                Call(Box<[<$prefix Expression>]>, Box<[<$prefix Expression>]>),
                Function([<$prefix Pattern>], Box<[<$prefix Expression>]>, lower::CaptureList),
                When(Box<[<$prefix Expression>]>, Vec<[<$prefix Arm>]>),
                External(InternedString, InternedString, Vec<[<$prefix Expression>]>),
                Runtime(RuntimeFunction, Vec<[<$prefix Expression>]>),
                Initialize([<$prefix Pattern>], Box<[<$prefix Expression>]>),
                Structure(Vec<[<$prefix Expression>]>),
                Variant(VariantIndex, Vec<[<$prefix Expression>]>),
                Tuple(Vec<[<$prefix Expression>]>),
                $($kinds)*
            }

            #[derive(Debug, Clone)]
            #[cfg_attr(feature = "serde", derive(serde::Serialize))]
            $vis struct [<$prefix Arm>] {
                $vis span: Span,
                $vis pattern: [<$prefix Pattern>],
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
            #[derive(Debug, Clone)]
            #[cfg_attr(feature = "serde", derive(serde::Serialize))]
            $vis struct [<$prefix Pattern>] {
                $vis span: Span,
                $vis kind: [<$prefix PatternKind>],
            }

            #[derive(Debug, Clone)]
            #[cfg_attr(feature = "serde", derive(serde::Serialize))]
            #[cfg_attr(feature = "serde", serde(tag = "type", content = "value"))]
            $vis enum [<$prefix PatternKind>] {
                Error(Backtrace),
                Wildcard,
                Text(InternedString),
                Variable(VariableId),
                Or(Box<[<$prefix Pattern>]>, Box<[<$prefix Pattern>]>),
                Where(Box<[<$prefix Pattern>]>, Box<[<$prefix Expression>]>),
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
    Constant(ItemId),
});

expr!(pub, "", engine::Type, {
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
            contains_error &= matches!(expr.kind, ExpressionKind::Error(_));

            contains_error &= match &expr.kind {
                ExpressionKind::Error(_) => true,
                ExpressionKind::Function(pattern, _, _)
                | ExpressionKind::Initialize(pattern, _) => pattern.contains_error(),
                ExpressionKind::When(_, arms) => {
                    arms.iter().any(|arm| arm.pattern.contains_error())
                }
                _ => false,
            };
        });

        contains_error
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
    Destructure(BTreeMap<FieldIndex, MonomorphizedPattern>),
    Variant(VariantIndex, Vec<MonomorphizedPattern>),
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
    Destructure(BTreeMap<FieldIndex, Pattern>),
    Variant(VariantIndex, Vec<Pattern>)
});

impl Pattern {
    pub fn contains_error(&self) -> bool {
        let mut contains_error = false;
        self.traverse(|pattern| {
            contains_error &= matches!(pattern.kind, PatternKind::Error(_));
        });

        contains_error
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub error: engine::TypeError,
    pub span: Span,
    pub notes: Vec<Note>,
    pub trace: Backtrace,
}

impl Error {
    pub fn with_note(mut self, note: Note) -> Self {
        self.notes.push(note);
        self
    }
}

impl Compiler<'_> {
    pub(crate) fn typecheck_with_progress(
        &self,
        entrypoint: lower::File,
        complete: bool,
        mut progress: impl FnMut(Progress),
    ) -> Program {
        let mut typechecker = Typechecker::new(self, entrypoint);

        progress(Progress::CollectingTypes);
        typechecker.collect_types();

        typechecker.resolve(complete, |count, remaining| {
            progress(Progress::ResolvingDeclarations { count, remaining });
        })
    }
}

#[derive(Debug, Clone)]
struct Typechecker<'a, 'l> {
    compiler: &'a Compiler<'l>,
    entrypoint: lower::File,
    ctx: engine::Context,
    declarations: RefCell<DeclarationsInner>,
    exported: Option<lower::ScopeValues>,
    scopes: RefCell<Vec<(Span, lower::ScopeValues)>>,
    block_end: Option<Option<(Span, engine::UnresolvedType)>>,
    instances: im::HashMap<TraitId, Vec<ConstantId>>,
    generic_constants: im::HashMap<ConstantId, (bool, lower::Expression)>,
    specialized_constants: im::HashMap<ConstantId, ConstantId>,
    item_queue: im::Vector<QueuedItem>,
    items: im::HashMap<ItemId, (Option<ConstantId>, Expression)>,
    entrypoint_expr: Option<UnresolvedExpression>,
    errors: im::Vector<Error>,
}

#[derive(Debug, Clone)]
struct QueuedItem {
    generic_id: Option<ConstantId>,
    id: ItemId,
    expr: UnresolvedExpression,
    info: MonomorphizeInfo,
}

impl<'a, 'l> Typechecker<'a, 'l> {
    fn error(&self, error: engine::TypeError, span: Span) -> Error {
        Error {
            error,
            span,
            notes: Vec::new(),
            trace: self.compiler.backtrace(),
        }
    }
}

impl<'a, 'l> Typechecker<'a, 'l> {
    pub fn new(compiler: &'a Compiler<'l>, entrypoint: lower::File) -> Self {
        Typechecker {
            compiler,
            entrypoint,
            ctx: Default::default(),
            declarations: Default::default(),
            exported: None,
            scopes: Default::default(),
            block_end: None,
            instances: Default::default(),
            generic_constants: Default::default(),
            specialized_constants: Default::default(),
            item_queue: Default::default(),
            items: Default::default(),
            entrypoint_expr: Default::default(),
            errors: Default::default(),
        }
    }

    pub fn add_error(&mut self, error: Error) {
        self.errors.push_back(error);
    }

    pub fn resolve(
        mut self,
        lowering_is_complete: bool,
        mut progress: impl FnMut(usize, usize),
    ) -> Program {
        // Queue the entrypoint

        let entrypoint_item = mem::take(&mut self.entrypoint_expr).map(|entrypoint| {
            let info = MonomorphizeInfo::default();

            let entrypoint_id = self.compiler.new_item_id_in(entrypoint.span.path);

            self.item_queue.push_back(QueuedItem {
                generic_id: None,
                id: entrypoint_id,
                expr: entrypoint,
                info,
            });

            entrypoint_id
        });

        // Monomorphize constants

        let mut count = 0;
        let total = self.item_queue.len();
        while let Some(mut item) = self.item_queue.pop_back() {
            count += 1;
            progress(count, total);

            let expr = self.monomorphize_expr(item.expr, &mut item.info);
            let expr = self.finalize_expr(expr);

            self.items.insert(item.id, (item.generic_id, expr));
        }

        // Ensure specialized constants unify with their generic counterparts

        for (specialized_id, generic_id) in self.specialized_constants.clone() {
            self.typecheck_specialized_constant(specialized_id, generic_id);
        }

        // Typecheck generic constants

        for (id, (instance, body)) in self.generic_constants.clone() {
            self.typecheck_generic_constant_expr(id, instance, body);
        }

        // Report errors if needed

        if lowering_is_complete {
            self.report_errors();
        }

        // Consolidate constants based on their type

        let mut cache = HashMap::new();
        let mut cached = BTreeSet::new();
        let mut map = BTreeMap::new();

        let mut items = self.items.into_iter().collect::<BTreeMap<_, _>>();

        for (id, (generic_id, expr)) in &items {
            let cached_id = *cache
                .entry((generic_id, expr.ty.clone()))
                .or_insert_with(|| {
                    cached.insert(*id);
                    *id
                });

            map.insert(*id, cached_id);
        }

        for id in items.keys().cloned().collect::<Vec<_>>() {
            if !cached.contains(&id) {
                items.remove(&id);
            }
        }

        for (_, expr) in items.values_mut() {
            expr.traverse_mut(|expr| {
                if let ExpressionKind::Constant(id) = &mut expr.kind {
                    if let Some(mapped_id) = map.get(id) {
                        *id = *mapped_id;
                    }
                }
            })
        }

        // Build the final program

        Program {
            items,
            entrypoint: entrypoint_item,
            exported: self.exported.unwrap_or_default(),
            declarations: self.declarations.into_inner().into(),
            scopes: self.scopes.into_inner(),
        }
    }
}

impl<'a, 'l> Typechecker<'a, 'l> {
    pub fn collect_types(&mut self) {
        let entrypoint = mem::take(&mut self.entrypoint.block);
        let exported = mem::take(&mut self.entrypoint.exported);
        self.scopes
            .borrow_mut()
            .extend(mem::take(&mut self.entrypoint.scopes));

        let mut info = ConvertInfo {
            variables: Default::default(),
        };

        let expr = self.convert_expr(
            lower::Expression {
                span: self.entrypoint.span,
                kind: lower::ExpressionKind::Block(entrypoint, true),
            },
            &mut info,
        );

        self.exported = Some(exported);
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
                    for id in declaration!([<$kind s>]) {
                        self.[<with_ $kind _decl>](id, |_| {});
                    }
                }
            };
            ($($kind:ident),* $(,)?) => {
                $(check!($kind);)*
            }
        }

        check!(
            type,
            trait,
            operator,
            template,
            builtin_type,
            type_parameter,
            // variables are handled inside `finalize_pattern`
        );
    }

    fn typecheck_generic_constant_expr(
        &mut self,
        id: ConstantId,
        instance: bool,
        expr: lower::Expression,
    ) {
        let (tr, generic_ty, bounds) = if instance {
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

            let ty = match self
                .substitute_trait_params(
                    tr,
                    trait_params.clone().into_iter().map(From::from).collect(),
                    span,
                )
                .finalize(&self.ctx)
            {
                Ok(ty) => ty,
                Err(error) => {
                    self.add_error(self.error(error, span));

                    let expr = Expression {
                        span: expr.span,
                        ty: engine::Type::Error,
                        kind: ExpressionKind::error(self.compiler),
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

            (Some(tr), ty, bounds)
        } else {
            self.with_constant_decl(id, |decl| (None, decl.ty.clone(), decl.bounds.clone()))
                .expect("constant should have already been accessed at least once")
        };

        let mut monomorphize_info = MonomorphizeInfo::default();
        for bound in bounds {
            monomorphize_info
                .bound_instances
                .entry(bound.trait_id)
                .or_default()
                .push((None, bound.params, bound.span));
        }

        let expr = self.convert_expr(expr, &mut ConvertInfo::default());
        if let Err(error) = self.ctx.unify_generic(expr.ty.clone(), generic_ty.clone()) {
            self.add_error(self.error(error, expr.span));
        }

        let expr = self.monomorphize_expr(expr, &mut monomorphize_info);
        let expr = self.finalize_expr(expr);

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
            self.declarations
                .borrow_mut()
                .constants
                .get_mut(&id)
                .unwrap()
                .body = Some(expr);
        }
    }

    fn typecheck_constant_expr(
        &mut self,
        is_instance: bool,
        id: ConstantId,
        use_span: Span,
        use_ty: engine::UnresolvedType,
        mut info: MonomorphizeInfo,
    ) -> ItemId {
        if let Some(monomorphized_id) = info.cache.get(&id) {
            return *monomorphized_id;
        }

        let monomorphized_id = self.compiler.new_item_id();

        let mut candidates = Vec::with_capacity(1);
        if !is_instance {
            self.with_constant_decl(id, |decl| {
                candidates.append(&mut decl.specializations.clone());
            });
        }

        for &candidate in &candidates {
            self.specialized_constants.insert(candidate, id);
        }

        candidates.push(id);

        let last_index = candidates.len() - 1;
        let is_last_candidate = |index: usize| index == last_index;

        'check: for (index, candidate) in candidates.into_iter().enumerate() {
            info.cache.insert(candidate, monomorphized_id);

            let mut info = info.clone();

            let (generic_ty, mut bounds) = if is_instance {
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

                let ty = match self
                    .substitute_trait_params(
                        tr,
                        trait_params.clone().into_iter().map(From::from).collect(),
                        span,
                    )
                    .finalize(&self.ctx)
                {
                    Ok(ty) => ty,
                    Err(error) => {
                        self.add_error(self.error(error, span));
                        continue;
                    }
                };

                (ty, bounds)
            } else {
                match self
                    .with_constant_decl(candidate, |decl| (decl.ty.clone(), decl.bounds.clone()))
                {
                    Some(constant) => constant,
                    None => continue,
                }
            };

            let mut generic_ty = engine::UnresolvedType::from(generic_ty);

            let mut substitutions = engine::GenericSubstitutions::new();
            self.add_substitutions(&mut generic_ty, &mut substitutions);
            for bound in &mut bounds {
                for param in &mut bound.params {
                    self.add_substitutions(param, &mut substitutions);
                }
            }

            let (_, body) = self.generic_constants.get(&candidate).unwrap().clone();

            let mut convert_info = ConvertInfo {
                variables: Default::default(),
            };

            let prev_ctx = self.ctx.clone();

            if let Err(error) = self.unify(use_span, use_ty.clone(), generic_ty.clone()) {
                if is_last_candidate(index) {
                    self.add_error(error);
                } else {
                    self.ctx = prev_ctx;
                    continue 'check;
                }
            }

            let generic_expr = self.convert_expr(body, &mut convert_info);

            if let Err(error) = self.unify(use_span, generic_ty, generic_expr.ty.clone()) {
                if is_last_candidate(index) {
                    self.add_error(error);
                } else {
                    self.ctx = prev_ctx;
                    continue 'check;
                }
            }

            for bound in bounds {
                let instance_id = match self.instance_for_params(
                    bound.trait_id,
                    bound.params.clone(),
                    use_span,
                    Some(bound.span),
                    &mut info,
                ) {
                    Ok(id) => id,
                    Err(error) => {
                        if is_last_candidate(index) {
                            self.add_error(error);
                        } else {
                            self.ctx = prev_ctx;
                            continue 'check;
                        }

                        None
                    }
                };

                info.bound_instances
                    .entry(bound.trait_id)
                    .or_default()
                    .push((instance_id, bound.params, bound.span));
            }

            self.item_queue.push_back(QueuedItem {
                generic_id: Some(candidate),
                id: monomorphized_id,
                expr: generic_expr,
                info,
            });

            break 'check;
        }

        monomorphized_id
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

        let generic_constant_decl = match self.with_constant_decl(generic_id, |decl| decl.clone()) {
            Some(decl) => decl,
            None => return,
        };

        let mut params = match self.ctx.unify_params(
            specialized_constant_decl.ty.into(),
            generic_constant_decl.ty,
        ) {
            (params, Ok(())) => params,
            (_, Err(error)) => {
                self.add_error(self.error(error, specialized_constant_decl.span).with_note(
                    Note::secondary(
                        specialized_constant_decl.span,
                        "this constant must have a more specific type than the original constant",
                    ),
                ));

                return;
            }
        };

        let mut monomorphize_info = MonomorphizeInfo::default();
        for bound in specialized_constant_decl.bounds {
            monomorphize_info
                .bound_instances
                .entry(bound.trait_id)
                .or_default()
                .push((None, bound.params, bound.span));
        }

        for mut bound in generic_constant_decl.bounds {
            // Any type parameters not unified with the specialized constant
            // won't appear here, so add them in manually
            for ty in &mut bound.params {
                for param in ty.params() {
                    params.entry(param).or_insert_with(|| {
                        engine::UnresolvedType::Variable(self.ctx.new_variable())
                    });
                }

                ty.instantiate_with(&mut self.ctx, &params);
            }

            let instance_id = match self.instance_for_params(
                bound.trait_id,
                bound.params.clone(),
                specialized_constant_decl.span,
                Some(bound.span),
                &mut monomorphize_info,
            ) {
                Ok(id) => id,
                Err(error) => {
                    self.add_error(error.with_note(Note::secondary(
                        specialized_constant_decl.span,
                        "this constant must satisfy the bounds of the original constant",
                    )));

                    continue;
                }
            };

            monomorphize_info
                .bound_instances
                .entry(bound.trait_id)
                .or_default()
                .push((instance_id, bound.params, bound.span));
        }

        self.ctx = prev_ctx;
    }
}

#[derive(Default)]
struct ConvertInfo {
    variables: BTreeMap<VariableId, engine::UnresolvedType>,
}

impl<'a, 'l> Typechecker<'a, 'l> {
    fn convert_expr(
        &mut self,
        expr: lower::Expression,
        info: &mut ConvertInfo,
    ) -> UnresolvedExpression {
        match expr.kind {
            lower::ExpressionKind::Error(trace) => UnresolvedExpression {
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
                                    .map(|(_, param)| engine::UnresolvedType::Parameter(param))
                                    .collect(),
                                engine::TypeStructure::Marker,
                            )
                        },
                    )
                };

                self.instantiate_generics(&mut ty);

                UnresolvedExpression {
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Marker,
                }
            }
            lower::ExpressionKind::Constant(id) => {
                let mut ty = self
                    .with_constant_decl(id, |constant| {
                        engine::UnresolvedType::from(constant.ty.clone())
                    })
                    .unwrap_or(engine::UnresolvedType::Error);

                self.instantiate_generics(&mut ty);

                UnresolvedExpression {
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Constant(id),
                }
            }
            lower::ExpressionKind::Trait(id) => {
                let mut ty = if let Some((span, false)) =
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
                    engine::UnresolvedType::Variable(self.ctx.new_variable())
                };

                self.instantiate_generics(&mut ty);

                UnresolvedExpression {
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Trait(id),
                }
            }
            lower::ExpressionKind::Variable(var) => {
                let ty =
                    info.variables.get(&var).cloned().unwrap_or_else(|| {
                        engine::UnresolvedType::Variable(self.ctx.new_variable())
                    });

                UnresolvedExpression {
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Variable(var),
                }
            }
            lower::ExpressionKind::Text(text) => UnresolvedExpression {
                span: expr.span,
                ty: engine::UnresolvedType::Builtin(engine::BuiltinType::Text),
                kind: UnresolvedExpressionKind::Text(text),
            },
            lower::ExpressionKind::Number(number) => UnresolvedExpression {
                span: expr.span,
                ty: engine::UnresolvedType::NumericVariable(self.ctx.new_variable()),
                kind: UnresolvedExpressionKind::Number(number),
            },
            lower::ExpressionKind::Block(statements, top_level) => {
                let prev_block_end = mem::replace(&mut self.block_end, Some(None));

                let statements = statements
                    .into_iter()
                    .map(|statement| self.convert_expr(statement, info))
                    .collect::<Vec<_>>();

                let mut ty = statements
                    .last()
                    .map(|statement| statement.ty.clone())
                    .unwrap_or_else(|| engine::UnresolvedType::Tuple(Vec::new()));

                if let Some((end_span, end_ty)) =
                    mem::replace(&mut self.block_end, prev_block_end).unwrap()
                {
                    if let Err(error) = self.unify(end_span, end_ty.clone(), ty) {
                        self.add_error(error);
                    }

                    ty = end_ty;
                }

                UnresolvedExpression {
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Block(statements, top_level),
                }
            }
            lower::ExpressionKind::End(value) => {
                let value = self.convert_expr(*value, info);

                match self.block_end {
                    Some(Some((_, ref existing_end_ty))) => {
                        if let Err(error) =
                            self.unify(value.span, value.ty.clone(), existing_end_ty.clone())
                        {
                            self.add_error(error);
                        }
                    }
                    Some(ref mut end @ None) => *end = Some((value.span, value.ty.clone())),
                    None => {
                        self.compiler.add_error(
                            "`end` outside block",
                            vec![Note::primary(expr.span, "cannot use `end` here")],
                        );
                    }
                }

                UnresolvedExpression {
                    span: expr.span,
                    ty: engine::UnresolvedType::Variable(self.ctx.new_variable()),
                    kind: UnresolvedExpressionKind::End(Box::new(value)),
                }
            }
            lower::ExpressionKind::Call(function, input) => {
                let function = self.convert_expr(*function, info);
                let input = self.convert_expr(*input, info);

                let input_ty = engine::UnresolvedType::Variable(self.ctx.new_variable());
                let output_ty = engine::UnresolvedType::Variable(self.ctx.new_variable());

                if let Err(error) = self.unify(
                    function.span,
                    function.ty.clone(),
                    engine::UnresolvedType::Function(
                        Box::new(input_ty.clone()),
                        Box::new(output_ty.clone()),
                    ),
                ) {
                    self.add_error(error);
                }

                if let Err(error) = self.unify(input.span, input.ty.clone(), input_ty) {
                    self.add_error(error);
                }

                UnresolvedExpression {
                    span: expr.span,
                    ty: output_ty,
                    kind: UnresolvedExpressionKind::Call(Box::new(function), Box::new(input)),
                }
            }
            lower::ExpressionKind::Function(pattern, body, captures) => {
                let input_ty = engine::UnresolvedType::Variable(self.ctx.new_variable());
                let pattern = self.convert_pattern(pattern, input_ty.clone(), None, info);

                let prev_block_end = mem::replace(&mut self.block_end, None);
                let body = self.convert_expr(*body, info);
                self.block_end = prev_block_end;

                UnresolvedExpression {
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
                            if let Err(error) =
                                self.unify(arm.body.span, arm.body.ty.clone(), first_type.clone())
                            {
                                self.add_error(error);
                            }
                        }

                        first_type
                    } else {
                        engine::UnresolvedType::Variable(self.ctx.new_variable())
                    }
                };

                UnresolvedExpression {
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
                    span: expr.span,
                    ty: engine::UnresolvedType::Variable(self.ctx.new_variable()),
                    kind: UnresolvedExpressionKind::External(lib, identifier, inputs),
                }
            }
            lower::ExpressionKind::Runtime(func, inputs) => {
                let inputs = inputs
                    .into_iter()
                    .map(|expr| self.convert_expr(expr, info))
                    .collect();

                UnresolvedExpression {
                    span: expr.span,
                    ty: engine::UnresolvedType::Variable(self.ctx.new_variable()),
                    kind: UnresolvedExpressionKind::Runtime(func, inputs),
                }
            }
            lower::ExpressionKind::Annotate(value, ty) => {
                let ty = self.convert_type_annotation(ty);
                let value = self.convert_expr(*value, info);

                if let Err(error) = self.unify(value.span, value.ty, ty.clone()) {
                    self.add_error(error);
                }

                UnresolvedExpression {
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
                            span: expr.span,
                            ty: engine::UnresolvedType::Error,
                            kind: UnresolvedExpressionKind::error(self.compiler),
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
                            span: expr.span,
                            ty: engine::UnresolvedType::Error,
                            kind: UnresolvedExpressionKind::error(self.compiler),
                        };
                    }
                };

                let mut ty = engine::UnresolvedType::Named(
                    id,
                    params
                        .into_iter()
                        .map(|(_, param)| engine::UnresolvedType::Parameter(param))
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

                for (name, expr) in fields {
                    let (index, ty) = match structure_field_names.get(&name) {
                        Some(index) if index.into_inner() < fields_by_index.len() => {
                            (*index, structure_field_tys[index.into_inner()].clone())
                        }
                        _ => {
                            extra_fields.push(name);
                            continue;
                        }
                    };

                    let span = expr.span;
                    let mut value = self.convert_expr(expr, info);

                    if let Err(error) = self.unify(span, value.ty.clone(), ty.clone()) {
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
                                    .map(|field| format!("`{}`", field))
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
                                    .map(|field| format!("`{}`", field))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                        )],
                    );
                }

                UnresolvedExpression {
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
                            span: expr.span,
                            ty: engine::UnresolvedType::Error,
                            kind: UnresolvedExpressionKind::error(self.compiler),
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
                            span: expr.span,
                            ty: engine::UnresolvedType::Error,
                            kind: UnresolvedExpressionKind::error(self.compiler),
                        };
                    }
                };

                let mut ty = engine::UnresolvedType::Named(
                    id,
                    params
                        .into_iter()
                        .map(|(_, param)| engine::UnresolvedType::Parameter(param))
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
                        let span = expr.span;
                        let mut value = self.convert_expr(expr, info);

                        if let Err(error) = self.unify(span, value.ty.clone(), ty.clone()) {
                            self.add_error(error);
                        }

                        value.ty = ty;

                        value
                    })
                    .collect();

                UnresolvedExpression {
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
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Tuple(exprs),
                }
            }
        }
    }

    fn convert_arm(
        &mut self,
        arm: lower::Arm,
        input_ty: engine::UnresolvedType,
        input_span: Span,
        info: &mut ConvertInfo,
    ) -> UnresolvedArm {
        UnresolvedArm {
            span: arm.span,
            pattern: self.convert_pattern(arm.pattern, input_ty, Some(input_span), info),
            body: self.convert_expr(arm.body, info),
        }
    }

    fn convert_pattern(
        &mut self,
        pattern: lower::Pattern,
        ty: engine::UnresolvedType,
        input_span: Option<Span>,
        info: &mut ConvertInfo,
    ) -> UnresolvedPattern {
        UnresolvedPattern {
            span: pattern.span,
            kind: (|| match pattern.kind {
                lower::PatternKind::Error(trace) => UnresolvedPatternKind::Error(trace),
                lower::PatternKind::Wildcard => UnresolvedPatternKind::Wildcard,
                lower::PatternKind::Number(number) => {
                    let numeric_ty =
                        engine::UnresolvedType::NumericVariable(self.ctx.new_variable());

                    if let Err(error) = self.unify(pattern.span, ty, numeric_ty) {
                        self.add_error(error);
                    }

                    UnresolvedPatternKind::Number(number)
                }
                lower::PatternKind::Text(text) => {
                    if let Err(error) = self.unify(
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
                            let ty = engine::UnresolvedType::Variable(self.ctx.new_variable());
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
                        None => return UnresolvedPatternKind::error(self.compiler),
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

                            return UnresolvedPatternKind::error(self.compiler);
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
                            .map(|(_, param)| {
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
                                            ty.instantiate_with(&mut self.ctx, &substitutions);
                                            ty
                                        })
                                        .collect()
                                })
                                .collect(),
                        ),
                    );

                    if let Err(error) = self.unify(pattern.span, ty, enumeration_ty) {
                        self.add_error(error);
                    }

                    for ty in &mut variant_tys {
                        ty.instantiate_with(&mut self.ctx, &substitutions);
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

                    if let Err(error) = self.unify(pattern.span, ty, target_ty.clone()) {
                        self.add_error(error);
                    }

                    self.convert_pattern(*inner, target_ty, input_span, info)
                        .kind
                }
                lower::PatternKind::Or(lhs, rhs) => UnresolvedPatternKind::Or(
                    Box::new(self.convert_pattern(*lhs, ty.clone(), input_span, info)),
                    Box::new(self.convert_pattern(*rhs, ty, input_span, info)),
                ),
                lower::PatternKind::Where(pattern, condition) => UnresolvedPatternKind::Where(
                    Box::new(self.convert_pattern(*pattern, ty, input_span, info)),
                    Box::new(self.convert_expr(*condition, info)),
                ),
                lower::PatternKind::Tuple(patterns) => {
                    let tuple_tys = patterns
                        .iter()
                        .map(|_| engine::UnresolvedType::Variable(self.ctx.new_variable()))
                        .collect::<Vec<_>>();

                    if let Err(error) = self.unify(
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
    cache: BTreeMap<ConstantId, ItemId>,
    bound_instances:
        BTreeMap<TraitId, Vec<(Option<ConstantId>, Vec<engine::UnresolvedType>, Span)>>,
    instance_stack: BTreeMap<TraitId, Vec<(ConstantId, Vec<engine::UnresolvedType>)>>,
    recursion_count: usize,
}

impl<'a, 'l> Typechecker<'a, 'l> {
    fn monomorphize_expr(
        &mut self,
        mut expr: UnresolvedExpression,
        info: &mut MonomorphizeInfo,
    ) -> MonomorphizedExpression {
        expr.ty.apply(&self.ctx);

        MonomorphizedExpression {
            span: expr.span,
            ty: expr.ty.clone(),
            kind: (|| match expr.kind {
                UnresolvedExpressionKind::Error(trace) => MonomorphizedExpressionKind::Error(trace),
                UnresolvedExpressionKind::Marker => MonomorphizedExpressionKind::Marker,
                UnresolvedExpressionKind::Constant(generic_id) => {
                    let id = self.typecheck_constant_expr(
                        false,
                        generic_id,
                        expr.span,
                        expr.ty.clone(),
                        info.clone(),
                    );

                    MonomorphizedExpressionKind::Constant(id)
                }
                UnresolvedExpressionKind::Variable(var) => {
                    MonomorphizedExpressionKind::Variable(var)
                }
                UnresolvedExpressionKind::Text(text) => MonomorphizedExpressionKind::Text(text),
                UnresolvedExpressionKind::Number(number) => {
                    MonomorphizedExpressionKind::Number(number)
                }
                UnresolvedExpressionKind::Block(statements, top_level) => {
                    let mut statements = statements
                        .into_iter()
                        .map(|expr| self.monomorphize_expr(expr, info))
                        .collect::<Vec<_>>();

                    // Non-terminator statements by default have a type of `()`
                    for statement in
                        statements
                            .iter_mut()
                            .dropping_back(if top_level { 0 } else { 1 })
                    {
                        let var = self.ctx.new_variable();

                        self.ctx
                            .unify(
                                engine::UnresolvedType::TerminatingVariable(var),
                                statement.ty.clone(),
                            )
                            .unwrap();
                    }

                    MonomorphizedExpressionKind::Block(statements, top_level)
                }
                UnresolvedExpressionKind::End(value) => {
                    MonomorphizedExpressionKind::End(Box::new(self.monomorphize_expr(*value, info)))
                }
                UnresolvedExpressionKind::Call(func, input) => {
                    // If the input is another function, monomorphize the outer
                    // function first for optimal type inference

                    let func = *func;
                    let input = *input;

                    let input_is_function = {
                        let mut input_ty = input.ty.clone();
                        input_ty.apply(&self.ctx);
                        matches!(input_ty, engine::UnresolvedType::Function(_, _))
                    };

                    let (func, input) = if input_is_function {
                        let func = self.monomorphize_expr(func, info);
                        let input = self.monomorphize_expr(input, info);
                        (func, input)
                    } else {
                        let input = self.monomorphize_expr(input, info);
                        let func = self.monomorphize_expr(func, info);
                        (func, input)
                    };

                    MonomorphizedExpressionKind::Call(Box::new(func), Box::new(input))
                }
                UnresolvedExpressionKind::Function(pattern, body, captures) => {
                    let pattern = match expr.ty {
                        engine::UnresolvedType::Function(input_ty, _) => {
                            let mut input_ty = *input_ty;
                            input_ty.apply(&self.ctx);

                            let mut match_set = MatchSet::new(&input_ty);

                            let pattern = self.monomorphize_pattern(
                                pattern,
                                input_ty.clone(),
                                &mut match_set,
                                info,
                            );

                            self.assert_matched(&match_set, &input_ty, pattern.span, true);

                            pattern
                        }
                        _ => self.monomorphize_pattern(
                            pattern,
                            engine::UnresolvedType::Error,
                            &mut MatchSet::Never,
                            info,
                        ),
                    };

                    let body = self.monomorphize_expr(*body, info);

                    MonomorphizedExpressionKind::Function(pattern, Box::new(body), captures)
                }
                UnresolvedExpressionKind::When(input, arms) => {
                    let mut input = self.monomorphize_expr(*input, info);
                    input.ty.apply(&self.ctx);

                    let mut match_set = MatchSet::new(&input.ty);

                    let arms = arms
                        .into_iter()
                        .map(|arm| {
                            self.monomorphize_arm(arm, input.ty.clone(), &mut match_set, info)
                        })
                        .collect();

                    self.assert_matched(&match_set, &input.ty, expr.span, false);

                    MonomorphizedExpressionKind::When(Box::new(input), arms)
                }
                UnresolvedExpressionKind::External(lib, identifier, inputs) => {
                    MonomorphizedExpressionKind::External(
                        lib,
                        identifier,
                        inputs
                            .into_iter()
                            .map(|expr| self.monomorphize_expr(expr, info))
                            .collect(),
                    )
                }
                UnresolvedExpressionKind::Runtime(func, inputs) => {
                    MonomorphizedExpressionKind::Runtime(
                        func,
                        inputs
                            .into_iter()
                            .map(|expr| self.monomorphize_expr(expr, info))
                            .collect(),
                    )
                }
                UnresolvedExpressionKind::Initialize(pattern, value) => {
                    // Resolve the right-hand side first
                    let mut value = self.monomorphize_expr(*value, info);
                    value.ty.apply(&self.ctx);

                    let mut match_set = MatchSet::new(&value.ty);

                    let pattern =
                        self.monomorphize_pattern(pattern, value.ty.clone(), &mut match_set, info);

                    self.assert_matched(&match_set, &value.ty, pattern.span, true);

                    MonomorphizedExpressionKind::Initialize(pattern, Box::new(value))
                }
                UnresolvedExpressionKind::Structure(fields) => {
                    MonomorphizedExpressionKind::Structure(
                        fields
                            .into_iter()
                            .map(|expr| self.monomorphize_expr(expr, info))
                            .collect(),
                    )
                }
                UnresolvedExpressionKind::Variant(index, values) => {
                    MonomorphizedExpressionKind::Variant(
                        index,
                        values
                            .into_iter()
                            .map(|expr| self.monomorphize_expr(expr, info))
                            .collect(),
                    )
                }
                UnresolvedExpressionKind::Trait(tr) => {
                    let instance_id =
                        match self.instance_for_ty(tr, expr.ty.clone(), expr.span, None, info) {
                            Ok(instance) => instance,
                            Err(error) => {
                                self.add_error(error);
                                return MonomorphizedExpressionKind::error(self.compiler);
                            }
                        };

                    let monomorphized_id = match instance_id {
                        Some(id) => self.typecheck_constant_expr(
                            true,
                            id,
                            expr.span,
                            expr.ty.clone(),
                            info.clone(),
                        ),
                        None => return MonomorphizedExpressionKind::error(self.compiler),
                    };

                    MonomorphizedExpressionKind::Constant(monomorphized_id)
                }
                UnresolvedExpressionKind::Tuple(exprs) => MonomorphizedExpressionKind::Tuple(
                    exprs
                        .into_iter()
                        .map(|expr| self.monomorphize_expr(expr, info))
                        .collect(),
                ),
            })(),
        }
    }

    fn monomorphize_arm(
        &mut self,
        arm: UnresolvedArm,
        ty: engine::UnresolvedType,
        match_set: &mut MatchSet,
        info: &mut MonomorphizeInfo,
    ) -> MonomorphizedArm {
        MonomorphizedArm {
            span: arm.span,
            pattern: self.monomorphize_pattern(arm.pattern, ty, match_set, info),
            body: self.monomorphize_expr(arm.body, info),
        }
    }

    fn monomorphize_pattern(
        &mut self,
        pattern: UnresolvedPattern,
        mut ty: engine::UnresolvedType,
        match_set: &mut MatchSet,
        info: &mut MonomorphizeInfo,
    ) -> MonomorphizedPattern {
        ty.apply(&self.ctx);

        let kind = (|| match pattern.kind {
            UnresolvedPatternKind::Error(trace) => {
                match_set.set_matched(true);
                MonomorphizedPatternKind::Error(trace)
            }
            UnresolvedPatternKind::Number(number) => {
                let numeric_ty = engine::UnresolvedType::NumericVariable(self.ctx.new_variable());

                if let Err(error) = self.unify(pattern.span, ty, numeric_ty) {
                    self.add_error(error);
                }

                MonomorphizedPatternKind::Number(number)
            }
            UnresolvedPatternKind::Text(text) => {
                if let Err(error) = self.unify(
                    pattern.span,
                    ty,
                    engine::UnresolvedType::Builtin(engine::BuiltinType::Text),
                ) {
                    self.add_error(error);
                }

                MonomorphizedPatternKind::Text(text)
            }
            UnresolvedPatternKind::Wildcard => {
                match_set.set_matched(true);
                MonomorphizedPatternKind::Wildcard
            }
            UnresolvedPatternKind::Variable(var) => {
                match_set.set_matched(true);
                MonomorphizedPatternKind::Variable(var)
            }
            UnresolvedPatternKind::Destructure(structure_ty, fields) => {
                if let Err(error) = self.unify(pattern.span, ty.clone(), structure_ty) {
                    self.add_error(error);
                }

                ty.apply(&self.ctx);

                let (id, params) = match ty.clone() {
                    engine::UnresolvedType::Named(id, params, _) => (id, params),
                    _ => {
                        self.compiler.add_error(
                            "cannot destructure this value",
                            vec![Note::primary(pattern.span, "value is not a structure")],
                        );

                        match_set.set_matched(true);

                        return MonomorphizedPatternKind::Destructure(
                            fields
                                .into_iter()
                                .map(|(_, (pattern, _))| {
                                    (
                                        FieldIndex::new(0),
                                        self.monomorphize_pattern(
                                            pattern,
                                            engine::UnresolvedType::Error,
                                            &mut MatchSet::Never,
                                            info,
                                        ),
                                    )
                                })
                                .collect(),
                        );
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

                        return MonomorphizedPatternKind::error(self.compiler);
                    }
                };

                let substitutions = structure
                    .params
                    .iter()
                    .map(|(_, param)| *param)
                    .zip(params)
                    .collect::<BTreeMap<_, _>>();

                let field_match_sets = match match_set {
                    MatchSet::Structure(fields) => fields,
                    _ => {
                        ty.apply(&self.ctx);
                        *match_set = MatchSet::new(&ty);

                        match match_set {
                            MatchSet::Structure(fields) => fields,
                            _ => {
                                self.compiler.add_error(
                                    "cannot destructure this value",
                                    vec![Note::primary(pattern.span, "value is not a structure")],
                                );

                                return MonomorphizedPatternKind::error(self.compiler);
                            }
                        }
                    }
                };

                let fields = fields
                    .into_iter()
                    .filter_map(|(name, (pattern, ty))| {
                        let index = match structure_field_names.get(&name) {
                            Some(index) => *index,
                            None => {
                                self.compiler.add_error(
                                    format!("value has no member named '{}'", name),
                                    vec![Note::primary(pattern.span, "no such member")],
                                );

                                return None;
                            }
                        };

                        let mut member_ty = engine::UnresolvedType::from(
                            structure_field_tys[index.into_inner()].1.clone(),
                        );

                        member_ty.instantiate_with(&mut self.ctx, &substitutions);

                        if let Err(error) = self.unify(pattern.span, ty, member_ty.clone()) {
                            self.add_error(error);
                        }

                        let (matches, match_set) = &mut field_match_sets[index.into_inner()];
                        *matches = true;

                        let pattern =
                            self.monomorphize_pattern(pattern, member_ty, match_set, info);

                        Some((index, pattern))
                    })
                    .collect();

                for (matches, field_match_set) in field_match_sets {
                    if *matches {
                        continue;
                    }

                    *matches = true;
                    field_match_set.set_matched(true);
                }

                MonomorphizedPatternKind::Destructure(fields)
            }
            UnresolvedPatternKind::Variant(variant_ty, variant, values) => {
                let (id, params) = match &ty {
                    engine::UnresolvedType::Named(id, params, _) => (id, params),
                    _ => {
                        return MonomorphizedPatternKind::Variant(
                            variant,
                            values
                                .into_iter()
                                .map(|pattern| {
                                    self.monomorphize_pattern(
                                        pattern,
                                        engine::UnresolvedType::Error,
                                        &mut MatchSet::Never,
                                        info,
                                    )
                                })
                                .collect(),
                        );
                    }
                };

                let enumeration = self
                    .with_type_decl(*id, Clone::clone)
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

                        return MonomorphizedPatternKind::error(self.compiler);
                    }
                };

                let substitutions = enumeration
                    .params
                    .iter()
                    .map(|(_, param)| *param)
                    .zip(params.iter().cloned())
                    .collect::<BTreeMap<_, _>>();

                for ty in &mut variant_tys {
                    ty.instantiate_with(&mut self.ctx, &substitutions);
                }

                if let Err(error) = self.unify(
                    pattern.span,
                    ty.clone(),
                    engine::UnresolvedType::Named(
                        variant_ty,
                        enumeration
                            .params
                            .iter()
                            .map(|(_, param)| substitutions.get(param).unwrap().clone())
                            .collect(),
                        // HACK: Optimization because unification doesn't take structure into
                        // account -- the structure can be applied during finalization
                        engine::TypeStructure::Marker,
                    ),
                ) {
                    self.add_error(error);
                }

                let (matches, variant_match_sets) = match match_set {
                    MatchSet::Enumeration(variants) => &mut variants[variant.into_inner()],
                    _ => {
                        ty.apply(&self.ctx);
                        *match_set = MatchSet::new(&ty);

                        match match_set {
                            MatchSet::Enumeration(variants) => &mut variants[variant.into_inner()],
                            _ => {
                                self.compiler.add_error(
                                    "cannot match a variant on this value",
                                    vec![Note::primary(
                                        pattern.span,
                                        "value is not an enumeration",
                                    )],
                                );

                                return MonomorphizedPatternKind::error(self.compiler);
                            }
                        }
                    }
                };

                *matches = true;

                MonomorphizedPatternKind::Variant(
                    variant,
                    values
                        .into_iter()
                        .zip(variant_tys)
                        .zip(variant_match_sets)
                        .map(|((pattern, variant_ty), match_set)| {
                            *matches = true;
                            self.monomorphize_pattern(pattern, variant_ty, match_set, info)
                        })
                        .collect(),
                )
            }
            UnresolvedPatternKind::Or(lhs, rhs) => MonomorphizedPatternKind::Or(
                Box::new(self.monomorphize_pattern(*lhs, ty.clone(), match_set, info)),
                Box::new(self.monomorphize_pattern(*rhs, ty, match_set, info)),
            ),
            UnresolvedPatternKind::Where(pattern, condition) => {
                let pattern = self.monomorphize_pattern(*pattern, ty, match_set, info);

                let condition_span = condition.span;
                let condition = self.monomorphize_expr(*condition, info);

                if let Some(boolean_ty) = self.entrypoint.global_attributes.language_items.boolean {
                    if let Err(error) = self.unify(
                        condition.span,
                        condition.ty.clone(),
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
                            condition_span,
                            "typechecking this condition requires the `boolean` language item",
                        )],
                    )
                }

                match_set.set_matched(false);

                MonomorphizedPatternKind::Where(Box::new(pattern), Box::new(condition))
            }
            UnresolvedPatternKind::Tuple(patterns) => {
                let tys = patterns
                    .iter()
                    .map(|_| engine::UnresolvedType::Variable(self.ctx.new_variable()))
                    .collect::<Vec<_>>();

                if let Err(error) = self.unify(
                    pattern.span,
                    ty.clone(),
                    engine::UnresolvedType::Tuple(tys.clone()),
                ) {
                    self.add_error(error);
                }

                let match_sets = match match_set {
                    MatchSet::Tuple(sets) => sets,
                    _ => {
                        ty.apply(&self.ctx);
                        *match_set = MatchSet::new(&ty);

                        match match_set {
                            MatchSet::Tuple(sets) => sets,
                            _ => return MonomorphizedPatternKind::error(self.compiler),
                        }
                    }
                };

                MonomorphizedPatternKind::Tuple(
                    patterns
                        .into_iter()
                        .zip(tys)
                        .zip(match_sets)
                        .map(|((pattern, ty), match_set)| {
                            self.monomorphize_pattern(pattern, ty, match_set, info)
                        })
                        .collect(),
                )
            }
        })();

        MonomorphizedPattern {
            span: pattern.span,
            kind,
        }
    }

    fn instance_for_ty(
        &mut self,
        tr: TraitId,
        ty: engine::UnresolvedType,
        use_span: Span,
        bound_span: Option<Span>,
        info: &mut MonomorphizeInfo,
    ) -> Result<Option<ConstantId>, Error> {
        let tr_decl = match self.with_trait_decl(tr, Clone::clone) {
            Some(decl) => decl,
            None => return Ok(None),
        };

        let mut trait_ty = tr_decl
            .ty
            .clone()
            .expect("`instance_for_ty` may only be used with traits that have values")
            .into();

        self.instantiate_generics(&mut trait_ty);

        if let Err(error) = self.unify(use_span, ty.clone(), trait_ty) {
            self.add_error(error);
            return Ok(None);
        }

        self.instance_for_inner(tr, tr_decl, Ok(ty), use_span, bound_span, info)
    }

    fn instance_for_params(
        &mut self,
        tr: TraitId,
        params: Vec<engine::UnresolvedType>,
        use_span: Span,
        bound_span: Option<Span>,
        info: &mut MonomorphizeInfo,
    ) -> Result<Option<ConstantId>, Error> {
        let tr_decl = match self.with_trait_decl(tr, Clone::clone) {
            Some(decl) => decl,
            None => return Ok(None),
        };

        self.instance_for_inner(tr, tr_decl, Err(params), use_span, bound_span, info)
    }

    fn instance_for_inner(
        &mut self,
        tr: TraitId,
        tr_decl: TraitDecl,
        ty_or_params: Result<engine::UnresolvedType, Vec<engine::UnresolvedType>>,
        use_span: Span,
        bound_span: Option<Span>,
        info: &mut MonomorphizeInfo,
    ) -> Result<Option<ConstantId>, Error> {
        let recursion_limit = self
            .entrypoint
            .global_attributes
            .recursion_limit
            .unwrap_or(Compiler::DEFAULT_RECURSION_LIMIT);

        if info.recursion_count > recursion_limit {
            self.compiler.add_error(
                "recursion limit reached",
                vec![Note::primary(use_span, "while computing this")],
            );

            return Ok(None);
        }

        let mut params = match ty_or_params {
            Ok(ty) => self.extract_params(tr, ty),
            Err(params) => params,
        };

        for param in &mut params {
            param.apply(&self.ctx);
        }

        // If a bound refers to itself, assume that the bound is satisfied
        if let Some(id) =
            info.instance_stack
                .entry(tr)
                .or_default()
                .iter()
                .find_map(|(id, stack_params)| {
                    let mut ctx = self.ctx.clone();
                    let mut error = false;
                    for (param, stack_param) in params.clone().into_iter().zip(stack_params.clone())
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
                    // ...if there are multiple candiates, try again finalizing default variables.
                    Some(Err(_)) => {
                        let params = params
                            .clone()
                            .into_iter()
                            .map(|mut ty| {
                                ty.finalize_default_variables(&self.ctx);
                                ty
                            })
                            .collect::<Vec<_>>();

                        match find_instance!(@find params, $resolve) {
                            Some(result) => return result,
                            None => {}
                        }
                    }
                    // ...if there are no candidates, continue the search.
                    None => {}
                }
            }};
            (@find $params:expr, $resolve:expr) => {{
                let params = $params;
                let mut candidates = $resolve(params.clone())
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
                    _ => Some(Err(self.error(
                        engine::TypeError::AmbiguousTrait(
                            tr,
                            params,
                            candidates.into_iter().map(|(_, _, span)| span).collect(),
                        ),
                        use_span,
                    ))),
                }
            }};
        }

        let bound_instances = info.bound_instances.get(&tr).cloned().unwrap_or_default();
        find_instance!(|params: Vec<engine::UnresolvedType>| {
            let mut candidates = Vec::new();
            for (instance_id, instance_params, span) in bound_instances.clone() {
                let prev_ctx = self.ctx.clone();

                let mut all_unify = true;
                for (param_ty, instance_param_ty) in params.clone().into_iter().zip(instance_params)
                {
                    if self.ctx.unify(param_ty, instance_param_ty).is_err() {
                        all_unify = false;
                        break;
                    }
                }

                if all_unify {
                    let ctx = mem::replace(&mut self.ctx, prev_ctx);
                    candidates.push((ctx, instance_id, span));
                } else {
                    self.ctx = prev_ctx;
                }
            }

            candidates
        });

        let declared_instances = self.instances.get(&tr).cloned().unwrap_or_default();
        find_instance!(|params: Vec<engine::UnresolvedType>| {
            let mut candidates = Vec::new();
            'check: for id in declared_instances.clone() {
                let (mut instance_params, instance_span, bounds) =
                    match self.with_instance_decl(id, |instance| {
                        (
                            instance
                                .trait_params
                                .clone()
                                .into_iter()
                                .map(From::from)
                                .collect::<Vec<_>>(),
                            instance.span,
                            instance.bounds.clone(),
                        )
                    }) {
                        Some((instance_params, instance_span, bounds)) => {
                            (instance_params, instance_span, bounds)
                        }
                        None => continue 'check,
                    };

                let mut substitutions = engine::GenericSubstitutions::new();
                for ty in &mut instance_params {
                    self.add_substitutions(ty, &mut substitutions);
                }

                let prev_ctx = self.ctx.clone();

                let mut all_unify = true;
                for (mut param_ty, mut instance_param_ty) in
                    params.clone().into_iter().zip(instance_params.clone())
                {
                    self.add_substitutions(&mut param_ty, &mut substitutions);
                    self.add_substitutions(&mut instance_param_ty, &mut substitutions);

                    if self.ctx.unify(param_ty, instance_param_ty).is_err() {
                        all_unify = false;
                    }
                }

                if !all_unify {
                    self.ctx = prev_ctx;
                    continue 'check;
                }

                for mut bound in bounds {
                    info.recursion_count += 1;

                    info.instance_stack
                        .entry(tr)
                        .or_default()
                        .push((id, bound.params.clone()));

                    for ty in &mut bound.params {
                        self.add_substitutions(ty, &mut substitutions);
                    }

                    let result = self.instance_for_params(
                        bound.trait_id,
                        bound.params,
                        bound.span,
                        Some(bound.span),
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

            candidates
        });

        Err(self.error(
            engine::TypeError::MissingInstance(tr, params, bound_span, error_candidates),
            use_span,
        ))
    }

    fn extract_params(
        &mut self,
        tr: TraitId,
        ty: engine::UnresolvedType,
    ) -> Vec<engine::UnresolvedType> {
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

        let params = self.ctx.unify_params(ty, tr_ty).0;

        tr_params
            .into_iter()
            .map(|(_, param)| {
                params
                    .get(&param)
                    .cloned()
                    .unwrap_or_else(|| engine::UnresolvedType::Variable(self.ctx.new_variable()))
            })
            .collect()
    }
}

#[derive(Debug, Clone)]
enum MatchSet {
    Never,
    Marker(bool),
    Structure(Vec<(bool, MatchSet)>),
    Enumeration(Vec<(bool, Vec<MatchSet>)>),
    Tuple(Vec<MatchSet>),
}

impl MatchSet {
    fn new(ty: &engine::UnresolvedType) -> Self {
        match ty {
            engine::UnresolvedType::Named(_, _, structure) => match structure {
                TypeStructure::Marker => MatchSet::Marker(false),
                TypeStructure::Structure(fields) => MatchSet::Structure(
                    fields.iter().map(|ty| (false, MatchSet::new(ty))).collect(),
                ),
                TypeStructure::Enumeration(variants) => MatchSet::Enumeration(
                    variants
                        .iter()
                        .map(|tys| (false, tys.iter().map(MatchSet::new).collect()))
                        .collect(),
                ),
                TypeStructure::Recursive(_) => {
                    // Returning Never is OK because the user has to stop matching on the structure
                    // of the type at some point -- eventually they will refer to the rest of the
                    // structure using a name, which always matches (FIXME: Verify that this is correct)
                    MatchSet::Never
                }
            },
            engine::UnresolvedType::Tuple(tys) => {
                MatchSet::Tuple(tys.iter().map(MatchSet::new).collect())
            }
            engine::UnresolvedType::Error => MatchSet::Never,
            _ => MatchSet::Marker(false),
        }
    }
}

impl<'a, 'l> Typechecker<'a, 'l> {
    fn assert_matched(
        &mut self,
        match_set: &MatchSet,
        ty: &engine::UnresolvedType,
        span: Span,
        for_exhaustive_pattern: bool,
    ) {
        if !match_set.is_matched() {
            if for_exhaustive_pattern {
                self.compiler.add_error(
                    "pattern is not exhaustive",
                    vec![Note::primary(
                        span,
                        "this pattern does not handle all possible values",
                    )],
                );
            } else {
                let mut ty = ty.clone();
                ty.apply(&self.ctx);

                let mut missing_patterns = match_set
                    .unmatched_patterns(&ty)
                    .unique()
                    .map(|pattern| format!("`{}`", self.format_unmatched_pattern(&pattern, false)))
                    .collect::<Vec<_>>();

                self.compiler.add_error(
                    "`when` expression is not exhaustive",
                    vec![Note::primary(
                        span,
                        match missing_patterns.len() {
                            0 => String::from(
                                "try adding some more patterns to cover all possible values",
                            ),
                            1 => format!(
                                "try adding a case for the {} pattern",
                                missing_patterns.pop().unwrap()
                            ),
                            _ => {
                                let last_missing_pattern = missing_patterns.pop().unwrap();

                                format!(
                                    "try adding cases for the {} and {} patterns",
                                    missing_patterns.join(", "),
                                    last_missing_pattern
                                )
                            }
                        },
                    )],
                );
            }
        }
    }
}

impl MatchSet {
    fn is_matched(&self) -> bool {
        match self {
            MatchSet::Never => true,
            MatchSet::Marker(matches) => *matches,
            MatchSet::Structure(fields) => fields
                .iter()
                .all(|(matches, field)| *matches && field.is_matched()),
            MatchSet::Enumeration(variants) => variants.iter().all(|(matches, variant)| {
                *matches && variant.iter().all(|variant| variant.is_matched())
            }),
            MatchSet::Tuple(sets) => sets.iter().all(|set| set.is_matched()),
        }
    }

    fn set_matched(&mut self, is_matched: bool) {
        match self {
            MatchSet::Marker(matches) => *matches = is_matched,
            MatchSet::Structure(fields) => {
                for (matches, field) in fields {
                    *matches = is_matched;
                    field.set_matched(is_matched);
                }
            }
            MatchSet::Enumeration(variants) => {
                for (matches, variant) in variants {
                    *matches = is_matched;
                    for match_set in variant {
                        match_set.set_matched(is_matched);
                    }
                }
            }
            MatchSet::Tuple(sets) => {
                for set in sets {
                    set.set_matched(is_matched);
                }
            }
            _ => {}
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct UnmatchedPattern<'a> {
    ty: &'a engine::UnresolvedType,
    kind: UnmatchedPatternKind<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum UnmatchedPatternKind<'a> {
    Wildcard,
    Marker,
    Structure(FieldIndex, Option<Box<UnmatchedPattern<'a>>>),
    Variant(VariantIndex, Vec<UnmatchedPattern<'a>>),
    Tuple(Vec<UnmatchedPattern<'a>>),
}

impl MatchSet {
    fn unmatched_patterns<'a>(
        &'a self,
        ty: &'a engine::UnresolvedType,
    ) -> Box<dyn Iterator<Item = UnmatchedPattern<'a>> + 'a> {
        if self.is_matched() {
            return Box::new(std::iter::empty());
        }

        match self {
            MatchSet::Never => Box::new(std::iter::once(UnmatchedPattern {
                ty,
                kind: UnmatchedPatternKind::Wildcard,
            })),
            MatchSet::Marker(_) => Box::new(std::iter::once(UnmatchedPattern {
                ty,
                kind: UnmatchedPatternKind::Marker,
            })),
            MatchSet::Structure(fields) => {
                let field_tys = match ty {
                    engine::UnresolvedType::Named(_, _, TypeStructure::Structure(field_tys)) => {
                        field_tys
                    }
                    _ => return Box::new(std::iter::empty()),
                };

                Box::new(fields.iter().enumerate().flat_map(
                    move |(index, (matched, field))| -> Box<dyn Iterator<Item = UnmatchedPattern>> {
                        if !matched {
                            return Box::new(std::iter::once(UnmatchedPattern {
                                ty,
                                kind: UnmatchedPatternKind::Structure(FieldIndex::new(index), None),
                            }));
                        }

                        let field_ty = &field_tys[index];

                        Box::new(field.unmatched_patterns(field_ty).map(move |item| {
                            UnmatchedPattern {
                                ty: field_ty,
                                kind: UnmatchedPatternKind::Structure(
                                    FieldIndex::new(index),
                                    Some(Box::new(item)),
                                ),
                            }
                        }))
                    },
                ))
            }
            MatchSet::Enumeration(variants) => {
                let variants_tys = match ty {
                    engine::UnresolvedType::Named(
                        _,
                        _,
                        TypeStructure::Enumeration(variants_tys),
                    ) => variants_tys,
                    _ => return Box::new(std::iter::empty()),
                };

                let mut patterns = vec![BTreeMap::new()];

                // First, collect all variants with unmatched associated values
                for (variant_index, (_, items)) in variants.iter().enumerate() {
                    let variant_tys = &variants_tys[variant_index];

                    for (item_index, item) in items.iter().enumerate() {
                        let item_ty = &variant_tys[item_index];

                        for (pattern_index, pattern) in item.unmatched_patterns(item_ty).enumerate()
                        {
                            if patterns.len() == pattern_index {
                                patterns.push(BTreeMap::new());
                            }

                            patterns[pattern_index]
                                .entry(VariantIndex::new(variant_index))
                                .or_insert_with(|| {
                                    let item_ty = &variant_tys[item_index];

                                    vec![
                                        UnmatchedPattern {
                                            ty: item_ty,
                                            kind: UnmatchedPatternKind::Wildcard
                                        };
                                        items.len()
                                    ]
                                })[item_index] = pattern;
                        }
                    }
                }

                // Then, collect all unmatched variants without associated values
                for (variant_index, (matched, items)) in variants.iter().enumerate() {
                    if !matched && items.is_empty() {
                        for variants in &mut patterns {
                            *variants
                                .entry(VariantIndex::new(variant_index))
                                .or_default() = Vec::new();
                        }
                    }
                }

                Box::new(patterns.into_iter().flat_map(|variants| {
                    variants
                        .into_iter()
                        .map(|(variant, items)| UnmatchedPattern {
                            ty,
                            kind: UnmatchedPatternKind::Variant(variant, items),
                        })
                }))
            }
            MatchSet::Tuple(items) => {
                let item_tys = match ty {
                    engine::UnresolvedType::Tuple(tys) => tys,
                    _ => return Box::new(std::iter::empty()),
                };

                let mut patterns = Vec::new();
                for (item_index, item) in items.iter().enumerate() {
                    let item_ty = &item_tys[item_index];

                    for (pattern_index, pattern) in item.unmatched_patterns(item_ty).enumerate() {
                        if patterns.len() == pattern_index {
                            patterns.push(vec![
                                UnmatchedPattern {
                                    ty: item_ty,
                                    kind: UnmatchedPatternKind::Wildcard,
                                };
                                items.len()
                            ]);
                        }

                        patterns[pattern_index][item_index] = pattern;
                    }
                }

                Box::new(patterns.into_iter().map(|items| UnmatchedPattern {
                    ty,
                    kind: UnmatchedPatternKind::Tuple(items),
                }))
            }
        }
    }
}

impl<'a, 'l> Typechecker<'a, 'l> {
    fn format_unmatched_pattern(&self, pattern: &UnmatchedPattern, parenthesize: bool) -> String {
        match &pattern.kind {
            UnmatchedPatternKind::Wildcard => String::from("_"),
            UnmatchedPatternKind::Marker => {
                let id = match pattern.ty {
                    engine::UnresolvedType::Named(id, _, _) => id,
                    _ => return String::from("_"),
                };

                let name = self.declarations.borrow().types.get(id).unwrap().name;

                name.to_string()
            }
            UnmatchedPatternKind::Structure(index, field) => {
                let id = match pattern.ty {
                    engine::UnresolvedType::Named(id, _, _) => id,
                    _ => return String::from("_"),
                };

                let declarations = self.declarations.borrow();

                let field_names = match &declarations.types.get(id).unwrap().kind {
                    TypeDeclKind::Structure { field_names, .. } => field_names,
                    _ => return String::from("_"),
                };

                let field_name = field_names
                    .iter()
                    .find_map(|(name, i)| (i == index).then_some(name.to_string()))
                    .unwrap_or_else(|| String::from("<unknown>"));

                if let Some(pattern) = field {
                    format!(
                        "{{ {} : {} }}",
                        field_name,
                        self.format_unmatched_pattern(pattern, false)
                    )
                } else {
                    format!("{{ {} }}", field_name)
                }
            }
            UnmatchedPatternKind::Variant(index, patterns) => {
                let id = match pattern.ty {
                    engine::UnresolvedType::Named(id, _, _) => id,
                    _ => return String::from("_"),
                };

                let declarations = self.declarations.borrow();
                let ty = declarations.types.get(id).unwrap();
                let variant_names = match &ty.kind {
                    TypeDeclKind::Enumeration { variant_names, .. } => variant_names,
                    _ => return String::from("_"),
                };

                let variant_name = variant_names
                    .iter()
                    .find_map(|(name, i)| (i == index).then_some(name.as_str()))
                    .unwrap_or("<unknown>");

                let formatted = if patterns.is_empty() {
                    format!("{} {}", ty.name, variant_name)
                } else {
                    format!(
                        "{} {} {}",
                        ty.name,
                        variant_name,
                        patterns
                            .iter()
                            .map(|pattern| self.format_unmatched_pattern(pattern, true))
                            .collect::<Vec<_>>()
                            .join(" ")
                    )
                };

                if parenthesize {
                    format!("({})", formatted)
                } else {
                    formatted
                }
            }
            UnmatchedPatternKind::Tuple(patterns) => {
                let formatted = patterns
                    .iter()
                    .map(|pattern| self.format_unmatched_pattern(pattern, true))
                    .collect::<Vec<_>>()
                    .join(" , ");

                if parenthesize {
                    format!("({})", formatted)
                } else {
                    formatted
                }
            }
        }
    }
}

impl<'a, 'l> Typechecker<'a, 'l> {
    fn finalize_expr(&mut self, expr: MonomorphizedExpression) -> Expression {
        let ty = match expr.ty.finalize(&self.ctx) {
            Ok(ty) => ty,
            Err(error) => {
                self.add_error(self.error(error, expr.span));
                engine::Type::Error
            }
        };

        let kind = (|| match expr.kind {
            MonomorphizedExpressionKind::Error(trace) => ExpressionKind::Error(trace),
            MonomorphizedExpressionKind::Marker => ExpressionKind::Marker,
            MonomorphizedExpressionKind::Constant(id) => ExpressionKind::Constant(id),
            MonomorphizedExpressionKind::Variable(var) => ExpressionKind::Variable(var),
            MonomorphizedExpressionKind::Text(text) => ExpressionKind::Text(text),
            MonomorphizedExpressionKind::Number(number) => {
                match parse_number!(number, ExpressionKind, &ty, Type) {
                    Some(Ok(number)) => number,
                    Some(Err(error)) => {
                        self.add_error(self.error(error, expr.span));
                        ExpressionKind::error(self.compiler)
                    }
                    None => {
                        self.add_error(self.error(
                            engine::TypeError::Mismatch(
                                engine::UnresolvedType::Builtin(engine::BuiltinType::Number),
                                expr.ty,
                            ),
                            expr.span,
                        ));

                        ExpressionKind::error(self.compiler)
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
            MonomorphizedExpressionKind::End(value) => {
                ExpressionKind::End(Box::new(self.finalize_expr(*value)))
            }
            MonomorphizedExpressionKind::Call(func, input) => ExpressionKind::Call(
                Box::new(self.finalize_expr(*func)),
                Box::new(self.finalize_expr(*input)),
            ),
            MonomorphizedExpressionKind::Function(pattern, body, captures) => {
                let input_ty = match &ty {
                    engine::Type::Function(input, _) => input.clone(),
                    _ => return ExpressionKind::error(self.compiler),
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
            MonomorphizedExpressionKind::Runtime(func, inputs) => ExpressionKind::Runtime(
                func,
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
        })();

        Expression {
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
            span: pattern.span,
            kind: (|| match pattern.kind {
                MonomorphizedPatternKind::Error(trace) => PatternKind::Error(trace),
                MonomorphizedPatternKind::Wildcard => PatternKind::Wildcard,
                MonomorphizedPatternKind::Number(number) => {
                    match parse_number!(number, PatternKind, input_ty, Type) {
                        Some(Ok(number)) => number,
                        Some(Err(error)) => {
                            self.add_error(self.error(error, pattern.span));
                            PatternKind::error(self.compiler)
                        }
                        None => {
                            self.add_error(self.error(
                                engine::TypeError::Mismatch(
                                    engine::UnresolvedType::Builtin(engine::BuiltinType::Number),
                                    input_ty.clone().into(),
                                ),
                                pattern.span,
                            ));

                            PatternKind::error(self.compiler)
                        }
                    }
                }
                MonomorphizedPatternKind::Text(text) => PatternKind::Text(text),
                MonomorphizedPatternKind::Variable(var) => {
                    self.with_variable_decl(var, input_ty.clone(), |_| {});
                    PatternKind::Variable(var)
                }
                MonomorphizedPatternKind::Destructure(fields) => {
                    let input_tys = match input_ty {
                        engine::Type::Named(_, _, engine::TypeStructure::Structure(fields)) => {
                            fields
                        }
                        _ => return PatternKind::error(self.compiler),
                    };

                    PatternKind::Destructure(
                        fields
                            .into_iter()
                            .zip(input_tys)
                            .map(|((index, field), ty)| (index, self.finalize_pattern(field, ty)))
                            .collect(),
                    )
                }
                MonomorphizedPatternKind::Variant(index, values) => {
                    let input_tys = match input_ty {
                        engine::Type::Named(_, _, engine::TypeStructure::Enumeration(variants)) => {
                            &variants[index.into_inner()]
                        }
                        _ => return PatternKind::error(self.compiler),
                    };

                    PatternKind::Variant(
                        index,
                        values
                            .into_iter()
                            .zip(input_tys)
                            .map(|(value, ty)| self.finalize_pattern(value, ty))
                            .collect(),
                    )
                }
                MonomorphizedPatternKind::Or(lhs, rhs) => PatternKind::Or(
                    Box::new(self.finalize_pattern(*lhs, input_ty)),
                    Box::new(self.finalize_pattern(*rhs, input_ty)),
                ),
                MonomorphizedPatternKind::Where(pattern, condition) => PatternKind::Where(
                    Box::new(self.finalize_pattern(*pattern, input_ty)),
                    Box::new(self.finalize_expr(*condition)),
                ),
                MonomorphizedPatternKind::Tuple(patterns) => {
                    let input_tys = match input_ty {
                        engine::Type::Tuple(tys) => tys,
                        _ => return PatternKind::error(self.compiler),
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
}

impl<'a, 'l> Typechecker<'a, 'l> {
    fn with_type_decl<T>(&mut self, id: TypeId, f: impl FnOnce(&TypeDecl) -> T) -> Option<T> {
        if let Some(decl) = self.declarations.borrow().types.get(&id) {
            return Some(f(decl));
        }

        let decl = self.entrypoint.declarations.types.get(&id)?.clone();

        let decl = TypeDecl {
            name: decl
                .name
                .unwrap_or_else(|| InternedString::new("<unknown>")),
            span: decl.span,
            params: decl.value.params,
            kind: match decl.value.kind {
                lower::TypeKind::Marker => TypeDeclKind::Marker,
                lower::TypeKind::Structure(fields, field_names) => TypeDeclKind::Structure {
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
                },
                lower::TypeKind::Enumeration(variants, variant_names) => {
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
            },
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

    fn with_trait_decl<T>(&mut self, id: TraitId, f: impl FnOnce(&TraitDecl) -> T) -> Option<T> {
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
            specializations: self
                .entrypoint
                .specializations
                .get(&id)
                .cloned()
                .unwrap_or_default(),
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
                        .unwrap_or_else(|| lower::Expression::error(self.compiler, decl.span)),
                ),
            );
        }

        let mut params = decl
            .value
            .trait_params
            .clone()
            .into_iter()
            .map(|ty| self.convert_generic_type_annotation(ty))
            .collect::<Vec<_>>();

        for (_, param) in tr.params.iter().skip(params.len()) {
            let name = match self.with_type_parameter_decl(*param, |decl| decl.name) {
                Some(name) => name,
                None => {
                    params.push(engine::Type::Error);
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

            params.push(engine::Type::Error);
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

                    let mut all_unify = true;
                    for (instance_param_ty, other_param_ty) in
                        params.clone().into_iter().zip(other.trait_params)
                    {
                        let mut substitutions = engine::GenericSubstitutions::new();

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
                    .chain(colliding_instances.into_iter().map(|span| {
                        Note::secondary(span, "this instance could apply to the same type(s)")
                    }))
                    .collect(),
                );
            }
        }

        let decl = InstanceDecl {
            span: decl.span,
            params: decl.value.params,
            bounds,
            bound_annotations: decl
                .value
                .bounds
                .into_iter()
                .map(|bound| (bound.tr, bound.parameters))
                .collect(),
            trait_id,
            trait_params: params,
            trait_param_annotations: decl.value.trait_params,
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

    fn with_operator_decl<T>(
        &mut self,
        id: TemplateId,
        f: impl FnOnce(&OperatorDecl) -> T,
    ) -> Option<T> {
        if let Some(decl) = self.declarations.borrow().operators.get(&id) {
            return Some(f(decl));
        }

        let operator = *self.entrypoint.declarations.operators.get(&id)?;

        let decl = self
            .entrypoint
            .declarations
            .templates
            .get(&operator.template)?
            .clone();

        let decl = OperatorDecl {
            name: decl.name,
            span: decl.span,
            uses: decl.uses,
        };

        Some(f(self
            .declarations
            .borrow_mut()
            .operators
            .entry(id)
            .or_insert(decl)))
    }

    fn with_template_decl<T>(
        &mut self,
        id: TemplateId,
        f: impl FnOnce(&TemplateDecl) -> T,
    ) -> Option<T> {
        if let Some(decl) = self.declarations.borrow().templates.get(&id) {
            return Some(f(decl));
        }

        let decl = self.entrypoint.declarations.templates.get(&id)?.clone();

        let decl = TemplateDecl {
            name: decl.name,
            span: decl.span,
            attributes: decl.attributes,
            uses: decl.uses,
        };

        Some(f(self
            .declarations
            .borrow_mut()
            .templates
            .entry(id)
            .or_insert(decl)))
    }

    fn with_builtin_type_decl<T>(
        &mut self,
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
        &mut self,
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
        &mut self,
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
}

impl<'a, 'l> Typechecker<'a, 'l> {
    fn unify(
        &mut self,
        span: Span,
        actual: engine::UnresolvedType,
        expected: impl Into<engine::UnresolvedType>,
    ) -> Result<(), Error> {
        self.ctx
            .unify(actual, expected)
            .map_err(|e| self.error(e, span))
    }

    fn add_substitutions(
        &mut self,
        ty: &mut engine::UnresolvedType,
        substitutions: &mut BTreeMap<TypeParameterId, engine::UnresolvedType>,
    ) {
        ty.apply(&self.ctx);

        for param in ty.params() {
            substitutions
                .entry(param)
                .or_insert_with(|| engine::UnresolvedType::Variable(self.ctx.new_variable()));
        }

        ty.instantiate_with(&mut self.ctx, substitutions);
    }

    fn instantiate_generics(&mut self, ty: &mut engine::UnresolvedType) {
        self.add_substitutions(ty, &mut GenericSubstitutions::new());
    }

    fn convert_type_annotation(&mut self, annotation: TypeAnnotation) -> engine::UnresolvedType {
        self.convert_type_annotation_inner(
            annotation,
            &|typechecker, _| {
                Some(engine::UnresolvedType::Variable(
                    typechecker.ctx.new_variable(),
                ))
            },
            &mut Vec::new(),
        )
    }

    fn convert_generic_type_annotation(&mut self, annotation: TypeAnnotation) -> engine::Type {
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
                            uses: HashSet::from([span]),
                        },
                    );

                Some(engine::UnresolvedType::Parameter(param))
            },
            &mut Vec::new(),
        );

        match ty.finalize(&self.ctx) {
            Ok(ty) => ty,
            Err(error) => {
                self.add_error(self.error(error, span));
                engine::Type::Error
            }
        }
    }

    fn convert_finalized_type_annotation(&mut self, annotation: TypeAnnotation) -> engine::Type {
        let span = annotation.span;

        let ty = self.convert_type_annotation_inner(annotation, &|_, _| None, &mut Vec::new());

        match ty.finalize(&self.ctx) {
            Ok(ty) => ty,
            Err(error) => {
                self.add_error(self.error(error, span));
                engine::Type::Error
            }
        }
    }

    fn convert_type_annotation_inner(
        &mut self,
        annotation: TypeAnnotation,
        convert_placeholder: &impl Fn(&mut Self, Span) -> Option<engine::UnresolvedType>,
        stack: &mut Vec<TypeId>,
    ) -> engine::UnresolvedType {
        match annotation.kind {
            TypeAnnotationKind::Error => engine::UnresolvedType::Error,
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

                for (_, param) in ty.value.params.iter().skip(params.len()) {
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
                    .params
                    .iter()
                    .map(|(_, param)| *param)
                    .zip(params.iter().cloned())
                    .collect::<GenericSubstitutions>();

                let mut convert_and_instantiate = |ty| {
                    let mut ty = self.convert_type_annotation_inner(ty, convert_placeholder, stack);
                    ty.instantiate_with(&mut self.ctx, &substitutions);
                    ty
                };

                let structure = match &ty.value.kind {
                    lower::TypeKind::Marker => engine::TypeStructure::Marker,
                    lower::TypeKind::Structure(fields, _) => engine::TypeStructure::Structure(
                        fields
                            .iter()
                            .map(|field| convert_and_instantiate(field.ty.clone()))
                            .collect(),
                    ),
                    lower::TypeKind::Enumeration(variants, _) => {
                        engine::TypeStructure::Enumeration(
                            variants
                                .iter()
                                .map(|variant| {
                                    variant
                                        .tys
                                        .iter()
                                        .map(|ty| convert_and_instantiate(ty.clone()))
                                        .collect()
                                })
                                .collect(),
                        )
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
                    lower::BuiltinTypeKind::Number => {
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
                    lower::BuiltinTypeKind::Integer => {
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
                    lower::BuiltinTypeKind::Natural => {
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
                    lower::BuiltinTypeKind::Byte => {
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
                    lower::BuiltinTypeKind::Signed => {
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
                    lower::BuiltinTypeKind::Unsigned => {
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
                    lower::BuiltinTypeKind::Float => {
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
                    lower::BuiltinTypeKind::Double => {
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
                    lower::BuiltinTypeKind::Text => {
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
                    lower::BuiltinTypeKind::Boolean => {
                        if !parameters.is_empty() {
                            self.compiler.add_error(
                                "`Boolean` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            );
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Number)
                    }
                    lower::BuiltinTypeKind::List => {
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
                    lower::BuiltinTypeKind::Mutable => {
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
        span: Span,
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
            .map(|(_, param)| param)
            .zip(params)
            .collect::<GenericSubstitutions>();

        let mut instance_ty = engine::UnresolvedType::from(trait_ty);
        instance_ty.instantiate_with(&mut self.ctx, &substitutions);

        instance_ty
    }
}

impl<'a, 'l> Typechecker<'a, 'l> {
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

        let (unresolved_type_errors, other_errors): (Vec<_>, Vec<_>) = mem::take(&mut self.errors)
            .into_iter()
            .partition(|e| matches!(e.error, engine::TypeError::UnresolvedType(_)));

        let should_report_unresolved_type_errors = other_errors.is_empty();

        for error in other_errors {
            self.report_error(error);
        }

        if should_report_unresolved_type_errors {
            for error in unresolved_type_errors {
                self.report_error(error);
            }
        }
    }

    fn report_error(&mut self, mut error: Error) {
        let format = format::Format {
            type_function: format::TypeFunctionFormat::Description,
            type_variable: format::TypeVariableFormat::Description,
            surround_in_backticks: true,
        };

        #[allow(unused_mut)]
        let mut diagnostic = match error.error {
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

                self.compiler.error_with(
                    "mismatched types",
                    std::iter::once(Note::primary(
                        error.span,
                        format!(
                            "expected {}, but found {}",
                            self.format_type(expected.clone(), format),
                            self.format_type(actual, format)
                        ),
                    ))
                    .chain(actual_ty.and_then(|(actual_ty, mut actual_params)| {
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
                                            .position(|(_, p)| p == param)
                                            .expect("type parameter associated with wrong type");

                                        let inner_ty = actual_params[param].clone();

                                        self.ctx.clone().unify(inner_ty, expected.clone()).is_ok()
                                    })
                                    .then(|| Note::secondary(error.span, message))
                            })
                    }))
                    .collect(),
                    error.trace,
                )
            }
            engine::TypeError::MissingInstance(id, params, bound_span, error_candidates) => {
                let trait_attributes = self
                    .declarations
                    .borrow()
                    .traits
                    .get(&id)
                    .unwrap()
                    .attributes
                    .clone();

                self.compiler.error_with(
                    "missing instance",
                    std::iter::once(Note::primary(
                        error.span,
                        format!(
                            "could not find instance {}",
                            self.format_type(format::FormattableType::r#trait(id, params), format)
                        ),
                    ))
                    .chain(
                        bound_span.map(|span| Note::secondary(span, "required by this bound here")),
                    )
                    .chain(error_candidates.into_iter().map(|span| {
                        Note::secondary(
                            span,
                            "this instance could apply, but its bounds weren't satisfied",
                        )
                    }))
                    .chain(
                        trait_attributes
                            .on_unimplemented
                            .map(|message| Note::secondary(error.span, message)),
                    )
                    .collect(),
                    error.trace,
                )
            }
            engine::TypeError::AmbiguousTrait(_, _, candidates) => self.compiler.error_with(
                "could not determine the type of this expression",
                std::iter::once(Note::primary(
                    error.span,
                    "try annotating the type with `::`",
                ))
                .chain(
                    candidates
                        .into_iter()
                        .map(|span| Note::secondary(span, "this instance could apply")),
                )
                .collect(),
                error.trace,
            ),
            engine::TypeError::UnresolvedType(mut ty) => {
                ty.apply(&self.ctx);

                self.compiler.error_with(
                    "could not determine the type of this expression",
                    std::iter::once(Note::primary(
                        error.span,
                        "try annotating the type with `::`",
                    ))
                    .chain(
                        (!matches!(ty, engine::UnresolvedType::Variable(_))).then(|| {
                            Note::primary(
                                error.span,
                                format!("this has type {}", self.format_type(ty, format)),
                            )
                        }),
                    )
                    .collect(),
                    error.trace,
                )
            }
            engine::TypeError::InvalidNumericLiteral(ty) => self.compiler.error_with(
                format!(
                    "number does not fit into a {}",
                    self.format_type(ty, format)
                ),
                vec![Note::primary(error.span, "invalid numeric literal")],
                error.trace,
            ),
        };

        diagnostic.notes.append(&mut error.notes);

        self.compiler.diagnostics.add(diagnostic);
    }
}
