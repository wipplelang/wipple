#![allow(clippy::type_complexity)]

#[macro_use]
mod number;

mod engine;
pub mod format;
pub mod traverse;

pub use engine::{BottomTypeReason, BuiltinType, GenericSubstitutions, Type, TypeStructure};
pub use lower::RuntimeFunction;

use crate::{
    analysis::{expand, lower},
    diagnostics::{Diagnostic, Note},
    helpers::InternedString,
    parse::Span,
    BuiltinTypeId, Compiler, ConstantId, FilePath, ItemId, TemplateId, TraitId, TypeId,
    TypeParameterId, Uses, VariableId,
};
use itertools::Itertools;
use serde::Serialize;
use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    mem,
    os::raw::{c_int, c_uint},
};

#[derive(Debug)]
pub enum Progress {
    Collecting {
        path: FilePath,
        current: usize,
        total: usize,
    },
    Resolving {
        count: usize,
        remaining: usize,
    },
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct Program {
    pub complete: bool,
    pub items: BTreeMap<ItemId, Expression>,
    pub entrypoint: Option<ItemId>,
    pub declarations: Declarations,
    pub exported: lower::ScopeValues,
    pub scopes: Vec<(Span, lower::ScopeValues)>,
}

macro_rules! declarations {
    ($name:ident<$($container:ident)::+>) => {
        #[derive(Debug, Clone, Default, Serialize)]
        pub struct $name {
            pub types: $($container)::+<TypeId, TypeDecl>,
            pub traits: $($container)::+<TraitId, TraitDecl>,
            pub constants: $($container)::+<ConstantId, ConstantDecl>,
            pub instances: $($container)::+<TraitId, BTreeMap<ConstantId, InstanceDecl>>,
            pub operators: $($container)::+<TemplateId, OperatorDecl>,
            pub templates: $($container)::+<TemplateId, TemplateDecl>,
            pub builtin_types: $($container)::+<BuiltinTypeId, BuiltinTypeDecl>,
            pub type_parameters: $($container)::+<TypeParameterId, TypeParameterDecl>,
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

#[derive(Debug, Clone, Serialize)]
pub struct TypeDecl {
    pub name: InternedString,
    pub span: Span,
    pub params: Vec<TypeParameterId>,
    pub kind: TypeDeclKind,
    pub attributes: lower::TypeAttributes,
    pub uses: HashSet<Span>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum TypeDeclKind {
    Marker,
    Structure {
        fields: Vec<engine::Type>,
        field_names: HashMap<InternedString, usize>,
    },
    Enumeration {
        variants: Vec<Vec<engine::Type>>,
        variant_names: HashMap<InternedString, usize>,
    },
}

#[derive(Debug, Clone, Serialize)]
pub struct TraitDecl {
    pub name: InternedString,
    pub span: Span,
    pub params: Vec<TypeParameterId>,
    pub ty: engine::Type,
    pub attributes: lower::TraitAttributes,
    pub uses: HashSet<Span>,
}

#[derive(Debug, Clone, Serialize)]
pub struct ConstantDecl {
    pub name: InternedString,
    pub span: Span,
    pub bounds: Vec<Bound>,
    pub ty: engine::Type,
    pub specializations: Vec<ConstantId>,
    pub attributes: lower::ConstantAttributes,
    pub uses: HashSet<Span>,
}

#[derive(Debug, Clone, Serialize)]
pub struct InstanceDecl {
    pub span: Span,
    pub trait_id: TraitId,
    pub bounds: Vec<Bound>,
    pub ty: engine::Type,
    pub item: ItemId,
}

#[derive(Debug, Clone, Serialize)]
pub struct Bound {
    pub span: Span,
    pub trait_id: TraitId,
    pub params: Vec<engine::UnresolvedType>,
}

#[derive(Debug, Clone, Serialize)]
pub struct OperatorDecl {
    pub name: InternedString,
    pub span: Span,
    pub uses: HashSet<Span>,
}

#[derive(Debug, Clone, Serialize)]
pub struct TemplateDecl {
    pub name: InternedString,
    pub span: Span,
    pub attributes: expand::TemplateAttributes,
    pub uses: HashSet<Span>,
}

#[derive(Debug, Clone, Serialize)]
pub struct BuiltinTypeDecl {
    pub name: InternedString,
    pub span: Span,
    pub attributes: lower::DeclarationAttributes,
    pub uses: HashSet<Span>,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypeParameterDecl {
    pub name: Option<InternedString>,
    pub span: Span,
    pub uses: HashSet<Span>,
}

#[derive(Debug, Clone, Serialize)]
pub struct VariableDecl {
    pub name: Option<InternedString>,
    pub span: Span,
    pub ty: engine::Type,
    pub uses: HashSet<Span>,
}

macro_rules! expr {
    ($vis:vis, $prefix:literal, $type:ty, { $($kinds:tt)* }) => {
        paste::paste! {
            #[derive(Debug, Clone, Serialize)]
            $vis struct [<$prefix Expression>] {
                $vis span: Span,
                $vis ty: $type,
                $vis kind: [<$prefix ExpressionKind>],
            }

            #[derive(Debug, Clone, Serialize)]
            #[serde(tag = "type", content = "value")]
            $vis enum [<$prefix ExpressionKind>] {
                Marker,
                Variable(VariableId),
                Text(InternedString),
                Block(Vec<[<$prefix Expression>]>),
                End(Box<[<$prefix Expression>]>),
                Call(Box<[<$prefix Expression>]>, Box<[<$prefix Expression>]>),
                Function([<$prefix Pattern>], Box<[<$prefix Expression>]>, lower::CaptureList),
                When(Box<[<$prefix Expression>]>, Vec<[<$prefix Arm>]>),
                External(InternedString, InternedString, Vec<[<$prefix Expression>]>),
                Runtime(RuntimeFunction, Vec<[<$prefix Expression>]>),
                Initialize([<$prefix Pattern>], Box<[<$prefix Expression>]>),
                Structure(Vec<[<$prefix Expression>]>),
                Variant(usize, Vec<[<$prefix Expression>]>),
                Tuple(Vec<[<$prefix Expression>]>),
                $($kinds)*
            }

            #[derive(Debug, Clone, Serialize)]
            $vis struct [<$prefix Arm>] {
                $vis span: Span,
                $vis pattern: [<$prefix Pattern>],
                $vis body: [<$prefix Expression>],
            }
        }
    };
}

macro_rules! pattern {
    ($vis:vis, $prefix:literal, { $($kinds:tt)* }) => {
        paste::paste! {
            #[derive(Debug, Clone, Serialize)]
            $vis struct [<$prefix Pattern>] {
                $vis span: Span,
                $vis kind: [<$prefix PatternKind>],
            }

            #[derive(Debug, Clone, Serialize)]
            #[serde(tag = "type", content = "value")]
            $vis enum [<$prefix PatternKind>] {
                Wildcard,
                Text(InternedString),
                Variable(VariableId),
                Or(Box<[<$prefix Pattern>]>, Box<[<$prefix Pattern>]>),
                Where(Box<[<$prefix Pattern>]>, Box<[<$prefix Expression>]>),
                Tuple(Vec<[<$prefix Pattern>]>),
                $($kinds)*
            }
        }
    };
}

expr!(, "Unresolved", engine::UnresolvedType, {
    Error,
    Number(InternedString),
    Trait(TraitId),
    Constant(ConstantId),
});

expr!(, "Monomorphized", engine::UnresolvedType, {
    Error,
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

pattern!(, "Unresolved", {
    Error,
    Number(InternedString),
    Destructure(
        engine::UnresolvedType,
        HashMap<InternedString, (UnresolvedPattern, engine::UnresolvedType)>,
    ),
    Variant(TypeId, usize, Vec<UnresolvedPattern>),
});

pattern!(, "Monomorphized", {
    Error,
    Number(InternedString),
    Destructure(BTreeMap<usize, MonomorphizedPattern>),
    Variant(usize, Vec<MonomorphizedPattern>),
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
    Destructure(BTreeMap<usize, Pattern>),
    Variant(usize, Vec<Pattern>)
});

#[derive(Debug, Clone)]
pub struct Error {
    pub error: engine::TypeError,
    pub span: Span,
    pub notes: Vec<Note>,
    #[cfg(debug_assertions)]
    pub trace: Option<backtrace::Backtrace>,
}

impl Error {
    pub fn new(error: engine::TypeError, span: Span) -> Self {
        Error {
            error,
            span,
            notes: Vec::new(),
            #[cfg(debug_assertions)]
            trace: crate::diagnostics::backtrace_enabled().then(backtrace::Backtrace::new),
        }
    }

    pub fn with_note(mut self, note: Note) -> Self {
        self.notes.push(note);
        self
    }
}

impl Compiler<'_> {
    pub fn typecheck_with_progress(
        &self,
        files: Vec<lower::File>,
        uses: Uses,
        mode: TypecheckMode,
        mut complete: bool,
        mut progress: impl FnMut(Progress),
    ) -> Program {
        let mut typechecker = Typechecker::new(self, mode, uses);

        if files.is_empty() {
            complete = false;
        } else {
            let file_count = files.len();
            let mut current_file = 1;
            for (index, file) in files.into_iter().enumerate() {
                progress(Progress::Collecting {
                    path: file.span.path,
                    current: current_file,
                    total: file_count,
                });

                let is_entrypoint = index + 1 == file_count;
                typechecker.typecheck_file(file, is_entrypoint);

                current_file += 1;
            }
        }

        typechecker.resolve(complete, |count, remaining| {
            progress(Progress::Resolving { count, remaining })
        })
    }
}

#[derive(Debug, Clone)]
struct Typechecker<'a, 'l> {
    compiler: &'a Compiler<'l>,
    mode: TypecheckMode,
    uses: Uses,
    is_complete: bool,
    ctx: engine::Context,
    files: im::HashMap<FilePath, lower::File>,
    declarations: RefCell<DeclarationsInner>,
    exported: Option<lower::ScopeValues>,
    scopes: RefCell<Vec<(Span, lower::ScopeValues)>>,
    block_end: Option<Option<(Span, engine::UnresolvedType)>>,
    instances: im::HashMap<TraitId, BTreeSet<ConstantId>>,
    generic_constants: im::HashMap<ConstantId, (bool, lower::Expression)>,
    specialized_constants: im::HashMap<ConstantId, ConstantId>,
    item_queue: im::Vector<QueuedItem>,
    items: im::HashMap<ItemId, (Option<ConstantId>, Expression)>,
    entrypoint: Option<UnresolvedExpression>,
    errors: im::Vector<Error>,
}

#[derive(Debug, Clone, Default)]
pub enum TypecheckMode {
    #[default]
    Everything,
    Only(BTreeSet<FilePath>),
}

#[derive(Debug, Clone)]
struct QueuedItem {
    generic_id: Option<ConstantId>,
    id: ItemId,
    expr: UnresolvedExpression,
    info: MonomorphizeInfo,
}

impl<'a, 'l> Typechecker<'a, 'l> {
    pub fn new(compiler: &'a Compiler<'l>, mode: TypecheckMode, uses: Uses) -> Self {
        Typechecker {
            compiler,
            mode,
            uses,
            is_complete: true,
            ctx: Default::default(),
            files: Default::default(),
            declarations: Default::default(),
            exported: None,
            scopes: Default::default(),
            block_end: None,
            instances: Default::default(),
            generic_constants: Default::default(),
            specialized_constants: Default::default(),
            item_queue: Default::default(),
            items: Default::default(),
            entrypoint: Default::default(),
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

        let entrypoint = mem::take(&mut self.entrypoint).map(|entrypoint| {
            let info = MonomorphizeInfo::default();

            let entrypoint_id = self.compiler.new_item_id(entrypoint.span.path);

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

            if let Some(expr) = self.finalize_expr(expr) {
                self.items.insert(item.id, (item.generic_id, expr));
            } else {
                self.is_complete = false;
            }
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
            complete: lowering_is_complete && self.is_complete,
            items: items
                .into_iter()
                .map(|(id, (_, expr))| (id, expr))
                .collect(),
            entrypoint,
            exported: self.exported.unwrap_or_default(),
            declarations: self.declarations.into_inner().into(),
            scopes: self.scopes.into_inner(),
        }
    }
}

impl<'a, 'l> Typechecker<'a, 'l> {
    pub fn typecheck_file(&mut self, mut file: lower::File, is_entrypoint: bool) {
        let file_span = file.span;
        let entrypoint = mem::take(&mut file.block);
        let exported = mem::take(&mut file.exported);
        self.scopes.borrow_mut().extend(mem::take(&mut file.scopes));
        self.files.insert(file_span.path, file);

        if is_entrypoint {
            if self.entrypoint.is_some() {
                panic!("entrypoint already provided");
            }

            let mut info = ConvertInfo {
                variables: Default::default(),
            };

            let expr = self.convert_expr(
                lower::Expression {
                    span: file_span,
                    kind: lower::ExpressionKind::Block(entrypoint),
                },
                &mut info,
            );

            self.exported = Some(exported);
            self.entrypoint = Some(expr);
        }

        if self.should_typecheck_everything_in_file(file_span.path) {
            macro_rules! declaration {
                ($kind:ident) => {
                    paste::paste! {
                        self.files
                            .get(&file_span.path)
                            .unwrap()
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
                    instances.entry(decl.trait_id).or_default().insert(id);
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
    }

    fn should_typecheck_everything_in_file(&mut self, file: FilePath) -> bool {
        match &self.mode {
            TypecheckMode::Everything => true,
            TypecheckMode::Only(files) => files.contains(&file),
        }
    }

    fn typecheck_generic_constant_expr(
        &mut self,
        id: ConstantId,
        instance: bool,
        expr: lower::Expression,
    ) {
        let (generic_ty, bounds) = if instance {
            self.with_instance_decl(id, |decl| (decl.ty.clone(), decl.bounds.clone()))
        } else {
            self.with_constant_decl(id, |decl| (decl.ty.clone(), decl.bounds.clone()))
        };

        let mut monomorphize_info = MonomorphizeInfo::default();
        for bound in bounds {
            let ty = self.substitute_trait_params(bound.trait_id, bound.params);

            monomorphize_info
                .bound_instances
                .entry(bound.trait_id)
                .or_default()
                .push((None, ty.clone(), bound.span));
        }

        let expr = self.convert_expr(expr, &mut ConvertInfo::default());

        if let Err(error) = self.unify_reverse(expr.span, generic_ty.clone(), expr.ty.clone()) {
            self.add_error(error);
        }

        let expr = self.monomorphize_expr(expr, &mut monomorphize_info);

        // Before finalizing, substitute the generics back in
        let _ = self.ctx.unify(expr.ty.clone(), generic_ty);

        self.finalize_expr(expr);
    }

    fn typecheck_constant_expr(
        &mut self,
        is_instance: bool,
        id: ConstantId,
        use_span: Span,
        use_ty: engine::UnresolvedType,
        mut info: MonomorphizeInfo,
    ) -> ItemId {
        let monomorphized_id = self.compiler.new_item_id(id.file);

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
                self.with_instance_decl(candidate, |decl| (decl.ty.clone(), decl.bounds.clone()))
            } else {
                self.with_constant_decl(candidate, |decl| (decl.ty.clone(), decl.bounds.clone()))
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

            let temp_ctx = self.ctx.clone();
            let prev_ctx = mem::replace(&mut self.ctx, temp_ctx);

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

            for bound in &mut bounds {
                let ty = self.substitute_trait_params(bound.trait_id, bound.params.clone());

                let instance_info = match self.instance_for(
                    bound.trait_id,
                    ty.clone(),
                    use_span,
                    Some(bound.span),
                    &mut info,
                ) {
                    Ok(info) => info,
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
                    .push((instance_info, ty, bound.span));
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
            self.with_constant_decl(specialized_id, |decl| decl.clone());

        let generic_constant_decl = self.with_constant_decl(generic_id, |decl| decl.clone());

        let params = match self.ctx.unify_params(
            specialized_constant_decl.ty.into(),
            generic_constant_decl.ty,
        ) {
            (params, Ok(())) => params,
            (_, Err(error)) => {
                self.add_error(Error::new(error, specialized_constant_decl.span).with_note(
                    Note::secondary(
                        specialized_constant_decl.span,
                        "this constant must have a more specific type than the original constant",
                    ),
                ));

                return;
            }
        };

        let mut info = MonomorphizeInfo::default();

        let mut monomorphize_info = MonomorphizeInfo::default();
        for bound in specialized_constant_decl.bounds {
            let ty = self.substitute_trait_params(bound.trait_id, bound.params);

            monomorphize_info
                .bound_instances
                .entry(bound.trait_id)
                .or_default()
                .push((None, ty.clone(), bound.span));
        }

        for bound in generic_constant_decl.bounds {
            let mut ty = self.substitute_trait_params(bound.trait_id, bound.params.clone());
            ty.instantiate_with(&self.ctx, &params);

            let instance_info = match self.instance_for(
                bound.trait_id,
                ty.clone(),
                specialized_constant_decl.span,
                Some(bound.span),
                &mut info,
            ) {
                Ok(info) => info,
                Err(error) => {
                    self.add_error(error.with_note(Note::secondary(
                        specialized_constant_decl.span,
                        "this constant must satisfy the bounds of the original constant",
                    )));

                    continue;
                }
            };

            info.bound_instances
                .entry(bound.trait_id)
                .or_default()
                .push((instance_info, ty, bound.span));
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
            lower::ExpressionKind::Error => UnresolvedExpression {
                span: expr.span,
                ty: engine::UnresolvedType::Bottom(engine::BottomTypeReason::Error),
                kind: UnresolvedExpressionKind::Error,
            },
            lower::ExpressionKind::Marker(id) => {
                let params = self.with_type_decl(id, |ty| ty.params.clone());

                let mut ty = engine::UnresolvedType::Named(
                    id,
                    params
                        .into_iter()
                        .map(engine::UnresolvedType::Parameter)
                        .collect(),
                    engine::TypeStructure::Marker,
                );

                self.instantiate_generics(&mut ty);

                UnresolvedExpression {
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Marker,
                }
            }
            lower::ExpressionKind::Constant(id) => {
                let mut ty = self.with_constant_decl(id, |constant| {
                    engine::UnresolvedType::from(constant.ty.clone())
                });

                self.instantiate_generics(&mut ty);

                UnresolvedExpression {
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Constant(id),
                }
            }
            lower::ExpressionKind::Trait(id) => {
                let mut ty =
                    self.with_trait_decl(id, |decl| engine::UnresolvedType::from(decl.ty.clone()));

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
            lower::ExpressionKind::Block(statements) => {
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
                    kind: UnresolvedExpressionKind::Block(statements),
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
                        self.compiler.diagnostics.add(Diagnostic::error(
                            "`end` outside block",
                            vec![Note::primary(expr.span, "cannot use `end` here")],
                        ));
                    }
                }

                UnresolvedExpression {
                    span: expr.span,
                    ty: engine::UnresolvedType::Bottom(BottomTypeReason::Annotated),
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
                let pattern = self.convert_pattern(pattern, input_ty.clone(), info);

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
                    .map(|arm| self.convert_arm(arm, input.ty.clone(), info))
                    .collect::<Vec<_>>();

                let ty = {
                    let first_type = arms.iter().find_map(|arm| {
                        let mut ty = arm.body.ty.clone();
                        ty.apply(&self.ctx);
                        (!matches!(ty, engine::UnresolvedType::Bottom(_))).then_some(ty)
                    });

                    if let Some(first_type) = first_type {
                        for arm in &arms {
                            if let Err(error) =
                                self.unify(arm.body.span, arm.body.ty.clone(), first_type.clone())
                            {
                                self.add_error(error);
                            }
                        }

                        first_type
                    } else {
                        engine::UnresolvedType::Bottom(engine::BottomTypeReason::Annotated)
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
                    span: expr.span,
                    ty,
                    kind: value.kind,
                }
            }
            lower::ExpressionKind::Initialize(pattern, value) => {
                let value = self.convert_expr(*value, info);
                let pattern = self.convert_pattern(pattern, value.ty.clone(), info);

                UnresolvedExpression {
                    span: expr.span,
                    ty: engine::UnresolvedType::Tuple(Vec::new()),
                    kind: UnresolvedExpressionKind::Initialize(pattern, Box::new(value)),
                }
            }
            lower::ExpressionKind::Instantiate(id, fields) => {
                let (kind, params) =
                    self.with_type_decl(id, |decl| (decl.kind.clone(), decl.params.clone()));

                let (mut structure_field_tys, structure_field_names) = match kind {
                    TypeDeclKind::Structure {
                        fields,
                        field_names,
                    } => (
                        fields
                            .into_iter()
                            .map(engine::UnresolvedType::from)
                            .collect::<Vec<_>>(),
                        field_names,
                    ),
                    _ => unreachable!(), // or do we need to display an error like above?
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
                    self.add_substitutions(&mut structure_field_tys[*index], &mut substitutions);
                }

                let mut fields_by_index = structure_field_names.iter().collect::<Vec<_>>();
                fields_by_index.sort_by_key(|(_, index)| *index);

                let mut unpopulated_fields = vec![None; fields_by_index.len()];
                let mut extra_fields = Vec::new();

                for (name, expr) in fields {
                    let (index, ty) = match structure_field_names.get(&name) {
                        Some(index) => (*index, structure_field_tys[*index].clone()),
                        None => {
                            extra_fields.push(name);
                            continue;
                        }
                    };

                    let span = expr.span;
                    let mut value = self.convert_expr(expr, info);

                    match self.unify(span, value.ty.clone(), ty.clone()) {
                        Ok(ty) => ty,
                        Err(error) => {
                            self.add_error(error);
                        }
                    };

                    value.ty = ty;

                    unpopulated_fields[index] = Some(value);
                }

                if !extra_fields.is_empty() {
                    self.compiler.diagnostics.add(Diagnostic::error(
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
                    ));
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
                    self.compiler.diagnostics.add(Diagnostic::error(
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
                    ));
                }

                UnresolvedExpression {
                    span: expr.span,
                    ty,
                    kind: UnresolvedExpressionKind::Structure(fields),
                }
            }
            lower::ExpressionKind::Variant(id, index, values) => {
                let (kind, params) =
                    self.with_type_decl(id, |decl| (decl.kind.clone(), decl.params.clone()));

                let variants_tys = match kind {
                    TypeDeclKind::Enumeration { variants, .. } => variants
                        .into_iter()
                        .map(|variant| {
                            variant
                                .into_iter()
                                .map(engine::UnresolvedType::from)
                                .collect::<Vec<_>>()
                        })
                        .collect::<Vec<_>>(),
                    _ => unreachable!(), // or do we need to display an error like above?
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

                let mut variant_tys = variants_tys[index].clone();

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
        info: &mut ConvertInfo,
    ) -> UnresolvedArm {
        UnresolvedArm {
            span: arm.span,
            pattern: self.convert_pattern(arm.pattern, input_ty, info),
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
            span: pattern.span,
            kind: match pattern.kind {
                lower::PatternKind::Error => UnresolvedPatternKind::Error,
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
                            (name, (self.convert_pattern(pattern, ty.clone(), info), ty))
                        })
                        .collect(),
                ),
                lower::PatternKind::Variant(id, variant, values) => {
                    let (params, variants_tys) = self.with_type_decl(id, |decl| {
                        (
                            decl.params.clone(),
                            match &decl.kind {
                                TypeDeclKind::Enumeration { variants, .. } => variants.clone(),
                                _ => unreachable!(),
                            },
                        )
                    });

                    let mut substitutions = engine::GenericSubstitutions::new();

                    let mut variant_tys = variants_tys[variant]
                        .clone()
                        .into_iter()
                        .map(engine::UnresolvedType::from)
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
                                        .map(|ty| {
                                            let mut ty = engine::UnresolvedType::from(ty);
                                            ty.instantiate_with(&self.ctx, &substitutions);
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
                    let target_ty = self.convert_type_annotation(target_ty);

                    if let Err(error) = self.unify(pattern.span, ty, target_ty.clone()) {
                        self.add_error(error);
                    }

                    self.convert_pattern(*inner, target_ty, info).kind
                }
                lower::PatternKind::Or(lhs, rhs) => UnresolvedPatternKind::Or(
                    Box::new(self.convert_pattern(*lhs, ty.clone(), info)),
                    Box::new(self.convert_pattern(*rhs, ty, info)),
                ),
                lower::PatternKind::Where(pattern, condition) => UnresolvedPatternKind::Where(
                    Box::new(self.convert_pattern(*pattern, ty, info)),
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
                            .map(|(pattern, ty)| self.convert_pattern(pattern, ty, info))
                            .collect(),
                    )
                }
            },
        }
    }
}

#[derive(Debug, Clone, Default)]
struct MonomorphizeInfo {
    cache: BTreeMap<ConstantId, ItemId>,
    bound_instances: BTreeMap<TraitId, Vec<(Option<ConstantId>, engine::UnresolvedType, Span)>>,
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
                UnresolvedExpressionKind::Error => MonomorphizedExpressionKind::Error,
                UnresolvedExpressionKind::Marker => MonomorphizedExpressionKind::Marker,
                UnresolvedExpressionKind::Constant(generic_id) => {
                    let id = info.cache.get(&generic_id).copied().unwrap_or_else(|| {
                        self.typecheck_constant_expr(
                            false,
                            generic_id,
                            expr.span,
                            expr.ty.clone(),
                            info.clone(),
                        )
                    });

                    MonomorphizedExpressionKind::Constant(id)
                }
                UnresolvedExpressionKind::Variable(var) => {
                    MonomorphizedExpressionKind::Variable(var)
                }
                UnresolvedExpressionKind::Text(text) => MonomorphizedExpressionKind::Text(text),
                UnresolvedExpressionKind::Number(number) => {
                    MonomorphizedExpressionKind::Number(number)
                }
                UnresolvedExpressionKind::Block(statements) => MonomorphizedExpressionKind::Block(
                    statements
                        .into_iter()
                        .map(|expr| self.monomorphize_expr(expr, info))
                        .collect(),
                ),
                UnresolvedExpressionKind::End(value) => {
                    MonomorphizedExpressionKind::End(Box::new(self.monomorphize_expr(*value, info)))
                }
                UnresolvedExpressionKind::Call(func, input) => {
                    let func = *func;
                    let input = *input;

                    // HACK: Resolve function calls in both orders to match as
                    // many instances as possible
                    let (func, input) = {
                        // This is cheap because typechecker uses persistent
                        // data structures
                        let mut typechecker = self.clone();

                        let errors = mem::take(&mut typechecker.errors);

                        let monomorphized_func = typechecker.monomorphize_expr(func.clone(), info);
                        let monomorphized_input =
                            typechecker.monomorphize_expr(input.clone(), info);

                        if typechecker.errors.is_empty() {
                            typechecker.errors = errors;
                            *self = typechecker;
                            (monomorphized_func, monomorphized_input)
                        } else {
                            let input = self.monomorphize_expr(input, info);
                            let func = self.monomorphize_expr(func, info);

                            (func, input)
                        }
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
                            engine::UnresolvedType::Bottom(engine::BottomTypeReason::Error),
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
                    let instance_info =
                        match self.instance_for(tr, expr.ty.clone(), expr.span, None, info) {
                            Ok(instance) => instance,
                            Err(error) => {
                                self.add_error(error);
                                return MonomorphizedExpressionKind::Error;
                            }
                        };

                    let monomorphized_id = match instance_info {
                        Some(id) => self.typecheck_constant_expr(
                            true,
                            id,
                            expr.span,
                            expr.ty.clone(),
                            info.clone(),
                        ),
                        None => return MonomorphizedExpressionKind::Error,
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
            UnresolvedPatternKind::Error => {
                match_set.set_matched(true);
                MonomorphizedPatternKind::Error
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
                        self.compiler.diagnostics.add(Diagnostic::error(
                            "cannot destructure this value",
                            vec![Note::primary(pattern.span, "value is not a data structure")],
                        ));

                        match_set.set_matched(true);

                        return MonomorphizedPatternKind::Destructure(
                            fields
                                .into_iter()
                                .map(|(_, (pattern, _))| {
                                    (
                                        0,
                                        self.monomorphize_pattern(
                                            pattern,
                                            engine::UnresolvedType::Bottom(
                                                engine::BottomTypeReason::Error,
                                            ),
                                            &mut MatchSet::Never,
                                            info,
                                        ),
                                    )
                                })
                                .collect(),
                        );
                    }
                };

                let structure = self.with_type_decl(id, Clone::clone);

                let (structure_field_tys, structure_field_names) = match &structure.kind {
                    TypeDeclKind::Structure {
                        fields,
                        field_names,
                    } => (fields.clone(), field_names.clone()),
                    _ => unreachable!(),
                };

                let substitutions = structure
                    .params
                    .iter()
                    .copied()
                    .zip(params)
                    .collect::<BTreeMap<_, _>>();

                let field_match_sets = match match_set {
                    MatchSet::Structure(fields) => fields,
                    _ => {
                        ty.apply(&self.ctx);
                        *match_set = MatchSet::new(&ty);

                        match match_set {
                            MatchSet::Structure(fields) => fields,
                            _ => unreachable!(),
                        }
                    }
                };

                let fields = fields
                    .into_iter()
                    .filter_map(|(name, (pattern, ty))| {
                        let index = match structure_field_names.get(&name) {
                            Some(index) => *index,
                            None => {
                                self.compiler.diagnostics.add(Diagnostic::error(
                                    format!("value has no member named '{}'", name),
                                    vec![Note::primary(pattern.span, "no such member")],
                                ));

                                return None;
                            }
                        };

                        let mut member_ty =
                            engine::UnresolvedType::from(structure_field_tys[index].clone());

                        member_ty.instantiate_with(&self.ctx, &substitutions);

                        if let Err(error) = self.unify(pattern.span, ty, member_ty.clone()) {
                            self.add_error(error);
                        }

                        let (matches, match_set) = &mut field_match_sets[index];
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
                                        engine::UnresolvedType::Bottom(
                                            engine::BottomTypeReason::Error,
                                        ),
                                        &mut MatchSet::Never,
                                        info,
                                    )
                                })
                                .collect(),
                        );
                    }
                };

                let enumeration = self.with_type_decl(*id, Clone::clone);

                let mut variant_tys = match enumeration.kind {
                    TypeDeclKind::Enumeration { mut variants, .. } => variants
                        .swap_remove(variant)
                        .into_iter()
                        .map(engine::UnresolvedType::from)
                        .collect::<Vec<_>>(),
                    _ => unreachable!(),
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

                let (matches, variant_match_sets) = match match_set {
                    MatchSet::Enumeration(variants) => &mut variants[variant],
                    _ => {
                        ty.apply(&self.ctx);
                        *match_set = MatchSet::new(&ty);

                        match match_set {
                            MatchSet::Enumeration(variants) => &mut variants[variant],
                            _ => unreachable!(),
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

                if let Some(boolean_ty) = self
                    .files
                    .get(&pattern.span.path)
                    .unwrap()
                    .global_attributes
                    .language_items
                    .boolean
                {
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
                    self.compiler.diagnostics.add(Diagnostic::error(
                        "cannot find `boolean` language item",
                        vec![Note::primary(
                            condition_span,
                            "typechecking this condition requires the `boolean` language item",
                        )],
                    ))
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
                            _ => return MonomorphizedPatternKind::Error,
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

    fn instance_for(
        &mut self,
        tr: TraitId,
        ty: impl Into<engine::UnresolvedType>,
        use_span: Span,
        bound_span: Option<Span>,
        info: &mut MonomorphizeInfo,
    ) -> Result<Option<ConstantId>, Error> {
        let ty = ty.into();
        let tr_decl = self.with_trait_decl(tr, Clone::clone);

        let mut trait_ty = tr_decl.ty.clone().into();
        self.instantiate_generics(&mut trait_ty);

        if let Err(error) = self.unify(use_span, ty.clone(), trait_ty) {
            self.add_error(error);
        }

        macro_rules! find_instance {
            ($resolve:expr) => {{
                // First try with numeric variables...
                match find_instance!(@find ty.clone(), $resolve) {
                    // ...if there is a single candidate, return it.
                    Some(Ok(candidate)) => return Ok(candidate),
                    // ...if there are multiple candiates, try again finalizing numeric variables.
                    Some(Err(_)) => {
                        let mut ty = ty.clone();
                        ty.apply(&self.ctx);
                        ty.finalize_numeric_variables(&self.ctx);

                        match find_instance!(@find ty, $resolve) {
                            Some(result) => return result,
                            None => {}
                        }
                    }
                    // ...if there are no candidates, continue the search.
                    None => {}
                }
            }};
            (@find $ty:expr, $resolve:expr) => {{
                let ty = engine::UnresolvedType::from($ty);
                let mut candidates = Vec::new();
                $resolve(&mut candidates, ty.clone());
                let mut candidates = candidates
                    .into_iter()
                    .unique_by(|(_, id, _)| *id)
                    .collect::<Vec<_>>();

                match candidates.len() {
                    0 => None,
                    1 => {
                        let (ctx, info, _) = candidates.pop().unwrap();
                        self.ctx = ctx;

                        Some(Ok(info))
                    }
                    _ => Some(Err(Error::new(
                        engine::TypeError::AmbiguousTrait(
                            ty,
                            tr,
                            candidates.into_iter().map(|(_, _, span)| span).collect(),
                        ),
                        use_span,
                    ))),
                }
            }};
        }

        let bound_instances = info.bound_instances.get(&tr).cloned().unwrap_or_default();

        find_instance!(|candidates: &mut Vec<_>, ty: engine::UnresolvedType| {
            for (info, instance_ty, span) in bound_instances.clone() {
                let mut ctx = self.ctx.clone();
                if ctx.unify_generic(ty.clone(), instance_ty.clone()).is_ok() {
                    candidates.push((ctx, info, span));
                }
            }
        });

        let declared_instances = self.instances.get(&tr).cloned().unwrap_or_default();

        find_instance!(|candidates: &mut Vec<_>, ty: engine::UnresolvedType| {
            for id in declared_instances.clone() {
                let (mut instance_ty, instance_span) = self.with_instance_decl(id, |instance| {
                    (
                        engine::UnresolvedType::from(instance.ty.clone()),
                        instance.span,
                    )
                });

                self.instantiate_generics(&mut instance_ty);

                let mut ctx = self.ctx.clone();
                if ctx.unify(ty.clone(), instance_ty.clone()).is_ok() {
                    candidates.push((ctx, Some(id), instance_span));
                }
            }
        });

        let params = self.ctx.unify_params(ty, tr_decl.ty).0;

        let params = tr_decl
            .params
            .into_iter()
            .map(|param| {
                params
                    .get(&param)
                    .cloned()
                    .unwrap_or_else(|| engine::UnresolvedType::Variable(self.ctx.new_variable()))
            })
            .collect();

        Err(Error::new(
            engine::TypeError::MissingInstance(tr, params, bound_span),
            use_span,
        ))
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
            engine::UnresolvedType::Bottom(_) => MatchSet::Never,
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
                self.compiler.diagnostics.add(Diagnostic::error(
                    "pattern is not exhaustive",
                    vec![Note::primary(
                        span,
                        "this pattern does not handle all possible values",
                    )],
                ));
            } else {
                let mut ty = ty.clone();
                ty.apply(&self.ctx);

                let mut missing_patterns = match_set
                    .unmatched_patterns(&ty)
                    .unique()
                    .map(|pattern| format!("`{}`", self.format_unmatched_pattern(&pattern, false)))
                    .collect::<Vec<_>>();

                self.compiler.diagnostics.add(Diagnostic::error(
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
                ));
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
    Structure(usize, Option<Box<UnmatchedPattern<'a>>>),
    Variant(usize, Vec<UnmatchedPattern<'a>>),
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
                    _ => unreachable!(),
                };

                Box::new(fields.iter().enumerate().flat_map(
                    move |(index, (matched, field))| -> Box<dyn Iterator<Item = UnmatchedPattern>> {
                        if !matched {
                            return Box::new(std::iter::once(UnmatchedPattern {
                                ty,
                                kind: UnmatchedPatternKind::Structure(index, None),
                            }));
                        }

                        let field_ty = &field_tys[index];

                        Box::new(field.unmatched_patterns(field_ty).map(move |item| {
                            UnmatchedPattern {
                                ty: field_ty,
                                kind: UnmatchedPatternKind::Structure(index, Some(Box::new(item))),
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
                    _ => unreachable!(),
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
                                .entry(variant_index)
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
                            *variants.entry(variant_index).or_default() = Vec::new();
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
                    _ => unreachable!(),
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
                    .find_map(|(name, i)| (i == index).then_some(name))
                    .unwrap();

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
                    .find_map(|(name, i)| (i == index).then_some(name))
                    .unwrap();

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
    fn finalize_expr(&mut self, expr: MonomorphizedExpression) -> Option<Expression> {
        let ty = match expr.ty.clone().finalize(&self.ctx) {
            Ok(ty) => ty,
            Err(error) => {
                self.add_error(Error::new(error, expr.span));
                return None;
            }
        };

        let kind = match expr.kind {
            MonomorphizedExpressionKind::Error => return None,
            MonomorphizedExpressionKind::Marker => ExpressionKind::Marker,
            MonomorphizedExpressionKind::Constant(id) => ExpressionKind::Constant(id),
            MonomorphizedExpressionKind::Variable(var) => ExpressionKind::Variable(var),
            MonomorphizedExpressionKind::Text(text) => ExpressionKind::Text(text),
            MonomorphizedExpressionKind::Number(number) => {
                match parse_number!(number, ExpressionKind, &ty, Type)? {
                    Ok(number) => number,
                    Err(error) => {
                        self.add_error(Error::new(error, expr.span));
                        return None;
                    }
                }
            }
            MonomorphizedExpressionKind::Block(statements) => ExpressionKind::Block(
                statements
                    .into_iter()
                    .map(|expr| self.finalize_expr(expr))
                    .collect::<Option<_>>()?,
            ),
            MonomorphizedExpressionKind::End(value) => {
                ExpressionKind::End(Box::new(self.finalize_expr(*value)?))
            }
            MonomorphizedExpressionKind::Call(func, input) => ExpressionKind::Call(
                Box::new(self.finalize_expr(*func)?),
                Box::new(self.finalize_expr(*input)?),
            ),
            MonomorphizedExpressionKind::Function(pattern, body, captures) => {
                let input_ty = match &ty {
                    engine::Type::Function(input, _) => input.clone(),
                    _ => unreachable!(),
                };

                ExpressionKind::Function(
                    self.finalize_pattern(pattern, &input_ty)?,
                    Box::new(self.finalize_expr(*body)?),
                    captures,
                )
            }
            MonomorphizedExpressionKind::When(input, arms) => {
                let input = self.finalize_expr(*input)?;

                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        Some(Arm {
                            span: arm.span,
                            pattern: self.finalize_pattern(arm.pattern, &input.ty)?,
                            body: self.finalize_expr(arm.body)?,
                        })
                    })
                    .collect::<Option<_>>()?;

                ExpressionKind::When(Box::new(input), arms)
            }
            MonomorphizedExpressionKind::External(lib, identifier, inputs) => {
                ExpressionKind::External(
                    lib,
                    identifier,
                    inputs
                        .into_iter()
                        .map(|expr| self.finalize_expr(expr))
                        .collect::<Option<_>>()?,
                )
            }
            MonomorphizedExpressionKind::Runtime(func, inputs) => ExpressionKind::Runtime(
                func,
                inputs
                    .into_iter()
                    .map(|expr| self.finalize_expr(expr))
                    .collect::<Option<_>>()?,
            ),
            MonomorphizedExpressionKind::Initialize(pattern, value) => {
                let value = self.finalize_expr(*value)?;

                ExpressionKind::Initialize(
                    self.finalize_pattern(pattern, &value.ty)?,
                    Box::new(value),
                )
            }
            MonomorphizedExpressionKind::Structure(fields) => ExpressionKind::Structure(
                fields
                    .into_iter()
                    .map(|expr| self.finalize_expr(expr))
                    .collect::<Option<_>>()?,
            ),
            MonomorphizedExpressionKind::Variant(index, values) => ExpressionKind::Variant(
                index,
                values
                    .into_iter()
                    .map(|expr| self.finalize_expr(expr))
                    .collect::<Option<_>>()?,
            ),
            MonomorphizedExpressionKind::Tuple(exprs) => ExpressionKind::Tuple(
                exprs
                    .into_iter()
                    .map(|expr| self.finalize_expr(expr))
                    .collect::<Option<_>>()?,
            ),
        };

        Some(Expression {
            span: expr.span,
            ty,
            kind,
        })
    }

    fn finalize_pattern(
        &mut self,
        pattern: MonomorphizedPattern,
        input_ty: &engine::Type,
    ) -> Option<Pattern> {
        Some(Pattern {
            span: pattern.span,
            kind: match pattern.kind {
                MonomorphizedPatternKind::Error => return None,
                MonomorphizedPatternKind::Wildcard => PatternKind::Wildcard,
                MonomorphizedPatternKind::Number(number) => {
                    match parse_number!(number, PatternKind, input_ty, Type)? {
                        Ok(number) => number,
                        Err(error) => {
                            self.add_error(Error::new(error, pattern.span));
                            return None;
                        }
                    }
                }
                MonomorphizedPatternKind::Text(text) => PatternKind::Text(text),
                MonomorphizedPatternKind::Variable(var) => {
                    self.with_variable_decl(var, input_ty.clone(), pattern.span.path, |_| {});
                    PatternKind::Variable(var)
                }
                MonomorphizedPatternKind::Destructure(fields) => {
                    let input_tys = match input_ty {
                        engine::Type::Named(_, _, engine::TypeStructure::Structure(fields)) => {
                            fields
                        }
                        _ => return None,
                    };

                    PatternKind::Destructure(
                        fields
                            .into_iter()
                            .zip(input_tys)
                            .map(|((index, field), ty)| {
                                Some((index, self.finalize_pattern(field, ty)?))
                            })
                            .collect::<Option<_>>()?,
                    )
                }
                MonomorphizedPatternKind::Variant(index, values) => {
                    let input_tys = match input_ty {
                        engine::Type::Named(_, _, engine::TypeStructure::Enumeration(variants)) => {
                            &variants[index]
                        }
                        _ => return None,
                    };

                    PatternKind::Variant(
                        index,
                        values
                            .into_iter()
                            .zip(input_tys)
                            .map(|(value, ty)| self.finalize_pattern(value, ty))
                            .collect::<Option<_>>()?,
                    )
                }
                MonomorphizedPatternKind::Or(lhs, rhs) => PatternKind::Or(
                    Box::new(self.finalize_pattern(*lhs, input_ty)?),
                    Box::new(self.finalize_pattern(*rhs, input_ty)?),
                ),
                MonomorphizedPatternKind::Where(pattern, condition) => PatternKind::Where(
                    Box::new(self.finalize_pattern(*pattern, input_ty)?),
                    Box::new(self.finalize_expr(*condition)?),
                ),
                MonomorphizedPatternKind::Tuple(patterns) => {
                    let input_tys = match input_ty {
                        engine::Type::Tuple(tys) => tys,
                        _ => return None,
                    };

                    PatternKind::Tuple(
                        patterns
                            .into_iter()
                            .zip(input_tys)
                            .map(|(pattern, ty)| self.finalize_pattern(pattern, ty))
                            .collect::<Option<_>>()?,
                    )
                }
            },
        })
    }
}

impl<'a, 'l> Typechecker<'a, 'l> {
    fn with_type_decl<T>(&mut self, id: TypeId, f: impl FnOnce(&TypeDecl) -> T) -> T {
        if let Some(decl) = self.declarations.borrow().types.get(&id) {
            return f(decl);
        }

        let decl = self
            .files
            .get(&id.file)
            .unwrap()
            .declarations
            .types
            .get(&id)
            .unwrap()
            .clone();

        let decl = TypeDecl {
            name: decl.name.expect("all types have names"),
            span: decl.span,
            params: decl.value.params,
            kind: match decl.value.kind {
                lower::TypeKind::Marker => TypeDeclKind::Marker,
                lower::TypeKind::Structure(fields, field_names) => TypeDeclKind::Structure {
                    fields: fields
                        .into_iter()
                        .map(|field| self.convert_finalized_type_annotation(field.ty))
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
                                    .map(|ty| self.convert_finalized_type_annotation(ty))
                                    .collect()
                            })
                            .collect(),
                        variant_names,
                    }
                }
            },
            attributes: decl.value.attributes,
            uses: self
                .uses
                .type_uses
                .get_mut(&id)
                .cloned()
                .unwrap_or_default(),
        };

        f(self
            .declarations
            .borrow_mut()
            .types
            .entry(id)
            .or_insert(decl))
    }

    fn with_trait_decl<T>(&mut self, id: TraitId, f: impl FnOnce(&TraitDecl) -> T) -> T {
        if let Some(decl) = self.declarations.borrow().traits.get(&id) {
            return f(decl);
        }

        let decl = self
            .files
            .get(&id.file)
            .unwrap()
            .declarations
            .traits
            .get(&id)
            .unwrap()
            .clone();

        let decl = TraitDecl {
            name: decl.name.expect("all traits have names"),
            span: decl.span,
            params: decl.value.parameters,
            ty: self.convert_finalized_type_annotation(decl.value.ty),
            attributes: decl.value.attributes,
            uses: self
                .uses
                .trait_uses
                .get_mut(&id)
                .cloned()
                .unwrap_or_default(),
        };

        f(self
            .declarations
            .borrow_mut()
            .traits
            .entry(id)
            .or_insert(decl))
    }

    fn with_constant_decl<T>(&mut self, id: ConstantId, f: impl FnOnce(&ConstantDecl) -> T) -> T {
        if let Some(decl) = self.declarations.borrow_mut().constants.get(&id) {
            return f(decl);
        }

        let decl = self
            .files
            .get(&id.file)
            .unwrap()
            .declarations
            .constants
            .get(&id)
            .unwrap_or_else(|| panic!("undefined constant {:?} in file {}", id, id.file))
            .clone();

        let body = decl
            .value
            .value
            .lock()
            .as_ref()
            .cloned()
            .unwrap_or(lower::Expression {
                span: decl.span,
                kind: lower::ExpressionKind::Error,
            });

        self.generic_constants.insert(id, (false, body));

        let ty = self.convert_generic_type_annotation(decl.value.ty, id.file);

        let bounds = decl
            .value
            .bounds
            .into_iter()
            .map(|bound| Bound {
                span: bound.span,
                trait_id: bound.tr,
                params: bound
                    .parameters
                    .into_iter()
                    .map(|ty| self.convert_generic_type_annotation(ty, id.file).into())
                    .collect(),
            })
            .collect::<Vec<_>>();

        let decl = ConstantDecl {
            name: decl.name.expect("all constants have names"),
            span: decl.span,
            bounds,
            ty,
            specializations: self
                .files
                .get(&id.file)
                .unwrap()
                .specializations
                .get(&id)
                .cloned()
                .unwrap_or_default(),
            attributes: decl.value.attributes,
            uses: self
                .uses
                .constant_uses
                .get_mut(&id)
                .cloned()
                .unwrap_or_default(),
        };

        f(self
            .declarations
            .borrow_mut()
            .constants
            .entry(id)
            .or_insert(decl))
    }

    fn with_instance_decl<T>(&mut self, id: ConstantId, f: impl FnOnce(&InstanceDecl) -> T) -> T {
        let decl = self
            .files
            .get(&id.file)
            .unwrap()
            .declarations
            .instances
            .get(&id)
            .unwrap()
            .clone();

        let trait_id = decl.value.tr;

        if let Some(decl) = self
            .declarations
            .borrow_mut()
            .instances
            .entry(trait_id)
            .or_default()
            .get(&id)
        {
            return f(decl);
        }

        self.generic_constants.insert(id, (true, decl.value.value));

        let mut params = decl
            .value
            .trait_params
            .into_iter()
            .map(|ty| self.convert_generic_type_annotation(ty, id.file).into())
            .collect::<Vec<_>>();

        let tr = self.with_trait_decl(decl.value.tr, Clone::clone);

        for param in tr.params.iter().skip(params.len()) {
            let name = self.with_type_parameter_decl(*param, |decl| decl.name);

            self.compiler.diagnostics.add(Diagnostic::error(
                format!(
                    "missing type for trait parameter `{}`",
                    name.as_deref().unwrap_or("_")
                ),
                vec![Note::primary(
                    decl.span,
                    "try adding another type after the trait provided to `instance`",
                )],
            ));

            params.push(engine::UnresolvedType::Bottom(
                engine::BottomTypeReason::Error,
            ));
        }

        let instance_ty = self.substitute_trait_params(trait_id, params);

        let has_bounds = !decl.value.bounds.is_empty();

        let bounds = decl
            .value
            .bounds
            .into_iter()
            .map(|bound| Bound {
                span: bound.span,
                trait_id: bound.tr,
                params: bound
                    .parameters
                    .into_iter()
                    .map(|ty| self.convert_generic_type_annotation(ty, id.file).into())
                    .collect(),
            })
            .collect::<Vec<_>>();

        let item = self.compiler.new_item_id(id.file);

        assert!(decl.name.is_none(), "instances never have names");

        // Check if the instance collides with any other instances -- there's no
        // need to check the bounds because there's no way to ensure a type
        // doesn't satisfy the bounds specified in both instances

        let colliding_instances = self
            .declarations
            .borrow_mut()
            .instances
            .entry(trait_id)
            .or_default()
            .values()
            .filter_map(|other| {
                let mut temp_ctx = self.ctx.clone();
                temp_ctx
                    .unify(instance_ty.clone(), other.ty.clone())
                    .is_ok()
                    .then_some(other.span)
            })
            .collect::<Vec<_>>();

        if !colliding_instances.is_empty() {
            self.compiler.diagnostics.add(Diagnostic::error(
                format!(
                    "this instance collides with {} other instances",
                    colliding_instances.len()
                ),
                std::iter::once(Note::primary(
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
            ));
        }

        let decl = InstanceDecl {
            span: decl.span,
            trait_id,
            bounds,
            ty: instance_ty.finalize(&self.ctx).unwrap(),
            item,
        };

        f(self
            .declarations
            .borrow_mut()
            .instances
            .entry(trait_id)
            .or_default()
            .entry(id)
            .or_insert(decl))
    }

    fn with_operator_decl<T>(&mut self, id: TemplateId, f: impl FnOnce(&OperatorDecl) -> T) -> T {
        if let Some(decl) = self.declarations.borrow().operators.get(&id) {
            return f(decl);
        }

        let operator = *self
            .files
            .get(&id.file)
            .unwrap()
            .declarations
            .operators
            .get(&id)
            .unwrap();

        let decl = self
            .files
            .get(&id.file)
            .unwrap()
            .declarations
            .templates
            .get(&operator.template)
            .unwrap()
            .clone();

        let decl = OperatorDecl {
            name: decl.name,
            span: decl.span,
            uses: self
                .uses
                .template_uses
                .get_mut(&id)
                .cloned()
                .unwrap_or_default(),
        };

        f(self
            .declarations
            .borrow_mut()
            .operators
            .entry(id)
            .or_insert(decl))
    }

    fn with_template_decl<T>(&mut self, id: TemplateId, f: impl FnOnce(&TemplateDecl) -> T) -> T {
        if let Some(decl) = self.declarations.borrow().templates.get(&id) {
            return f(decl);
        }

        let decl = self
            .files
            .get(&id.file)
            .unwrap()
            .declarations
            .templates
            .get(&id)
            .unwrap()
            .clone();

        let decl = TemplateDecl {
            name: decl.name,
            span: decl.span,
            attributes: decl.attributes,
            uses: self
                .uses
                .template_uses
                .get_mut(&id)
                .cloned()
                .unwrap_or_default(),
        };

        f(self
            .declarations
            .borrow_mut()
            .templates
            .entry(id)
            .or_insert(decl))
    }

    fn with_builtin_type_decl<T>(
        &mut self,
        id: BuiltinTypeId,
        f: impl FnOnce(&BuiltinTypeDecl) -> T,
    ) -> T {
        if let Some(decl) = self.declarations.borrow().builtin_types.get(&id) {
            return f(decl);
        }

        let decl = self
            .files
            .get(&id.file)
            .unwrap()
            .declarations
            .builtin_types
            .get(&id)
            .unwrap()
            .clone();

        let decl = BuiltinTypeDecl {
            name: decl.name.expect("all builtin types have names"),
            span: decl.span,
            attributes: decl.value.attributes,
            uses: self
                .uses
                .builtin_type_uses
                .get_mut(&id)
                .cloned()
                .unwrap_or_default(),
        };

        f(self
            .declarations
            .borrow_mut()
            .builtin_types
            .entry(id)
            .or_insert(decl))
    }

    fn with_type_parameter_decl<T>(
        &mut self,
        id: TypeParameterId,
        f: impl FnOnce(&TypeParameterDecl) -> T,
    ) -> T {
        if let Some(decl) = self.declarations.borrow().type_parameters.get(&id) {
            return f(decl);
        }

        let decl = self
            .files
            .get(&id.file)
            .unwrap()
            .declarations
            .type_parameters
            .get(&id)
            .unwrap()
            .clone();

        let decl = TypeParameterDecl {
            name: decl.name,
            span: decl.span,
            uses: self
                .uses
                .type_parameter_uses
                .get_mut(&id)
                .cloned()
                .unwrap_or_default(),
        };

        f(self
            .declarations
            .borrow_mut()
            .type_parameters
            .entry(id)
            .or_insert(decl))
    }

    fn with_variable_decl<T>(
        &mut self,
        id: VariableId,
        ty: engine::Type,
        file: FilePath,
        f: impl FnOnce(&VariableDecl) -> T,
    ) -> T {
        if let Some(decl) = self.declarations.borrow().variables.get(&id) {
            return f(decl);
        }

        let decl = self
            .files
            .get(&file)
            .unwrap_or_else(|| panic!("cannot find file {}", file))
            .declarations
            .variables
            .get(&id)
            .unwrap()
            .clone();

        let decl = VariableDecl {
            name: decl.name,
            span: decl.span,
            ty,
            uses: self
                .uses
                .variable_uses
                .get_mut(&id)
                .cloned()
                .unwrap_or_default(),
        };

        f(self
            .declarations
            .borrow_mut()
            .variables
            .entry(id)
            .or_insert(decl))
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
            .map_err(|e| Error::new(e, span))
    }

    fn unify_reverse(
        &mut self,
        span: Span,
        actual: impl Into<engine::UnresolvedType>,
        expected: engine::UnresolvedType,
    ) -> Result<(), Error> {
        self.ctx
            .unify_reverse(actual, expected)
            .map_err(|e| Error::new(e, span))
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

        ty.instantiate_with(&self.ctx, substitutions);
    }

    fn instantiate_generics(&mut self, ty: &mut engine::UnresolvedType) {
        self.add_substitutions(ty, &mut GenericSubstitutions::new());
    }

    fn convert_type_annotation(
        &mut self,
        annotation: lower::TypeAnnotation,
    ) -> engine::UnresolvedType {
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

    fn convert_generic_type_annotation(
        &mut self,
        annotation: lower::TypeAnnotation,
        file: FilePath,
    ) -> engine::Type {
        let ty = self.convert_type_annotation_inner(
            annotation,
            &|typechecker, span| {
                let param = typechecker.compiler.new_type_parameter_id(file);

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

        ty.finalize(&self.ctx)
            .expect("type should not contain variables")
    }

    fn convert_finalized_type_annotation(
        &mut self,
        annotation: lower::TypeAnnotation,
    ) -> engine::Type {
        let ty = self.convert_type_annotation_inner(annotation, &|_, _| None, &mut Vec::new());

        ty.finalize(&self.ctx)
            .expect("type should not contain variables")
    }

    fn convert_type_annotation_inner(
        &mut self,
        annotation: lower::TypeAnnotation,
        convert_placeholder: &impl Fn(&mut Self, Span) -> Option<engine::UnresolvedType>,
        stack: &mut Vec<TypeId>,
    ) -> engine::UnresolvedType {
        match annotation.kind {
            lower::TypeAnnotationKind::Error => {
                engine::UnresolvedType::Bottom(engine::BottomTypeReason::Error)
            }
            lower::TypeAnnotationKind::Placeholder => {
                if let Some(ty) = convert_placeholder(self, annotation.span) {
                    ty
                } else {
                    self.compiler.diagnostics.add(Diagnostic::error(
                        "type placeholder is not allowed here",
                        vec![Note::primary(
                            annotation.span,
                            "try providing an actual type in place of `_`",
                        )],
                    ));

                    engine::UnresolvedType::Bottom(engine::BottomTypeReason::Error)
                }
            }
            lower::TypeAnnotationKind::Named(id, params) => {
                let mut params = params
                    .into_iter()
                    .map(|param| {
                        self.convert_type_annotation_inner(param, convert_placeholder, stack)
                    })
                    .collect::<Vec<_>>();

                let ty = self
                    .files
                    .get(&id.file)
                    .unwrap()
                    .declarations
                    .types
                    .get(&id)
                    .unwrap()
                    .clone();

                for param in ty.value.params.iter().skip(params.len()) {
                    let name = self.with_type_parameter_decl(*param, |decl| decl.name);

                    self.compiler.diagnostics.add(Diagnostic::error(
                        format!(
                            "missing type for type parameter `{}`",
                            name.expect("all type parameters on named types are named")
                        ),
                        vec![Note::primary(
                            annotation.span,
                            "try adding another type after this",
                        )],
                    ));

                    params.push(engine::UnresolvedType::Bottom(
                        engine::BottomTypeReason::Error,
                    ));
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
                    .copied()
                    .zip(params.iter().cloned())
                    .collect::<GenericSubstitutions>();

                let mut convert_and_instantiate = |ty| {
                    let mut ty = self.convert_type_annotation_inner(ty, convert_placeholder, stack);
                    ty.instantiate_with(&self.ctx, &substitutions);
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
            lower::TypeAnnotationKind::Parameter(id) => engine::UnresolvedType::Parameter(id),
            lower::TypeAnnotationKind::Builtin(id, mut parameters) => {
                let builtin_ty = self
                    .files
                    .get(&id.file)
                    .unwrap()
                    .declarations
                    .builtin_types
                    .get(&id)
                    .unwrap()
                    .clone();

                match builtin_ty.value.kind {
                    lower::BuiltinTypeKind::Never => {
                        if !parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`!` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            ));
                        }

                        engine::UnresolvedType::Bottom(engine::BottomTypeReason::Annotated)
                    }
                    lower::BuiltinTypeKind::Number => {
                        if !parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`Number` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            ));
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Number)
                    }
                    lower::BuiltinTypeKind::Integer => {
                        if !parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`Integer` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            ));
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Integer)
                    }
                    lower::BuiltinTypeKind::Natural => {
                        if !parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`Natural` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            ));
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Natural)
                    }
                    lower::BuiltinTypeKind::Byte => {
                        if !parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`Byte` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            ));
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Byte)
                    }
                    lower::BuiltinTypeKind::Signed => {
                        if !parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`Signed` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            ));
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Signed)
                    }
                    lower::BuiltinTypeKind::Unsigned => {
                        if !parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`Unsigned` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            ));
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Unsigned)
                    }
                    lower::BuiltinTypeKind::Float => {
                        if !parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`Float` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            ));
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Float)
                    }
                    lower::BuiltinTypeKind::Double => {
                        if !parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`Double` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            ));
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Double)
                    }
                    lower::BuiltinTypeKind::Text => {
                        if !parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`Text` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            ));
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Text)
                    }
                    lower::BuiltinTypeKind::Boolean => {
                        if !parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`Boolean` does not accept parameters",
                                vec![Note::primary(
                                    annotation.span,
                                    "try removing these parameters",
                                )],
                            ));
                        }

                        engine::UnresolvedType::Builtin(engine::BuiltinType::Number)
                    }
                    lower::BuiltinTypeKind::List => {
                        if parameters.is_empty() {
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`List` accepts 1 parameter, but none were provided",
                                vec![Note::primary(
                                    annotation.span,
                                    "try adding `_` here to infer the type of `Element`",
                                )],
                            ));

                            engine::UnresolvedType::Builtin(engine::BuiltinType::List(Box::new(
                                engine::UnresolvedType::Bottom(engine::BottomTypeReason::Error),
                            )))
                        } else {
                            if parameters.len() > 1 {
                                self.compiler.diagnostics.add(Diagnostic::error(
                                    format!(
                                        "`List` accepts 1 parameter, but {} were provided",
                                        parameters.len()
                                    ),
                                    vec![Note::primary(
                                        annotation.span,
                                        "try removing some of these",
                                    )],
                                ));
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
                            self.compiler.diagnostics.add(Diagnostic::error(
                                "`Mutable` accepts 1 parameter, but none were provided",
                                vec![Note::primary(
                                    annotation.span,
                                    "try adding `_` here to infer the type of `Value`",
                                )],
                            ));

                            engine::UnresolvedType::Builtin(engine::BuiltinType::Mutable(Box::new(
                                engine::UnresolvedType::Bottom(engine::BottomTypeReason::Error),
                            )))
                        } else {
                            if parameters.len() > 1 {
                                self.compiler.diagnostics.add(Diagnostic::error(
                                    format!(
                                        "`Mutable` accepts 1 parameter, but {} were provided",
                                        parameters.len()
                                    ),
                                    vec![Note::primary(
                                        annotation.span,
                                        "try removing some of these",
                                    )],
                                ));
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
            lower::TypeAnnotationKind::Function(input, output) => engine::UnresolvedType::Function(
                Box::new(self.convert_type_annotation_inner(*input, convert_placeholder, stack)),
                Box::new(self.convert_type_annotation_inner(*output, convert_placeholder, stack)),
            ),
            lower::TypeAnnotationKind::Tuple(tys) => engine::UnresolvedType::Tuple(
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
    ) -> engine::UnresolvedType {
        let (trait_ty, trait_params) =
            self.with_trait_decl(trait_id, |decl| (decl.ty.clone(), decl.params.clone()));

        assert!(trait_params.len() == params.len());

        let substitutions = trait_params
            .into_iter()
            .zip(params)
            .collect::<GenericSubstitutions>();

        let mut instance_ty = engine::UnresolvedType::from(trait_ty);
        instance_ty.instantiate_with(&self.ctx, &substitutions);

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
            ($x:ident, $f:expr) => {
                paste::paste!(|id| {
                    typechecker
                        .borrow_mut()
                        .[<with_ $x _decl>](id, |decl| $f(decl.name))
                })
            };
        }

        let type_names = getter!(type, |name: InternedString| name.to_string());
        let trait_names = getter!(trait, |name: InternedString| name.to_string());
        let param_names = getter!(type_parameter, |name: Option<_>| {
            name.as_ref().map(ToString::to_string)
        });

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
            engine::TypeError::Recursive(_) => Diagnostic::error(
                "recursive type",
                vec![Note::primary(
                    error.span,
                    "the type of this references itself",
                )],
            ),
            engine::TypeError::Mismatch(actual, expected) => {
                let actual_ty = match &actual {
                    engine::UnresolvedType::Named(id, params, _) => Some((
                        self.declarations.borrow().types.get(id).unwrap().clone(),
                        params.clone(),
                    )),
                    _ => None,
                };

                Diagnostic::error(
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
                                            .position(|p| p == param)
                                            .expect("type parameter associated with wrong type");

                                        let inner_ty = actual_params[param].clone();

                                        self.ctx.clone().unify(inner_ty, expected.clone()).is_ok()
                                    })
                                    .then(|| Note::secondary(error.span, message))
                            })
                    }))
                    .collect(),
                )
            }
            engine::TypeError::MissingInstance(id, params, bound_span) => {
                let trait_attributes = self
                    .declarations
                    .borrow()
                    .traits
                    .get(&id)
                    .unwrap()
                    .attributes
                    .clone();

                Diagnostic::error(
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
                    .chain(
                        trait_attributes
                            .on_unimplemented
                            .map(|message| Note::secondary(error.span, message)),
                    )
                    .collect(),
                )
            }
            engine::TypeError::AmbiguousTrait(mut ty, _, candidates) => {
                ty.apply(&self.ctx);

                Diagnostic::error(
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
                    .chain(
                        candidates
                            .into_iter()
                            .map(|span| Note::secondary(span, "this instance could apply")),
                    )
                    .collect(),
                )
            }
            engine::TypeError::UnresolvedType(mut ty) => {
                ty.apply(&self.ctx);

                Diagnostic::error(
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
                )
            }
            engine::TypeError::InvalidNumericLiteral(ty) => Diagnostic::error(
                format!(
                    "number does not fit into a {}",
                    self.format_type(ty, format)
                ),
                vec![Note::primary(error.span, "invalid numeric literal")],
            ),
        };

        diagnostic.notes.append(&mut error.notes);

        #[cfg(debug_assertions)]
        {
            diagnostic.trace = error.trace;
        }

        self.compiler.diagnostics.add(diagnostic);
    }
}
