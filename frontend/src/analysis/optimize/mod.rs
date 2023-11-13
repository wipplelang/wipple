use crate::{
    analysis::{
        Arm, Expression, ExpressionKind, Pattern, PatternKind, Program, Semantics, SpanList,
        TypeKind,
    },
    Compiler, ConstantId, ItemId, Optimize, VariableId,
};
use parking_lot::RwLock;
use std::{
    collections::{BTreeMap, BTreeSet},
    mem,
    ops::ControlFlow,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Options {
    /// Options for converting program to SSA form, enabling more optimizations.
    /// `None` disables this conversion.
    pub ssa: Option<ssa::Options>,

    /// Options for propagating constants. `None` disables constant propagation.
    pub propagate: Option<propagate::Options>,

    /// Options for inlining function calls. `None` disables inlining.
    pub inline: Option<inline::Options>,

    /// Options for removing unused declarations. `None` disables removing
    /// unused declarations.
    pub unused: Option<unused::Options>,
}

impl Default for Options {
    fn default() -> Self {
        Options {
            ssa: Some(Default::default()),
            propagate: Some(Default::default()),
            inline: Some(Default::default()),
            unused: Some(Default::default()),
        }
    }
}

impl Optimize for Program {
    type Options = Options;

    fn optimize(self, options: Self::Options, compiler: &Compiler) -> Program {
        let program = self;

        macro_rules! passes {
            ($ir:expr, [$($pass:ident),* $(,)?]) => {
                {
                    $(
                        let program = match options.$pass {
                            Some(options) => program.$pass(options, compiler),
                            None => program,
                        };
                    )*

                    program
                }
            };
        }

        passes!(ir, [unused, ssa, propagate, inline, unused])
    }
}

pub mod ssa {
    use super::*;

    #[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
    pub struct Options {}

    impl Program {
        pub(super) fn ssa(mut self, _options: Options, compiler: &Compiler) -> Program {
            for item in self.items.values_mut() {
                let mut item = item.write();
                let (id, expr) = &mut *item;
                let id = id.map(|(_, id)| id);

                expr.traverse_mut(
                    |expr| {
                        if let ExpressionKind::Block(exprs, _) = &mut expr.kind {
                            fn convert_block(
                                exprs: &[Expression],
                                span: SpanList,
                                id: Option<ConstantId>,
                                compiler: &Compiler,
                            ) -> Vec<Expression> {
                                let mut result = Vec::new();
                                for (index, expr) in exprs.iter().enumerate() {
                                    if let ExpressionKind::Initialize(pattern, value) = &expr.kind {
                                        let remaining = &exprs[(index + 1)..];

                                        result.push(Expression {
                                            id: expr.id,
                                            span: expr.span,
                                            ty: expr.ty.clone(),
                                            kind: ExpressionKind::When(
                                                value.clone(),
                                                vec![Arm {
                                                    span: pattern.span,
                                                    pattern: pattern.clone(),
                                                    guard: None,
                                                    body: Expression {
                                                        id: compiler.new_expression_id(id),
                                                        ty: remaining
                                                            .last()
                                                            .map(|expr| expr.ty.clone())
                                                            .unwrap_or_else(|| {
                                                                compiler.new_ty(
                                                                    TypeKind::Tuple(Vec::new()),
                                                                    expr.span,
                                                                )
                                                            }),
                                                        span,
                                                        kind: ExpressionKind::Block(
                                                            convert_block(
                                                                remaining,
                                                                remaining.first().map_or(
                                                                    span,
                                                                    |expr| {
                                                                        SpanList::join(
                                                                            expr.span,
                                                                            remaining
                                                                                .last()
                                                                                .unwrap()
                                                                                .span,
                                                                        )
                                                                    },
                                                                ),
                                                                id,
                                                                compiler,
                                                            ),
                                                            false,
                                                        ),
                                                    },
                                                }],
                                            ),
                                        });

                                        break;
                                    } else {
                                        result.push(expr.clone());
                                    }
                                }

                                result
                            }

                            *exprs = convert_block(exprs, expr.span, id, compiler);

                            if exprs.len() == 1 {
                                *expr = exprs.pop().unwrap();
                            }
                        }

                        ControlFlow::Continue(())
                    },
                    |_| ControlFlow::Continue(()),
                );
            }

            self
        }
    }
}

pub mod propagate {
    use super::*;

    #[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
    pub struct Options {
        /// Perform no more than this many propagation passes. `None` indicates
        /// no limit.
        pub pass_limit: Option<usize>,
    }

    impl Program {
        pub(super) fn propagate(self, options: Options, compiler: &Compiler) -> Program {
            let item_ids = self.items.keys().cloned().collect::<Vec<_>>();

            for &item_id in &item_ids {
                let mut passes: usize = 1;

                loop {
                    let mut propagated = false;

                    let mut item = self.items.get(&item_id).unwrap().write();
                    let (_, expr) = &mut *item;

                    expr.traverse_mut(
                        |expr| {
                            let constant = match &expr.kind {
                                ExpressionKind::Constant(constant) if *constant != item_id => {
                                    *constant
                                }
                                _ => return ControlFlow::Continue(()),
                            };

                            let body = &self.items.get(&constant).unwrap().read().1;

                            if !body.is_pure(&self) {
                                return ControlFlow::Continue(());
                            }

                            propagated = true;
                            *expr = body.clone();

                            // Replace the variables in the inlined constant with new variables

                            let mut new_vars = BTreeMap::new();
                            expr.traverse_mut(
                                |expr| {
                                    match &mut expr.kind {
                                        ExpressionKind::Constant(c) if *c == constant => {
                                            expr.kind = ExpressionKind::ExpandedConstant(constant);
                                        }
                                        ExpressionKind::When(_, arms) => {
                                            for arm in arms {
                                                for var in arm.pattern.variables() {
                                                    new_vars.insert(
                                                        var,
                                                        compiler.new_variable_id(var.owner),
                                                    );
                                                }

                                                arm.pattern.traverse_mut(|pattern| {
                                                    if let PatternKind::Variable(var) =
                                                        &mut pattern.kind
                                                    {
                                                        *var = *new_vars.get(var).unwrap();
                                                    }
                                                });
                                            }
                                        }
                                        ExpressionKind::Initialize(pattern, _) => {
                                            for var in pattern.variables() {
                                                new_vars.insert(
                                                    var,
                                                    compiler.new_variable_id(var.owner),
                                                );
                                            }

                                            pattern.traverse_mut(|pattern| {
                                                if let PatternKind::Variable(var) =
                                                    &mut pattern.kind
                                                {
                                                    *var = *new_vars.get(var).unwrap();
                                                }
                                            });
                                        }
                                        ExpressionKind::Function(pattern, _, captures) => {
                                            for (var, _) in captures {
                                                if let Some(v) = new_vars.get(var) {
                                                    *var = *v;
                                                }
                                            }

                                            for var in pattern.variables() {
                                                new_vars.insert(
                                                    var,
                                                    compiler.new_variable_id(var.owner),
                                                );
                                            }

                                            pattern.traverse_mut(|pattern| {
                                                if let PatternKind::Variable(var) =
                                                    &mut pattern.kind
                                                {
                                                    *var = *new_vars.get(var).unwrap();
                                                }
                                            });
                                        }
                                        ExpressionKind::Variable(var) => {
                                            if let Some(v) = new_vars.get(var) {
                                                *var = *v;
                                            }
                                        }
                                        _ => {}
                                    }

                                    ControlFlow::Continue(())
                                },
                                |_| ControlFlow::Continue(()),
                            );

                            ControlFlow::Break(false)
                        },
                        |_| ControlFlow::Continue(()),
                    );

                    if !propagated || options.pass_limit.map_or(false, |limit| passes > limit) {
                        break;
                    }

                    passes += 1;
                }
            }

            self
        }
    }
}

pub mod inline {
    use super::*;

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Options {
        /// Functions with more than this number of expressions will not be
        /// inlined.
        pub threshold: usize,

        /// Perform no more than this many inline passes. `None` indicates no
        /// limit.
        pub pass_limit: Option<usize>,
    }

    impl Default for Options {
        fn default() -> Self {
            Options {
                threshold: 200,
                pass_limit: None,
            }
        }
    }

    impl Program {
        pub(super) fn inline(mut self, options: Options, compiler: &Compiler) -> Program {
            let mut passes: usize = 1;
            loop {
                let mut inlined = false;

                for item in self.items.keys().copied().collect::<Vec<_>>() {
                    let (constant, mut expr) = self.items.get(&item).unwrap().write().clone();

                    // Inline function calls
                    expr.traverse_mut_with(
                        im::HashSet::new(),
                        |expr, scope| {
                            // We only perform this optimization when the program is in SSA form, so we
                            // only need to check for functions and `when` expressions. In addition, we
                            // only need to check for calls to function expressions because constants
                            // are inlined before this pass.
                            match &mut expr.kind {
                                ExpressionKind::Function(pattern, _, _) => {
                                    for var in pattern.variables() {
                                        scope.insert(var);
                                    }
                                }
                                ExpressionKind::When(_, arms) => {
                                    for arm in arms {
                                        for var in arm.pattern.variables() {
                                            scope.insert(var);
                                        }
                                    }
                                }
                                ExpressionKind::Call(func, input, _first) => {
                                    let (pattern, body, captures) = match &func.kind {
                                        ExpressionKind::Function(pattern, body, captures) => {
                                            (pattern, body, captures)
                                        }
                                        _ => return ControlFlow::Continue(()),
                                    };

                                    let captures = captures
                                        .iter()
                                        .map(|(var, _)| *var)
                                        .collect::<im::HashSet<VariableId>>();

                                    if !captures.is_subset(scope)
                                        || body.sub_expression_count() > options.threshold
                                    {
                                        return ControlFlow::Continue(());
                                    }

                                    inlined = true;

                                    // Replace the variables in the inlined function with new variables
                                    // to prevent a cycle, where a variable references itself in its own
                                    // definition

                                    let new_vars = pattern
                                        .variables()
                                        .into_iter()
                                        .map(|var| (var, compiler.new_variable_id(var.owner)))
                                        .collect::<BTreeMap<_, _>>();

                                    let mut pattern = pattern.clone();
                                    pattern.traverse_mut(|pattern| {
                                        if let PatternKind::Variable(var) = &mut pattern.kind {
                                            *var = *new_vars.get(var).unwrap();
                                        }
                                    });

                                    let mut body = body.as_ref().clone();
                                    body.traverse_mut(
                                        |expr| {
                                            match &mut expr.kind {
                                                ExpressionKind::Variable(var) => {
                                                    if let Some(v) = new_vars.get(var) {
                                                        *var = *v;
                                                    }
                                                }
                                                ExpressionKind::Function(_, _, captures) => {
                                                    for (var, _) in captures {
                                                        if let Some(v) = new_vars.get(var) {
                                                            *var = *v;
                                                        }
                                                    }
                                                }
                                                _ => {}
                                            }

                                            ControlFlow::Continue(())
                                        },
                                        |_| ControlFlow::Continue(()),
                                    );

                                    expr.kind = ExpressionKind::When(
                                        input.clone(),
                                        vec![Arm {
                                            span: input.span,
                                            pattern,
                                            guard: None,
                                            body,
                                        }],
                                    );
                                }
                                _ => {}
                            }

                            ControlFlow::Continue(())
                        },
                        |_, _| ControlFlow::Continue(()),
                    );

                    // Inline variables
                    expr.traverse_mut(
                        |expr| {
                            if let ExpressionKind::When(input, arms) = &mut expr.kind {
                                if input.is_pure(&self) && arms.len() == 1 {
                                    let input = input.as_ref().clone();
                                    let arm = arms.first().unwrap();

                                    if let PatternKind::Variable(var) = arm.pattern.kind {
                                        if arm.guard.is_none() {
                                            inlined = true;

                                            *expr = arms.pop().unwrap().body;
                                            expr.traverse_mut(
                                                |expr| {
                                                    match &mut expr.kind {
                                                        ExpressionKind::Variable(v) => {
                                                            if *v == var {
                                                                *expr = input.clone();
                                                            }
                                                        }
                                                        ExpressionKind::Function(
                                                            _,
                                                            _,
                                                            captures,
                                                        ) => {
                                                            *captures = mem::take(captures)
                                                                .into_iter()
                                                                .filter(|(v, _)| *v != var)
                                                                .collect();
                                                        }
                                                        _ => {}
                                                    }

                                                    ControlFlow::Continue(())
                                                },
                                                |_| ControlFlow::Continue(()),
                                            );
                                        }
                                    }
                                }
                            }

                            ControlFlow::Continue(())
                        },
                        |_| ControlFlow::Continue(()),
                    );

                    self.items.insert(item, RwLock::new((constant, expr)));
                }

                if !inlined || options.pass_limit.map_or(false, |limit| passes > limit) {
                    break;
                }

                passes += 1;
            }

            self
        }
    }
}

pub mod unused {
    use super::*;

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Options {
        /// Whether to remove unused constants.
        pub constants: bool,
        // TODO: More options
    }

    impl Default for Options {
        fn default() -> Self {
            Options { constants: true }
        }
    }

    impl Program {
        pub(super) fn unused(mut self, options: Options, _compiler: &Compiler) -> Program {
            if options.constants {
                if let Some(entrypoint) = self.top_level {
                    let mut used = BTreeSet::from([entrypoint]);

                    fn track_used(program: &Program, item: ItemId, used: &mut BTreeSet<ItemId>) {
                        if let Some(item) = program.items.get(&item) {
                            let item = item.read();
                            let (_, expr) = &*item;

                            expr.traverse(
                                |expr| {
                                    if let ExpressionKind::Constant(item)
                                    | ExpressionKind::ExpandedConstant(item) = expr.kind
                                    {
                                        if used.insert(item) {
                                            track_used(program, item, used);
                                        }
                                    }

                                    ControlFlow::Continue(())
                                },
                                |_| ControlFlow::Continue(()),
                            );
                        }
                    }

                    track_used(&self, entrypoint, &mut used);

                    for item in self.items.keys().cloned().collect::<Vec<_>>() {
                        if !used.contains(&item) {
                            self.items.remove(&item);
                        }
                    }
                }
            }

            self
        }
    }
}

mod util {
    use super::*;

    #[derive(Debug, Default)]
    struct Info {
        function_call: Option<BTreeSet<VariableId>>,
        expanded_constant: bool,
    }

    impl Expression {
        pub fn sub_expression_count(&self) -> usize {
            let mut count = 0;
            self.traverse(
                |_| {
                    count += 1;
                    ControlFlow::Continue(())
                },
                |_| ControlFlow::Continue(()),
            );
            count
        }

        pub fn is_pure(&self, program: &Program) -> bool {
            self.is_pure_inner(program, &mut Info::default())
        }

        fn is_pure_inner(&self, program: &Program, info: &mut Info) -> bool {
            match &self.kind {
                ExpressionKind::Error(_)
                | ExpressionKind::UnresolvedConstant(_)
                | ExpressionKind::UnresolvedTrait(_)
                | ExpressionKind::UnresolvedExtend => false,
                ExpressionKind::Marker
                | ExpressionKind::Text(_)
                | ExpressionKind::Number(_)
                | ExpressionKind::Integer(_)
                | ExpressionKind::Natural(_)
                | ExpressionKind::Byte(_)
                | ExpressionKind::Signed(_)
                | ExpressionKind::Unsigned(_)
                | ExpressionKind::Float(_)
                | ExpressionKind::Double(_) => true,
                ExpressionKind::Variable(var) => info
                    .function_call
                    .as_ref()
                    .map_or(false, |vars| vars.contains(var)),
                ExpressionKind::Function(_, _, _) => true,
                ExpressionKind::Block(exprs, _) => {
                    exprs.iter().all(|expr| expr.is_pure_inner(program, info))
                }
                ExpressionKind::Call(func, input, _) => {
                    let mut func = func.as_ref().clone();
                    let prev_function_call = mem::take(&mut info.function_call);
                    let prev_expanded_constant = info.expanded_constant;
                    loop {
                        match &func.kind {
                            ExpressionKind::Function(pattern, body, captures) => {
                                let function_call =
                                    info.function_call.get_or_insert_with(BTreeSet::new);
                                function_call.extend(pattern.variables());
                                function_call.extend(captures.iter().map(|(var, _)| *var));

                                func = body.as_ref().clone();
                            }
                            ExpressionKind::Constant(constant)
                            | ExpressionKind::ExpandedConstant(constant) => {
                                if info.expanded_constant {
                                    return false;
                                } else if let Some(body) =
                                    Self::pure_constant_body(constant, program, info)
                                {
                                    func = body;
                                    info.expanded_constant = true;
                                } else {
                                    return false;
                                }
                            }
                            _ => break,
                        }
                    }

                    let func_is_pure = func.is_pure_inner(program, info);

                    info.function_call = prev_function_call;
                    info.expanded_constant = prev_expanded_constant;

                    func_is_pure && input.is_pure_inner(program, info)
                }
                ExpressionKind::When(expr, arms) => {
                    expr.is_pure_inner(program, info)
                        && arms
                            .iter()
                            .map(|arm| &arm.body)
                            .all(|expr| expr.is_pure_inner(program, info))
                }
                ExpressionKind::Structure(exprs) => {
                    exprs.iter().all(|expr| expr.is_pure_inner(program, info))
                }
                ExpressionKind::Variant(_, exprs) => {
                    exprs.iter().all(|expr| expr.is_pure_inner(program, info))
                }
                ExpressionKind::Tuple(exprs) => {
                    exprs.iter().all(|expr| expr.is_pure_inner(program, info))
                }
                ExpressionKind::Format(segments, _) => segments
                    .iter()
                    .all(|(_, expr)| expr.is_pure_inner(program, info)),
                ExpressionKind::Plugin(_, _, _) => panic!("found unresolved plugin"),
                ExpressionKind::Intrinsic(_, _)
                | ExpressionKind::External(_, _, _)
                | ExpressionKind::Initialize(_, _)
                | ExpressionKind::With(_, _)
                | ExpressionKind::ContextualConstant(_)
                | ExpressionKind::End(_) => false,
                ExpressionKind::Constant(constant) | ExpressionKind::ExpandedConstant(constant) => {
                    Self::pure_constant_body(constant, program, info).is_some()
                }
                ExpressionKind::Extend(value, fields) => {
                    value.is_pure_inner(program, info)
                        && fields
                            .values()
                            .all(|field| field.is_pure_inner(program, info))
                }
                ExpressionKind::Semantics(semantics, expr) => {
                    *semantics == Semantics::Pure || expr.is_pure_inner(program, info)
                }
            }
        }

        fn pure_constant_body(
            constant: &ItemId,
            program: &Program,
            info: &mut Info,
        ) -> Option<Expression> {
            let item = match program.items.get(constant) {
                Some(item) => item,
                None => return None,
            };

            let item = item.read();
            let (_, body) = &*item;

            body.is_pure_inner(program, info).then(|| body.clone())
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
    }
}
