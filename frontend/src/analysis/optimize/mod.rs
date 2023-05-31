use crate::{
    analysis::{
        Arm, Expression, ExpressionKind, Pattern, PatternKind, Program, RuntimeFunction, Type,
    },
    Compiler, ItemId, Optimize, VariableId,
};
use parking_lot::RwLock;
use std::{
    collections::{BTreeMap, BTreeSet},
    mem,
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
    use crate::{analysis::SpanList, ConstantId};

    #[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
    pub struct Options {}

    impl Program {
        pub(super) fn ssa(mut self, _options: Options, compiler: &Compiler) -> Program {
            for item in self.items.values_mut() {
                let mut item = item.write();
                let (id, expr) = &mut *item;
                let id = id.map(|(_, id)| id);

                expr.traverse_mut(|expr| {
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
                                                        .unwrap_or_else(|| Type::Tuple(Vec::new())),
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
                });
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

                    expr.traverse_mut(|expr| {
                        let constant = match &expr.kind {
                            ExpressionKind::Constant(constant) if *constant != item_id => *constant,
                            _ => return,
                        };

                        let body = &self.items.get(&constant).unwrap().read().1;

                        if !body.is_pure(&self) {
                            return;
                        }

                        propagated = true;
                        *expr = body.clone();
                        drop(body);

                        // Replace the variables in the inlined constant with new variables

                        let mut new_vars = BTreeMap::new();
                        expr.traverse_mut(|expr| match &mut expr.kind {
                            ExpressionKind::Constant(c) if *c == constant => {
                                expr.kind = ExpressionKind::ExpandedConstant(constant);
                            }
                            ExpressionKind::When(_, arms) => {
                                for arm in arms {
                                    for var in arm.pattern.variables() {
                                        new_vars.insert(var, compiler.new_variable_id());
                                    }

                                    arm.pattern.traverse_mut(|pattern| {
                                        if let PatternKind::Variable(var) = &mut pattern.kind {
                                            *var = *new_vars.get(var).unwrap();
                                        }
                                    });
                                }
                            }
                            ExpressionKind::Initialize(pattern, _) => {
                                for var in pattern.variables() {
                                    new_vars.insert(var, compiler.new_variable_id());
                                }

                                pattern.traverse_mut(|pattern| {
                                    if let PatternKind::Variable(var) = &mut pattern.kind {
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
                                    new_vars.insert(var, compiler.new_variable_id());
                                }

                                pattern.traverse_mut(|pattern| {
                                    if let PatternKind::Variable(var) = &mut pattern.kind {
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
                        });
                    });

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
                    expr.traverse_mut_with(im::HashSet::new(), |expr, scope| {
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
                                    _ => return,
                                };

                                let captures = captures
                                    .iter()
                                    .map(|(var, _)| *var)
                                    .collect::<im::HashSet<VariableId>>();

                                if !captures.is_subset(scope)
                                    || body.sub_expression_count() > options.threshold
                                {
                                    return;
                                }

                                inlined = true;

                                // Replace the variables in the inlined function with new variables
                                // to prevent a cycle, where a variable references itself in its own
                                // definition

                                let new_vars = pattern
                                    .variables()
                                    .into_iter()
                                    .map(|var| (var, compiler.new_variable_id()))
                                    .collect::<BTreeMap<_, _>>();

                                let mut pattern = pattern.clone();
                                pattern.traverse_mut(|pattern| {
                                    if let PatternKind::Variable(var) = &mut pattern.kind {
                                        *var = *new_vars.get(var).unwrap();
                                    }
                                });

                                let mut body = body.as_ref().clone();
                                body.traverse_mut(|expr| match &mut expr.kind {
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
                                });

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
                    });

                    // Inline variables
                    expr.traverse_mut(|expr| {
                        if let ExpressionKind::When(input, arms) = &mut expr.kind {
                            if input.is_pure(&self) && arms.len() == 1 {
                                let input = input.as_ref().clone();
                                let arm = arms.first().unwrap();

                                if let PatternKind::Variable(var) = arm.pattern.kind {
                                    if arm.guard.is_none() {
                                        inlined = true;

                                        *expr = arms.pop().unwrap().body;
                                        expr.traverse_mut(|expr| match &mut expr.kind {
                                            ExpressionKind::Variable(v) => {
                                                if *v == var {
                                                    *expr = input.clone();
                                                }
                                            }
                                            ExpressionKind::Function(_, _, captures) => {
                                                *captures = mem::take(captures)
                                                    .into_iter()
                                                    .filter(|(v, _)| *v != var)
                                                    .collect();
                                            }
                                            _ => {}
                                        });
                                    }
                                }
                            }
                        }
                    });

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
    use std::collections::BTreeSet;

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
                if let Some(entrypoint) = self.entrypoint {
                    let mut used = BTreeSet::from([entrypoint]);

                    fn track_used(program: &Program, item: ItemId, used: &mut BTreeSet<ItemId>) {
                        if let Some(item) = program.items.get(&item) {
                            let item = item.read();
                            let (_, expr) = &*item;

                            expr.traverse(|expr| {
                                if let ExpressionKind::Constant(item)
                                | ExpressionKind::ExpandedConstant(item) = expr.kind
                                {
                                    if used.insert(item) {
                                        track_used(program, item, used);
                                    }
                                }
                            });
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

    impl Expression {
        pub fn sub_expression_count(&self) -> usize {
            let mut count = 0;
            self.traverse(|_| count += 1);
            count
        }

        pub fn is_pure(&self, program: &Program) -> bool {
            self.is_pure_inner(program, None, &mut Vec::new())
        }

        fn is_pure_inner(
            &self,
            program: &Program,
            function_call: Option<&BTreeSet<VariableId>>,
            stack: &mut Vec<ItemId>,
        ) -> bool {
            match &self.kind {
                ExpressionKind::Error(_) => false,
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
                ExpressionKind::Variable(var) => {
                    function_call.map_or(false, |vars| vars.contains(var))
                }
                ExpressionKind::Function(_, _, _) => true,
                ExpressionKind::Block(exprs, _) => exprs
                    .iter()
                    .all(|expr| expr.is_pure_inner(program, function_call, stack)),
                ExpressionKind::Call(func, input, _) => {
                    let mut func = func.as_ref().clone();
                    let mut inner_function_call = None;
                    loop {
                        match &func.kind {
                            ExpressionKind::Function(pattern, body, captures) => {
                                let function_call =
                                    inner_function_call.get_or_insert_with(BTreeSet::new);
                                function_call.extend(pattern.variables());
                                function_call.extend(captures.iter().map(|(var, _)| *var));

                                func = body.as_ref().clone();
                            }
                            ExpressionKind::Constant(constant)
                            | ExpressionKind::ExpandedConstant(constant) => {
                                if let Some(body) = Self::constant_is_pure(
                                    constant,
                                    program,
                                    inner_function_call.as_ref(),
                                    stack,
                                ) {
                                    func = body;
                                } else {
                                    break;
                                }
                            }
                            _ => break,
                        }
                    }

                    func.is_pure_inner(program, inner_function_call.as_ref(), stack)
                        && input.is_pure_inner(program, function_call, stack)
                }
                ExpressionKind::When(expr, arms) => {
                    expr.is_pure_inner(program, function_call, stack)
                        && arms
                            .iter()
                            .map(|arm| &arm.body)
                            .all(|expr| expr.is_pure_inner(program, function_call, stack))
                }
                ExpressionKind::Structure(exprs) => exprs
                    .iter()
                    .all(|expr| expr.is_pure_inner(program, function_call, stack)),
                ExpressionKind::Variant(_, exprs) => exprs
                    .iter()
                    .all(|expr| expr.is_pure_inner(program, function_call, stack)),
                ExpressionKind::Tuple(exprs) => exprs
                    .iter()
                    .all(|expr| expr.is_pure_inner(program, function_call, stack)),
                ExpressionKind::Format(segments, _) => segments
                    .iter()
                    .all(|(_, expr)| expr.is_pure_inner(program, function_call, stack)),
                ExpressionKind::Runtime(func, inputs) => {
                    func.is_pure()
                        && inputs
                            .iter()
                            .all(|expr| expr.is_pure_inner(program, function_call, stack))
                }
                ExpressionKind::External(_, _, _)
                | ExpressionKind::Initialize(_, _)
                | ExpressionKind::With(_, _)
                | ExpressionKind::ContextualConstant(_) => false,
                ExpressionKind::Constant(constant) | ExpressionKind::ExpandedConstant(constant) => {
                    Self::constant_is_pure(constant, program, function_call, stack).is_some()
                }
            }
        }

        fn constant_is_pure<'a>(
            constant: &ItemId,
            program: &'a Program,
            function_call: Option<&BTreeSet<VariableId>>,
            stack: &mut Vec<ItemId>,
        ) -> Option<Expression> {
            if stack.contains(constant) {
                return None;
            }

            let item = match program.items.get(constant) {
                Some(item) => item,
                None => return None,
            };

            stack.push(*constant);

            let item = item.read();
            let (_, body) = &*item;

            let is_pure = body.is_pure_inner(program, function_call, stack);
            stack.pop();

            is_pure.then(|| body.clone())
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

    impl RuntimeFunction {
        // TODO: In the future, add a [pure] attribute instead of checking these manually
        fn is_pure(&self) -> bool {
            match self {
                RuntimeFunction::Crash => false,
                RuntimeFunction::Display => false,
                RuntimeFunction::Prompt => false,
                RuntimeFunction::Choice => false,
                RuntimeFunction::WithUi => false,
                RuntimeFunction::MessageUi => false,
                RuntimeFunction::WithContinuation => false,
                RuntimeFunction::WithTaskGroup => false,
                RuntimeFunction::Task => false,
                RuntimeFunction::InBackground => false,
                RuntimeFunction::Delay => false,
                RuntimeFunction::NumberToText => true,
                RuntimeFunction::IntegerToText => true,
                RuntimeFunction::NaturalToText => true,
                RuntimeFunction::ByteToText => true,
                RuntimeFunction::SignedToText => true,
                RuntimeFunction::UnsignedToText => true,
                RuntimeFunction::FloatToText => true,
                RuntimeFunction::DoubleToText => true,
                RuntimeFunction::TextToNumber => true,
                RuntimeFunction::TextToInteger => true,
                RuntimeFunction::TextToNatural => true,
                RuntimeFunction::TextToByte => true,
                RuntimeFunction::TextToSigned => true,
                RuntimeFunction::TextToUnsigned => true,
                RuntimeFunction::TextToFloat => true,
                RuntimeFunction::TextToDouble => true,
                RuntimeFunction::NaturalToNumber => true,
                RuntimeFunction::NumberToNatural => true,
                RuntimeFunction::AddNumber => true,
                RuntimeFunction::SubtractNumber => true,
                RuntimeFunction::MultiplyNumber => true,
                RuntimeFunction::DivideNumber => true,
                RuntimeFunction::ModuloNumber => true,
                RuntimeFunction::PowerNumber => true,
                RuntimeFunction::FloorNumber => true,
                RuntimeFunction::CeilNumber => true,
                RuntimeFunction::SqrtNumber => true,
                RuntimeFunction::AddInteger => true,
                RuntimeFunction::SubtractInteger => true,
                RuntimeFunction::MultiplyInteger => true,
                RuntimeFunction::DivideInteger => true,
                RuntimeFunction::ModuloInteger => true,
                RuntimeFunction::PowerInteger => true,
                RuntimeFunction::AddNatural => true,
                RuntimeFunction::SubtractNatural => true,
                RuntimeFunction::MultiplyNatural => true,
                RuntimeFunction::DivideNatural => true,
                RuntimeFunction::ModuloNatural => true,
                RuntimeFunction::PowerNatural => true,
                RuntimeFunction::AddByte => true,
                RuntimeFunction::SubtractByte => true,
                RuntimeFunction::MultiplyByte => true,
                RuntimeFunction::DivideByte => true,
                RuntimeFunction::ModuloByte => true,
                RuntimeFunction::PowerByte => true,
                RuntimeFunction::AddSigned => true,
                RuntimeFunction::SubtractSigned => true,
                RuntimeFunction::MultiplySigned => true,
                RuntimeFunction::DivideSigned => true,
                RuntimeFunction::ModuloSigned => true,
                RuntimeFunction::PowerSigned => true,
                RuntimeFunction::AddUnsigned => true,
                RuntimeFunction::SubtractUnsigned => true,
                RuntimeFunction::MultiplyUnsigned => true,
                RuntimeFunction::DivideUnsigned => true,
                RuntimeFunction::ModuloUnsigned => true,
                RuntimeFunction::PowerUnsigned => true,
                RuntimeFunction::AddFloat => true,
                RuntimeFunction::SubtractFloat => true,
                RuntimeFunction::MultiplyFloat => true,
                RuntimeFunction::DivideFloat => true,
                RuntimeFunction::ModuloFloat => true,
                RuntimeFunction::PowerFloat => true,
                RuntimeFunction::FloorFloat => true,
                RuntimeFunction::CeilFloat => true,
                RuntimeFunction::SqrtFloat => true,
                RuntimeFunction::AddDouble => true,
                RuntimeFunction::SubtractDouble => true,
                RuntimeFunction::MultiplyDouble => true,
                RuntimeFunction::DivideDouble => true,
                RuntimeFunction::ModuloDouble => true,
                RuntimeFunction::PowerDouble => true,
                RuntimeFunction::FloorDouble => true,
                RuntimeFunction::CeilDouble => true,
                RuntimeFunction::SqrtDouble => true,
                RuntimeFunction::TextEquality => true,
                RuntimeFunction::NumberEquality => true,
                RuntimeFunction::IntegerEquality => true,
                RuntimeFunction::NaturalEquality => true,
                RuntimeFunction::ByteEquality => true,
                RuntimeFunction::SignedEquality => true,
                RuntimeFunction::UnsignedEquality => true,
                RuntimeFunction::FloatEquality => true,
                RuntimeFunction::DoubleEquality => true,
                RuntimeFunction::TextOrdering => true,
                RuntimeFunction::NumberOrdering => true,
                RuntimeFunction::IntegerOrdering => true,
                RuntimeFunction::NaturalOrdering => true,
                RuntimeFunction::ByteOrdering => true,
                RuntimeFunction::SignedOrdering => true,
                RuntimeFunction::UnsignedOrdering => true,
                RuntimeFunction::FloatOrdering => true,
                RuntimeFunction::DoubleOrdering => true,
                RuntimeFunction::MakeMutable => true,
                RuntimeFunction::GetMutable => false,
                RuntimeFunction::SetMutable => false,
                RuntimeFunction::MakeEmptyList => true,
                RuntimeFunction::ListFirst => true,
                RuntimeFunction::ListLast => true,
                RuntimeFunction::ListInitial => true,
                RuntimeFunction::ListTail => true,
                RuntimeFunction::ListNth => true,
                RuntimeFunction::ListAppend => true,
                RuntimeFunction::ListPrepend => true,
                RuntimeFunction::ListInsert => true,
                RuntimeFunction::ListRemove => true,
                RuntimeFunction::TextHeadTail => true,
            }
        }
    }
}
