use crate::{
    analysis::{
        Arm, Expression, ExpressionKind, Pattern, PatternKind, Program, RuntimeFunction, Type,
    },
    Compiler, ItemId, Optimize, Span, VariableId,
};
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

        passes!(ir, [ssa, propagate, inline, unused])
    }
}

pub mod ssa {
    use super::*;

    #[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
    pub struct Options {}

    impl Program {
        pub(super) fn ssa(mut self, _options: Options, _compiler: &Compiler) -> Program {
            for (_, expr) in self.items.values_mut() {
                expr.traverse_mut(|expr| {
                    if let ExpressionKind::Block(exprs, _) = &mut expr.kind {
                        fn convert_block(exprs: &[Expression], span: Span) -> Vec<Expression> {
                            let mut result = Vec::new();
                            for (index, expr) in exprs.iter().enumerate() {
                                if let ExpressionKind::Initialize(pattern, value) = &expr.kind {
                                    let remaining = &exprs[(index + 1)..];

                                    result.push(Expression {
                                        span: expr.span,
                                        ty: expr.ty.clone(),
                                        kind: ExpressionKind::When(
                                            value.clone(),
                                            vec![Arm {
                                                span: pattern.span,
                                                pattern: pattern.clone(),
                                                body: Expression {
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
                                                                    expr.span.with_end(
                                                                        remaining
                                                                            .last()
                                                                            .unwrap()
                                                                            .span
                                                                            .end,
                                                                    )
                                                                },
                                                            ),
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

                        *exprs = convert_block(exprs, expr.span);

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
        pub(super) fn propagate(mut self, options: Options, _compiler: &Compiler) -> Program {
            let mut items = self
                .items
                .iter()
                .map(|(item, (constant, expr))| (*item, (*constant, Some(expr.clone()))))
                .collect::<BTreeMap<_, _>>();

            let item_ids = items.keys().cloned().collect::<Vec<_>>();

            for &item in &item_ids {
                let mut passes: usize = 1;

                loop {
                    let mut propagated = false;

                    let mut expr = mem::take(&mut items.get_mut(&item).unwrap().1).unwrap();

                    expr.traverse_mut_with(Vec::new(), |expr, stack| {
                        let constant = match &expr.kind {
                            ExpressionKind::Constant(constant)
                                if *constant != item && !stack.contains(constant) =>
                            {
                                *constant
                            }
                            _ => return,
                        };

                        stack.push(constant);

                        let body = match &items.get(&constant).unwrap().1 {
                            Some(expr) => expr,
                            None => return,
                        };

                        if body.is_pure(&self, &mut Vec::new()) {
                            propagated = true;
                            *expr = body.clone();

                            expr.traverse_mut(|expr| {
                                if let ExpressionKind::Constant(c) = expr.kind {
                                    if c == constant {
                                        expr.kind = ExpressionKind::ExpandedConstant(c);
                                    }
                                }
                            });
                        }
                    });

                    items.get_mut(&item).unwrap().1.replace(expr);

                    if !propagated || options.pass_limit.map_or(false, |limit| passes > limit) {
                        break;
                    }

                    passes += 1;
                }
            }

            self.items = items
                .into_iter()
                .map(|(item, (constant, expr))| (item, (constant, expr.unwrap())))
                .collect();

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
                    let (constant, mut expr) = self.items.get(&item).unwrap().clone();

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
                            ExpressionKind::Call(func, input) => {
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
                            if input.is_pure(&self, &mut Vec::new()) && arms.len() == 1 {
                                let input = input.as_ref().clone();

                                if let PatternKind::Variable(var) =
                                    arms.first().unwrap().pattern.kind
                                {
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
                    });

                    self.items.insert(item, (constant, expr));
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

                    for (_, expr) in self.items.values() {
                        expr.traverse(|expr| {
                            if let ExpressionKind::Constant(item) = &expr.kind {
                                used.insert(*item);
                            }
                        })
                    }

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

        pub fn is_pure(&self, program: &Program, stack: &mut Vec<ItemId>) -> bool {
            match &self.kind {
                ExpressionKind::Error(trace) => {
                    panic!(
                        "found error expression in program: {:?}",
                        trace.clone().into_inner()
                    )
                }
                ExpressionKind::Marker
                | ExpressionKind::Text(_)
                | ExpressionKind::Number(_)
                | ExpressionKind::Integer(_)
                | ExpressionKind::Natural(_)
                | ExpressionKind::Byte(_)
                | ExpressionKind::Signed(_)
                | ExpressionKind::Unsigned(_)
                | ExpressionKind::Float(_)
                | ExpressionKind::Double(_)
                | ExpressionKind::Function(_, _, _)
                | ExpressionKind::Variable(_) => true,
                ExpressionKind::Block(exprs, _) => {
                    exprs.iter().all(|expr| expr.is_pure(program, stack))
                }
                ExpressionKind::End(expr) => expr.is_pure(program, stack),
                ExpressionKind::Call(func, input) => {
                    func.is_pure(program, stack) && input.is_pure(program, stack)
                }
                ExpressionKind::When(expr, arms) => {
                    expr.is_pure(program, stack)
                        && arms
                            .iter()
                            .map(|arm| &arm.body)
                            .all(|expr| expr.is_pure(program, stack))
                }
                ExpressionKind::Structure(exprs) => {
                    exprs.iter().all(|expr| expr.is_pure(program, stack))
                }
                ExpressionKind::Variant(_, exprs) => {
                    exprs.iter().all(|expr| expr.is_pure(program, stack))
                }
                ExpressionKind::Tuple(exprs) => {
                    exprs.iter().all(|expr| expr.is_pure(program, stack))
                }
                ExpressionKind::Runtime(func, inputs) => {
                    func.is_pure() && inputs.iter().all(|expr| expr.is_pure(program, stack))
                }
                ExpressionKind::External(_, _, _) | ExpressionKind::Initialize(_, _) => false,
                ExpressionKind::Constant(constant) | ExpressionKind::ExpandedConstant(constant) => {
                    stack.push(*constant);

                    let is_pure = program
                        .items
                        .get(constant)
                        .unwrap()
                        .1
                        .is_pure(program, stack);

                    stack.pop();

                    is_pure
                }
            }
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
                RuntimeFunction::WriteStdout => false,
                RuntimeFunction::Format => true,
                RuntimeFunction::NumberToText => true,
                RuntimeFunction::IntegerToText => true,
                RuntimeFunction::NaturalToText => true,
                RuntimeFunction::ByteToText => true,
                RuntimeFunction::SignedToText => true,
                RuntimeFunction::UnsignedToText => true,
                RuntimeFunction::FloatToText => true,
                RuntimeFunction::DoubleToText => true,
                RuntimeFunction::AddNumber => true,
                RuntimeFunction::SubtractNumber => true,
                RuntimeFunction::MultiplyNumber => true,
                RuntimeFunction::DivideNumber => true,
                RuntimeFunction::PowerNumber => true,
                RuntimeFunction::FloorNumber => true,
                RuntimeFunction::CeilNumber => true,
                RuntimeFunction::SqrtNumber => true,
                RuntimeFunction::AddInteger => true,
                RuntimeFunction::SubtractInteger => true,
                RuntimeFunction::MultiplyInteger => true,
                RuntimeFunction::DivideInteger => true,
                RuntimeFunction::PowerInteger => true,
                RuntimeFunction::AddNatural => true,
                RuntimeFunction::SubtractNatural => true,
                RuntimeFunction::MultiplyNatural => true,
                RuntimeFunction::DivideNatural => true,
                RuntimeFunction::PowerNatural => true,
                RuntimeFunction::AddByte => true,
                RuntimeFunction::SubtractByte => true,
                RuntimeFunction::MultiplyByte => true,
                RuntimeFunction::DivideByte => true,
                RuntimeFunction::PowerByte => true,
                RuntimeFunction::AddSigned => true,
                RuntimeFunction::SubtractSigned => true,
                RuntimeFunction::MultiplySigned => true,
                RuntimeFunction::DivideSigned => true,
                RuntimeFunction::PowerSigned => true,
                RuntimeFunction::AddUnsigned => true,
                RuntimeFunction::SubtractUnsigned => true,
                RuntimeFunction::MultiplyUnsigned => true,
                RuntimeFunction::DivideUnsigned => true,
                RuntimeFunction::PowerUnsigned => true,
                RuntimeFunction::AddFloat => true,
                RuntimeFunction::SubtractFloat => true,
                RuntimeFunction::MultiplyFloat => true,
                RuntimeFunction::DivideFloat => true,
                RuntimeFunction::PowerFloat => true,
                RuntimeFunction::FloorFloat => true,
                RuntimeFunction::CeilFloat => true,
                RuntimeFunction::SqrtFloat => true,
                RuntimeFunction::AddDouble => true,
                RuntimeFunction::SubtractDouble => true,
                RuntimeFunction::MultiplyDouble => true,
                RuntimeFunction::DivideDouble => true,
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
                RuntimeFunction::MakeMutable => false,
                RuntimeFunction::GetMutable => false,
                RuntimeFunction::SetMutable => false,
                RuntimeFunction::MakeList => true,
                RuntimeFunction::ListFirst => true,
                RuntimeFunction::ListLast => true,
                RuntimeFunction::ListInitial => true,
                RuntimeFunction::ListTail => true,
                RuntimeFunction::ListNth => true,
                RuntimeFunction::ListAppend => true,
                RuntimeFunction::ListPrepend => true,
                RuntimeFunction::ListInsert => true,
                RuntimeFunction::ListRemove => true,
            }
        }
    }
}
