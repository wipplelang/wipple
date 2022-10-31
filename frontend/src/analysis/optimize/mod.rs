use crate::{
    analysis::{Arm, Expression, ExpressionKind, Pattern, PatternKind, Program, Type},
    Compiler, Span, VariableId,
};
use std::{
    collections::{HashMap, HashSet},
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

impl Compiler<'_> {
    pub fn optimize(&self, program: Program, options: Options) -> Program {
        macro_rules! passes {
            ($ir:expr, [$($pass:ident),*]) => {
                {
                    $(
                        let program = match options.$pass {
                            Some(options) => self.$pass(program, options),
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

    impl Compiler<'_> {
        pub(super) fn ssa(&self, mut program: Program, _options: Options) -> Program {
            for expr in program.items.values_mut() {
                expr.traverse_mut(|expr| {
                    if let ExpressionKind::Block(exprs) = &mut expr.kind {
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
                                                    kind: ExpressionKind::Block(convert_block(
                                                        remaining,
                                                        remaining.first().map_or(span, |expr| {
                                                            expr.span.with_end(
                                                                remaining.last().unwrap().span.end,
                                                            )
                                                        }),
                                                    )),
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

            program
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

    impl Compiler<'_> {
        pub(super) fn propagate(&self, mut program: Program, options: Options) -> Program {
            let mut items = program
                .items
                .into_iter()
                .map(|(item, expr)| (item, Some(expr)))
                .collect::<HashMap<_, _>>();

            let item_ids = items.keys().cloned().collect::<Vec<_>>();

            let mut passes: usize = 1;
            loop {
                let mut propagated = false;

                for &item in &item_ids {
                    let mut expr = mem::take(items.get_mut(&item).unwrap()).unwrap();

                    expr.traverse_mut(|expr| {
                        let constant = match &expr.kind {
                            ExpressionKind::Constant(item) => *item,
                            _ => return,
                        };

                        let body = match items.get(&constant).unwrap() {
                            Some(expr) => expr,
                            None => return, // don't propagate recursive constants
                        };

                        if body.is_simple() {
                            let mut recursive = false;
                            body.traverse(|expr| {
                                if let ExpressionKind::Constant(c) = expr.kind {
                                    if c == constant {
                                        recursive = true;
                                    }
                                }
                            });

                            if !recursive {
                                propagated = true;
                                *expr = body.clone();
                            }
                        }
                    });

                    items.get_mut(&item).unwrap().replace(expr);
                }

                if !propagated || options.pass_limit.map_or(false, |limit| passes > limit) {
                    break;
                }

                passes += 1;
            }

            program.items = items
                .into_iter()
                .map(|(item, expr)| (item, expr.unwrap()))
                .collect();

            program
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

    impl Compiler<'_> {
        pub(super) fn inline(&self, mut program: Program, options: Options) -> Program {
            let mut passes: usize = 1;
            loop {
                let mut inlined = false;

                for expr in program.items.values_mut() {
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

                                if let PatternKind::Variable(input_var) = pattern.kind {
                                    if input.is_simple() {
                                        let input = input.as_ref().clone();

                                        *expr = body.as_ref().clone();

                                        expr.traverse_mut(|expr| match &mut expr.kind {
                                            ExpressionKind::Variable(var) => {
                                                if *var == input_var {
                                                    *expr = input.clone();
                                                }
                                            }
                                            ExpressionKind::Function(_, _, captures) => {
                                                *captures = mem::take(captures)
                                                    .into_iter()
                                                    .filter(|(var, _)| *var != input_var)
                                                    .collect();
                                            }
                                            _ => {}
                                        });

                                        return;
                                    }
                                }

                                expr.kind = ExpressionKind::When(
                                    input.clone(),
                                    vec![Arm {
                                        span: input.span,
                                        pattern: pattern.clone(),
                                        body: body.as_ref().clone(),
                                    }],
                                );
                            }
                            _ => {}
                        }
                    });
                }

                if !inlined || options.pass_limit.map_or(false, |limit| passes > limit) {
                    break;
                }

                passes += 1;
            }

            program
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

    impl Compiler<'_> {
        pub(super) fn unused(&self, mut program: Program, options: Options) -> Program {
            let entrypoint = match program.entrypoint {
                Some(item) => item,
                None => return program,
            };

            if options.constants {
                let mut used = HashSet::from([entrypoint]);

                for expr in program.items.values() {
                    expr.traverse(|expr| {
                        if let ExpressionKind::Constant(item) = &expr.kind {
                            used.insert(*item);
                        }
                    })
                }

                for item in program.items.keys().cloned().collect::<Vec<_>>() {
                    if !used.contains(&item) {
                        program.items.remove(&item);
                    }
                }
            }

            program
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

        pub fn is_simple(&self) -> bool {
            match &self.kind {
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
                | ExpressionKind::Variable(_)
                | ExpressionKind::Constant(_) => true,
                ExpressionKind::Block(exprs) => exprs.iter().all(Expression::is_simple),
                ExpressionKind::Call(func, input) => func.is_simple() && input.is_simple(),
                ExpressionKind::When(expr, arms) => {
                    expr.is_simple() && arms.iter().map(|arm| &arm.body).all(Expression::is_simple)
                }
                ExpressionKind::External(_, _, inputs) => inputs.iter().all(Expression::is_simple),
                ExpressionKind::Structure(exprs) => exprs.iter().all(Expression::is_simple),
                ExpressionKind::Variant(_, exprs) => exprs.iter().all(Expression::is_simple),
                ExpressionKind::Tuple(exprs) => exprs.iter().all(Expression::is_simple),
                ExpressionKind::Initialize(_, _) => false,
            }
        }
    }

    impl Pattern {
        pub fn variables(&self) -> HashSet<VariableId> {
            fn collect_variables(pattern: &Pattern, variables: &mut HashSet<VariableId>) {
                match &pattern.kind {
                    PatternKind::Variable(var) => {
                        variables.insert(*var);
                    }
                    PatternKind::Or(left, right) => {
                        collect_variables(left, variables);
                        collect_variables(right, variables);
                    }
                    PatternKind::Where(pattern, _) => {
                        collect_variables(pattern, variables);
                    }
                    PatternKind::Tuple(patterns) => {
                        for pattern in patterns {
                            collect_variables(pattern, variables);
                        }
                    }
                    PatternKind::Destructure(fields) => {
                        for pattern in fields.values() {
                            collect_variables(pattern, variables);
                        }
                    }
                    PatternKind::Variant(_, patterns) => {
                        for pattern in patterns {
                            collect_variables(pattern, variables);
                        }
                    }
                    PatternKind::Wildcard
                    | PatternKind::Text(_)
                    | PatternKind::Number(_)
                    | PatternKind::Integer(_)
                    | PatternKind::Natural(_)
                    | PatternKind::Byte(_)
                    | PatternKind::Signed(_)
                    | PatternKind::Unsigned(_)
                    | PatternKind::Float(_)
                    | PatternKind::Double(_) => {}
                }
            }

            let mut variables = HashSet::new();
            collect_variables(self, &mut variables);
            variables
        }
    }
}
