use super::{Expression, LabelKind, Program, Statement, Terminator, Type};
use crate::{Compiler, Optimize};
use std::{collections::BTreeMap, mem};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Options {
    /// Options for propagating constants. `None` disables constant propagation.
    pub propagate: Option<propagate::Options>,
}

impl Default for Options {
    fn default() -> Self {
        Options {
            propagate: Some(Default::default()),
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

        passes!(program, [propagate])
    }
}

pub mod propagate {
    use super::*;

    #[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
    pub struct Options {}

    impl Program {
        pub(super) fn propagate(mut self, _options: Options, _compiler: &Compiler) -> Program {
            // Eliminate the overhead of initializing function constants of the form:
            //   constant #0 (0 vars):
            //     bb0:
            //       function #1
            //       return

            let mut functions = BTreeMap::new();
            for (label, (kind, vars, blocks)) in self.labels.iter_mut().enumerate() {
                if matches!(kind, LabelKind::Constant(_)) && *vars == 0 && blocks.len() == 1 {
                    let block = blocks.first().unwrap();
                    if block.statements.len() == 1 {
                        if let Statement::Expression(_, Expression::Function(func)) =
                            block.statements.first().unwrap()
                        {
                            if matches!(block.terminator, Terminator::Return) {
                                functions.insert(label, *func);
                            }
                        }
                    }
                }
            }

            for (_, _, blocks) in self.labels.iter_mut() {
                for block in blocks {
                    for statement in &mut block.statements {
                        if let Statement::Expression(ty, Expression::Constant(constant)) = statement
                        {
                            if let Some(func) = functions.get(constant) {
                                *statement = Statement::Expression(
                                    mem::replace(ty, Type::Marker),
                                    Expression::Function(*func),
                                );
                            }
                        }
                    }
                }
            }

            // NOTE: We can't remove the propagated constants because all the indices would change.
            // In the future, we can rewrite the indices too.

            self
        }
    }
}
