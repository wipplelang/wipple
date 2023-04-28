use super::{BasicBlock, Expression, LabelKind, Program, Statement, Terminator, Type};
use crate::{Compiler, Optimize};
use std::{collections::BTreeMap, mem};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Options {
    /// Options for propagating constants. `None` disables constant propagation.
    pub propagate: Option<propagate::Options>,

    /// Options for reordering basic blocks. `None` disables reordering.
    pub reorder: Option<reorder::Options>,
}

impl Default for Options {
    fn default() -> Self {
        Options {
            propagate: Some(Default::default()),
            reorder: Some(Default::default()),
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

        passes!(program, [propagate, reorder])
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
                            if matches!(block.terminator.unwrap(), Terminator::Return) {
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

pub mod reorder {
    use super::*;

    #[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
    pub struct Options {}

    impl Program {
        pub(super) fn reorder(mut self, _options: Options, _compiler: &Compiler) -> Program {
            self.labels = self
                .labels
                .into_iter()
                .map(|(kind, vars, blocks)| {
                    let mut blocks = blocks.into_iter().map(Some).collect::<Vec<_>>();

                    fn gen(
                        block_index: usize,
                        blocks: &mut Vec<Option<BasicBlock>>,
                        reordered: &mut BTreeMap<usize, BasicBlock>,
                        indices: &mut BTreeMap<usize, usize>,
                    ) -> Option<usize> {
                        if let Some(&index) = indices.get(&block_index) {
                            return Some(index);
                        }

                        let mut block = blocks.get_mut(block_index).unwrap().take().unwrap();

                        *block.terminator.as_mut().unwrap() = match block.terminator.unwrap() {
                            Terminator::Unreachable => return None,
                            Terminator::Return => Terminator::Return,
                            Terminator::Jump(jump_index) => {
                                let jump_index = gen(jump_index, blocks, reordered, indices)?;
                                Terminator::Jump(jump_index)
                            }
                            Terminator::If(variant, then_index, else_index) => {
                                let then_index = gen(then_index, blocks, reordered, indices);
                                let else_index = gen(else_index, blocks, reordered, indices);

                                match (then_index, else_index) {
                                    (Some(then_index), Some(else_index)) => {
                                        Terminator::If(variant, then_index, else_index)
                                    }
                                    (Some(jump_index), None) | (None, Some(jump_index)) => {
                                        block.statements.push(Statement::Drop);
                                        Terminator::Jump(jump_index)
                                    }
                                    (None, None) => return None,
                                }
                            }
                            Terminator::TailCall => Terminator::TailCall,
                        };

                        let index = indices.len();
                        indices.insert(block_index, index);

                        reordered.insert(index, block);

                        assert!(reordered.len() == indices.len());

                        Some(index)
                    }

                    let mut reordered = BTreeMap::new();
                    let mut indices = BTreeMap::new();
                    gen(0, &mut blocks, &mut reordered, &mut indices);

                    let entrypoint = *indices.get(&0).unwrap();

                    fn sort(
                        block_index: usize,
                        blocks: &mut Vec<Option<BasicBlock>>,
                        sorted: &mut BTreeMap<usize, BasicBlock>,
                        indices: &mut BTreeMap<usize, usize>,
                    ) -> usize {
                        if let Some(&index) = indices.get(&block_index) {
                            return index;
                        }

                        let mut block = blocks.get_mut(block_index).unwrap().take().unwrap();

                        let index = indices.len();
                        indices.insert(block_index, index);

                        *block.terminator.as_mut().unwrap() = match block.terminator.unwrap() {
                            Terminator::Unreachable => unreachable!(),
                            Terminator::Return => Terminator::Return,
                            Terminator::Jump(jump_index) => {
                                let jump_index = sort(jump_index, blocks, sorted, indices);
                                Terminator::Jump(jump_index)
                            }
                            Terminator::If(variant, then_index, else_index) => {
                                let then_index = sort(then_index, blocks, sorted, indices);
                                let else_index = sort(else_index, blocks, sorted, indices);
                                Terminator::If(variant, then_index, else_index)
                            }
                            Terminator::TailCall => Terminator::TailCall,
                        };

                        sorted.insert(index, block);

                        index
                    }

                    let mut blocks = reordered.into_values().map(Some).collect::<Vec<_>>();

                    let mut sorted = BTreeMap::new();
                    let mut indices = BTreeMap::new();
                    sort(entrypoint, &mut blocks, &mut sorted, &mut indices);

                    let blocks = sorted.into_values().collect::<Vec<_>>();

                    (kind, vars, blocks)
                })
                .collect();

            self
        }
    }
}
