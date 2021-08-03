pub mod bytecode;
pub mod constant;
pub mod object;

use serde::{Deserialize, Serialize};
use std::{cell::RefCell, collections::HashMap, mem};

pub type Block<BlockReference, ValueReference> = Vec<Instruction<BlockReference, ValueReference>>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Instruction<BlockReference, ValueReference> {
    Enter(BlockReference, Vec<ValueReference>),
    Exit(Vec<ValueReference>),
}

pub trait Input {
    type Error: std::error::Error;

    fn get_file<'a>(&self, index: usize) -> Result<bytecode::File<'a>, Self::Error>;
}

pub enum Error<I: Input> {
    Input(I::Error),
    InvalidReference,
    NoEntrypoint,
    MultipleEntrypoints { first: usize, second: usize },
}

pub fn link<'a, I: Input>(input: I) -> Result<object::File<'a>, Error<I>> {
    fn resolve_file_reference<I: Input>(
        reference: &bytecode::FileReference,
        file: LinkedFile,
        ctx: &RefCell<Context<I>>,
    ) -> Result<LinkedFile, Error<I>> {
        match reference {
            bytecode::FileReference::Current => Ok(file),
            bytecode::FileReference::Dependency(index) => {
                let file = link_file(*index, ctx)?;
                ctx.borrow_mut().cache.insert(*index, file);
                Ok(file)
            }
        }
    }

    fn resolve_block_reference<'a, I: Input>(
        reference: &bytecode::BlockReference<'a>,
        file: LinkedFile,
        ctx: &RefCell<Context<'a, I>>,
    ) -> Result<object::BlockReference<'a>, Error<I>> {
        match reference {
            bytecode::BlockReference::External { namespace, name } => {
                Ok(object::BlockReference::External {
                    namespace: namespace.clone(),
                    name: name.clone(),
                })
            }
            bytecode::BlockReference::File(file_ref, index) => {
                let file = resolve_file_reference(file_ref, file, ctx)?;
                ctx.borrow()
                    .resolve_block(file, *index)
                    .map(object::BlockReference::Block)
                    .ok_or(Error::InvalidReference)
            }
        }
    }

    fn resolve_value_reference<I: Input>(
        reference: &bytecode::ValueReference,
        file: LinkedFile,
        ctx: &RefCell<Context<I>>,
    ) -> Result<object::ValueReference, Error<I>> {
        match reference {
            bytecode::ValueReference::Constant(file_ref, index) => {
                let file = resolve_file_reference(file_ref, file, ctx)?;
                ctx.borrow()
                    .resolve_constant(file, *index)
                    .map(object::ValueReference::Constant)
                    .ok_or(Error::InvalidReference)
            }
            bytecode::ValueReference::Variable(index) => {
                Ok(object::ValueReference::Variable(*index))
            }
        }
    }

    fn link_file<I: Input>(
        index: usize,
        ctx: &RefCell<Context<I>>,
    ) -> Result<LinkedFile, Error<I>> {
        if let Some(file) = ctx.borrow().cache.get(&index).copied() {
            return Ok(file);
        }

        let file = LinkedFile {
            constant_offset: ctx.borrow().constants.len(),
            block_offset: ctx.borrow().blocks.len(),
        };

        let mut bytecode = ctx.borrow().input.get_file(index).map_err(Error::Input)?;

        let mut blocks = bytecode
            .blocks
            .iter()
            .map(|block| {
                block
                    .iter()
                    .map(|instruction| match instruction {
                        bytecode::Instruction::Enter(block, inputs) => {
                            Ok(object::Instruction::Enter(
                                resolve_block_reference(block, file, ctx)?,
                                inputs
                                    .iter()
                                    .map(|i| resolve_value_reference(i, file, ctx))
                                    .collect::<Result<_, _>>()?,
                            ))
                        }
                        bytecode::Instruction::Exit(outputs) => outputs
                            .iter()
                            .map(|i| resolve_value_reference(i, file, ctx))
                            .collect::<Result<_, _>>()
                            .map(object::Instruction::Exit),
                    })
                    .collect()
            })
            .collect::<Result<_, _>>()?;

        let mut ctx = ctx.borrow_mut();

        ctx.constants.append(&mut bytecode.constants);
        ctx.blocks.append(&mut blocks);

        if let Some(first) = ctx.entrypoint {
            if let Some(second) = bytecode.entrypoint {
                return Err(Error::MultipleEntrypoints { first, second });
            }
        }

        ctx.entrypoint = bytecode.entrypoint;

        Ok(file)
    }

    let ctx = RefCell::new(Context::new(input));
    link_file(0, &ctx)?;

    let mut ctx = ctx.borrow_mut();
    let blocks = mem::take(&mut ctx.blocks);
    let constants = mem::take(&mut ctx.constants);
    let entrypoint = ctx.entrypoint.ok_or(Error::NoEntrypoint)?;

    Ok(object::File {
        blocks,
        constants,
        entrypoint,
    })
}

struct Context<'a, I: Input> {
    input: I,
    constants: Vec<constant::Constant<'a>>,
    blocks: Vec<object::Block<'a>>,
    entrypoint: Option<usize>,
    cache: HashMap<usize, LinkedFile>,
}

#[derive(Clone, Copy)]
struct LinkedFile {
    constant_offset: usize,
    block_offset: usize,
}

impl<I: Input> Context<'_, I> {
    fn new(input: I) -> Self {
        Context {
            input,
            constants: Default::default(),
            blocks: Default::default(),
            entrypoint: Default::default(),
            cache: Default::default(),
        }
    }

    fn resolve_constant(&self, file: LinkedFile, index: usize) -> Option<usize> {
        let index = file.constant_offset + index;
        self.constants.get(index).map(|_| index)
    }

    fn resolve_block(&self, file: LinkedFile, index: usize) -> Option<usize> {
        let index = file.block_offset + index;
        self.blocks.get(index).map(|_| index)
    }
}
