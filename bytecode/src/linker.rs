use crate::*;

pub trait Input<'a> {
    type Error: std::error::Error;

    fn get_file(&mut self, index: usize) -> Result<module::File<'a>, Self::Error>;
}

#[derive(Debug)]
pub enum Error<'a, I: Input<'a>> {
    Input(I::Error),
    InvalidReference,
    NoEntrypoint,
    MultipleEntrypoints { first: usize, second: usize },
}

impl<'a, I: Input<'a>> std::fmt::Display for Error<'a, I> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::Input(err) => write!(f, "Input error: {}", err),
            Error::InvalidReference => f.write_str("Invalid reference"),
            Error::NoEntrypoint => f.write_str("No entrypoint"),
            Error::MultipleEntrypoints { first, second } => {
                write!(f, "Multiple entrypoints: {} and {}", first, second)
            }
        }
    }
}

impl<'a, I: Input<'a> + std::fmt::Debug> std::error::Error for Error<'a, I> {}

#[derive(Debug)]
pub struct SingleInput<'a> {
    file: Option<module::File<'a>>,
}

impl<'a> Input<'a> for SingleInput<'a> {
    type Error = SingleInputError;

    fn get_file(&mut self, _: usize) -> Result<module::File<'a>, Self::Error> {
        Ok(mem::take(&mut self.file).unwrap())
    }
}

#[derive(Debug)]
pub struct SingleInputError;

impl std::fmt::Display for SingleInputError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("Standalone files may not have dependencies")
    }
}

impl std::error::Error for SingleInputError {}

pub fn link_single(module: module::File) -> Result<object::File, Error<SingleInput>> {
    link(SingleInput { file: Some(module) })
}

pub fn link<'a, I: Input<'a>>(input: I) -> Result<object::File<'a>, Error<'a, I>> {
    fn resolve_file_reference<'a, I: Input<'a>>(
        reference: &module::FileReference,
        file: LinkedFile,
        ctx: &RefCell<Context<'a, I>>,
    ) -> Result<LinkedFile, Error<'a, I>> {
        match reference {
            module::FileReference::Current => Ok(file),
            module::FileReference::Dependency(index) => {
                let file = link_file(*index, ctx)?;
                ctx.borrow_mut().cache.insert(*index, file);
                Ok(file)
            }
        }
    }

    fn resolve_block_reference<'a, I: Input<'a>>(
        reference: &module::BlockReference<'a>,
        file: LinkedFile,
        ctx: &RefCell<Context<'a, I>>,
    ) -> Result<object::BlockReference<'a>, Error<'a, I>> {
        match reference {
            module::BlockReference::External { namespace, name } => {
                Ok(object::BlockReference::External {
                    namespace: namespace.clone(),
                    name: name.clone(),
                })
            }
            module::BlockReference::File(file_ref, index) => {
                let file = resolve_file_reference(file_ref, file, ctx)?;
                ctx.borrow()
                    .resolve_block(file, *index)
                    .map(object::BlockReference::Block)
                    .ok_or(Error::InvalidReference)
            }
        }
    }

    fn resolve_value_reference<'a, I: Input<'a>>(
        reference: &module::ValueReference,
        file: LinkedFile,
        ctx: &RefCell<Context<'a, I>>,
    ) -> Result<object::ValueReference, Error<'a, I>> {
        match reference {
            module::ValueReference::Constant(file_ref, index) => {
                let file = resolve_file_reference(file_ref, file, ctx)?;
                ctx.borrow()
                    .resolve_constant(file, *index)
                    .map(object::ValueReference::Constant)
                    .ok_or(Error::InvalidReference)
            }
            module::ValueReference::Variable(index) => Ok(object::ValueReference::Variable(*index)),
        }
    }

    fn link_file<'a, I: Input<'a>>(
        index: usize,
        ctx: &RefCell<Context<'a, I>>,
    ) -> Result<LinkedFile, Error<'a, I>> {
        if let Some(file) = ctx.borrow().cache.get(&index).copied() {
            return Ok(file);
        }

        let file = LinkedFile {
            constant_offset: ctx.borrow().constants.len(),
            block_offset: ctx.borrow().blocks.len(),
        };

        let mut module = ctx
            .borrow_mut()
            .input
            .get_file(index)
            .map_err(Error::Input)?;

        let mut blocks = module
            .blocks
            .iter()
            .map(|block| {
                block
                    .iter()
                    .map(|instruction| match instruction {
                        module::Instruction::Enter(block, inputs) => {
                            Ok(object::Instruction::Enter(
                                resolve_block_reference(block, file, ctx)?,
                                inputs
                                    .iter()
                                    .map(|i| resolve_value_reference(i, file, ctx))
                                    .collect::<Result<_, _>>()?,
                            ))
                        }
                        module::Instruction::Exit(outputs) => outputs
                            .iter()
                            .map(|i| resolve_value_reference(i, file, ctx))
                            .collect::<Result<_, _>>()
                            .map(object::Instruction::Exit),
                    })
                    .collect()
            })
            .collect::<Result<_, _>>()?;

        let mut ctx = ctx.borrow_mut();

        ctx.constants.append(&mut module.constants);
        ctx.blocks.append(&mut blocks);

        if let Some(first) = ctx.entrypoint {
            if let Some(second) = module.entrypoint {
                return Err(Error::MultipleEntrypoints { first, second });
            }
        }

        ctx.entrypoint = module.entrypoint;

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

struct Context<'a, I: Input<'a>> {
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

impl<'a, I: Input<'a>> Context<'a, I> {
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
