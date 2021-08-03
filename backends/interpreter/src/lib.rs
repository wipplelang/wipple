use std::{borrow::Cow, collections::HashMap, mem};
use wipple_bytecode::{constant::Constant, object::*};

pub type Value<'a> = Constant<'a>;

#[derive(Default)]
pub struct Interpreter {
    pub builtins: Namespace,
    pub namespaces: HashMap<String, Namespace>,
    pub trace: bool,
}

impl Interpreter {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn trace(mut self, trace: bool) -> Self {
        self.trace = trace;
        self
    }

    pub fn builtins(mut self, namespace: Namespace) -> Self {
        self.builtins = namespace;
        self
    }

    pub fn namespace(mut self, name: impl ToString, namespace: Namespace) -> Self {
        self.namespaces.insert(name.to_string(), namespace);
        self
    }
}

pub type ExternFn = Box<
    dyn for<'a> Fn(Vec<Value<'a>>, &mut Context<'a>, &Interpreter) -> Result<'a, Vec<Value<'a>>>,
>;

#[derive(Default)]
pub struct Namespace {
    pub items: HashMap<String, ExternFn>,
}

impl Namespace {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with(
        mut self,
        name: impl ToString,
        func: impl for<'a> Fn(Vec<Value<'a>>, &mut Context<'a>, &Interpreter) -> Result<'a, Vec<Value<'a>>>
            + 'static,
    ) -> Self {
        self.items.insert(name.to_string(), Box::new(func));
        self
    }
}

pub struct Context<'a> {
    pub stack: Stack<'a>,
    pub trace: Vec<Trace<'a>>,
}

#[derive(Default)]
pub struct Stack<'a> {
    pub frames: Vec<StackFrame<'a>>,
}

impl<'a> Stack<'a> {
    pub fn new() -> Self {
        Stack::default()
    }

    pub fn push(&mut self, frame: StackFrame<'a>) {
        self.frames.push(frame)
    }

    pub fn pop(&mut self) {
        self.frames.pop().expect("Stack is empty");
    }

    pub fn current(&self) -> &StackFrame<'a> {
        self.frames.last().expect("Stack is empty")
    }

    pub fn current_mut(&mut self) -> &mut StackFrame<'a> {
        self.frames.last_mut().expect("Stack is empty")
    }
}

pub type StackFrame<'a> = Vec<Value<'a>>;

#[derive(Debug, Clone)]
pub struct Error<'a> {
    pub message: String,
    pub trace: Vec<Trace<'a>>,
}

impl<'a> Error<'a> {
    pub fn new(message: impl ToString, trace: Vec<Trace<'a>>) -> Self {
        Error {
            message: message.to_string(),
            trace,
        }
    }
}

impl std::fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // TODO: Custom format
        write!(f, "{:#?}", self)
    }
}

impl std::error::Error for Error<'_> {}

pub type Result<'a, T> = core::result::Result<T, Error<'a>>;

#[derive(Debug, Clone)]
pub enum Trace<'a> {
    Block {
        reference: BlockReference<'a>,
        instruction: usize,
    },
    Extern {
        namespace: Option<Cow<'a, str>>,
        name: Cow<'a, str>,
    },
}

impl Interpreter {
    pub fn execute<'a>(&self, file: File<'a>) -> Result<'a, ()> {
        let mut ctx = Context {
            stack: Stack::new(),
            trace: Vec::new(),
        };

        let block = &file.blocks[file.entrypoint];

        self.execute_block(
            &BlockReference::Block(file.entrypoint),
            block,
            Vec::new(),
            &file,
            &mut ctx,
        )?;

        Ok(())
    }

    pub fn execute_block<'a>(
        &self,
        reference: &BlockReference<'a>,
        block: &[Instruction<'a>],
        inputs: Vec<Value<'a>>,
        file: &File<'a>,
        ctx: &mut Context<'a>,
    ) -> Result<'a, Vec<Value<'a>>> {
        ctx.stack.push(inputs);

        if self.trace {
            ctx.trace.push(Trace::Block {
                reference: reference.clone(),
                instruction: 0,
            });
        }

        let outputs = 'outer: loop {
            for instruction in block {
                if let Some(outputs) = self.execute_instruction(instruction, file, ctx)? {
                    break 'outer outputs;
                }

                match ctx.trace.last_mut().unwrap() {
                    Trace::Block { instruction, .. } => *instruction += 1,
                    _ => unreachable!(),
                }
            }
        };

        ctx.stack.pop();
        ctx.trace.pop();

        Ok(outputs)
    }

    pub fn execute_instruction<'a>(
        &self,
        instruction: &Instruction<'a>,
        file: &File<'a>,
        ctx: &mut Context<'a>,
    ) -> Result<'a, Option<Vec<Value<'a>>>> {
        match instruction {
            Instruction::Enter(reference, inputs) => {
                match reference {
                    BlockReference::Block(index) => {
                        let block = &file.blocks[*index];

                        let inputs = ctx.copy(inputs, file);

                        let mut outputs =
                            self.execute_block(reference, block, inputs, file, ctx)?;

                        ctx.stack.current_mut().append(&mut outputs);
                    }
                    BlockReference::External { namespace, name } => {
                        if self.trace {
                            ctx.trace.push(Trace::Extern {
                                namespace: namespace.clone(),
                                name: name.clone(),
                            });
                        }

                        let namespace = match namespace {
                            Some(n) => self.namespaces.get(n.as_ref()).ok_or_else(|| {
                                Error::new("Invalid namespace", mem::take(&mut ctx.trace))
                            })?,
                            None => &self.builtins,
                        };

                        let func = namespace
                            .items
                            .get(name.as_ref())
                            .ok_or_else(|| Error::new("Invalid name", mem::take(&mut ctx.trace)))?;

                        let inputs = ctx.copy(inputs, file);
                        let mut outputs = func(inputs, ctx, self)?;
                        ctx.stack.current_mut().append(&mut outputs);
                    }
                }

                Ok(None)
            }
            Instruction::Exit(outputs) => {
                let outputs = ctx.copy(outputs, file);
                Ok(Some(outputs))
            }
        }
    }
}

impl<'a> Context<'a> {
    fn resolve(&self, reference: &ValueReference, file: &File<'a>) -> Value<'a> {
        match reference {
            ValueReference::Variable(index) => self.stack.current()[*index].clone(),
            ValueReference::Constant(index) => file.constants[*index].clone(),
        }
    }

    fn copy(&self, references: &[ValueReference], file: &File<'a>) -> Vec<Value<'a>> {
        references.iter().map(|r| self.resolve(r, file)).collect()
    }
}
