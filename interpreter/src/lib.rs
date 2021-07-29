use std::mem::ManuallyDrop;
use wipple_bytecode::{self as bytecode, binary::Index};

pub struct Interpreter<R: ExternResolver> {
    pub resolver: R,
}

impl<R: ExternResolver> Interpreter<R> {
    pub fn new(resolver: R) -> Self {
        Interpreter { resolver }
    }
}

pub struct Context<'a> {
    pub file: &'a bytecode::BinFile<'a>,
    pub stack: Stack,
    pub trace: Vec<Trace>,
}

#[derive(Default)]
pub struct Stack {
    pub frames: Vec<StackFrame>,
}

impl Stack {
    pub fn new() -> Self {
        Stack::default()
    }

    pub fn push(&mut self, frame: StackFrame) {
        self.frames.push(frame)
    }

    pub fn pop(&mut self) {
        self.frames.pop().expect("Stack is empty");
    }

    pub fn current(&self) -> &StackFrame {
        self.frames.last().expect("Stack is empty")
    }

    pub fn current_mut(&mut self) -> &mut StackFrame {
        self.frames.last_mut().expect("Stack is empty")
    }
}

pub type StackFrame = Vec<Value>;

#[derive(Debug, Clone)]
pub enum Value {
    Thunk(Thunk),
    Reference(Reference),
}

impl Value {
    pub fn thunk(&self) -> Option<&Thunk> {
        match self {
            Value::Thunk(thunk) => Some(thunk),
            _ => None,
        }
    }

    pub fn reference(&self) -> Option<&Reference> {
        match self {
            Value::Reference(reference) => Some(reference),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Thunk {
    pub index: usize,
    pub inputs: Vec<Value>,
}

#[derive(Debug)]
pub struct Reference {
    references: *mut usize,
    ptr: *const (),
}

impl Reference {
    pub fn from_data(data: Vec<u8>) -> Self {
        let data = ManuallyDrop::new(data);
        let ptr = data.as_ptr() as *const ();
        unsafe { Reference::new(ptr) }
    }

    /// Create a managed value from a pointer.
    ///
    /// # Safety
    /// This function takes ownership of the memory behind the pointer, so the
    /// pointer must be valid.
    pub unsafe fn new(ptr: *const ()) -> Self {
        Reference {
            references: Box::leak(Box::new(1)),
            ptr,
        }
    }

    pub fn as_ptr(&self) -> *const () {
        self.ptr
    }
}

impl Clone for Reference {
    fn clone(&self) -> Self {
        unsafe {
            *self.references += 1;

            Reference {
                references: self.references,
                ptr: self.ptr,
            }
        }
    }
}

impl Drop for Reference {
    fn drop(&mut self) {
        unsafe {
            *self.references -= 1;

            if *self.references == 0 {
                drop(Box::from_raw(self.ptr as *mut ()))
            }
        }
    }
}

pub type ExternFn = Box<dyn Fn(&[Index], &mut Context) -> Result<()>>;

pub trait ExternResolver {
    fn resolve(&self, r#extern: &bytecode::binary::Extern) -> Option<&ExternFn>;
}

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
    pub trace: Vec<Trace>,
}

impl Error {
    pub fn new(message: impl ToString, trace: &[Trace]) -> Self {
        Error {
            message: message.to_string(),
            trace: trace.to_vec(),
        }
    }
}

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub enum Trace {
    Block { index: Index, instruction: usize },
    Extern(Index),
}

impl Trace {
    fn instruction_mut(&mut self) -> Option<&mut usize> {
        match self {
            Trace::Block { instruction, .. } => Some(instruction),
            Trace::Extern(_) => None,
        }
    }
}

impl<R: ExternResolver> Interpreter<R> {
    pub fn execute(&self, file: &bytecode::BinFile) -> Result<()> {
        let index = file.blocks.len() - 1;

        let block = file
            .blocks
            .last()
            .ok_or_else(|| Error::new("Program must contain as least one block", &[]))?;

        let mut context = Context {
            file,
            stack: Stack::new(),
            trace: Vec::new(),
        };

        let outputs = self.execute_block(index, block, Vec::new(), &mut context)?;

        if !outputs.is_empty() {
            return Err(Error::new(
                "Top-level block must not return any output",
                &[],
            ));
        }

        Ok(())
    }

    #[allow(clippy::ptr_arg)]
    fn execute_block<'a>(
        &self,
        index: Index,
        block: &'a bytecode::binary::Block,
        inputs: Vec<Value>,
        context: &mut Context<'a>,
    ) -> Result<Vec<Value>> {
        context.stack.push(inputs);

        context.trace.push(Trace::Block {
            index,
            instruction: 0,
        });

        let outputs = 'outer: loop {
            for instruction in block {
                if let Some(outputs) = self.execute_instruction(instruction, context)? {
                    break 'outer outputs;
                }

                *context.trace.last_mut().unwrap().instruction_mut().unwrap() += 1;
            }
        };

        context.stack.pop();
        context.trace.pop();

        Ok(outputs)
    }

    fn execute_instruction<'a>(
        &self,
        instruction: &bytecode::binary::Instruction,
        context: &mut Context<'a>,
    ) -> Result<Option<Vec<Value>>> {
        use bytecode::binary::Instruction;

        match instruction {
            Instruction::Enter(index, additional_inputs) => {
                let value = context.get(*index)?;

                let thunk = match value {
                    Value::Thunk(thunk) => thunk,
                    _ => return Err(Error::new("Expected thunk", &context.trace)),
                };

                let block = &context.file.blocks[thunk.index];

                let mut inputs = thunk.inputs.clone();
                inputs.append(&mut context.copy(additional_inputs)?);

                let mut outputs = self.execute_block(thunk.index, block, inputs, context)?;
                context.stack.current_mut().append(&mut outputs);
            }
            Instruction::Exit(outputs) => {
                let outputs = context.copy(outputs)?;
                return Ok(Some(outputs));
            }
            Instruction::Call(index, inputs) => {
                context.trace.push(Trace::Extern(*index));

                let r#extern = context.file.externs.get(*index).ok_or_else(|| {
                    Error::new(
                        format!("No external reference exists at index {}", index),
                        &context.trace,
                    )
                })?;

                let func = self.resolver.resolve(r#extern).ok_or_else(|| {
                    Error::new("Failed to resolve external reference", &context.trace)
                })?;

                func(inputs, context)?;

                context.trace.pop();
            }
            Instruction::Use(index) => {
                let data = context.file.globals.get(*index).ok_or_else(|| {
                    Error::new(
                        format!("No global data exists at index {}", index),
                        &context.trace,
                    )
                })?;

                let value = Value::Reference(Reference::from_data(data.to_vec()));
                context.stack.current_mut().push(value);
            }
            Instruction::Thunk(index, inputs) => {
                if context.file.blocks.get(*index).is_none() {
                    return Err(Error::new(
                        format!("No block exists at index {}", index),
                        &context.trace,
                    ));
                }

                let inputs = context.copy(inputs)?;

                let value = Value::Thunk(Thunk {
                    index: *index,
                    inputs,
                });

                context.stack.current_mut().push(value);
            }
        }

        Ok(None)
    }
}

impl<'a> Context<'a> {
    pub fn get(&self, index: usize) -> Result<&Value> {
        self.stack
            .current()
            .get(index)
            .ok_or_else(|| Error::new(format!("No data exists at index {}", index), &self.trace))
    }

    fn copy(&self, indices: &[Index]) -> Result<Vec<Value>> {
        let mut values = Vec::with_capacity(indices.len());

        for index in indices {
            let value = self.get(*index)?;
            values.push(value.clone());
        }

        Ok(values)
    }
}
