mod runtime;

use async_recursion::async_recursion;
use futures::future::BoxFuture;
use parking_lot::Mutex;
use std::{
    collections::BTreeMap,
    os::raw::{c_int, c_uint},
    sync::Arc,
};
use tokio::sync::mpsc::{Receiver, Sender};
use wipple_frontend::{helpers::Shared, ir, VariantIndex};

pub type Error = String;

#[allow(clippy::type_complexity)]
pub enum IoRequest<'a> {
    Display(Interpreter, &'a str, Box<dyn FnOnce() + Send>),
    Prompt(
        Interpreter,
        &'a str,
        Sender<String>,
        Receiver<bool>,
        Box<dyn FnOnce() + Send>,
    ),
    Choice(
        Interpreter,
        &'a str,
        Vec<&'a str>,
        Box<dyn FnOnce(usize) + Send>,
    ),
    Ui(
        Interpreter,
        &'a str,
        Box<
            dyn FnOnce(
                    Box<
                        dyn FnMut(
                                String,
                                Value,
                                Context,
                                Box<dyn FnOnce(Result<Value, Error>) -> Result<(), Error> + Send>,
                            ) + Send,
                    >,
                    Box<dyn FnOnce() + Send>,
                ) -> BoxFuture<'static, Result<(), Error>>
                + Send
                + Sync,
        >,
    ),
    Schedule(Interpreter, BoxFuture<'static, Result<(), Error>>),
    Sleep(Interpreter, std::time::Duration, Box<dyn FnOnce() + Send>),
}

#[allow(clippy::type_complexity)]
#[derive(Clone)]
pub struct UiHandle {
    on_message: Arc<
        Mutex<
            dyn FnMut(
                    String,
                    Value,
                    Context,
                    Box<dyn FnOnce(Result<Value, Error>) -> Result<(), Error> + Send>,
                ) + Send,
        >,
    >,
    on_finish: Arc<Mutex<Option<Box<dyn FnOnce() + Send>>>>,
}

#[allow(clippy::type_complexity)]
#[derive(Clone, Default)]
pub struct TaskGroup(Arc<Mutex<Vec<futures::channel::oneshot::Receiver<Result<(), Error>>>>>);

#[derive(Clone)]
pub struct Interpreter {
    inner: Arc<Mutex<InterpreterInner>>,
}

#[allow(clippy::type_complexity)]
struct InterpreterInner {
    io: Arc<dyn Fn(IoRequest) -> BoxFuture<Result<(), Error>> + Send + Sync>,
    labels: Vec<(usize, Vec<ir::BasicBlock>)>,
    initialized_constants: BTreeMap<usize, Value>,
}

impl Interpreter {
    pub fn new(
        io_handler: impl Fn(IoRequest) -> BoxFuture<Result<(), Error>> + Send + Sync + 'static,
    ) -> Self {
        Interpreter {
            inner: Arc::new(Mutex::new(InterpreterInner {
                io: Arc::new(io_handler),
                labels: Default::default(),
                initialized_constants: Default::default(),
            })),
        }
    }

    fn lock(&self) -> parking_lot::MutexGuard<InterpreterInner> {
        self.inner.lock()
    }
}

#[derive(Clone)]
pub enum Value {
    Marker,
    Number(rust_decimal::Decimal),
    Integer(i64),
    Natural(u64),
    Byte(u8),
    Signed(c_int),
    Unsigned(c_uint),
    Float(f32),
    Double(f64),
    Text(Arc<str>),
    Function(Scope, usize),
    NativeFunction(Arc<dyn Fn(Value) -> BoxFuture<'static, Result<Value, Error>> + Send + Sync>),
    Variant(VariantIndex, Vec<Value>),
    Mutable(Shared<Value>),
    List(im::Vector<Value>),
    Structure(Vec<Value>),
    Tuple(Vec<Value>),
    UiHandle(UiHandle),
    TaskGroup(TaskGroup),
}

#[derive(Clone, Default)]
pub struct Scope(Vec<Option<Value>>);

impl Scope {
    pub fn empty() -> Self {
        Default::default()
    }

    fn new(count: usize) -> Self {
        Scope(vec![None; count])
    }

    fn get(&self, var: usize) -> Value {
        self.0[var].clone().expect("uninitialized variable")
    }

    fn set(&mut self, var: usize, value: Value) {
        self.0[var] = Some(value);
    }

    fn free(&mut self, var: usize) {
        self.0[var] = None;
    }
}

#[derive(Clone, Default)]
pub struct Context(Arc<Mutex<Vec<im::HashMap<usize, Value>>>>);

impl Context {
    fn new() -> Self {
        Default::default()
    }

    fn get(&self, id: usize) -> Option<Value> {
        self.0
            .lock()
            .last()
            .and_then(|values| values.get(&id).cloned())
    }

    fn with(&self, id: usize, value: Value) {
        let mut inner = self.0.lock();
        let mut values = inner.last().cloned().unwrap_or_default();
        values.insert(id, value);
        inner.push(values);
    }

    fn reset(&self) {
        self.0.lock().pop().unwrap();
    }

    pub fn deep_clone(&self) -> Context {
        Context(Arc::new(Mutex::new(self.0.lock().clone())))
    }
}

#[derive(Default)]
struct Stack(Vec<Vec<Value>>);

impl Stack {
    fn new() -> Self {
        Stack(vec![Vec::new()])
    }

    fn current_frame(&self) -> &[Value] {
        self.0.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut Vec<Value> {
        self.0.last_mut().unwrap()
    }

    fn push_frame(&mut self) {
        self.0.push(Vec::new());
    }

    fn pop_frame(&mut self) {
        let value = self
            .0
            .pop()
            .expect("stack is empty")
            .pop()
            .expect("stack is empty");

        self.push(value);
    }

    fn push(&mut self, value: Value) {
        self.current_frame_mut().push(value);
    }

    fn copy(&mut self) {
        let value = self.current_frame().last().expect("stack is empty").clone();
        self.current_frame_mut().push(value);
    }

    fn pop(&mut self) -> Value {
        self.current_frame_mut().pop().expect("stack is empty")
    }

    fn popn(&mut self, n: usize) -> Vec<Value> {
        let mut values = vec![Value::Marker; n];

        for index in (0..n).rev() {
            values[index] = self.pop();
        }

        values
    }

    fn is_empty(&self) -> bool {
        self.current_frame().is_empty()
    }
}

impl Interpreter {
    pub async fn run(&self, program: &ir::Program) -> Result<(), Error> {
        self.lock().labels = program
            .labels
            .iter()
            .map(|(_, vars, blocks)| (*vars, blocks.clone()))
            .collect();

        self.lock().initialized_constants = BTreeMap::new();

        let mut stack = Stack::new();
        let mut context = Arc::new(Context::new());

        self.evaluate_label(program.entrypoint, &mut stack, &mut context)
            .await?;

        stack.pop();
        assert!(stack.is_empty());

        Ok(())
    }

    async fn evaluate_label(
        &self,
        label: usize,
        stack: &mut Stack,
        context: &Context,
    ) -> Result<(), Error> {
        self.evaluate_label_inner(label, stack, Scope::new(0), context)
            .await
    }

    async fn evaluate_label_in_scope(
        &self,
        label: usize,
        stack: &mut Stack,
        scope: Scope,
        mut context: &Context,
    ) -> Result<(), Error> {
        self.evaluate_label_inner(label, stack, scope, &mut context)
            .await
    }

    async fn evaluate_label_inner(
        &self,
        label: usize,
        stack: &mut Stack,
        mut scope: Scope,
        context: &Context,
    ) -> Result<(), Error> {
        let (vars, blocks) = self.lock().labels[label].clone();

        scope.0.reserve(vars);
        for _ in 0..vars {
            scope.0.push(None);
        }

        self.evaluate(blocks, stack, &mut scope, context).await
    }

    pub async fn call_function(
        &self,
        label: usize,
        scope: Scope,
        context: &Context,
        input: Value,
    ) -> Result<Value, Error> {
        let mut stack = Stack::new();
        stack.push(input);

        self.evaluate_label_in_scope(label, &mut stack, scope, context)
            .await?;

        Ok(stack.pop())
    }

    #[async_recursion]
    async fn evaluate(
        &self,
        blocks: Vec<ir::BasicBlock>,
        stack: &mut Stack,
        scope: &mut Scope,
        context: &Context,
    ) -> Result<(), Error> {
        let mut block = &blocks[0];

        loop {
            for statement in &block.statements {
                match statement {
                    ir::Statement::Copy => {
                        stack.copy();
                    }
                    ir::Statement::Drop => {
                        stack.pop();
                    }
                    ir::Statement::PushFrame => {
                        stack.push_frame();
                    }
                    ir::Statement::PopFrame => {
                        stack.pop_frame();
                    }
                    ir::Statement::Initialize(var) => {
                        scope.set(*var, stack.pop());
                    }
                    ir::Statement::Free(var) => {
                        scope.free(*var);
                    }
                    ir::Statement::WithContext(ctx) => {
                        context.with(*ctx, stack.pop());
                    }
                    ir::Statement::ResetContext => {
                        context.reset();
                    }
                    ir::Statement::Unpack(_) => {
                        // The interpreter provides the captured scope while calling the functions
                    }
                    ir::Statement::Expression(_, expr) => match expr {
                        ir::Expression::Marker => stack.push(Value::Marker),
                        ir::Expression::Text(text) => {
                            stack.push(Value::Text(Arc::from(text.as_str())))
                        }
                        ir::Expression::Number(number) => stack.push(Value::Number(*number)),
                        ir::Expression::Integer(integer) => stack.push(Value::Integer(*integer)),
                        ir::Expression::Natural(natural) => stack.push(Value::Natural(*natural)),
                        ir::Expression::Byte(byte) => stack.push(Value::Byte(*byte)),
                        ir::Expression::Signed(signed) => stack.push(Value::Signed(*signed)),
                        ir::Expression::Unsigned(unsigned) => {
                            stack.push(Value::Unsigned(*unsigned))
                        }
                        ir::Expression::Float(float) => stack.push(Value::Float(*float)),
                        ir::Expression::Double(double) => stack.push(Value::Double(*double)),
                        ir::Expression::Variable(var) => stack.push(scope.get(*var)),
                        ir::Expression::Constant(label) => {
                            let value = self.evaluate_constant(*label, context).await?;
                            stack.push(value);
                        }
                        ir::Expression::Function(label) => {
                            stack.push(Value::Function(Scope::new(0), *label))
                        }
                        ir::Expression::Closure(captures, label) => {
                            let mut closure_scope = Scope::new(captures.0.len());
                            for (captured, var) in &captures.0 {
                                closure_scope.set(*var, scope.get(*captured));
                            }

                            stack.push(Value::Function(closure_scope, *label));
                        }
                        ir::Expression::Call => {
                            let input = stack.pop();

                            match stack.pop() {
                                Value::Function(scope, label) => {
                                    stack.push(input);
                                    self.evaluate_label_in_scope(label, stack, scope, &context)
                                        .await?;
                                }
                                Value::NativeFunction(f) => {
                                    let output = f(input).await?;
                                    stack.push(output);
                                }
                                _ => unreachable!(),
                            };
                        }
                        ir::Expression::External(..) => {
                            return Err(Error::from("'external' is currently unsupported"));
                        }
                        ir::Expression::Runtime(func, inputs) => {
                            let inputs = stack.popn(*inputs);
                            self.call_runtime(*func, inputs, stack, context).await?;
                        }
                        ir::Expression::Tuple(inputs) => {
                            let inputs = stack.popn(*inputs);
                            stack.push(Value::Tuple(inputs));
                        }
                        ir::Expression::Format(segments, trailing_segment) => {
                            let inputs = stack.popn(segments.len());

                            let text = segments
                                .iter()
                                .zip(inputs)
                                .map(|(text, expr)| match expr {
                                    Value::Text(right) => text.to_string() + right.as_ref(),
                                    _ => unreachable!(),
                                })
                                .chain(trailing_segment.map(|text| text.to_string()))
                                .collect::<String>();

                            stack.push(Value::Text(Arc::from(text)));
                        }
                        ir::Expression::Structure(inputs) => {
                            let inputs = stack.popn(*inputs);
                            stack.push(Value::Structure(inputs));
                        }
                        ir::Expression::Variant(discriminant, inputs) => {
                            let inputs = stack.popn(*inputs);
                            stack.push(Value::Variant(*discriminant, inputs));
                        }
                        ir::Expression::TupleElement(index) => {
                            let tuple = match stack.pop() {
                                Value::Tuple(tuple) => tuple,
                                _ => unreachable!(),
                            };

                            stack.push(tuple[*index].clone());
                        }
                        ir::Expression::StructureElement(index) => {
                            let structure = match stack.pop() {
                                Value::Structure(structure) => structure,
                                _ => unreachable!(),
                            };

                            stack.push(structure[index.into_inner()].clone());
                        }
                        ir::Expression::VariantElement(_, index) => {
                            let variant = match stack.pop() {
                                Value::Variant(_, variant) => variant,
                                _ => unreachable!(),
                            };

                            stack.push(variant[*index].clone());
                        }
                        ir::Expression::Reference | ir::Expression::Dereference => {
                            // The interpreter doesn't use references
                        }
                        ir::Expression::Context(ctx) => {
                            let value = match context.get(*ctx) {
                                Some(value) => value,
                                None => self.evaluate_constant(*ctx, context).await?,
                            };

                            stack.push(value);
                        }
                    },
                }
            }

            match &block.terminator {
                ir::Terminator::Return => return Ok(()),
                ir::Terminator::Jump(index) => {
                    block = &blocks[*index];
                }
                ir::Terminator::If(matching_discriminant, then_index, else_index) => {
                    let discriminant = match stack.pop() {
                        Value::Variant(discriminant, _) => discriminant,
                        _ => unreachable!(),
                    };

                    let index = if discriminant == *matching_discriminant {
                        then_index
                    } else {
                        else_index
                    };

                    block = &blocks[*index];
                }
            }
        }
    }

    async fn evaluate_constant(&self, label: usize, context: &Context) -> Result<Value, Error> {
        if let Some(value) = self.lock().initialized_constants.get(&label) {
            return Ok(value.clone());
        }

        let mut stack = Stack::new();
        self.evaluate_label(label, &mut stack, context).await?;

        let value = stack.pop();
        self.inner
            .lock()
            .initialized_constants
            .insert(label, value.clone());

        Ok(value)
    }
}
