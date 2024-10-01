//! Execute compiled IR.

#![allow(missing_docs)] // TODO: Documentation

mod convert;
mod intrinsics;

pub use rust_decimal;

use async_lock::Mutex;
use derivative::Derivative;
use futures::{future::BoxFuture, Future, FutureExt};
use intrinsics::{intrinsics, Intrinsic};
use rand::Rng;
use rust_decimal::prelude::*;
use rust_decimal::Decimal;
use rust_decimal_macros::dec;
use std::{
    collections::{HashMap, VecDeque},
    future::IntoFuture,
    mem,
    sync::Arc,
};

pub use convert::{call_function, FunctionHandle, StoredFunction};

pub type Path = wipple_driver::lower::Path;
pub type Executable = wipple_driver::Executable;
pub type Item = wipple_driver::Item;
pub type TypeDescriptor = wipple_driver::ir::TypeDescriptor<wipple_driver::Driver>;
pub type Instruction = wipple_driver::ir::Instruction<wipple_driver::Driver>;
pub type Bounds = Vec<Path>;

pub trait Runtime
where
    Self: 'static,
{
    type Value: Clone + Send + Sync + 'static;
    type JoinHandle: Future + Send + 'static;

    fn run(future: impl Future<Output = ()> + Send + 'static) -> Self::JoinHandle;

    fn from_value(
        value: Value<Self>,
        task: &TaskLocals<Self>,
        context: &Context<Self>,
    ) -> impl Future<Output = Self::Value> + Send;

    fn to_value(value: Self::Value) -> impl Future<Output = Value<Self>> + Send;

    fn with_functions<T>(
        f: impl FnOnce(&mut Vec<StoredFunction<Self>>) -> T + Send,
    ) -> impl Future<Output = T> + Send;
}

#[derive(Debug, Clone)]
pub struct Error(pub String);

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
pub enum Value<R: Runtime + ?Sized> {
    Number(Option<Decimal>),
    Text(String),
    Function {
        path: Path,
        substitutions: HashMap<Path, TypeDescriptor>,
        bounds: Bounds,
        captures: Vec<Arc<Mutex<Value<R>>>>,
    },
    NativeFunction(
        #[derivative(Debug = "ignore")]
        Arc<
            dyn Fn(Vec<Value<R>>, TaskLocals<R>, Context<R>) -> BoxFuture<'static, Result<Value<R>>>
                + Send
                + Sync,
        >,
    ),
    Variant {
        variant: u32,
        values: Vec<Value<R>>,
    },
    List(Vec<Value<R>>),
    Marker,
    Wrapper(Box<Value<R>>),
    Structure(Vec<Value<R>>),
    Tuple(Vec<Value<R>>),
    TaskGroup(TaskGroup<R>),
    TaskLocalKey(TaskLocalKey),
    Hasher(std::collections::hash_map::DefaultHasher),
}

#[derive(Derivative)]
#[derivative(Debug, Clone, Default)]
pub struct TaskGroup<R: Runtime + ?Sized>(
    #[derivative(Debug = "ignore")] pub(crate) Arc<Mutex<Vec<R::JoinHandle>>>,
);

impl<R: Runtime + ?Sized> TaskGroup<R> {
    pub async fn add(&self, task: impl Future + Send + 'static) {
        self.0.lock_arc().await.push(R::run(async move {
            task.await;
        }));
    }
}

impl<R: Runtime + ?Sized> IntoFuture for TaskGroup<R> {
    type Output = Result<()>;

    type IntoFuture = BoxFuture<'static, Self::Output>;

    fn into_future(self) -> Self::IntoFuture {
        async move {
            let tasks = mem::take(&mut *self.0.lock_arc().await);
            for task in tasks {
                task.await;
            }

            Ok(())
        }
        .boxed()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TaskLocalKey(usize);

impl TaskLocalKey {
    pub fn new() -> Self {
        use std::sync::atomic::{AtomicUsize, Ordering};

        static NEXT_KEY: AtomicUsize = AtomicUsize::new(0);

        TaskLocalKey(NEXT_KEY.fetch_add(1, Ordering::Relaxed))
    }
}

pub type TaskLocals<R> = Arc<Mutex<HashMap<TaskLocalKey, Value<R>>>>;

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct Context<R: Runtime + ?Sized> {
    pub(crate) executable: Arc<Executable>,
    pub(crate) debug: Option<Arc<dyn Fn(String) -> BoxFuture<'static, ()> + Send + Sync>>,
    pub(crate) io: Io<R>,
    pub(crate) intrinsics: Arc<HashMap<&'static str, Intrinsic<R>>>,
    pub(crate) call: Arc<
        dyn Fn(
                Value<R>,
                Vec<Value<R>>,
                TaskLocals<R>,
                Context<R>,
            ) -> BoxFuture<'static, Result<Option<Value<R>>>>
            + Send
            + Sync,
    >,
    pub(crate) item_cache: Arc<Mutex<HashMap<Path, Arc<Mutex<Option<Value<R>>>>>>>,
    pub(crate) get_item: Arc<
        dyn Fn(
                Path,
                std::result::Result<HashMap<Path, TypeDescriptor>, Vec<TypeDescriptor>>,
                Bounds,
                TaskLocals<R>,
                Context<R>,
            ) -> BoxFuture<'static, Result<Option<Value<R>>>>
            + Send
            + Sync,
    >,
    pub(crate) background_tasks: TaskGroup<R>,
}

impl<R: Runtime + ?Sized> std::fmt::Debug for Context<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Context").finish()
    }
}

#[derive(Derivative)]
#[derivative(Clone)]
pub struct Io<R: Runtime + ?Sized> {
    pub display: Arc<dyn Fn(String) -> BoxFuture<'static, Result<()>> + Send + Sync>,
    pub prompt: Arc<
        dyn Fn(
                String,
                Arc<dyn Fn(String) -> BoxFuture<'static, Result<bool>> + Send + Sync>,
            ) -> BoxFuture<'static, Result<()>>
            + Send
            + Sync,
    >,
    pub choice: Arc<dyn Fn(String, Vec<String>) -> BoxFuture<'static, Result<usize>> + Send + Sync>,
    pub ui: Arc<dyn Fn(String, R::Value) -> BoxFuture<'static, Result<R::Value>> + Send + Sync>,
    pub sleep: Arc<dyn Fn(u64) -> BoxFuture<'static, Result<()>> + Send + Sync>,
}

pub struct Options<R: Runtime> {
    pub debug: Option<Arc<dyn Fn(String) -> BoxFuture<'static, ()> + Send + Sync>>,
    pub io: Io<R>,
}

impl<R: Runtime> Options<R> {
    pub fn with_io(io: Io<R>) -> Self {
        Options { debug: None, io }
    }

    pub fn with_debug(
        mut self,
        debug: impl Fn(String) -> BoxFuture<'static, ()> + Send + Sync + 'static,
    ) -> Self {
        self.debug = Some(Arc::new(debug));
        self
    }
}

pub async fn evaluate<R: Runtime>(executable: Executable, options: Options<R>) -> Result<()> {
    let context = Context {
        executable: Arc::new(executable),
        debug: options.debug,
        io: options.io,
        intrinsics: Arc::from(intrinsics()),
        call: Arc::new(|func, mut inputs, task, context| match func {
            Value::Function {
                path,
                substitutions,
                bounds,
                captures,
            } => {
                inputs.reverse();

                let instructions = context.executable.items.get(&path).unwrap().ir.clone();

                evaluate_item(
                    path,
                    instructions,
                    inputs,
                    captures.into_iter().enumerate().collect(),
                    substitutions,
                    bounds,
                    task,
                    context,
                )
                .boxed()
            }
            Value::NativeFunction(func) => {
                async move { Ok(Some(func(inputs, task, context).await.unwrap())) }.boxed()
            }
            _ => panic!("expected function"),
        }),
        item_cache: Default::default(),
        get_item: Arc::new(|path, substitutions, bounds, task, context| {
            async move {
                let item = context
                    .executable
                    .items
                    .get(&path)
                    .unwrap_or_else(|| panic!("cannot find item {path:?}"));

                let mut cached = None;
                if item.evaluate_once {
                    let mut item_cache = context.item_cache.lock_arc().await;

                    if let Some(mutex) = item_cache.get(&path) {
                        return Ok(Some(mutex.lock_arc().await.clone().unwrap()));
                    } else {
                        let item_mutex = Arc::new(Mutex::new(None));
                        cached = Some(item_mutex.lock_arc().await);
                        item_cache.insert(path.clone(), item_mutex);
                    }
                }

                let mut substitutions = substitutions.unwrap_or_else(|parameters| {
                    item.parameters
                        .iter()
                        .enumerate()
                        .map(|(index, parameter)| (parameter.clone(), parameters[index].clone()))
                        .collect()
                });

                // Any type parameters not mentioned in the item's type must be
                // inserted into the substitutions by resolving the bounds
                let bounds = item
                    .bounds
                    .iter()
                    .map(|bound| {
                        let parameters = bound
                            .parameters
                            .iter()
                            .cloned()
                            .map(|mut parameter| {
                                substitute_type_descriptor(&mut parameter, &substitutions);
                                parameter
                            })
                            .collect::<Vec<_>>();

                        find_instance(
                            &bound.trait_path,
                            &parameters,
                            &bounds,
                            &context.executable,
                            &mut substitutions,
                        )
                    })
                    .collect();

                context
                    .debug(|| {
                        format!(
                            "initializing constant {path:?} with substitutions {substitutions:#?} and bounds {bounds:#?}",
                        )
                    })
                    .await;

                let result = evaluate_item(
                    path,
                    item.ir.clone(),
                    Vec::new(),
                    HashMap::new(),
                    substitutions,
                    bounds,
                    task.clone(),
                    context.clone(),
                )
                .await?;

                if let Some(mut cached) = cached {
                    *cached = Some(result.clone().unwrap());
                }

                Ok(result)
            }
            .boxed()
        }),
        background_tasks: Default::default(),
    };

    for path in &context.executable.entrypoints {
        let entrypoint = context.executable.items.get(path).unwrap();

        let task = TaskLocals::default();

        context.debug(|| "evaluating entrypoint block").await;

        let block = evaluate_item(
            path.clone(),
            entrypoint.ir.clone(),
            Vec::new(),
            HashMap::new(),
            HashMap::new(),
            Bounds::new(),
            task.clone(),
            context.clone(),
        )
        .await?
        .unwrap();

        context.debug(|| "executing entrypoint block").await;

        context.call(block, Vec::new(), task).await?;
    }

    context.background_tasks.await?;

    Ok(())
}

async fn evaluate_item<R: Runtime>(
    mut path: Path,
    instructions: Vec<Instruction>,
    mut stack: Vec<Value<R>>,
    mut scope: HashMap<usize, Arc<Mutex<Value<R>>>>,
    mut substitutions: HashMap<Path, TypeDescriptor>,
    mut bounds: Bounds,
    task: TaskLocals<R>,
    context: Context<R>,
) -> Result<Option<Value<R>>> {
    use wipple_driver::ir::Instruction;

    let mut blocks = vec![instructions.iter().collect::<VecDeque<_>>()];

    macro_rules! peek {
        () => {
            stack.last().expect("stack is empty").clone()
        };
    }

    macro_rules! try_pop {
        () => {
            stack.pop()
        };
    }

    macro_rules! pop {
        () => {
            stack.pop().expect("stack is empty")
        };
    }

    macro_rules! r#break {
        ($n:expr) => {
            for _ in 0..=$n {
                blocks.pop().expect("ran out of blocks");
            }
        };
    }

    loop {
        while let Some(instruction) = blocks
            .last_mut()
            .and_then(|instructions| instructions.pop_front())
        {
            context
                .debug(|| format!("evaluating {path:?}:\ninstruction = {instruction:#?}\nstack = {stack:#?}\nscope = {scope:#?}\nsubstitutions = {substitutions:#?}"))
                .await;

            match instruction {
                Instruction::Copy => {
                    let value = peek!();
                    stack.push(value);
                }
                Instruction::Drop => {
                    pop!();
                }
                Instruction::Initialize(variable) => {
                    let value = peek!();

                    assert!(
                        !scope.contains_key(&(*variable as usize)),
                        "variable {variable} already initialized",
                    );

                    scope.insert(*variable as usize, Arc::new(Mutex::new(value)));
                }
                Instruction::Field(index) => {
                    let value = peek!();
                    if let Value::Structure(mut fields) = value {
                        stack.push(fields.swap_remove(*index as usize));
                    } else {
                        panic!("expected structure");
                    }
                }
                Instruction::VariantElement(index) => {
                    let value = peek!();
                    if let Value::Variant { mut values, .. } = value {
                        stack.push(values.swap_remove(*index as usize));
                    } else {
                        panic!("expected variant");
                    }
                }
                Instruction::TupleElement(index) => {
                    let value = peek!();
                    if let Value::Tuple(values) = value {
                        stack.push(values[*index as usize].clone());
                    } else {
                        panic!("expected tuple");
                    }
                }
                Instruction::Unwrap => {
                    let value = peek!();
                    if let Value::Wrapper(value) = value {
                        stack.push(*value);
                    } else {
                        panic!("expected wrapper");
                    }
                }
                Instruction::Variable(variable) => {
                    let value = scope
                        .get(&(*variable as usize))
                        .unwrap()
                        .lock_arc()
                        .await
                        .clone();

                    stack.push(value);
                }
                Instruction::Call(inputs) => {
                    let mut inputs = (0..*inputs).map(|_| pop!()).collect::<Vec<_>>();
                    inputs.reverse();

                    let func = pop!();

                    let result = context.call(func, inputs, task.clone()).await?.unwrap();

                    stack.push(result);
                }
                Instruction::Do => {
                    let block = pop!();

                    let result = context
                        .call(block, Vec::new(), task.clone())
                        .await?
                        .unwrap();

                    stack.push(result);
                }
                Instruction::Mutate(variable) => {
                    let value = pop!();
                    *scope.get(&(*variable as usize)).unwrap().lock_arc().await = value;
                }
                Instruction::Tuple(elements) => {
                    let mut elements = (0..*elements).map(|_| pop!()).collect::<Vec<_>>();
                    elements.reverse();

                    stack.push(Value::Tuple(elements));
                }
                wipple_driver::ir::Instruction::Intrinsic(name, inputs) => {
                    let intrinsic = context
                        .intrinsics
                        .get(name.as_str())
                        .unwrap_or_else(|| panic!("unknown intrinsic {name:?}"));

                    let mut inputs = (0..*inputs).map(|_| pop!()).collect::<Vec<_>>();
                    inputs.reverse();

                    let result = intrinsic(inputs, context.clone(), task.clone()).await?;
                    stack.push(result);
                }
                wipple_driver::ir::Instruction::Text(text) => {
                    stack.push(Value::Text(text.clone()));
                }
                wipple_driver::ir::Instruction::Number(number) => {
                    stack.push(Value::Number(Some(number.parse().unwrap())));
                }
                wipple_driver::ir::Instruction::Format(segments, trailing) => {
                    let mut inputs = segments.iter().map(|_| pop!()).collect::<Vec<_>>();

                    let mut result = String::new();
                    for segment in segments {
                        let text = match inputs.pop().unwrap() {
                            Value::Text(text) => text,
                            _ => panic!("expected text"),
                        };

                        result.push_str(segment);
                        result.push_str(&text);
                    }

                    result.push_str(trailing);

                    stack.push(Value::Text(result));
                }
                wipple_driver::ir::Instruction::Marker => {
                    stack.push(Value::Marker);
                }
                wipple_driver::ir::Instruction::Structure(fields) => {
                    let mut elements = fields.iter().map(|_| pop!()).collect::<Vec<_>>();

                    let mut field_values = vec![None; fields.len()];
                    for index in fields {
                        field_values[*index as usize] = Some(elements.pop().unwrap());
                    }

                    stack.push(Value::Structure(
                        field_values.into_iter().map(Option::unwrap).collect(),
                    ));
                }
                wipple_driver::ir::Instruction::Variant(variant, elements) => {
                    let mut elements = (0..*elements).map(|_| pop!()).collect::<Vec<_>>();
                    elements.reverse();

                    stack.push(Value::Variant {
                        variant: *variant,
                        values: elements,
                    });
                }
                wipple_driver::ir::Instruction::Wrapper => {
                    let value = pop!();
                    stack.push(Value::Wrapper(Box::new(value)));
                }
                wipple_driver::ir::Instruction::Function(capture_list, path) => {
                    let captures = capture_list
                        .iter()
                        .map(|index| scope.get(&(*index as usize)).unwrap().clone())
                        .collect::<Vec<_>>();

                    stack.push(Value::Function {
                        path: path.clone(),
                        substitutions: substitutions.clone(),
                        bounds: bounds.clone(),
                        captures,
                    });
                }
                wipple_driver::ir::Instruction::Constant(path, parameters) => {
                    let parameters = parameters
                        .iter()
                        .cloned()
                        .map(|mut parameter| {
                            substitute_type_descriptor(&mut parameter, &substitutions);
                            parameter
                        })
                        .collect::<Vec<_>>();

                    let value = context
                        .get_item(path.clone(), Err(parameters), &bounds, task.clone())
                        .await?;

                    stack.push(value);
                }
                wipple_driver::ir::Instruction::Instance(r#trait, parameters) => {
                    let parameters = parameters
                        .iter()
                        .cloned()
                        .map(|mut parameter| {
                            substitute_type_descriptor(&mut parameter, &substitutions);
                            parameter
                        })
                        .collect::<Vec<_>>();

                    let mut substitutions = HashMap::new();

                    let path = find_instance(
                        r#trait,
                        &parameters,
                        &bounds,
                        &context.executable,
                        &mut substitutions,
                    );

                    let value = context
                        .get_item(path, Ok(substitutions), &bounds, task.clone())
                        .await?;

                    stack.push(value);
                }
                Instruction::Block(instructions) => {
                    blocks.push(instructions.iter().collect::<VecDeque<_>>());
                }
                Instruction::Break(count) => {
                    r#break!(*count);
                }
                Instruction::BreakIfNot(condition, count) => {
                    if let Value::Variant { variant, .. } = peek!() {
                        if variant != *condition {
                            r#break!(*count);
                        }
                    } else {
                        panic!("expected variant");
                    }
                }
                Instruction::Return => {
                    return Ok(try_pop!());
                }
                Instruction::TailCall(inputs) => {
                    let mut inputs = (0..*inputs).map(|_| pop!()).collect::<Vec<_>>();

                    let func = pop!();

                    if !stack.is_empty() {
                        panic!("stack is not empty for tail call");
                    }

                    if let Value::Function {
                        path: func_path,
                        substitutions: func_substitutions,
                        bounds: func_bounds,
                        captures: func_captures,
                    } = func
                    {
                        path = func_path;

                        blocks = vec![context
                            .executable
                            .items
                            .get(&path)
                            .unwrap()
                            .ir
                            .iter()
                            .collect::<VecDeque<_>>()];

                        stack = inputs;

                        scope = func_captures.into_iter().enumerate().collect();

                        substitutions = func_substitutions;
                        bounds = func_bounds;

                        continue;
                    } else {
                        inputs.reverse();
                        return context.call(func, inputs, task).await;
                    }
                }
                Instruction::TailDo => {
                    let block = pop!();

                    if !stack.is_empty() {
                        panic!("stack is not empty for tail call");
                    }

                    if let Value::Function {
                        path: block_path,
                        substitutions: block_substitutions,
                        bounds: block_bounds,
                        captures: block_captures,
                    } = block
                    {
                        path = block_path;

                        blocks = vec![context
                            .executable
                            .items
                            .get(&path)
                            .unwrap()
                            .ir
                            .iter()
                            .collect::<VecDeque<_>>()];

                        scope = block_captures.into_iter().enumerate().collect();

                        substitutions = block_substitutions;
                        bounds = block_bounds;

                        continue;
                    } else {
                        panic!("expected function");
                    }
                }
                Instruction::Unreachable => panic!("evaluated unreachable instruction"),
            }
        }

        if blocks.pop().is_none() {
            panic!(
                "ran out of instructions: {path:?}, {:#?}",
                context.executable.items.get(&path).unwrap()
            );
        }
    }
}

fn find_instance(
    r#trait: &Path,
    parameters: &[TypeDescriptor],
    bounds: &Bounds,
    executable: &Executable,
    substitutions: &mut HashMap<Path, TypeDescriptor>,
) -> Path {
    macro_rules! try_instance {
        ($instance:expr) => {
            let mut instance_substitutions = HashMap::new();
            let mut unified = true;
            for (type_descriptor, (trait_parameter, instance_type_descriptor)) in
                parameters.iter().zip(&$instance.trait_parameters)
            {
                if unify(
                    type_descriptor,
                    instance_type_descriptor,
                    &mut instance_substitutions,
                ) {
                    instance_substitutions.insert(trait_parameter.clone(), type_descriptor.clone());
                } else {
                    unified = false;
                    break;
                }
            }

            if unified {
                // Important: don't override existing parameters -- that would
                // prevent recursive instances from being resolved properly
                for (parameter, type_descriptor) in instance_substitutions {
                    substitutions.entry(parameter).or_insert(type_descriptor);
                }

                return $instance.path.clone();
            }
        };
    }

    for bound in bounds {
        if let Some(instance) = executable
            .instances
            .get(r#trait)
            .and_then(|instances| instances.get(bound))
            .or_else(|| {
                executable
                    .default_instances
                    .get(r#trait)
                    .and_then(|instances| instances.get(bound))
            })
        {
            try_instance!(instance);
        }
    }

    for instances in executable
        .instances
        .get(r#trait)
        .into_iter()
        .chain(executable.default_instances.get(r#trait))
    {
        for instance in instances.values() {
            try_instance!(instance);
        }
    }

    panic!("no instance found for trait {trait:?} with parameters {parameters:#?}")
}

fn unify(
    left: &TypeDescriptor,
    right: &TypeDescriptor,
    substitutions: &mut HashMap<Path, TypeDescriptor>,
) -> bool {
    match (left, right) {
        (TypeDescriptor::Equal(a, b), right) => {
            unify(a, right, substitutions) && unify(b, a, substitutions)
        }
        (left, TypeDescriptor::Equal(a, b)) => {
            unify(left, a, substitutions) && unify(b, a, substitutions)
        }

        (TypeDescriptor::Parameter(_), _) => {
            // This occurs when bounds are being resolved that involve type
            // parameters not mentioned in the item's type descriptor; no
            // value's type will ever contain a `Parameter`
            true
        }

        (left, TypeDescriptor::Parameter(right)) => {
            if let Some(substitution) = substitutions.get(right).cloned() {
                return unify(left, &substitution, substitutions);
            }

            if let TypeDescriptor::Parameter(left) = left {
                if left != right {
                    return false;
                }
            }

            substitutions.insert(right.clone(), left.clone());
            true
        }

        (
            TypeDescriptor::Function(left_inputs, left_output),
            TypeDescriptor::Function(right_inputs, right_output),
        ) => {
            (left_inputs.len() == right_inputs.len())
                && left_inputs
                    .iter()
                    .zip(right_inputs)
                    .all(|(left, right)| unify(left, right, substitutions))
                && unify(left_output, right_output, substitutions)
        }
        (_, TypeDescriptor::Function(_, _)) => false,

        (
            TypeDescriptor::Named(left_path, left_parameters),
            TypeDescriptor::Named(right_path, right_parameters),
        ) => {
            (left_path == right_path)
                && (left_parameters.len() == right_parameters.len())
                && left_parameters
                    .iter()
                    .zip(right_parameters)
                    .all(|(left, right)| unify(left, right, substitutions))
        }
        (_, TypeDescriptor::Named(_, _)) => false,

        (TypeDescriptor::Tuple(left_elements), TypeDescriptor::Tuple(right_elements)) => {
            (left_elements.len() == right_elements.len())
                && left_elements
                    .iter()
                    .zip(right_elements)
                    .all(|(left, right)| unify(left, right, substitutions))
        }
        (_, TypeDescriptor::Tuple(_)) => false,

        (TypeDescriptor::Block(left_value), TypeDescriptor::Block(right_value)) => {
            unify(left_value, right_value, substitutions)
        }
        (_, TypeDescriptor::Block(_)) => false,

        (TypeDescriptor::Intrinsic, TypeDescriptor::Intrinsic) => true,
        (_, TypeDescriptor::Intrinsic) => false,
    }
}

fn substitute_type_descriptor(
    type_descriptor: &mut TypeDescriptor,
    substitutions: &HashMap<Path, TypeDescriptor>,
) {
    match type_descriptor {
        wipple_driver::ir::TypeDescriptor::Parameter(parameter) => {
            if let Some(substitution) = substitutions.get(parameter) {
                *type_descriptor = substitution.clone();
            } else {
                // This occurs when bounds are being resolved that involve type
                // parameters not mentioned in the item's type descriptor; no
                // value's type will ever contain a `Parameter`
            }
        }
        wipple_driver::ir::TypeDescriptor::Named(_, parameters) => {
            for parameter in parameters {
                substitute_type_descriptor(parameter, substitutions);
            }
        }
        wipple_driver::ir::TypeDescriptor::Function(inputs, output) => {
            for input in inputs {
                substitute_type_descriptor(input, substitutions);
            }

            substitute_type_descriptor(output, substitutions);
        }
        wipple_driver::ir::TypeDescriptor::Tuple(elements) => {
            for element in elements {
                substitute_type_descriptor(element, substitutions);
            }
        }
        wipple_driver::ir::TypeDescriptor::Block(value) => {
            substitute_type_descriptor(value, substitutions);
        }
        wipple_driver::ir::TypeDescriptor::Intrinsic => {}
        wipple_driver::ir::TypeDescriptor::Equal(left, right) => {
            substitute_type_descriptor(left, substitutions);
            substitute_type_descriptor(right, substitutions);
        }
    }
}

impl<R: Runtime> Context<R> {
    pub(crate) async fn debug<S: ToString>(&self, message: impl FnOnce() -> S) {
        if let Some(debug) = self.debug.clone() {
            debug(message().to_string()).await;
        }
    }

    pub async fn call(
        &self,
        func: Value<R>,
        inputs: Vec<Value<R>>,
        task: TaskLocals<R>,
    ) -> Result<Option<Value<R>>> {
        (self.call)(func, inputs, task, self.clone()).await
    }

    pub(crate) async fn get_item(
        &self,
        path: Path,
        substitutions: std::result::Result<HashMap<Path, TypeDescriptor>, Vec<TypeDescriptor>>,
        bounds: &Bounds,
        task: TaskLocals<R>,
    ) -> Result<Value<R>> {
        // TODO: Don't clone `bounds` once async closures are stable
        let bounds = bounds.clone();

        Ok(
            (self.get_item)(path, substitutions, bounds, task, self.clone())
                .await?
                .unwrap(),
        )
    }
}
