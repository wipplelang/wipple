#![allow(clippy::non_canonical_clone_impl)]

use super::*;
use futures::future;
use std::marker::PhantomData;

impl<R: Runtime + ?Sized> Value<R> {
    pub fn from_text(text: impl ToString) -> Self {
        Value::Text(text.to_string())
    }

    pub fn to_text(self) -> String {
        match self {
            Value::Text(text) => text,
            _ => panic!("expected text"),
        }
    }

    pub fn from_number(number: Option<Decimal>) -> Self {
        Value::Number(number)
    }

    pub fn to_number(self) -> Option<Decimal> {
        match self {
            Value::Number(number) => number,
            _ => panic!("expected number; found {self:#?}"),
        }
    }

    pub fn from_bool(bool: bool) -> Self {
        Value::Variant {
            variant: bool as u32,
            values: Vec::new(),
        }
    }

    pub fn none() -> Self {
        Value::Variant {
            variant: 0,
            values: Vec::new(),
        }
    }

    pub fn from_some(value: Value<R>) -> Self {
        Value::Variant {
            variant: 1,
            values: vec![value],
        }
    }

    pub fn from_maybe(value: Option<Value<R>>) -> Self {
        match value {
            Some(value) => Value::from_some(value),
            None => Value::none(),
        }
    }

    pub fn to_maybe(self) -> Option<Self> {
        match self {
            Value::Variant { variant, values } => match variant {
                0 => None,
                1 => Some(values.into_iter().next().unwrap()),
                _ => panic!("expected maybe"),
            },
            _ => panic!("expected variant"),
        }
    }

    pub fn is_less_than() -> Self {
        Value::Variant {
            variant: 0,
            values: Vec::new(),
        }
    }

    pub fn is_equal() -> Self {
        Value::Variant {
            variant: 1,
            values: Vec::new(),
        }
    }

    pub fn is_greater_than() -> Self {
        Value::Variant {
            variant: 2,
            values: Vec::new(),
        }
    }

    pub fn from_ordering(ordering: std::cmp::Ordering) -> Self {
        use std::cmp::Ordering;

        match ordering {
            Ordering::Less => Value::is_less_than(),
            Ordering::Equal => Value::is_equal(),
            Ordering::Greater => Value::is_greater_than(),
        }
    }

    pub fn from_list(list: Vec<Value<R>>) -> Self {
        Value::List(list)
    }

    pub fn to_list(self) -> Vec<Value<R>> {
        match self {
            Value::List(list) => list,
            _ => panic!("expected list"),
        }
    }

    pub fn from_tuple(tuple: Vec<Value<R>>) -> Self {
        Value::Tuple(tuple)
    }

    pub fn to_tuple(self) -> Vec<Value<R>> {
        match self {
            Value::Tuple(tuple) => tuple,
            _ => panic!("expected tuple"),
        }
    }

    pub fn from_function(
        function: impl Fn(Vec<Value<R>>, TaskLocals<R>, Context<R>) -> BoxFuture<'static, Result<Value<R>>>
            + Send
            + Sync
            + 'static,
    ) -> Self {
        Value::NativeFunction(Arc::new(function))
    }

    pub fn from_task_group(task_group: TaskGroup<R>) -> Self {
        Value::TaskGroup(task_group)
    }

    pub fn to_task_group(self) -> TaskGroup<R> {
        match self {
            Value::TaskGroup(task_group) => task_group,
            _ => panic!("expected task group"),
        }
    }

    pub fn from_task_local_key(task_local_key: TaskLocalKey) -> Self {
        Value::TaskLocalKey(task_local_key)
    }

    pub fn to_task_local_key(self) -> TaskLocalKey {
        match self {
            Value::TaskLocalKey(task_local_key) => task_local_key,
            _ => panic!("expected task local key"),
        }
    }

    pub fn hasher() -> Self {
        Value::Hasher(std::collections::hash_map::DefaultHasher::new())
    }

    pub fn to_hasher(self) -> std::collections::hash_map::DefaultHasher {
        match self {
            Value::Hasher(hasher) => hasher,
            _ => panic!("expected hasher"),
        }
    }
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct StoredFunction<R: Runtime + ?Sized> {
    pub context: Context<R>,
    pub task: TaskLocals<R>,
    pub function: Arc<
        dyn Fn(Vec<Value<R>>, TaskLocals<R>, Context<R>) -> BoxFuture<'static, Result<Value<R>>>
            + Send
            + Sync,
    >,
}

impl<R: Runtime + ?Sized> StoredFunction<R> {
    pub fn new(
        task: TaskLocals<R>,
        context: Context<R>,
        function: impl Fn(Vec<Value<R>>, TaskLocals<R>, Context<R>) -> BoxFuture<'static, Result<Value<R>>>
            + Send
            + Sync
            + 'static,
    ) -> Self {
        StoredFunction {
            task,
            context,
            function: Arc::new(function),
        }
    }
}

#[derive(Derivative)]
#[derivative(Clone, Copy, PartialEq, Eq)]
pub struct FunctionHandle<R: Runtime + ?Sized>(usize, PhantomData<R>);

impl<R: Runtime + ?Sized> FunctionHandle<R> {
    pub fn new(handle: usize) -> Self {
        FunctionHandle(handle, PhantomData)
    }

    pub fn into_raw(self) -> usize {
        self.0
    }
}

pub async fn call_function<R: Runtime>(
    handle: FunctionHandle<R>,
    inputs: Vec<R::Value>,
) -> Result<R::Value> {
    let function = R::with_functions(|functions| functions.get(handle.0).unwrap().clone()).await;

    let inputs = future::join_all(inputs.into_iter().map(R::to_value)).await;

    let output =
        (function.function)(inputs, function.task.clone(), function.context.clone()).await?;

    Ok(R::from_value(output, &function.task, &function.context).await)
}
