#[cfg(target_arch = "wasm32")]
use getrandom as _;

use parking_lot::{MappedMutexGuard, Mutex, MutexGuard};
use serde::Serialize;
use std::{
    borrow::Borrow,
    cmp,
    collections::{VecDeque, hash_map::DefaultHasher},
    fmt::Debug,
    hash::{Hash, Hasher},
    ops::ControlFlow,
    sync::Arc,
};
use unicode_segmentation::UnicodeSegmentation;
use wipple_core::codegen::mir;

pub use wipple_core::span::Span;

pub struct Interpreter<'a, 'ctx, Ext, Err> {
    external:
        Box<dyn FnMut(&str, Handle<'a, Ext>) -> Result<Handle<'a, Ext>, Err> + Send + Sync + 'ctx>,
    program: &'a mir::Program,
    state: Vec<State<'a, Ext>>,
    debug_events: Option<VecDeque<DebugEvent<'a, Ext>>>,
}

#[derive(Debug, Clone)]
pub enum Handle<'a, Ext> {
    External(Ext),
    Primitive(Primitive<Self>),
    Unit,
    Value(ValueHandle<'a, Ext>),
}

#[derive(Debug, Clone)]
pub enum Primitive<T> {
    Number(f64),
    String(Arc<str>),
    List(Vec<T>),
}

#[derive(Debug, Clone)]
pub struct ValueHandle<'a, Ext>(Value<'a, Ext>);

#[derive(Debug, Clone)]
pub enum DebugEvent<'a, Ext> {
    Span(Span),
    Value(Handle<'a, Ext>),
}

#[derive(Debug, Clone, Serialize)]
pub enum DebugTree<Ext> {
    Value {
        representation: String,
        children: Vec<(Option<String>, DebugTree<Ext>)>,
    },
    External(Ext),
}

impl<'a, 'ctx, Ext: Debug + Clone, Err> Interpreter<'a, 'ctx, Ext, Err> {
    pub fn new(
        program: &'a mir::Program,
        external: impl FnMut(&str, Handle<'a, Ext>) -> Result<Handle<'a, Ext>, Err> + Send + Sync + 'ctx,
    ) -> Self {
        let mut interpeter = Interpreter {
            external: Box::new(external),
            program,
            state: Vec::new(),
            debug_events: None,
        };

        if let Some(main) = program.main {
            interpeter.jump_to_function(&program.functions[main], Vec::new(), Vec::new(), None);
        }

        interpeter
    }

    pub fn enable_debugging(&mut self) {
        self.debug_events = Some(VecDeque::new());
    }

    pub fn next_debug_event(&mut self) -> Option<DebugEvent<'a, Ext>> {
        self.debug_events
            .as_mut()
            .and_then(|events| events.pop_front())
    }
}

#[derive(Debug)]
struct State<'a, Ext> {
    scope: Scope,
    function: &'a mir::Function,
    statements: &'a [mir::Statement],
    statement: usize,
    storage: Option<(Locals<'a, Ext>, Option<usize>)>,
}

#[derive(Debug, Clone, Copy)]
enum Scope {
    Function,
    Loop,
    Inline,
}

type Locals<'a, Ext> = Vec<Arc<Local<'a, Ext>>>;

#[derive(Debug)]
struct Local<'a, Ext> {
    value: Mutex<Option<Value<'a, Ext>>>,
    debug_info: &'a mir::LocalDebugInfo,
}

fn get_storage<'a, 'state, Ext>(
    state: &'state mut [State<'a, Ext>],
) -> (
    &'a mir::Function,
    &'state Locals<'a, Ext>,
    &'state mut Option<usize>,
) {
    for state in state.iter_mut().rev() {
        if let Some((ref locals, ref mut return_value)) = state.storage {
            return (state.function, locals, return_value);
        }
    }

    unreachable!()
}

#[derive(Debug, Clone)]
enum Value<'a, Ext> {
    External(Ext),
    Primitive(Primitive<Self>),
    Closure(&'a mir::Function, Locals<'a, Ext>),
    Structure(Box<[(&'a mir::FieldDebugInfo, Self)]>),
    Tuple(Box<[Self]>),
    Marker,
    Variant((usize, &'a mir::VariantDebugInfo), Box<[Self]>),
}

impl<'a, 'ctx, Ext: Debug + Clone, Err> Interpreter<'a, 'ctx, Ext, Err> {
    pub fn run(mut self) -> Result<(), Err> {
        while let ControlFlow::Continue(()) = self.step()? {}

        Ok(())
    }

    pub fn step(&mut self) -> Result<ControlFlow<()>, Err> {
        let Some(state) = self.state.last_mut() else {
            return Ok(ControlFlow::Break(()));
        };

        if state.statement == state.statements.len() {
            if let Scope::Loop = state.scope {
                state.statement = 0;
            } else {
                self.state.pop();
                return Ok(ControlFlow::Continue(()));
            }
        }

        let statement = &state.statements[state.statement];
        state.statement += 1;

        match &statement.inner {
            mir::StatementInner::If {
                branches,
                else_branch,
            } => {
                let mut else_branch = else_branch.as_ref();
                for (condition, body) in branches {
                    if self.eval_condition(condition)? {
                        self.jump_to_statements(body, Scope::Inline);
                        else_branch = None;
                        break;
                    }
                }

                if let Some(else_branch) = else_branch {
                    self.jump_to_statements(else_branch, Scope::Inline);
                }
            }
            mir::StatementInner::Return { value } => {
                let value = self.get_local_value(*value).clone();
                let return_value = self.get_return_value();

                self.state.pop().unwrap();

                self.set_local_value(return_value, value);
            }

            mir::StatementInner::Loop { body } => {
                self.jump_to_statements(body, Scope::Loop);
            }
            mir::StatementInner::Break => {
                while self
                    .state
                    .pop_if(|state| !matches!(state.scope, Scope::Loop))
                    .is_some()
                {}

                // Exit the loop
                self.state.pop().unwrap();
            }
            mir::StatementInner::Assign { local, value } => {
                self.jump_to_expression(value, *local)?;
            }
            mir::StatementInner::Trace { span } => {
                if let Some(debug_events) = self.debug_events.as_mut() {
                    debug_events.push_back(DebugEvent::Span(span.clone()));
                }
            }
        }

        Ok(ControlFlow::Continue(()))
    }

    fn jump_to_function(
        &mut self,
        function: &'a mir::Function,
        captures: Locals<'a, Ext>,
        inputs: Vec<Value<'a, Ext>>,
        return_value: Option<usize>,
    ) {
        let mut function_locals = captures;

        function_locals.extend(function.inputs.iter().zip(inputs).map(|(input, value)| {
            Arc::new(Local {
                value: Mutex::new(Some(value)),
                debug_info: &input.debug_info,
            })
        }));

        for local in &function.locals {
            function_locals.push(Arc::new(Local {
                value: Mutex::new(None),
                debug_info: &local.debug_info,
            }));
        }

        self.state.push(State {
            function,
            statements: &function.body,
            statement: 0,
            scope: Scope::Function,
            storage: Some((function_locals, return_value)),
        });
    }

    fn jump_to_statements(&mut self, statements: &'a [mir::Statement], kind: Scope) {
        let function = self.state.last_mut().unwrap().function;

        self.state.push(State {
            function,
            statements,
            statement: 0,
            scope: kind,
            storage: None,
        });
    }

    fn jump_to_expression<E: Borrow<mir::Expression>>(
        &mut self,
        expression: &'a mir::SourceMapped<E>,
        result: usize,
    ) -> Result<(), Err> {
        match expression.inner.borrow() {
            mir::Expression::Constant {
                function: index,
                bounds,
            } => {
                let function = &self.program.functions[*index];

                let captures = function
                    .captures
                    .iter()
                    .map(|&capture| self.get_local(capture).clone())
                    .collect::<Vec<_>>();

                let inputs = bounds
                    .iter()
                    .map(|bound| self.get_local_value(*bound).clone())
                    .collect::<Vec<_>>();

                self.jump_to_function(function, captures, inputs, Some(result));
            }
            mir::Expression::Call { function, inputs } => {
                let (function, captures) = {
                    let Value::Closure(function, ref captures) = *self.get_local_value(*function)
                    else {
                        panic!("not a closure");
                    };

                    (function, captures.clone())
                };

                let inputs = inputs
                    .iter()
                    .map(|input| self.get_local_value(*input).clone())
                    .collect::<Vec<_>>();

                self.jump_to_function(function, captures, inputs, Some(result));
            }
            mir::Expression::Closure(function) => {
                let captures = function
                    .captures
                    .iter()
                    .map(|&capture| self.get_local(capture).clone())
                    .collect::<Vec<_>>();

                self.set_local_value(result, Value::Closure(function, captures));
            }
            mir::Expression::Tuple { elements } => {
                let elements = elements
                    .iter()
                    .map(|element| self.get_local_value(*element).clone())
                    .collect::<Vec<_>>();

                self.set_local_value(result, Value::Tuple(elements.into_boxed_slice()));
            }
            mir::Expression::Marker => self.set_local_value(result, Value::Marker),
            mir::Expression::Local { local } | mir::Expression::MutableLocal { local } => {
                let value = self.get_local_value(*local).clone();
                self.set_local_value(result, value);
            }
            mir::Expression::Number { value } => {
                let number = value.parse().expect("invalid number");
                self.set_local_value(result, Value::Primitive(Primitive::Number(number)));
            }
            mir::Expression::Intrinsic { intrinsic } => {
                let intrinsic = self.eval_intrinsic(intrinsic)?;
                self.set_local_value(result, intrinsic);
            }
            mir::Expression::String { value } => {
                self.set_local_value(
                    result,
                    Value::Primitive(Primitive::String(Arc::from(value.as_str()))),
                );
            }
            mir::Expression::Structure { fields } => {
                let mut elements = vec![None; fields.len()];
                for (index, debug_info, field) in fields {
                    let value = self.get_local_value(*field).clone();
                    elements[*index].replace((debug_info, value));
                }

                let elements = elements
                    .into_iter()
                    .map(|value| value.expect("uninitialized field"))
                    .collect::<Vec<_>>();

                self.set_local_value(result, Value::Structure(elements.into_boxed_slice()));
            }
            mir::Expression::Variant {
                variant,
                debug_info,
                elements,
            } => {
                let elements = elements
                    .iter()
                    .map(|element| self.get_local_value(*element).clone())
                    .collect::<Vec<_>>();

                self.set_local_value(
                    result,
                    Value::Variant((*variant, debug_info), elements.into_boxed_slice()),
                );
            }
        };

        Ok(())
    }

    fn eval_condition(&mut self, condition: &'a mir::Condition) -> Result<bool, Err> {
        Ok(match condition {
            mir::Condition::True => true,
            mir::Condition::False => false,
            mir::Condition::And { left, right } => {
                self.eval_condition(left)? && self.eval_condition(right)?
            }
            mir::Condition::Or { left, right } => {
                self.eval_condition(left)? || self.eval_condition(right)?
            }
            mir::Condition::EqualToNumber { input, value } => {
                let Value::Primitive(Primitive::Number(input)) = *self.get_local_value(*input)
                else {
                    panic!("not a number");
                };

                let number = value.parse::<f64>().expect("invalid number");

                input == number
            }
            mir::Condition::EqualToString { input, value } => {
                let Value::Primitive(Primitive::String(ref input)) = *self.get_local_value(*input)
                else {
                    panic!("not a string");
                };

                input.as_ref() == value
            }
            mir::Condition::Variant { value, variant } => {
                let Value::Variant((v, _), ..) = *self.get_local_value(*value) else {
                    panic!("not a variant");
                };

                *variant == v
            }
            mir::Condition::Field {
                local,
                value,
                index,
            } => {
                let value = {
                    let Value::Structure(ref fields) = *self.get_local_value(*value) else {
                        panic!("not a structure");
                    };

                    let (_, value) = &fields[*index];

                    value.clone()
                };

                self.set_local_value(*local, value);

                true
            }
            mir::Condition::TupleElement {
                local,
                value,
                index,
            } => {
                let element = {
                    let Value::Tuple(ref elements) = *self.get_local_value(*value) else {
                        panic!("not a tuple");
                    };

                    elements[*index].clone()
                };

                self.set_local_value(*local, element);

                true
            }
            mir::Condition::VariantElement {
                local,
                value,
                variant,
                index,
            } => {
                let element = {
                    let Value::Variant((v, _), ref elements) = *self.get_local_value(*value) else {
                        panic!("not a variant");
                    };

                    if *variant != v {
                        panic!("expected variant {variant:?}, found variant {v:?}");
                    }

                    elements[*index].clone()
                };

                self.set_local_value(*local, element);

                true
            }
            mir::Condition::Initialize { local, value } => {
                let value = self.get_local_value(*value).clone();
                self.set_local_value(*local, value);

                true
            }
            mir::Condition::Mutate { local, value } => {
                let value = self.get_local_value(*value).clone();
                self.set_local_value(*local, value);

                true
            }
        })
    }

    fn eval_intrinsic(
        &mut self,
        intrinsic: &'a mir::Intrinsic<usize>,
    ) -> Result<Value<'a, Ext>, Err> {
        macro_rules! eval {
            ($value:expr) => {
                self.get_local_value($value).clone()
            };
            ($value:expr, Primitive::$kind:ident) => {
                match eval!($value) {
                    Value::Primitive(Primitive::$kind(x)) => (x),
                    value => panic!("expected {}, but found {value:?}", stringify!($kind)),
                }
            };
        }

        Ok(match *intrinsic {
            mir::Intrinsic::Debug { value } => {
                let value = eval!(value);

                if let Some(debug_events) = self.debug_events.as_mut() {
                    debug_events.push_back(DebugEvent::Value(value.clone().into()));
                }

                value
            }
            mir::Intrinsic::StringCount { value } => {
                let string = eval!(value, Primitive::String);
                Value::Primitive(Primitive::Number(string.len() as f64))
            }
            mir::Intrinsic::StringConcat { left, right } => {
                let left = eval!(left, Primitive::String);
                let right = eval!(right, Primitive::String);

                Value::Primitive(Primitive::String(Arc::from(
                    left.to_string() + right.as_ref(),
                )))
            }
            mir::Intrinsic::External { name, value } => {
                let name = eval!(name, Primitive::String);
                let value = eval!(value);
                (self.external)(name.as_ref(), value.into())?.into()
            }
            mir::Intrinsic::NumberToString { value } => {
                let number = eval!(value, Primitive::Number);

                let string = if number.is_nan() {
                    Arc::from("NaN")
                } else if number.is_infinite() {
                    if number.is_sign_positive() {
                        Arc::from("Infinity")
                    } else {
                        Arc::from("-Infinity")
                    }
                } else {
                    Arc::from(number.to_string())
                };

                Value::Primitive(Primitive::String(string))
            }
            mir::Intrinsic::StringToNumber { value } => {
                let string = eval!(value, Primitive::String);
                Value::Primitive(Primitive::Number(string.parse().ok().unwrap_or(f64::NAN)))
            }
            mir::Intrinsic::Add { left, right } => {
                let left = eval!(left, Primitive::Number);
                let right = eval!(right, Primitive::Number);

                Value::Primitive(Primitive::Number(left + right))
            }
            mir::Intrinsic::Sub { left, right } => {
                let left = eval!(left, Primitive::Number);
                let right = eval!(right, Primitive::Number);

                Value::Primitive(Primitive::Number(left - right))
            }
            mir::Intrinsic::Mul { left, right } => {
                let left = eval!(left, Primitive::Number);
                let right = eval!(right, Primitive::Number);

                Value::Primitive(Primitive::Number(left * right))
            }
            mir::Intrinsic::Div { left, right } => {
                let left = eval!(left, Primitive::Number);
                let right = eval!(right, Primitive::Number);

                Value::Primitive(Primitive::Number(left / right))
            }
            mir::Intrinsic::Rem { left, right } => {
                let left = eval!(left, Primitive::Number);
                let right = eval!(right, Primitive::Number);

                Value::Primitive(Primitive::Number(left % right))
            }
            mir::Intrinsic::Pow { left, right } => {
                let left = eval!(left, Primitive::Number);
                let right = eval!(right, Primitive::Number);

                Value::Primitive(Primitive::Number(left.powf(right)))
            }
            mir::Intrinsic::Floor { value } => {
                let value = eval!(value, Primitive::Number);
                Value::Primitive(Primitive::Number(value.floor()))
            }
            mir::Intrinsic::Ceil { value } => {
                let value = eval!(value, Primitive::Number);
                Value::Primitive(Primitive::Number(value.ceil()))
            }
            mir::Intrinsic::Sqrt { value } => {
                let value = eval!(value, Primitive::Number);
                Value::Primitive(Primitive::Number(value.sqrt()))
            }
            mir::Intrinsic::Neg { value } => {
                let value = eval!(value, Primitive::Number);
                Value::Primitive(Primitive::Number(-value))
            }
            mir::Intrinsic::Sin { value } => {
                let value = eval!(value, Primitive::Number);
                Value::Primitive(Primitive::Number(value.sin()))
            }
            mir::Intrinsic::Cos { value } => {
                let value = eval!(value, Primitive::Number);
                Value::Primitive(Primitive::Number(value.cos()))
            }
            mir::Intrinsic::Tan { value } => {
                let value = eval!(value, Primitive::Number);
                Value::Primitive(Primitive::Number(value.tan()))
            }
            mir::Intrinsic::NumberEqual {
                left,
                right,
                true_variant: (true_variant, ref true_variant_debug_info),
                false_variant: (false_variant, ref false_variant_debug_info),
            } => {
                let left = eval!(left, Primitive::Number);
                let right = eval!(right, Primitive::Number);

                if left == right {
                    Value::Variant((true_variant, true_variant_debug_info), Box::new([]))
                } else {
                    Value::Variant((false_variant, false_variant_debug_info), Box::new([]))
                }
            }
            mir::Intrinsic::StringEqual {
                left,
                right,
                true_variant: (true_variant, ref true_variant_debug_info),
                false_variant: (false_variant, ref false_variant_debug_info),
            } => {
                let left = eval!(left, Primitive::String);
                let right = eval!(right, Primitive::String);

                if left == right {
                    Value::Variant((true_variant, true_variant_debug_info), Box::new([]))
                } else {
                    Value::Variant((false_variant, false_variant_debug_info), Box::new([]))
                }
            }
            mir::Intrinsic::Order {
                left,
                right,
                is_less_than_variant: (is_less_than_variant, ref is_less_than_variant_debug_info),
                is_equal_variant: (is_equal_variant, ref is_equal_variant_debug_info),
                is_greater_than_variant:
                    (is_greater_than_variant, ref is_greater_than_variant_debug_info),
            } => {
                let left = eval!(left, Primitive::Number);
                let right = eval!(right, Primitive::Number);

                match left.total_cmp(&right) {
                    cmp::Ordering::Less => Value::Variant(
                        (is_less_than_variant, is_less_than_variant_debug_info),
                        Box::new([]),
                    ),
                    cmp::Ordering::Equal => Value::Variant(
                        (is_equal_variant, is_equal_variant_debug_info),
                        Box::new([]),
                    ),
                    cmp::Ordering::Greater => Value::Variant(
                        (is_greater_than_variant, is_greater_than_variant_debug_info),
                        Box::new([]),
                    ),
                }
            }
            mir::Intrinsic::EmptyList => Value::Primitive(Primitive::List(Vec::new())),
            mir::Intrinsic::ListCount { value } => {
                let list = eval!(value, Primitive::List);
                Value::Primitive(Primitive::Number(list.len() as f64))
            }
            mir::Intrinsic::ListFirst { value } => {
                let list = eval!(value, Primitive::List);

                list.first().expect("empty list").clone()
            }
            mir::Intrinsic::ListLast { value } => {
                let list = eval!(value, Primitive::List);

                list.last().expect("empty list").clone()
            }
            mir::Intrinsic::ListInitial { value } => {
                let list = eval!(value, Primitive::List);

                let (_, initial) = list.split_last().expect("empty list");
                Value::Primitive(Primitive::List(initial.to_vec()))
            }
            mir::Intrinsic::ListTail { value } => {
                let list = eval!(value, Primitive::List);

                let (_, tail) = list.split_first().expect("empty list");
                Value::Primitive(Primitive::List(tail.to_vec()))
            }
            mir::Intrinsic::ListNth { value, index } => {
                let list = eval!(value, Primitive::List);
                let index = eval!(index, Primitive::Number);

                list[index as usize].clone()
            }
            mir::Intrinsic::ListAppend { value, element } => {
                let mut list = eval!(value, Primitive::List);
                let element = eval!(element);

                list.push(element);

                Value::Primitive(Primitive::List(list))
            }
            mir::Intrinsic::ListPrepend { value, element } => {
                let mut list = eval!(value, Primitive::List);
                let element = eval!(element);

                list.insert(0, element);

                Value::Primitive(Primitive::List(list))
            }
            mir::Intrinsic::ListInsertAt {
                value,
                index,
                element,
            } => {
                let mut list = eval!(value, Primitive::List);
                let index = eval!(index, Primitive::Number);
                let element = eval!(element);

                let index = index as usize;

                if index > list.len() {
                    panic!("index out of bounds");
                }

                list.insert(index, element);

                Value::Primitive(Primitive::List(list))
            }
            mir::Intrinsic::ListRemoveAt { value, index } => {
                let mut list = eval!(value, Primitive::List);
                let index = eval!(index, Primitive::Number);

                let index = index as usize;

                if index >= list.len() {
                    panic!("index out of bounds");
                }

                list.remove(index);

                Value::Primitive(Primitive::List(list))
            }
            mir::Intrinsic::StringCharacters { value } => {
                let string = eval!(value, Primitive::String);

                let characters = string
                    .graphemes(true)
                    .map(|c| Value::Primitive(Primitive::String(Arc::from(c.to_string()))))
                    .collect::<Vec<_>>();

                Value::Primitive(Primitive::List(characters))
            }
            mir::Intrinsic::RandomNumber { min, max } => {
                let min = eval!(min, Primitive::Number);
                let max = eval!(max, Primitive::Number);

                if min > max {
                    panic!("min must be less than or equal to max");
                }

                let random = rand::random_range(min..max);

                Value::Primitive(Primitive::Number(random))
            }
            mir::Intrinsic::Nan => Value::Primitive(Primitive::Number(f64::NAN)),
            mir::Intrinsic::IsNan {
                value,
                true_variant: (true_variant, ref true_variant_debug_info),
                false_variant: (false_variant, ref false_variant_debug_info),
            } => {
                let value = eval!(value, Primitive::Number);

                if value.is_nan() {
                    Value::Variant((true_variant, true_variant_debug_info), Box::new([]))
                } else {
                    Value::Variant((false_variant, false_variant_debug_info), Box::new([]))
                }
            }
            mir::Intrinsic::HashString { value } => {
                let string = eval!(value, Primitive::String).clone();

                let mut hasher = DefaultHasher::new();
                string.hash(&mut hasher);
                let hash = hasher.finish();

                Value::Primitive(Primitive::Number(hash as f64))
            }
            mir::Intrinsic::Unreachable => unreachable!(),
        })
    }

    fn get_local(&mut self, index: usize) -> &Arc<Local<'a, Ext>> {
        let (_, locals, _) = get_storage(&mut self.state);
        &locals[index]
    }

    fn get_local_value(&mut self, index: usize) -> MappedMutexGuard<'_, Value<'a, Ext>> {
        MutexGuard::map(self.get_local(index).value.lock(), |local| {
            local
                .as_mut()
                .unwrap_or_else(|| panic!("local {index:?} is uninitialized"))
        })
    }

    fn set_local_value(&mut self, index: usize, value: Value<'a, Ext>) {
        let (_, locals, _) = get_storage(&mut self.state);
        *locals[index].value.lock() = Some(value);
    }

    fn get_return_value(&mut self) -> usize {
        let (_, _, return_value) = get_storage(&mut self.state);
        return_value.expect("unexpected return value")
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct StackTrace<'a, Ext> {
    pub frames: Vec<StackFrame<'a, Ext>>,
}

#[derive(Debug, Clone, Serialize)]
pub struct StackFrame<'a, Ext> {
    pub function: &'a mir::FunctionDebugInfo,
    pub statement: Option<&'a mir::StatementDebugInfo>,
    pub variables: Vec<(&'a mir::LocalDebugInfo, DebugTree<Ext>)>,
}

impl<'a, 'ctx, Ext: Debug + Clone, Err> Interpreter<'a, 'ctx, Ext, Err> {
    pub fn stack_trace(&self) -> StackTrace<'a, Ext> {
        let frames = self
            .state
            .iter()
            .rev()
            .filter(|state| matches!(state.scope, Scope::Function))
            .filter_map(|state| {
                let (locals, _) = state.storage.as_ref()?;

                let variables = locals
                    .iter()
                    .filter_map(|local| {
                        let value = local.value.lock().as_ref()?.into();
                        Some((local.debug_info, value))
                    })
                    .collect::<Vec<_>>();

                Some(StackFrame {
                    function: &state.function.debug_info,
                    statement: state
                        .statements
                        .get(state.statement)
                        .map(|statement| &statement.debug_info),
                    variables,
                })
            })
            .collect::<Vec<_>>();

        StackTrace { frames }
    }

    pub fn matches_breakpoint(&self, path: &str, line: usize, column: Option<usize>) -> bool {
        let Some(state) = self.state.last() else {
            return false;
        };

        let matches = |index: usize| {
            state.statements[index]
                .debug_info
                .span
                .as_ref()
                .is_some_and(|span| {
                    span.path == path
                        && span.start.line == line
                        && column.is_none_or(|column| span.start.column == column)
                })
        };

        // Only return true if this is the first statement to match the breakpoint
        state.statement < state.statements.len()
            && matches(state.statement)
            && state
                .statement
                .checked_sub(1)
                .is_none_or(|index| !matches(index))
    }
}

impl<'a, Ext> From<Value<'a, Ext>> for Handle<'a, Ext> {
    fn from(value: Value<'a, Ext>) -> Self {
        match value {
            Value::External(value) => Handle::External(value),
            Value::Primitive(primitive) => Handle::Primitive(primitive.into()),
            Value::Tuple(elements) if elements.is_empty() => Handle::Unit,
            _ => Handle::Value(ValueHandle(value)),
        }
    }
}

impl<'a, Ext> From<Handle<'a, Ext>> for Value<'a, Ext> {
    fn from(primitive: Handle<'a, Ext>) -> Self {
        match primitive {
            Handle::External(ext) => Value::External(ext),
            Handle::Primitive(primitive) => Value::Primitive(primitive.into()),
            Handle::Unit => Value::Tuple(Box::new([])),
            Handle::Value(ValueHandle(value)) => value,
        }
    }
}

impl<'a, Ext> From<Primitive<Value<'a, Ext>>> for Primitive<Handle<'a, Ext>> {
    fn from(primitive: Primitive<Value<'a, Ext>>) -> Self {
        match primitive {
            Primitive::Number(number) => Primitive::Number(number),
            Primitive::String(string) => Primitive::String(string),
            Primitive::List(elements) => {
                Primitive::List(elements.into_iter().map(Handle::from).collect())
            }
        }
    }
}

impl<'a, Ext> From<Primitive<Handle<'a, Ext>>> for Primitive<Value<'a, Ext>> {
    fn from(primitive: Primitive<Handle<'a, Ext>>) -> Self {
        match primitive {
            Primitive::Number(number) => Primitive::Number(number),
            Primitive::String(string) => Primitive::String(string),
            Primitive::List(elements) => {
                Primitive::List(elements.into_iter().map(Value::from).collect())
            }
        }
    }
}

impl<'a, Ext: Clone> From<&Value<'a, Ext>> for DebugTree<Ext> {
    fn from(value: &Value<'a, Ext>) -> Self {
        match value {
            Value::External(ext) => DebugTree::External(ext.clone()),
            Value::Primitive(primitive) => match primitive {
                Primitive::Number(number) => DebugTree::Value {
                    representation: format!("{number}"),
                    children: Vec::new(),
                },
                Primitive::String(string) => DebugTree::Value {
                    representation: format!("{string:?}"),
                    children: Vec::new(),
                },
                Primitive::List(items) => DebugTree::Value {
                    representation: String::from("list"),
                    children: items.iter().map(|item| (None, item.into())).collect(),
                },
            },
            Value::Closure(..) => DebugTree::Value {
                representation: String::from("closure"),
                children: Vec::new(),
            },
            Value::Structure(fields) => DebugTree::Value {
                representation: String::from("structure"),
                children: fields
                    .iter()
                    .map(|(field, value)| {
                        (
                            field.name.as_deref().map(|name| name.to_string()),
                            value.into(),
                        )
                    })
                    .collect(),
            },
            Value::Tuple(values) => DebugTree::Value {
                representation: String::from("tuple"),
                children: values.iter().map(|value| (None, value.into())).collect(),
            },
            Value::Marker => DebugTree::Value {
                representation: String::from("marker"),
                children: Vec::new(),
            },
            Value::Variant((variant, variant_debug_info), values) => DebugTree::Value {
                representation: variant_debug_info
                    .name
                    .as_deref()
                    .map_or_else(|| format!("variant {variant}"), |name| name.to_string()),
                children: values.iter().map(|value| (None, value.into())).collect(),
            },
        }
    }
}
