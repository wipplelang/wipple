#[cfg(target_arch = "wasm32")]
use getrandom as _;

use std::{
    borrow::Borrow,
    cell::{RefCell, RefMut},
    cmp,
    collections::hash_map::DefaultHasher,
    fmt::Debug,
    hash::{Hash, Hasher},
    rc::Rc,
    sync::Arc,
};
use unicode_segmentation::UnicodeSegmentation;
use wipple_core::codegen::mir;

pub use wipple_core::span::Span;

pub struct Interpreter<'ctx, Ext, Err> {
    external: Box<dyn for<'a> FnMut(&str, Handle<'a, Ext>) -> Result<Handle<'a, Ext>, Err> + 'ctx>,
    debugger: Option<Debugger<'ctx, Ext, Err>>,
}

pub struct Debugger<'ctx, Ext, Err> {
    debug: Box<dyn FnMut(DebugEvent<'_, Ext>) -> Result<(), Err> + 'ctx>,
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

impl<'ctx, Ext, Err> Interpreter<'ctx, Ext, Err> {
    pub fn new(
        external: impl for<'a> FnMut(&str, Handle<'a, Ext>) -> Result<Handle<'a, Ext>, Err> + 'ctx,
    ) -> Self {
        Interpreter {
            external: Box::new(external),
            debugger: None,
        }
    }

    pub fn with_debugger(mut self, debugger: Debugger<'ctx, Ext, Err>) -> Self {
        self.debugger = Some(debugger);
        self
    }
}

impl<'ctx, Ext: Debug + Clone, Err> Interpreter<'ctx, Ext, Err> {
    pub fn run(mut self, program: &mir::Program) -> Result<(), Err> {
        if let Some(main) = program.main {
            let function = &program.functions[main];
            self.run_function(program, function, Vec::new(), Vec::new())?;
        }

        Ok(())
    }
}

impl<'ctx, Ext, Err> Debugger<'ctx, Ext, Err> {
    pub fn new(debug: impl FnMut(DebugEvent<'_, Ext>) -> Result<(), Err> + 'ctx) -> Self {
        Debugger {
            debug: Box::new(debug),
        }
    }
}

#[derive(Debug, Clone)]
enum Value<'a, Ext> {
    External(Ext),
    Primitive(Primitive<Self>),
    Closure(&'a mir::Function, Locals<'a, Ext>),
    Tuple(Box<[Self]>),
    Marker,
    Variant(usize, Box<[Self]>),
}

type Locals<'a, Ext> = Vec<Rc<RefCell<Option<Value<'a, Ext>>>>>;

#[derive(Debug)]
enum ControlFlow<'a, Ext> {
    Break,
    Return(Value<'a, Ext>),
}

impl<'ctx, Ext: Debug + Clone, Err> Interpreter<'ctx, Ext, Err> {
    fn run_function<'a>(
        &mut self,
        program: &'a mir::Program,
        function: &'a mir::Function,
        captures: Locals<'a, Ext>,
        inputs: Vec<Value<'a, Ext>>,
    ) -> Result<Option<Value<'a, Ext>>, Err> {
        let mut function_locals = captures;

        function_locals.extend(
            inputs
                .into_iter()
                .map(|input| Rc::new(RefCell::new(Some(input)))),
        );

        function_locals.resize_with(
            function_locals.len() + function.locals.len(),
            Default::default,
        );

        match self.run_statements(program, &function.body, &function_locals)? {
            None => Ok(None),
            Some(ControlFlow::Break) => panic!("break outside loop"),
            Some(ControlFlow::Return(value)) => Ok(Some(value)),
        }
    }

    fn run_statements<'a>(
        &mut self,
        program: &'a mir::Program,
        statements: &'a [mir::Statement],
        locals: &Locals<'a, Ext>,
    ) -> Result<Option<ControlFlow<'a, Ext>>, Err> {
        for statement in statements {
            match statement {
                mir::Statement::If {
                    branches,
                    else_branch,
                } => {
                    let mut else_branch = else_branch.as_ref();
                    for (condition, body) in branches {
                        if self.run_condition(program, condition, locals)? {
                            if let Some(control_flow) =
                                self.run_statements(program, body, locals)?
                            {
                                return Ok(Some(control_flow));
                            } else {
                                else_branch = None;
                                break;
                            }
                        }
                    }

                    if let Some(else_branch) = else_branch
                        && let Some(control_flow) =
                            self.run_statements(program, else_branch, locals)?
                    {
                        return Ok(Some(control_flow));
                    }
                }
                mir::Statement::Return { value } => {
                    let value = self.run_expression(program, value, locals)?;
                    return Ok(Some(ControlFlow::Return(value)));
                }
                mir::Statement::Loop { body } => loop {
                    match self.run_statements(program, body, locals)? {
                        None => continue,
                        Some(ControlFlow::Break) => break,
                        Some(ControlFlow::Return(value)) => {
                            return Ok(Some(ControlFlow::Return(value)));
                        }
                    }
                },
                mir::Statement::Break => return Ok(Some(ControlFlow::Break)),
                mir::Statement::Assign { local, value } => {
                    let value = self.run_expression(program, value, locals)?;
                    set_local(locals, *local, value);
                }
                mir::Statement::Trace { span } => {
                    if let Some(debugger) = &mut self.debugger {
                        (debugger.debug)(DebugEvent::Span(span.clone()))?;
                    }
                }
            }
        }

        Ok(None)
    }

    fn run_condition<'a>(
        &mut self,
        program: &'a mir::Program,
        condition: &'a mir::Condition,
        locals: &Locals<'a, Ext>,
    ) -> Result<bool, Err> {
        Ok(match condition {
            mir::Condition::True => true,
            mir::Condition::False => false,
            mir::Condition::And { left, right } => {
                self.run_condition(program, left, locals)?
                    && self.run_condition(program, right, locals)?
            }
            mir::Condition::Or { left, right } => {
                self.run_condition(program, left, locals)?
                    || self.run_condition(program, right, locals)?
            }
            mir::Condition::Variant { value, variant } => {
                let Value::Variant(index, ..) = self.run_expression(program, value, locals)? else {
                    panic!("not a variant");
                };

                *variant == index
            }
            mir::Condition::Initialize { local, value } => {
                let value = self.run_expression(program, value, locals)?;
                set_local(locals, *local, value);
                true
            }
            mir::Condition::Mutate { local, value } => {
                let value = get_local(locals, *value).clone();
                set_local(locals, *local, value);
                true
            }
        })
    }

    fn run_expression<'a, E: Borrow<mir::Expression>>(
        &mut self,
        program: &'a mir::Program,
        expression: &'a mir::SourceMapped<E>,
        locals: &Locals<'a, Ext>,
    ) -> Result<Value<'a, Ext>, Err> {
        Ok(match expression.inner.borrow() {
            mir::Expression::Function { index, bounds } => {
                let function = &program.functions[*index];

                let captures = function
                    .captures
                    .iter()
                    .map(|&capture| locals[capture].clone())
                    .collect::<Vec<_>>();

                let inputs = bounds
                    .iter()
                    .map(|bound| get_local(locals, *bound).clone())
                    .collect::<Vec<_>>();

                self.run_function(program, function, captures, inputs)?
                    .expect("missing return value")
            }
            mir::Expression::Call { function, inputs } => {
                let Value::Closure(function, ref captures) = *get_local(locals, *function) else {
                    panic!("not a closure");
                };

                let inputs = inputs
                    .iter()
                    .map(|input| get_local(locals, *input).clone())
                    .collect::<Vec<_>>();

                self.run_function(program, function, captures.clone(), inputs)?
                    .expect("missing return value")
            }
            mir::Expression::Closure(function) => {
                let captures = function
                    .captures
                    .iter()
                    .map(|&capture| locals[capture].clone())
                    .collect::<Vec<_>>();

                Value::Closure(function, captures)
            }
            mir::Expression::Element { value, index } => {
                let Value::Tuple(ref elements) = *get_local(locals, *value) else {
                    panic!("not a list");
                };

                elements[*index].clone()
            }
            mir::Expression::Tuple { elements } => {
                let elements = elements
                    .iter()
                    .map(|element| get_local(locals, *element).clone())
                    .collect::<Vec<_>>();

                Value::Tuple(elements.into_boxed_slice())
            }
            mir::Expression::Marker => Value::Marker,
            mir::Expression::Local { local } | mir::Expression::MutableLocal { local } => {
                get_local(locals, *local).clone()
            }
            mir::Expression::Number { value } => {
                let number = value.parse().expect("invalid number");
                Value::Primitive(Primitive::Number(number))
            }
            mir::Expression::Intrinsic { intrinsic } => {
                self.run_intrinsic(program, intrinsic, locals)?
            }
            mir::Expression::String { value } => {
                Value::Primitive(Primitive::String(Arc::from(value.as_str())))
            }
            mir::Expression::Structure { fields } => {
                let mut elements = vec![None; fields.len()];
                for (index, field) in fields {
                    let value = get_local(locals, *field).clone();
                    elements[*index].replace(value);
                }

                let elements = elements
                    .into_iter()
                    .map(|value| value.expect("uninitialized field"))
                    .collect::<Vec<_>>();

                Value::Tuple(elements.into_boxed_slice())
            }
            mir::Expression::Variant { variant, elements } => {
                let elements = elements
                    .iter()
                    .map(|element| get_local(locals, *element).clone())
                    .collect::<Vec<_>>();

                Value::Variant(*variant, elements.into_boxed_slice())
            }
            mir::Expression::VariantElement {
                value,
                variant,
                index,
            } => {
                let Value::Variant(v, ref elements) = *get_local(locals, *value) else {
                    panic!("not a variant");
                };

                if v != *variant {
                    panic!("expected variant {variant:?}, found variant {v:?}");
                }

                elements[*index].clone()
            }
        })
    }

    fn run_intrinsic<'a>(
        &mut self,
        program: &'a mir::Program,
        intrinsic: &'a mir::Intrinsic<mir::SourceMapped<Box<mir::Expression>>>,
        locals: &Locals<'a, Ext>,
    ) -> Result<Value<'a, Ext>, Err> {
        macro_rules! eval {
            ($value:expr) => {
                self.run_expression(program, $value, locals)?
            };
            ($value:expr, Primitive::$kind:ident) => {
                match eval!($value) {
                    Value::Primitive(Primitive::$kind(x)) => (x),
                    value => panic!("expected {}, but found {value:?}", stringify!($kind)),
                }
            };
        }

        Ok(match intrinsic {
            mir::Intrinsic::Debug { value } => {
                let value = eval!(value);

                if let Some(debugger) = &mut self.debugger {
                    (debugger.debug)(DebugEvent::Value(value.clone().into()))?;
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
                true_variant,
                false_variant,
            } => {
                let left = eval!(left, Primitive::Number);
                let right = eval!(right, Primitive::Number);

                if left == right {
                    Value::Variant(*true_variant, Box::new([]))
                } else {
                    Value::Variant(*false_variant, Box::new([]))
                }
            }
            mir::Intrinsic::StringEqual {
                left,
                right,
                true_variant,
                false_variant,
            } => {
                let left = eval!(left, Primitive::String);
                let right = eval!(right, Primitive::String);

                if left == right {
                    Value::Variant(*true_variant, Box::new([]))
                } else {
                    Value::Variant(*false_variant, Box::new([]))
                }
            }
            mir::Intrinsic::Order {
                left,
                right,
                is_less_than_variant,
                is_equal_variant,
                is_greater_than_variant,
            } => {
                let left = eval!(left, Primitive::Number);
                let right = eval!(right, Primitive::Number);

                match left.total_cmp(&right) {
                    cmp::Ordering::Less => Value::Variant(*is_less_than_variant, Box::new([])),
                    cmp::Ordering::Equal => Value::Variant(*is_equal_variant, Box::new([])),
                    cmp::Ordering::Greater => {
                        Value::Variant(*is_greater_than_variant, Box::new([]))
                    }
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
                true_variant,
                false_variant,
            } => {
                let value = eval!(value, Primitive::Number);

                if value.is_nan() {
                    Value::Variant(*true_variant, Box::new([]))
                } else {
                    Value::Variant(*false_variant, Box::new([]))
                }
            }
            mir::Intrinsic::HashString { value } => {
                let string = eval!(value, Primitive::String);

                let mut hasher = DefaultHasher::new();
                string.hash(&mut hasher);
                let hash = hasher.finish();

                Value::Primitive(Primitive::Number(hash as f64))
            }
            mir::Intrinsic::Unreachable => unreachable!(),
        })
    }
}

fn get_local<'a, 'l, Ext: Debug>(
    locals: &'l Locals<'a, Ext>,
    index: usize,
) -> RefMut<'l, Value<'a, Ext>> {
    RefMut::map(locals[index].borrow_mut(), |local| {
        local
            .as_mut()
            .unwrap_or_else(|| panic!("local {index:?} is uninitialized"))
    })
}

fn set_local<'a, Ext>(locals: &Locals<'a, Ext>, index: usize, value: Value<'a, Ext>) {
    locals[index].replace(Some(value));
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
