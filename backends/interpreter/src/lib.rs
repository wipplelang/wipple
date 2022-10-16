mod runtime;

use std::{
    cell::RefCell,
    collections::BTreeMap,
    os::raw::{c_int, c_uint},
    rc::Rc,
};
use wipple_frontend::{ir, Label, VariableId};

type Error = String;

#[derive(Default)]
pub struct Interpreter<'a> {
    #[allow(clippy::type_complexity)]
    output: Option<Rc<RefCell<Box<dyn FnMut(&str) + 'a>>>>,
}

impl<'a> Interpreter<'a> {
    pub fn handling_output(output: impl FnMut(&str) + 'a) -> Self {
        Interpreter {
            output: Some(Rc::new(RefCell::new(Box::new(output)))),
        }
    }

    pub fn ignoring_output() -> Self {
        Interpreter { output: None }
    }
}

#[derive(Debug, Clone)]
enum Value {
    Marker,
    Number(rust_decimal::Decimal),
    Integer(i64),
    Natural(u64),
    Byte(u8),
    Signed(c_int),
    Unsigned(c_uint),
    Float(f32),
    Double(f64),
    Text(Rc<str>),
    Function(Label),
    Closure(Label, Rc<Scope>),
    Variant(usize, Vec<Value>),
    Mutable(Rc<RefCell<Value>>),
    List(im::Vector<Value>),
    Structure(Vec<Value>),
    Tuple(Vec<Value>),
}

#[derive(Debug, Clone, Default)]
struct Scope {
    vars: RefCell<BTreeMap<usize, Value>>,
    parent: Option<Rc<Scope>>,
}

impl Scope {
    fn new() -> Rc<Self> {
        Default::default()
    }

    fn child(self: &Rc<Self>) -> Rc<Self> {
        Rc::new(Scope {
            vars: Default::default(),
            parent: Some(self.clone()),
        })
    }

    fn get(&self, var: VariableId) -> Value {
        let mut parent = Some(self);

        while let Some(scope) = parent {
            if let Some(value) = scope.vars.borrow().get(&var.counter) {
                return value.clone();
            }

            parent = scope.parent.as_deref();
        }

        panic!("uninitialized variable {:?}", var)
    }

    fn set(&self, var: VariableId, value: Value) {
        self.vars.borrow_mut().insert(var.counter, value);
    }
}

struct Info<'a> {
    program: &'a ir::Program,
    initialized_constants: BTreeMap<Label, Value>,
}

#[derive(Debug, Default)]
struct Stack(Vec<Value>);

impl Stack {
    fn new() -> Self {
        Stack(Vec::new())
    }

    fn push(&mut self, value: Value) {
        self.0.push(value);
    }

    fn copy(&mut self) {
        self.0.push(self.0.last().expect("stack is empty").clone());
    }

    fn pop(&mut self) -> Value {
        self.0.pop().expect("stack is empty")
    }

    fn popn(&mut self, n: usize) -> Vec<Value> {
        (0..n)
            .map(|_| self.pop())
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .collect()
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<'a> Interpreter<'a> {
    pub fn run(&self, program: &ir::Program) -> Result<(), Error> {
        let mut info = Info {
            program,
            initialized_constants: Default::default(),
        };

        let mut stack = Stack::new();

        self.evaluate_label(
            &program.entrypoint,
            &mut stack,
            &mut Scope::new(),
            &mut info,
        )?;

        assert!(stack.is_empty());

        Ok(())
    }

    fn evaluate_label(
        &self,
        label: &Label,
        stack: &mut Stack,
        scope: &mut Rc<Scope>,
        info: &mut Info,
    ) -> Result<Value, Error> {
        let statements = &info.program.labels.get(label).unwrap().1;
        self.evaluate(statements, stack, scope, info)
    }

    fn evaluate(
        &self,
        statements: &[ir::Statement],
        stack: &mut Stack,
        scope: &mut Rc<Scope>,
        info: &mut Info,
    ) -> Result<Value, Error> {
        let mut statements = statements.iter();

        while let Some(statement) = statements.next() {
            match statement {
                ir::Statement::Comment(_) => {}
                ir::Statement::Begin => {
                    *scope = scope.child();
                }
                ir::Statement::End => {
                    *scope = scope.parent.as_ref().unwrap().clone();
                }
                ir::Statement::Copy => {
                    stack.copy();
                }
                ir::Statement::Drop => {
                    stack.pop();
                }
                ir::Statement::Jump(label) => {
                    statements = info.program.labels.get(label).unwrap().1.iter();
                }
                ir::Statement::If(matching_discriminant, label) => {
                    if let Value::Variant(discriminant, _) = stack.pop() {
                        if discriminant == *matching_discriminant {
                            statements = info.program.labels.get(label).unwrap().1.iter();
                        }
                    }
                }
                ir::Statement::Unreachable => unreachable!(),
                ir::Statement::Initialize(var) => scope.set(*var, stack.pop()),
                ir::Statement::Value(value) => {
                    let value = match value {
                        ir::Value::Marker => Value::Marker,
                        ir::Value::Text(text) => Value::Text(Rc::from(text.as_str())),
                        ir::Value::Number(number) => Value::Number(*number),
                        ir::Value::Integer(integer) => Value::Integer(*integer),
                        ir::Value::Natural(natural) => Value::Natural(*natural),
                        ir::Value::Byte(byte) => Value::Byte(*byte),
                        ir::Value::Signed(signed) => Value::Signed(*signed),
                        ir::Value::Unsigned(unsigned) => Value::Unsigned(*unsigned),
                        ir::Value::Float(float) => Value::Float(*float),
                        ir::Value::Double(double) => Value::Double(*double),
                        ir::Value::Variable(var) => scope.get(*var),
                        ir::Value::Constant(label) => {
                            if let Some(value) = info.initialized_constants.get(label) {
                                value.clone()
                            } else {
                                self.evaluate_label(label, stack, &mut scope.child(), info)?
                            }
                        }
                        ir::Value::Function(label) => Value::Function(*label),
                        ir::Value::Closure(label) => Value::Closure(*label, scope.clone()),
                    };

                    stack.push(value);
                }
                ir::Statement::Call => {
                    let input = stack.pop();

                    let (label, scope) = match stack.pop() {
                        Value::Function(label) => (label, Scope::new()),
                        Value::Closure(label, scope) => (label, scope),
                        _ => unreachable!(),
                    };

                    stack.push(input);
                    let result = self.evaluate_label(&label, stack, &mut scope.child(), info)?;
                    stack.push(result);
                }
                ir::Statement::TailCall => {
                    let input = stack.pop();

                    let (label, scope) = match stack.pop() {
                        Value::Function(label) => (label, None),
                        Value::Closure(label, scope) => (label, Some(scope)),
                        _ => unreachable!(),
                    };

                    stack.push(input);

                    if let Some(scope) = scope {
                        let result =
                            self.evaluate_label(&label, stack, &mut scope.child(), info)?;
                        stack.push(result);
                    } else {
                        statements = info.program.labels.get(&label).unwrap().1.iter();
                    }
                }
                ir::Statement::External(abi, identifier, inputs) => {
                    let inputs = stack.popn(*inputs);

                    if abi.as_str() != ir::abi::RUNTIME {
                        return Err(Error::from(
                            "unknown ABI (only 'runtime' is supported in the interpreter)",
                        ));
                    }

                    let result = self.call_runtime(identifier, inputs)?;
                    stack.push(result);
                }
                ir::Statement::Tuple(inputs) => {
                    let inputs = stack.popn(*inputs);
                    stack.push(Value::Tuple(inputs));
                }
                ir::Statement::Structure(inputs) => {
                    let inputs = stack.popn(*inputs);
                    stack.push(Value::Structure(inputs));
                }
                ir::Statement::Variant(discriminant, inputs) => {
                    let inputs = stack.popn(*inputs);
                    stack.push(Value::Variant(*discriminant, inputs));
                }
                ir::Statement::TupleElement(index) => {
                    let tuple = match stack.pop() {
                        Value::Tuple(tuple) => tuple,
                        _ => unreachable!(),
                    };

                    stack.push(tuple[*index].clone());
                }
                ir::Statement::StructureElement(index) => {
                    let structure = match stack.pop() {
                        Value::Structure(structure) => structure,
                        _ => unreachable!(),
                    };

                    stack.push(structure[*index].clone());
                }
                ir::Statement::VariantElement(_, index) => {
                    let variant = match stack.pop() {
                        Value::Variant(_, variant) => variant,
                        _ => unreachable!(),
                    };

                    stack.push(variant[*index].clone());
                }
            }
        }

        Ok(stack.pop())
    }
}
