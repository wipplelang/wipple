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
    Closure(Label, Rc<Scope>),
    Variant(usize, Vec<Value>),
    Mutable(Rc<RefCell<Value>>),
    List(im::Vector<Value>),
    Structure(Vec<Value>),
    Tuple(Vec<Value>),
}

type Scope = BTreeMap<VariableId, Value>;

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

    fn with(value: Value) -> Self {
        Stack(vec![value])
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
}

impl<'a> Interpreter<'a> {
    pub fn run(&self, program: &ir::Program) -> Result<(), Error> {
        let mut info = Info {
            program,
            initialized_constants: Default::default(),
        };

        self.sub(&program.entrypoint, Stack::new(), Scope::new(), &mut info)?;

        Ok(())
    }

    fn sub(
        &self,
        label: &Label,
        stack: Stack,
        mut scope: Scope,
        info: &mut Info,
    ) -> Result<Value, Error> {
        let statements = info.program.labels.get(label).unwrap();
        self.evaluate(statements, stack, &mut scope, info)
    }

    fn evaluate(
        &self,
        statements: &[ir::Statement],
        mut stack: Stack,
        scope: &mut Scope,
        info: &mut Info,
    ) -> Result<Value, Error> {
        for statement in statements {
            match statement {
                ir::Statement::Copy => {
                    stack.copy();
                }
                ir::Statement::Drop => {
                    stack.pop();
                }
                ir::Statement::Initialize(var) => {
                    scope.insert(*var, stack.pop());
                }
                ir::Statement::Goto(label, _) => {
                    let result = self.sub(label, Stack::new(), scope.clone(), info)?;
                    stack.push(result);
                }
                ir::Statement::Sub(label, _) => {
                    let result = self.sub(label, Stack::with(stack.pop()), scope.clone(), info)?;
                    stack.push(result);
                }
                ir::Statement::If(then_label, else_label) => {
                    let label = match stack.pop() {
                        Value::Variant(1, values) if values.is_empty() => then_label,
                        Value::Variant(0, values) if values.is_empty() => else_label,
                        _ => unreachable!(),
                    };

                    let result = self.sub(label, Stack::with(stack.pop()), scope.clone(), info)?;
                    stack.push(result);
                }
                ir::Statement::Marker => stack.push(Value::Marker),
                ir::Statement::Closure(label) => {
                    stack.push(Value::Closure(*label, Rc::new(scope.clone())));
                }
                ir::Statement::Variable(var) => stack.push(scope.get(var).unwrap().clone()),
                ir::Statement::Constant(label) => {
                    if let Some(value) = info.initialized_constants.get(label) {
                        stack.push(value.clone());
                    } else {
                        let value = self.sub(label, Stack::new(), scope.clone(), info)?;
                        stack.push(value);
                    }
                }
                ir::Statement::Number(number) => stack.push(Value::Number(*number)),
                ir::Statement::Integer(integer) => stack.push(Value::Integer(*integer)),
                ir::Statement::Natural(natural) => stack.push(Value::Natural(*natural)),
                ir::Statement::Byte(byte) => stack.push(Value::Byte(*byte)),
                ir::Statement::Signed(signed) => stack.push(Value::Signed(*signed)),
                ir::Statement::Unsigned(unsigned) => stack.push(Value::Unsigned(*unsigned)),
                ir::Statement::Float(float) => stack.push(Value::Float(*float)),
                ir::Statement::Double(double) => stack.push(Value::Double(*double)),
                ir::Statement::Text(text) => stack.push(Value::Text(Rc::from(text.as_str()))),
                ir::Statement::Call(_) => {
                    let input = stack.pop();

                    let (label, scope) = match stack.pop() {
                        Value::Closure(label, scope) => (label, scope),
                        _ => unreachable!(),
                    };

                    let result = self.sub(&label, Stack::with(input), (*scope).clone(), info)?;
                    stack.push(result);
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
                ir::Statement::CompareDiscriminants(other) => {
                    let discriminant = match stack.pop() {
                        Value::Variant(discriminant, _) => discriminant,
                        _ => unreachable!(),
                    };

                    stack.push(Value::Variant(
                        (discriminant == *other) as usize,
                        Vec::new(),
                    ));
                }
            }
        }

        Ok(stack.pop())
    }
}
