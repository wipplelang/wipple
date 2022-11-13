mod runtime;

use std::{
    cell::RefCell,
    os::raw::{c_int, c_uint},
    rc::Rc,
};
use wipple_frontend::ir;

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
    Function(Scope, usize),
    Variant(usize, Vec<Value>),
    Mutable(Rc<RefCell<Value>>),
    List(im::Vector<Value>),
    Structure(Vec<Value>),
    Tuple(Vec<Value>),
}

#[derive(Debug, Clone, Default)]
struct Scope(Vec<Option<Value>>);

impl Scope {
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

struct Info {
    labels: Vec<(usize, Vec<ir::BasicBlock>)>,
    initialized_constants: Vec<RefCell<Option<Value>>>,
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
        self.push(self.0.last().expect("stack is empty").clone());
    }

    fn pop(&mut self) -> Value {
        self.0.pop().expect("stack is empty")
    }

    fn popn(&mut self, n: usize) -> Vec<Value> {
        let mut values = vec![Value::Marker; n];

        for index in (0..n).rev() {
            values[index] = self.pop();
        }

        values
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<'a> Interpreter<'a> {
    pub fn run(&mut self, program: &ir::Program) -> Result<(), Error> {
        let info = Info {
            labels: program
                .labels
                .iter()
                .map(|(_, vars, blocks)| (*vars, blocks.clone()))
                .collect(),
            initialized_constants: vec![Default::default(); program.labels.len()],
        };

        let mut stack = Stack::new();

        self.evaluate_label(program.entrypoint, &mut stack, &info)?;

        stack.pop();
        assert!(stack.is_empty());

        Ok(())
    }

    fn evaluate_label(
        &mut self,
        label: usize,
        stack: &mut Stack,
        info: &Info,
    ) -> Result<(), Error> {
        self.evaluate_label_inner(label, stack, Scope::new(0), info)
    }

    fn evaluate_label_in_scope(
        &mut self,
        label: usize,
        stack: &mut Stack,
        scope: Scope,
        info: &Info,
    ) -> Result<(), Error> {
        self.evaluate_label_inner(label, stack, scope, info)
    }

    fn evaluate_label_inner(
        &mut self,
        label: usize,
        stack: &mut Stack,
        mut scope: Scope,
        info: &Info,
    ) -> Result<(), Error> {
        let (vars, blocks) = &info.labels[label];

        scope.0.reserve(*vars);
        for _ in 0..*vars {
            scope.0.push(None);
        }

        self.evaluate(blocks, stack, &mut scope, info)
    }

    fn evaluate(
        &mut self,
        blocks: &[ir::BasicBlock],
        stack: &mut Stack,
        scope: &mut Scope,
        info: &Info,
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
                    ir::Statement::Initialize(var) => {
                        scope.set(*var, stack.pop());
                    }
                    ir::Statement::Free(var) => {
                        scope.free(*var);
                    }
                    ir::Statement::Unpack(_) => {
                        // The interpreter provides the captured scope while calling the functions
                    }
                    ir::Statement::Expression(_, expr) => match expr {
                        ir::Expression::Marker => stack.push(Value::Marker),
                        ir::Expression::Text(text) => {
                            stack.push(Value::Text(Rc::from(text.as_str())))
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
                            self.evaluate_constant(*label, stack, info)?
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

                            let (scope, label) = match stack.pop() {
                                Value::Function(scope, label) => (scope, label),
                                _ => unreachable!(),
                            };

                            stack.push(input);

                            self.evaluate_label_in_scope(label, stack, scope, info)?;
                        }
                        ir::Expression::External(..) => {
                            return Err(Error::from("'external' is currently unsupported"));
                        }
                        ir::Expression::Runtime(func, inputs) => {
                            let inputs = stack.popn(*inputs);
                            stack.push(self.call_runtime(*func, inputs)?);
                        }
                        ir::Expression::Tuple(inputs) => {
                            let inputs = stack.popn(*inputs);
                            stack.push(Value::Tuple(inputs));
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

                            stack.push(structure[*index].clone());
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

    fn evaluate_constant(
        &mut self,
        label: usize,
        stack: &mut Stack,
        info: &Info,
    ) -> Result<(), Error> {
        let constant = &mut *info.initialized_constants[label].borrow_mut();

        if let Some(value) = constant {
            stack.push(value.clone());
        } else {
            self.evaluate_label(label, stack, info)?;
            let value = stack.pop();
            *constant = Some(value.clone());
            stack.push(value);
        }

        Ok(())
    }
}
