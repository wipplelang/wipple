mod runtime;

use std::{
    cell::RefCell,
    mem,
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
    Function(usize),
    Closure(Scope, usize),
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
    const SIZE: usize = 1024 * 1024; // 1 MB

    fn new() -> Self {
        Stack(Vec::with_capacity(Self::SIZE / mem::size_of::<Value>()))
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

    fn evaluate_label(&self, label: usize, stack: &mut Stack, info: &Info) -> Result<(), Error> {
        self.evaluate_label_inner(label, stack, Scope::new(0), info)
    }

    fn evaluate_label_in_scope(
        &self,
        label: usize,
        stack: &mut Stack,
        scope: Scope,
        info: &Info,
    ) -> Result<(), Error> {
        self.evaluate_label_inner(label, stack, scope, info)
    }

    fn evaluate_label_inner(
        &self,
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

    #[inline(never)] // FIXME: REMOVE ONCE DONE PROFILING
    fn evaluate(
        &self,
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
                            let constant = &mut *info.initialized_constants[*label].borrow_mut();
                            if let Some(value) = constant {
                                stack.push(value.clone());
                            } else {
                                self.evaluate_label(*label, stack, info)?;
                                let value = stack.pop();
                                *constant = Some(value.clone());
                                stack.push(value);
                            }
                        }
                        ir::Expression::Function(label) => stack.push(Value::Function(*label)),
                        ir::Expression::Closure(captures, label) => {
                            let mut closure_scope = Scope::new(captures.0.len());
                            for (captured, var) in &captures.0 {
                                closure_scope.set(*var, scope.get(*captured));
                            }

                            stack.push(Value::Closure(closure_scope, *label));
                        }
                        ir::Expression::Call => {
                            let input = stack.pop();

                            let (scope, label) = match stack.pop() {
                                Value::Function(label) => (Scope::new(0), label),
                                Value::Closure(scope, label) => (scope, label),
                                _ => unreachable!(),
                            };

                            stack.push(input);

                            self.evaluate_label_in_scope(label, stack, scope, info)?;
                        }
                        ir::Expression::External(abi, identifier, inputs) => {
                            let inputs = stack.popn(*inputs);

                            if abi.as_str() != ir::abi::RUNTIME {
                                return Err(Error::from(
                                    "unknown ABI (only 'runtime' is supported in the interpreter)",
                                ));
                            }

                            stack.push(self.call_runtime(identifier, inputs)?);
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
}
