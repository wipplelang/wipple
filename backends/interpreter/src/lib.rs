#![allow(clippy::type_complexity)]

mod runtime;

use std::{cell::RefCell, collections::BTreeMap, rc::Rc};
use wipple_compiler::{
    ir::{abi, Expression, Program, Reference, SectionIndex, Statement, Terminator},
    VariableId,
};

type Error = String;

#[derive(Default)]
pub struct Interpreter<'a> {
    output: Option<Rc<RefCell<Box<dyn FnMut(&str) + 'a>>>>,
}

impl<'a> Interpreter<'a> {
    pub fn handling_output(output: impl FnMut(&str) + 'a) -> Self {
        Interpreter {
            output: Some(Rc::new(RefCell::new(Box::new(output) as Box<_>))),
        }
    }

    pub fn ignoring_output() -> Self {
        Interpreter { output: None }
    }
}

#[derive(Debug, Clone)]
enum Value {
    Marker,
    Number(f64),
    Text(Rc<str>),
    Function(usize, Rc<Scope>),
    Variant(usize, Box<[Value]>),
    Mutable(Rc<RefCell<Value>>),
    List(im::Vector<Value>),
}

type Scope = BTreeMap<VariableId, Value>;

impl<'a> Interpreter<'a> {
    pub fn run(&self, program: &Program) -> Result<(), Error> {
        let entrypoint = program.functions.len() - 1;
        self.call(entrypoint, None, program, BTreeMap::new())?;
        Ok(())
    }

    fn call(
        &self,
        function: usize,
        input: Option<&Value>,
        program: &Program,
        mut scope: Scope,
    ) -> Result<Value, Error> {
        let function = &program.functions[function];

        let mut section_index = SectionIndex(0);
        let mut computations = BTreeMap::new();

        loop {
            let section = &function.sections[section_index];

            for statement in &section.statements {
                match statement {
                    Statement::Compute(id, expr) => {
                        let value = match expr {
                            Expression::Marker => Value::Marker,
                            Expression::Reference(reference) => match reference {
                                Reference::Constant(_) => todo!(),
                                Reference::Variable(var) => scope.get(var).unwrap().clone(),
                                Reference::FunctionInput => input.unwrap().clone(),
                            },
                            Expression::Function(function) => {
                                let captures = program
                                    .functions
                                    .get(*function)
                                    .unwrap()
                                    .captures
                                    .iter()
                                    .map(|&var| (var, scope.get(&var).unwrap().clone()))
                                    .collect();

                                Value::Function(*function, Rc::new(captures))
                            }
                            Expression::Number(number) => Value::Number(*number),
                            Expression::Text(text) => Value::Text(Rc::from(text.as_str())),
                            Expression::Call(function, input) => {
                                let (function, captures) = match computations.get(function).unwrap()
                                {
                                    Value::Function(function, captures) => {
                                        (function, captures.as_ref().clone())
                                    }
                                    _ => unreachable!(),
                                };

                                let input = computations.get(input).unwrap();

                                self.call(*function, Some(input), program, captures)?
                            }
                            Expression::External(abi, identifier, inputs) => {
                                if abi.as_str() != abi::RUNTIME {
                                    return Err(Error::from("unknown ABI (only 'runtime' is supported in the interpreter)"));
                                }

                                let inputs = inputs
                                    .iter()
                                    .map(|id| computations.get(id).unwrap().clone())
                                    .collect();

                                self.call_runtime(identifier, inputs)?
                            }
                        };

                        computations.insert(*id, value);
                    }
                    Statement::Initialize(var, computation) => {
                        let value = computations.get(computation).unwrap().clone();
                        scope.insert(*var, value);
                    }
                }
            }

            match section.terminator() {
                Terminator::If(condition, then_section, else_section) => {
                    let condition = computations.get(condition).unwrap().clone();

                    let condition = match condition {
                        Value::Variant(n, _) => n == 1,
                        _ => unreachable!(),
                    };

                    section_index = if condition {
                        *then_section
                    } else {
                        *else_section
                    };
                }
                Terminator::Return(computation) => {
                    break Ok(computations.get(computation).unwrap().clone());
                }
                Terminator::Goto(index) => {
                    section_index = *index;
                }
                Terminator::Unreachable => unreachable!(),
            }
        }
    }
}
