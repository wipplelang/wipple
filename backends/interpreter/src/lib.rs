mod runtime;

use std::{cell::RefCell, collections::BTreeMap, rc::Rc};
use wipple_frontend::{
    ir::{abi, Expression, Program, SectionIndex, Statement, Terminator},
    VariableId,
};

type Error = String;

#[derive(Default)]
pub struct Interpreter<'a> {
    #[allow(clippy::type_complexity)]
    output: Option<Rc<RefCell<Box<dyn FnMut(&str) + 'a>>>>,

    #[cfg(debug_assertions)]
    trace_ir: bool,
}

impl<'a> Interpreter<'a> {
    pub fn handling_output(output: impl FnMut(&str) + 'a) -> Self {
        Interpreter {
            output: Some(Rc::new(RefCell::new(Box::new(output) as Box<_>))),

            #[cfg(debug_assertions)]
            trace_ir: false,
        }
    }

    pub fn ignoring_output() -> Self {
        Interpreter {
            output: None,

            #[cfg(debug_assertions)]
            trace_ir: false,
        }
    }

    #[cfg(debug_assertions)]
    pub fn tracing_ir(mut self, trace_ir: bool) -> Self {
        self.trace_ir = trace_ir;
        self
    }
}

#[derive(Debug, Clone)]
enum Value {
    Marker,
    Number(f64),
    Discriminant(usize),
    Text(Rc<str>),
    Function(usize, Rc<Scope>),
    Variant(usize, Box<[Value]>),
    Mutable(Rc<RefCell<Value>>),
    List(im::Vector<Value>),
}

type Scope = BTreeMap<VariableId, Value>;

struct Info<'a> {
    program: &'a Program,
    initialized_constants: BTreeMap<usize, Value>,
}

impl<'a> Interpreter<'a> {
    pub fn run(&self, program: &Program) -> Result<(), Error> {
        let mut info = Info {
            program,
            initialized_constants: Default::default(),
        };

        self.evaluate(
            &program.entrypoint,
            &mut info,
            &mut Scope::new(),
            None,
            false,
            #[cfg(debug_assertions)]
            0,
        )?;

        Ok(())
    }

    fn call(
        &self,
        function: usize,
        input: &Value,
        info: &mut Info,
        scope: &mut Scope,
        #[cfg(debug_assertions)] indent: usize,
    ) -> Result<Value, Error> {
        let sections = &info.program.functions[function].sections;

        let value = self
            .evaluate(
                sections,
                info,
                scope,
                Some(input),
                true,
                #[cfg(debug_assertions)]
                indent,
            )?
            .unwrap();

        Ok(value)
    }

    fn evaluate(
        &self,
        sections: &wipple_frontend::ir::Sections,
        info: &mut Info,
        scope: &mut Scope,
        input: Option<&Value>,
        expect_terminator: bool,
        #[cfg(debug_assertions)] indent: usize,
    ) -> Result<Option<Value>, String> {
        let mut section_index = SectionIndex(0);
        let mut computations = BTreeMap::new();

        macro_rules! log {
            ($fmt:literal $(,)? $($e:expr)*) => {
                #[cfg(debug_assertions)]
                if self.trace_ir {
                    eprintln!(concat!("{}", $fmt), "\t".repeat(indent), $($e)*);
                }
            };
        }

        loop {
            let section = &sections[section_index];

            log!("{}:", section_index);

            for statement in &section.statements {
                log!("\t{}", statement);

                match statement {
                    Statement::Compute(id, expr) => {
                        let value = match expr {
                            Expression::Marker => Value::Marker,
                            Expression::Constant(constant) => {
                                if let Some(value) = info.initialized_constants.get(constant) {
                                    value.clone()
                                } else {
                                    let sections = &info.program.constants[*constant].sections;

                                    let value = self
                                        .evaluate(
                                            sections,
                                            info,
                                            scope,
                                            None,
                                            true,
                                            #[cfg(debug_assertions)]
                                            indent,
                                        )?
                                        .unwrap();

                                    info.initialized_constants.insert(*constant, value.clone());

                                    value
                                }
                            }
                            Expression::Function(function) => {
                                let captures = info
                                    .program
                                    .functions
                                    .get(*function)
                                    .unwrap()
                                    .captures
                                    .iter()
                                    .map(|&var| (var, scope.get(&var).unwrap().clone()))
                                    .collect();

                                Value::Function(*function, Rc::new(captures))
                            }
                            Expression::Variable(var) => scope.get(var).unwrap().clone(),
                            Expression::FunctionInput => input.unwrap().clone(),
                            Expression::Number(number) => Value::Number(*number),
                            Expression::Text(text) => Value::Text(Rc::from(text.as_str())),
                            Expression::Call(function, input) => {
                                let (function, mut captures) =
                                    match computations.get(function).unwrap() {
                                        Value::Function(function, captures) => {
                                            (function, captures.as_ref().clone())
                                        }
                                        _ => unreachable!(),
                                    };

                                let input = computations.get(input).unwrap();

                                self.call(
                                    *function,
                                    input,
                                    info,
                                    &mut captures,
                                    #[cfg(debug_assertions)]
                                    (indent + 1),
                                )?
                            }
                            Expression::External(abi, identifier, inputs) => {
                                if abi.as_str() != abi::RUNTIME {
                                    return Err(Error::from("unknown ABI (only 'runtime' is supported in the interpreter)"));
                                }

                                self.call_runtime(
                                    identifier,
                                    inputs
                                        .iter()
                                        .map(|id| computations.get(id).unwrap().clone())
                                        .collect(),
                                )?
                            }
                            Expression::Tuple(elements) => Value::List(
                                elements
                                    .iter()
                                    .map(|id| computations.get(id).unwrap().clone())
                                    .collect(),
                            ),
                            Expression::Variant(discriminant, elements) => Value::Variant(
                                *discriminant,
                                elements
                                    .iter()
                                    .map(|id| computations.get(id).unwrap().clone())
                                    .collect::<Vec<_>>()
                                    .into_boxed_slice(),
                            ),
                            Expression::TupleElement(tuple, element) => {
                                let tuple = match computations.get(tuple).unwrap().clone() {
                                    Value::List(tuple) => tuple,
                                    _ => unreachable!(),
                                };

                                tuple[*element].clone()
                            }
                            Expression::VariantElement(variant, element) => {
                                let elements = match computations.get(variant).unwrap().clone() {
                                    Value::Variant(_, elements) => elements,
                                    _ => unreachable!(),
                                };

                                elements[*element].clone()
                            }
                            Expression::Discriminant(variant) => {
                                match computations.get(variant).unwrap().clone() {
                                    Value::Variant(discriminant, _) => {
                                        Value::Discriminant(discriminant)
                                    }
                                    _ => unreachable!(),
                                }
                            }
                            Expression::CompareDiscriminants(left, right) => {
                                let left = match computations.get(left).unwrap().clone() {
                                    Value::Discriminant(discriminant) => discriminant,
                                    _ => unreachable!(),
                                };

                                Value::Variant(
                                    (left == *right) as usize,
                                    Vec::new().into_boxed_slice(),
                                )
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

            match &section.terminator {
                Some(terminator) => {
                    log!("\t{}", terminator);

                    match terminator {
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
                            break Ok(Some(
                                computations
                                    .get(computation)
                                    .unwrap_or_else(|| {
                                        panic!(
                                            "could not find {:?} in {:#?}",
                                            computation, computations
                                        )
                                    })
                                    .clone(),
                            ));
                        }
                        Terminator::Goto(index) => {
                            section_index = *index;
                        }
                        Terminator::Unreachable => unreachable!(),
                    }
                }
                None if !expect_terminator => {
                    log!("\t<no terminator>");

                    break Ok(None);
                }
                _ => unreachable!(),
            }
        }
    }
}
