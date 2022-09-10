mod runtime;

use std::{
    cell::RefCell,
    collections::BTreeMap,
    os::raw::{c_int, c_uint},
    rc::Rc,
};
use wipple_frontend::{ir, MonomorphizedConstantId, VariableId};

type Error = String;

#[derive(Default)]
pub struct Interpreter<'a> {
    #[allow(clippy::type_complexity)]
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
    Number(rust_decimal::Decimal),
    Integer(i64),
    Natural(u64),
    Byte(u8),
    Signed(c_int),
    Unsigned(c_uint),
    Float(f32),
    Double(f64),
    Discriminant(usize),
    Text(Rc<str>),
    Function(ir::Function, Rc<Scope>),
    Variant(usize, Box<[Value]>),
    Mutable(Rc<RefCell<Value>>),
    List(im::Vector<Value>),
}

type Scope = BTreeMap<VariableId, Value>;

struct Info<'a> {
    program: &'a ir::Program,
    initialized_constants: BTreeMap<MonomorphizedConstantId, Value>,
}

impl<'a> Interpreter<'a> {
    pub fn run(&self, program: &ir::Program) -> Result<(), Error> {
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
        )?;

        Ok(())
    }

    fn call(
        &self,
        function: &ir::Function,
        input: &Value,
        info: &mut Info,
        scope: &mut Scope,
    ) -> Result<Value, Error> {
        let value = self
            .evaluate(&function.sections, info, scope, Some(input), true)?
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
    ) -> Result<Option<Value>, String> {
        let mut section_index = ir::SectionIndex(0);
        let mut computations = BTreeMap::new();

        loop {
            let section = &sections[section_index];

            for statement in &section.statements {
                match statement {
                    ir::Statement::Compute(id, expr) => {
                        let value = match &expr.kind {
                            ir::ExpressionKind::Marker => Value::Marker,
                            ir::ExpressionKind::Constant(constant) => {
                                if let Some(value) = info.initialized_constants.get(constant) {
                                    value.clone()
                                } else {
                                    let sections =
                                        &info.program.constants.get(constant).unwrap().sections;

                                    let value =
                                        self.evaluate(sections, info, scope, None, true)?.unwrap();

                                    info.initialized_constants.insert(*constant, value.clone());

                                    value
                                }
                            }
                            ir::ExpressionKind::Function(function) => {
                                Value::Function(function.clone(), Rc::new(scope.clone()))
                            }
                            ir::ExpressionKind::Variable(var) => scope.get(var).unwrap().clone(),
                            ir::ExpressionKind::FunctionInput => input.unwrap().clone(),
                            ir::ExpressionKind::Number(number) => Value::Number(*number),
                            ir::ExpressionKind::Integer(integer) => Value::Integer(*integer),
                            ir::ExpressionKind::Natural(natural) => Value::Natural(*natural),
                            ir::ExpressionKind::Byte(byte) => Value::Byte(*byte),
                            ir::ExpressionKind::Signed(signed) => Value::Signed(*signed),
                            ir::ExpressionKind::Unsigned(unsigned) => Value::Unsigned(*unsigned),
                            ir::ExpressionKind::Float(float) => Value::Float(*float),
                            ir::ExpressionKind::Double(double) => Value::Double(*double),
                            ir::ExpressionKind::Text(text) => Value::Text(Rc::from(text.as_str())),
                            ir::ExpressionKind::Call(function, input) => {
                                let (function, captures) = match computations.get(function).unwrap()
                                {
                                    Value::Function(function, captures) => (function, captures),
                                    _ => unreachable!(),
                                };

                                let input = computations.get(input).unwrap();

                                self.call(function, input, info, &mut (**captures).clone())?
                            }
                            ir::ExpressionKind::External(lib, identifier, inputs) => {
                                if lib.as_str() != ir::lib::RUNTIME {
                                    return Err(Error::from("unknown external library (only 'runtime' is supported in the interpreter)"));
                                }

                                self.call_runtime(
                                    identifier,
                                    inputs
                                        .iter()
                                        .map(|id| computations.get(id).unwrap().clone())
                                        .collect(),
                                )?
                            }
                            ir::ExpressionKind::Tuple(elements) => Value::List(
                                elements
                                    .iter()
                                    .map(|id| computations.get(id).unwrap().clone())
                                    .collect(),
                            ),
                            ir::ExpressionKind::Structure(fields) => Value::List(
                                fields
                                    .iter()
                                    .map(|id| computations.get(id).unwrap().clone())
                                    .collect(),
                            ),
                            ir::ExpressionKind::Variant(discriminant, elements) => Value::Variant(
                                *discriminant,
                                elements
                                    .iter()
                                    .map(|id| computations.get(id).unwrap().clone())
                                    .collect::<Vec<_>>()
                                    .into_boxed_slice(),
                            ),
                            ir::ExpressionKind::TupleElement(tuple, element) => {
                                let tuple = match computations.get(tuple).unwrap().clone() {
                                    Value::List(tuple) => tuple,
                                    _ => unreachable!(),
                                };

                                tuple[*element].clone()
                            }
                            ir::ExpressionKind::StructureElement(structure, _, element) => {
                                let structure = match computations.get(structure).unwrap().clone() {
                                    Value::List(structure) => structure,
                                    _ => unreachable!(),
                                };

                                structure[*element].clone()
                            }
                            ir::ExpressionKind::VariantElement(variant, _, element) => {
                                let elements = match computations.get(variant).unwrap().clone() {
                                    Value::Variant(_, elements) => elements,
                                    _ => unreachable!(),
                                };

                                elements[*element].clone()
                            }
                            ir::ExpressionKind::Discriminant(variant) => {
                                match computations.get(variant).unwrap().clone() {
                                    Value::Variant(discriminant, _) => {
                                        Value::Discriminant(discriminant)
                                    }
                                    _ => unreachable!(),
                                }
                            }
                            ir::ExpressionKind::CompareDiscriminants(left, right) => {
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
                    ir::Statement::Initialize(var, computation) => {
                        let value = computations.get(computation).unwrap().clone();
                        scope.insert(*var, value);
                    }
                    ir::Statement::Drop(vars) => {
                        for var in vars {
                            scope.remove(var);
                        }
                    }
                }
            }

            match &section.terminator {
                Some(terminator) => match terminator {
                    ir::Terminator::If(condition, then_section, else_section) => {
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
                    ir::Terminator::Return(computation) => {
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
                    ir::Terminator::Goto(index) => {
                        section_index = *index;
                    }
                    ir::Terminator::Unreachable => unreachable!(),
                },
                None if !expect_terminator => break Ok(None),
                _ => unreachable!(),
            }
        }
    }
}
