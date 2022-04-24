mod builtin;

#[cfg(not(target_arch = "wasm32"))]
mod external;
#[cfg(not(target_arch = "wasm32"))]
pub use external::*;

use rust_decimal::Decimal;
use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    rc::Rc,
};
use wipple_compiler::{
    compile::{
        typecheck::{Declarations, Expression, ExpressionKind},
        Program,
    },
    parser::Span,
    ConstantId, VariableId,
};

#[derive(Debug)]
pub enum Value {
    Marker,
    Text(String),
    Number(Decimal),
    Function {
        body: Box<Expression>,
        captures: BTreeMap<VariableId, Rc<Value>>,
    },
    Structure(Vec<Rc<Value>>),
}

#[derive(Debug)]
struct Diverge {
    pub stack: Vec<Span>,
    pub kind: DivergeKind,
}

#[derive(Debug)]
enum DivergeKind {
    Error(String),
}

type Error = String;

impl Diverge {
    #[allow(clippy::ptr_arg)]
    pub fn new(stack: &Vec<Span>, kind: DivergeKind) -> Self {
        Diverge {
            stack: stack.clone(),
            kind,
        }
    }
}

#[derive(Default)]
pub struct Interpreter<'a> {
    #[allow(clippy::type_complexity)]
    output: Option<Rc<RefCell<Box<dyn FnMut(&str, &[Span]) + 'a>>>>,
}

impl<'a> Interpreter<'a> {
    pub fn handling_output(mut output: impl FnMut(&str) + 'a) -> Self {
        Interpreter::handling_output_with_span(move |s, _| output(s))
    }

    pub fn ignoring_output() -> Self {
        Interpreter { output: None }
    }

    pub fn handling_output_with_span(output: impl FnMut(&str, &[Span]) + 'a) -> Self {
        Interpreter {
            output: Some(Rc::new(RefCell::new(Box::new(output) as Box<_>))),
        }
    }

    pub fn eval(&self, program: Program) -> Result<(), (Error, Vec<Span>)> {
        let mut info = Info {
            declarations: program.declarations,
            scope: Default::default(),
            function_input: None,
            constants: Default::default(),
            stack: Vec::new(),
        };

        for statement in program.body {
            self.eval_expr(&statement, &mut info)
                .map_err(|diverge| match diverge.kind {
                    DivergeKind::Error(error) => (error, diverge.stack),
                    /* _ => unreachable!(), */
                })?;
        }

        Ok(())
    }
}

pub struct Info {
    declarations: Declarations,
    scope: Rc<RefCell<Scope>>,
    function_input: Option<Rc<Value>>,
    constants: HashMap<ConstantId, Rc<Value>>,
    stack: Vec<Span>,
}

#[derive(Debug, Default)]
struct Scope {
    variables: BTreeMap<VariableId, Rc<Value>>,
    parent: Option<Rc<RefCell<Scope>>>,
}

impl Scope {
    fn child(scope: &Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Scope {
            variables: Default::default(),
            parent: Some(scope.clone()),
        }))
    }
}

impl<'a> Interpreter<'a> {
    fn eval_expr(&self, expr: &Expression, info: &mut Info) -> Result<Rc<Value>, Diverge> {
        info.stack.push(expr.span);

        let value = match &expr.kind {
            ExpressionKind::Error => panic!("program is not well-typed"),
            ExpressionKind::Marker => Rc::new(Value::Marker),
            ExpressionKind::Constant(constant) => {
                if let Some(value) = info.constants.get(constant) {
                    value.clone()
                } else {
                    let expr = &info.declarations.constants.remove(constant).unwrap().value;
                    let value = self.eval_expr(expr, info)?;
                    info.constants.insert(*constant, value.clone());

                    value
                }
            }
            ExpressionKind::Number(number) => Rc::new(Value::Number(*number)),
            ExpressionKind::Text(text) => Rc::new(Value::Text(text.to_string())),
            ExpressionKind::Block(statements, _) => {
                let parent = info.scope.clone();
                let child = Scope::child(&parent);
                info.scope = child;

                let mut value = Rc::new(Value::Marker);
                for statement in statements {
                    value = self.eval_expr(statement, info)?;
                }

                info.scope = parent;

                value
            }
            ExpressionKind::Call(function, input) => {
                match self.eval_expr(function, info)?.as_ref() {
                    Value::Function { body, captures } => {
                        let input = self.eval_expr(input, info)?;
                        info.function_input = Some(input);

                        let parent = info.scope.clone();
                        let child = Scope::child(&parent);
                        child.borrow_mut().variables.extend(captures.clone());
                        info.scope = child;

                        let value = match self.eval_expr(body, info) {
                        Ok(value)
                        /* | Err(Diverge {
                            kind: DivergeKind::Return(value),
                            ..
                        }) */ => value,
                        diverge => return diverge,
                    };

                        info.scope = parent;
                        info.function_input = None;

                        value
                    }
                    _ => unreachable!(),
                }
            }
            ExpressionKind::Member(value, index) => match self.eval_expr(value, info)?.as_ref() {
                Value::Structure(structure) => structure[*index].clone(),
                _ => unreachable!(),
            },
            ExpressionKind::Initialize(variable, value) => {
                let value = self.eval_expr(value, info)?;
                info.scope.borrow_mut().variables.insert(*variable, value);
                Rc::new(Value::Marker)
            }
            ExpressionKind::Variable(variable) => {
                self.resolve(*variable, info).unwrap_or_else(|| {
                    panic!("variable {:?} not found in {:#?}", variable, info.scope)
                })
            }
            ExpressionKind::Function(body, captures) => Rc::new(Value::Function {
                body: Box::new(body.as_ref().clone()),
                captures: captures
                    .iter()
                    .filter_map(|&variable| Some((variable, self.resolve(variable, info)?)))
                    .collect(),
            }),
            ExpressionKind::When(_, _) => todo!(),
            ExpressionKind::External(namespace, identifier, inputs) => {
                if namespace.as_str() == "builtin" {
                    let inputs = inputs
                        .iter()
                        .map(|input| self.eval_expr(input, info))
                        .collect::<Result<Vec<_>, _>>()?;

                    builtin::call(self, identifier, inputs, info)?
                } else {
                    #[cfg(target_arch = "wasm32")]
                    {
                        return Err(Diverge::new(
                            &info.stack,
                            DivergeKind::Error(Error::from(
                                "external functions are unsupported in the playground",
                            )),
                        ));
                    }

                    #[cfg(not(target_arch = "wasm32"))]
                    {
                        let input_tys = inputs.iter().map(|input| &input.ty).collect::<Vec<_>>();
                        let return_ty = &expr.ty;

                        let function = match ExternalFunction::new(
                            namespace, identifier, input_tys, return_ty,
                        ) {
                            Ok(function) => function,
                            Err(error) => {
                                return Err(Diverge::new(
                                    &info.stack,
                                    DivergeKind::Error(format!(
                                        "unsupported external function type: {}",
                                        error
                                    )),
                                ))
                            }
                        };

                        let inputs = inputs
                            .iter()
                            .map(|input| self.eval_expr(input, info))
                            .collect::<Result<Vec<_>, _>>()?;

                        function
                            .call_with(inputs)
                            .map_err(|error| Diverge::new(&info.stack, DivergeKind::Error(error)))?
                    }
                }
            }
            ExpressionKind::Structure(exprs) => Rc::new(Value::Structure(
                exprs
                    .iter()
                    .map(|expr| self.eval_expr(expr, info))
                    .collect::<Result<_, _>>()?,
            )),
            ExpressionKind::FunctionInput => info.function_input.as_ref().unwrap().clone(),
        };

        info.stack.pop();

        Ok(value)
    }

    fn resolve(&self, variable: VariableId, info: &mut Info) -> Option<Rc<Value>> {
        let mut value = None;
        let mut scope = Some(info.scope.clone());

        while let Some(s) = scope {
            let s = s.borrow();

            if let Some(v) = s.variables.get(&variable) {
                value = Some(v.clone());
                break;
            } else {
                scope = s.parent.clone();
            }
        }

        value
    }
}
