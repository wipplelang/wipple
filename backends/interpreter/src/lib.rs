#![allow(clippy::type_complexity)]

mod builtin;

#[cfg(not(target_arch = "wasm32"))]
mod external;
#[cfg(not(target_arch = "wasm32"))]
pub use external::*;

use rust_decimal::Decimal;
use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    mem,
    rc::Rc,
};
use wipple_compiler::{
    optimize::{Expression, ExpressionKind, Pattern, PatternKind, Program},
    parse::Span,
    MonomorphizedConstantId, VariableId,
};

#[derive(Debug, Clone)]
pub enum Value {
    Marker,
    Text(Rc<str>),
    Number(Decimal),
    Function(Rc<Function>),
    List(im::Vector<Value>),
    Variant(usize, im::Vector<Value>),
    Mutable(Rc<RefCell<Value>>),
}

#[derive(Debug)]
pub struct Function {
    pub pattern: Pattern,
    pub body: Box<Expression>,
    pub scope: Rc<RefCell<Scope>>,
}

#[derive(Debug)]
struct Diverge {
    pub stack: Vec<Span>,
    pub kind: DivergeKind,
}

#[derive(Debug)]
enum DivergeKind {
    Error(String),
    Return(Value),
}

type Error = String;

impl Diverge {
    pub fn new(stack: Vec<Span>, kind: DivergeKind) -> Self {
        Diverge { stack, kind }
    }
}

#[derive(Default)]
pub struct Interpreter<'a> {
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
            constants: program.constants,
            scope: Default::default(),
            initialized_constants: Default::default(),
            stack: Vec::new(),
        };

        for statement in program.body {
            self.eval_expr(&statement, &mut info)
                .map_err(|diverge| match diverge.kind {
                    DivergeKind::Error(error) => (error, diverge.stack),
                    _ => unreachable!(),
                })?;
        }

        Ok(())
    }
}

pub struct Info {
    constants: BTreeMap<MonomorphizedConstantId, Expression>,
    scope: Rc<RefCell<Scope>>,
    initialized_constants: HashMap<MonomorphizedConstantId, Value>,
    stack: Vec<Span>,
}

#[derive(Debug, Default)]
pub struct Scope {
    variables: BTreeMap<VariableId, Value>,
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
    fn eval_expr(&self, expr: &Expression, info: &mut Info) -> Result<Value, Diverge> {
        if let Some(span) = expr.span {
            info.stack.push(span);
        }

        let value = match &expr.kind {
            ExpressionKind::Marker => Value::Marker,
            ExpressionKind::Constant(constant) => {
                if let Some(value) = info.initialized_constants.get(constant) {
                    value.clone()
                } else {
                    let expr = info
                        .constants
                        .get(constant)
                        .unwrap_or_else(|| panic!("constant {:?} not registered", constant))
                        .clone();

                    let value = self.eval_expr(&expr, info)?;
                    info.initialized_constants.insert(*constant, value.clone());

                    value
                }
            }
            ExpressionKind::Number(number) => Value::Number(*number),
            ExpressionKind::Text(text) => Value::Text(Rc::from(text.as_str())),
            ExpressionKind::Block(statements) => {
                let parent = info.scope.clone();
                let child = Scope::child(&parent);
                info.scope = child;

                let mut value = Value::Marker;
                for statement in statements {
                    value = self.eval_expr(statement, info)?;
                }

                info.scope = parent;

                value
            }
            ExpressionKind::Call(function, input) => match self.eval_expr(function, info)? {
                Value::Function(func) => {
                    let input = self.eval_expr(input, info)?;
                    self.call_function(&func, input, info)?
                }
                _ => unreachable!(),
            },
            ExpressionKind::Initialize(pattern, value) => {
                let value = self.eval_expr(value, info)?;

                let matches = self.eval_pattern(pattern, value, info)?;
                assert!(matches, "no matches for pattern in initialization");

                Value::Marker
            }
            ExpressionKind::Variable(variable) => {
                self.resolve(*variable, info).unwrap_or_else(|| {
                    panic!("variable {:?} not found in {:#?}", variable, info.scope)
                })
            }
            ExpressionKind::Function(pattern, body) => Value::Function(Rc::new(Function {
                pattern: pattern.clone(),
                body: Box::new(body.as_ref().clone()),
                scope: info.scope.clone(),
            })),
            ExpressionKind::When(input, arms) => {
                let input = self.eval_expr(input, info)?;

                let parent = info.scope.clone();
                let child = Scope::child(&parent);
                info.scope = child;

                let mut value = None;
                for arm in arms {
                    if self.eval_pattern(&arm.pattern, input.clone(), info)? {
                        value = Some(self.eval_expr(&arm.body, info)?);
                        break;
                    }
                }

                info.scope = parent;

                value.expect("no patterns matched input")
            }
            ExpressionKind::External(namespace, identifier, inputs, return_ty) => {
                if namespace.as_str() == "builtin" {
                    let inputs = inputs
                        .iter()
                        .map(|(input, _)| self.eval_expr(input, info))
                        .collect::<Result<Vec<_>, _>>()?;

                    builtin::call(self, identifier, inputs, info)?
                } else {
                    #[cfg(target_arch = "wasm32")]
                    {
                        let _ = return_ty;

                        return Err(Diverge::new(
                            info.stack.clone(),
                            DivergeKind::Error(Error::from(
                                "external functions are unsupported in the playground",
                            )),
                        ));
                    }

                    #[cfg(not(target_arch = "wasm32"))]
                    {
                        let input_tys = inputs.iter().map(|(_, ty)| ty).collect::<Vec<_>>();

                        let function = match ExternalFunction::new(
                            namespace, identifier, input_tys, return_ty,
                        ) {
                            Ok(function) => function,
                            Err(error) => {
                                return Err(Diverge::new(
                                    info.stack.clone(),
                                    DivergeKind::Error(format!(
                                        "unsupported external function type: {}",
                                        error
                                    )),
                                ))
                            }
                        };

                        let inputs = inputs
                            .iter()
                            .map(|(input, _)| self.eval_expr(input, info))
                            .collect::<Result<Vec<_>, _>>()?;

                        function.call_with(inputs).map_err(|error| {
                            Diverge::new(info.stack.clone(), DivergeKind::Error(error))
                        })?
                    }
                }
            }
            ExpressionKind::Structure(exprs) => Value::List(
                exprs
                    .iter()
                    .map(|expr| self.eval_expr(expr, info))
                    .collect::<Result<_, _>>()?,
            ),
            ExpressionKind::Variant(index, exprs) => Value::Variant(
                *index,
                exprs
                    .iter()
                    .map(|expr| self.eval_expr(expr, info))
                    .collect::<Result<_, _>>()?,
            ),
            ExpressionKind::ListLiteral(exprs) => Value::List(
                exprs
                    .iter()
                    .map(|expr| self.eval_expr(expr, info))
                    .collect::<Result<_, _>>()?,
            ),
            ExpressionKind::Return(value) => {
                let value = self.eval_expr(value, info)?;
                return Err(Diverge::new(info.stack.clone(), DivergeKind::Return(value)));
            }
        };

        info.stack.pop();

        Ok(value)
    }

    fn call_function(
        &self,
        function: &Function,
        input: Value,
        info: &mut Info,
    ) -> Result<Value, Diverge> {
        let parent = mem::replace(&mut info.scope, function.scope.clone());

        let matches = self.eval_pattern(&function.pattern, input, info)?;
        assert!(matches, "no matches for pattern in initialization");

        let value = match self.eval_expr(&function.body, info) {
            Ok(value)
            | Err(Diverge {
                kind: DivergeKind::Return(value),
                ..
            }) => value,
            diverge => return diverge,
        };

        info.scope = parent;

        Ok(value)
    }

    fn eval_pattern(
        &self,
        pattern: &Pattern,
        value: Value,
        info: &mut Info,
    ) -> Result<bool, Diverge> {
        match &pattern.kind {
            PatternKind::Wildcard => Ok(true),
            PatternKind::Number(number) => {
                let input = match value {
                    Value::Number(number) => number,
                    _ => unreachable!(),
                };

                Ok(input == *number)
            }
            PatternKind::Text(text) => {
                let input = match value {
                    Value::Text(text) => text,
                    _ => unreachable!(),
                };

                Ok(input.as_ref() == text.as_str())
            }
            PatternKind::Variable(var) => {
                info.scope.borrow_mut().variables.insert(*var, value);
                Ok(true)
            }
            PatternKind::Destructure(fields) => {
                let structure = match value {
                    Value::List(values) => values,
                    _ => unreachable!(),
                };

                for (index, pattern) in fields {
                    let value = structure[*index].clone();

                    if !self.eval_pattern(pattern, value, info)? {
                        return Ok(false);
                    }
                }

                Ok(true)
            }
            PatternKind::Variant(index, patterns) => {
                let (value_index, values) = match value {
                    Value::Variant(index, values) => (index, values),
                    _ => unreachable!(),
                };

                if *index != value_index {
                    return Ok(false);
                }

                for (pattern, value) in patterns.iter().zip(values) {
                    if !self.eval_pattern(pattern, value.clone(), info)? {
                        return Ok(false);
                    }
                }

                Ok(true)
            }
            PatternKind::Or(lhs, rhs) => {
                if self.eval_pattern(lhs, value.clone(), info)? {
                    Ok(true)
                } else {
                    self.eval_pattern(rhs, value, info)
                }
            }
            PatternKind::Where(pattern, condition) => {
                if !self.eval_pattern(pattern, value, info)? {
                    return Ok(false);
                }

                match self.eval_expr(condition, info)? {
                    Value::Variant(index, _) => Ok(index != 0),
                    _ => unreachable!(),
                }
            }
        }
    }

    fn resolve(&self, variable: VariableId, info: &mut Info) -> Option<Value> {
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
