mod builtin;
mod output;

pub use output::*;

#[cfg(not(target_arch = "wasm32"))]
mod external;
#[cfg(not(target_arch = "wasm32"))]
pub use external::*;

use rust_decimal::Decimal;
use std::{cell::RefCell, collections::BTreeMap, sync::Arc};
use wipple_frontend::{typecheck::*, *};

#[derive(Debug)]
pub enum Value {
    Unit,
    Text(String),
    Number(Decimal),
    Function {
        body: Box<Item>,
        captures: BTreeMap<VariableId, Arc<Value>>,
    },
    Data(Vec<Arc<Value>>),
}

#[derive(Debug)]
pub struct Diverge {
    pub callstack: Callstack,
    pub kind: DivergeKind,
}

#[derive(Debug)]
pub enum DivergeKind {
    End(Arc<Value>),
    Return(Arc<Value>),
    Error(String),
}

type Error = String;

type Callstack = Vec<(Option<InternedString>, Span)>;

impl Diverge {
    #[allow(clippy::ptr_arg)]
    pub fn new(callstack: &Callstack, kind: DivergeKind) -> Self {
        Diverge {
            callstack: callstack.clone(),
            kind,
        }
    }
}

pub fn eval(item: &Item) -> Result<(), (Error, Callstack)> {
    let mut info = Info {
        scope: Default::default(),
        function_input: None,
        callstack: Vec::new(),
    };

    eval_item(item, &mut info).map_err(|diverge| match diverge.kind {
        DivergeKind::Error(error) => (error, diverge.callstack),
        _ => unreachable!(),
    })?;

    Ok(())
}

pub struct Info {
    scope: Arc<RefCell<Scope>>,
    function_input: Option<Arc<Value>>,
    callstack: Callstack,
}

#[derive(Debug, Default)]
struct Scope {
    variables: BTreeMap<VariableId, Arc<Value>>,
    parent: Option<Arc<RefCell<Scope>>>,
}

impl Scope {
    fn child(scope: &Arc<RefCell<Self>>) -> Arc<RefCell<Self>> {
        Arc::new(RefCell::new(Scope {
            variables: Default::default(),
            parent: Some(scope.clone()),
        }))
    }
}

fn eval_item(item: &Item, info: &mut Info) -> Result<Arc<Value>, Diverge> {
    let value = match &item.kind {
        ItemKind::Unit(_) => Arc::new(Value::Unit),
        ItemKind::Number(number) => Arc::new(Value::Number(number.value)),
        ItemKind::Text(text) => Arc::new(Value::Text(text.value.to_string())),
        ItemKind::Block(block) => {
            let parent = info.scope.clone();
            let child = Scope::child(&parent);
            info.scope = child;

            let mut value = Arc::new(Value::Unit);
            for statement in &block.statements {
                value = eval_item(statement, info)?;
            }

            info.scope = parent;

            value
        }
        ItemKind::Apply(apply) => match eval_item(&apply.function, info)?.as_ref() {
            Value::Function { body, captures } => {
                let input = eval_item(&apply.input, info)?;
                info.function_input = Some(input);

                let parent = info.scope.clone();
                let child = Scope::child(&parent);
                child.borrow_mut().variables.extend(captures.clone());
                info.scope = child;

                info.callstack.push((
                    apply.function.compile_info.declared_name,
                    item.compile_info.span,
                ));

                let value = match eval_item(body, info) {
                    Ok(value)
                    | Err(Diverge {
                        kind: DivergeKind::Return(value),
                        ..
                    }) => value,
                    diverge => return diverge,
                };

                info.scope = parent;
                info.function_input = None;
                info.callstack.pop();

                value
            }
            _ => unreachable!(),
        },
        ItemKind::Initialize(initialize) => {
            let value = eval_item(&initialize.value, info)?;
            info.scope
                .borrow_mut()
                .variables
                .insert(initialize.variable, value);
            Arc::new(Value::Unit)
        }
        ItemKind::Variable(variable) => resolve(variable.variable, info),
        ItemKind::Function(function) => Arc::new(Value::Function {
            body: Box::new(function.body.as_ref().clone()),
            captures: function
                .captures
                .iter()
                .map(|&variable| (variable, resolve(variable, info)))
                .collect(),
        }),
        ItemKind::FunctionInput(_) => info.function_input.as_ref().unwrap().clone(),
        ItemKind::External(external) => {
            if external.namespace.as_str() == "builtin" {
                let inputs = external
                    .inputs
                    .iter()
                    .map(|input| eval_item(input, info))
                    .collect::<Result<Vec<_>, _>>()?;

                builtin::call(&external.identifier, inputs, info)?
            } else {
                #[cfg(target_arch = "wasm32")]
                {
                    return Err(Diverge::Error(Error::from(
                        "External functions are unsupported in the playground",
                    )));
                }

                #[cfg(not(target_arch = "wasm32"))]
                {
                    let inputs = external
                        .inputs
                        .iter()
                        .map(|input| match &input.ty {
                            TypeSchema::Monotype(ty) => Ok(ty),
                            _ => Err(Diverge::new(
                                &info.callstack,
                                DivergeKind::Error(Error::from(
                                    "Unsupported external function input type",
                                )),
                            )),
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    let return_ty = match &item.ty {
                        TypeSchema::Monotype(ty) => ty,
                        TypeSchema::Polytype { .. } => {
                            return Err(Diverge::new(
                                &info.callstack,
                                DivergeKind::Error(Error::from(
                                    "Unsupported external function return type",
                                )),
                            ));
                        }
                    };

                    let function = match ExternalFunction::from_item(external, inputs, return_ty) {
                        Ok(function) => function,
                        Err(error) => {
                            return Err(Diverge::new(
                                &info.callstack,
                                DivergeKind::Error(format!(
                                    "Unsupported external function type: {}",
                                    error
                                )),
                            ))
                        }
                    };

                    let inputs = external
                        .inputs
                        .iter()
                        .map(|input| eval_item(input, info))
                        .collect::<Result<Vec<_>, _>>()?;

                    function.call_with(inputs, info)?
                }
            }
        }
        ItemKind::Data(data) => Arc::new(Value::Data(
            data.fields
                .iter()
                .map(|field| eval_item(field, info))
                .collect::<Result<_, _>>()?,
        )),
        ItemKind::Loop(r#loop) => loop {
            match eval_item(&r#loop.body, info) {
                Ok(_) => continue,
                Err(Diverge {
                    kind: DivergeKind::End(value),
                    ..
                }) => break value,
                diverge => return diverge,
            }
        },
        ItemKind::End(end) => {
            let value = eval_item(&end.value, info)?;
            return Err(Diverge::new(&info.callstack, DivergeKind::End(value)));
        }
        ItemKind::Return(r#return) => {
            let value = eval_item(&r#return.value, info)?;
            return Err(Diverge::new(&info.callstack, DivergeKind::Return(value)));
        }
        ItemKind::Field(field) => match eval_item(&field.value, info)?.as_ref() {
            Value::Data(data) => data[field.index].clone(),
            _ => unreachable!(),
        },
        ItemKind::Error(_) => panic!("Program is not well-typed"),
    };

    Ok(value)
}

fn resolve(variable: VariableId, info: &mut Info) -> Arc<Value> {
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

    if value.is_none() {
        panic!("Variable {:?} not found in {:#?}", variable, info.scope);
    }

    value.unwrap()
}
