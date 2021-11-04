mod external;

pub use external::*;

use rust_decimal::Decimal;
use serde::Serialize;
use std::{cell::RefCell, collections::BTreeMap, rc::Rc};
use wipple_frontend::{id::*, typecheck::*};

#[derive(Debug, Serialize)]
pub enum Value {
    Unit,
    Text(String),
    Number(Decimal),
    Function {
        body: Item,
        captures: BTreeMap<VariableId, Rc<Value>>,
    },
}

pub fn eval(files: &[File], external: ExternalFunctions) {
    let mut info = Info {
        external,
        scope: Default::default(),
        function_input: None,
    };

    for file in files.iter().rev() {
        for statement in &file.statements {
            eval_item(statement, &mut info);
        }
    }
}

struct Info {
    external: ExternalFunctions,
    scope: Rc<RefCell<Scope>>,
    function_input: Option<Rc<Value>>,
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

fn eval_item(item: &Item, info: &mut Info) -> Rc<Value> {
    match &item.kind {
        ItemKind::Unit => Rc::new(Value::Unit),
        ItemKind::Number { value } => Rc::new(Value::Number(**value)),
        ItemKind::Text { value } => Rc::new(Value::Text(value.to_string())),
        ItemKind::Block { statements } => {
            let parent = info.scope.clone();
            let child = Scope::child(&parent);
            info.scope = child;

            let value = statements
                .iter()
                .map(|statement| eval_item(statement, info))
                .last()
                .unwrap_or_else(|| Rc::new(Value::Unit));

            info.scope = parent;

            value
        }
        ItemKind::Apply { function, input } => match eval_item(function, info).as_ref() {
            Value::Function { body, captures } => {
                let input = eval_item(input, info);
                info.function_input = Some(input);

                let parent = info.scope.clone();
                let child = Scope::child(&parent);
                child.borrow_mut().variables.extend(captures.clone());
                info.scope = child;

                let value = eval_item(body, info);

                info.scope = parent;
                info.function_input = None;

                value
            }
            _ => unreachable!(),
        },
        ItemKind::Initialize { variable, value } => {
            let value = eval_item(value, info);
            info.scope.borrow_mut().variables.insert(*variable, value);
            Rc::new(Value::Unit)
        }
        ItemKind::Variable { variable } => resolve(*variable, info),
        ItemKind::Function { body, captures, .. } => Rc::new(Value::Function {
            body: body.as_ref().clone(),
            captures: captures
                .iter()
                .map(|&variable| (variable, resolve(variable, info)))
                .collect(),
        }),
        ItemKind::FunctionInput => info.function_input.as_ref().unwrap().clone(),
        ItemKind::External {
            namespace,
            identifier,
            inputs,
        } => {
            let inputs = inputs.iter().map(|item| eval_item(item, info)).collect();
            info.external.get(namespace, identifier).call(inputs)
        }
    }
}

fn resolve(variable: VariableId, info: &mut Info) -> Rc<Value> {
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
