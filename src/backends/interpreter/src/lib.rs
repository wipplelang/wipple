mod external;

pub use external::*;

use rust_decimal::Decimal;
use serde::Serialize;
use std::{cell::RefCell, collections::BTreeMap, sync::Arc};
use wipple_frontend::*;

#[derive(Debug, Serialize)]
pub enum Value {
    Unit,
    Text(String),
    Number(Decimal),
    Function {
        body: Item,
        captures: BTreeMap<VariableId, Arc<Value>>,
    },
    ExternalFunction(ExternalFunction),
}

#[derive(Debug, Serialize)]
pub enum Error {
    UnknownExternalNamespace(String),
    UnknownExternalIdentifier(String),
}

pub fn eval(files: &[Arc<File>], external: ExternalValues) -> Result<(), Error> {
    let mut info = Info {
        external,
        scope: Default::default(),
        function_input: None,
    };

    for file in files {
        for statement in &file.statements {
            eval_item(statement, &mut info)?;
        }
    }

    Ok(())
}

struct Info {
    external: ExternalValues,
    scope: Arc<RefCell<Scope>>,
    function_input: Option<Arc<Value>>,
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

fn eval_item(item: &Item, info: &mut Info) -> Result<Arc<Value>, Error> {
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

                let value = eval_item(body, info)?;

                info.scope = parent;
                info.function_input = None;

                value
            }
            Value::ExternalFunction(function) => {
                let input = eval_item(&apply.input, info)?;
                function.call(input)?
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
            body: function.body.as_ref().clone(),
            captures: function
                .captures
                .iter()
                .map(|&variable| (variable, resolve(variable, info)))
                .collect(),
        }),
        ItemKind::FunctionInput(_) => info.function_input.as_ref().unwrap().clone(),
        ItemKind::External(external) => info
            .external
            .get(&external.namespace.get(), &external.identifier.get())?
            .clone(),
        ItemKind::Annotate(annotate) => eval_item(&annotate.item, info)?,
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
