mod external;

pub use external::*;

use rust_decimal::Decimal;
use serde::Serialize;
use std::{collections::BTreeMap, rc::Rc};
use wipple_frontend::{id::VariableId, typecheck::*};

#[derive(Debug, Serialize)]
pub enum Value<'a> {
    Unit,
    Text(String),
    Number(Decimal),
    Function(&'a Item),
}

pub fn eval(item: &Item, external: ExternalFunctions) -> Value {
    let mut info = Info {
        external,
        variables: Vec::new(),
        function_inputs: Vec::new(),
    };

    let value = eval_item(item, &mut info);

    drop(info);

    Rc::try_unwrap(value).unwrap_or_else(|_| unreachable!())
}

struct Info<'a> {
    external: ExternalFunctions,
    variables: Vec<BTreeMap<VariableId, Rc<Value<'a>>>>,
    function_inputs: Vec<Rc<Value<'a>>>,
}

fn eval_item<'a>(item: &'a Item, info: &mut Info<'a>) -> Rc<Value<'a>> {
    match &item.kind {
        ItemKind::Unit => Rc::new(Value::Unit),
        ItemKind::Number { value } => Rc::new(Value::Number(**value)),
        ItemKind::Text { value } => Rc::new(Value::Text(value.to_string())),
        ItemKind::Block { statements } => {
            info.variables.push(BTreeMap::new());

            let value = statements
                .iter()
                .map(|statement| eval_item(statement, info))
                .last()
                .unwrap_or_else(|| Rc::new(Value::Unit));

            info.variables.pop();

            value
        }
        ItemKind::Apply { function, input } => match eval_item(function, info).as_ref() {
            Value::Function(body) => {
                let input = eval_item(input, info);
                info.function_inputs.push(input);

                let value = eval_item(body, info);

                info.function_inputs.pop();

                value
            }
            _ => unreachable!(),
        },
        ItemKind::Initialize { variable, value } => {
            let value = eval_item(value, info);
            info.variables.last_mut().unwrap().insert(*variable, value);
            Rc::new(Value::Unit)
        }
        ItemKind::Variable { variable } => {
            let mut index = info.variables.len() - 1;
            let mut value = None;

            while let Some(scope) = info.variables.get(index) {
                if let Some(v) = scope.get(variable) {
                    value = Some(v);
                    break;
                } else {
                    index -= 1;
                }
            }

            value.unwrap().clone()
        }
        ItemKind::Function { body, .. } => Rc::new(Value::Function(body)),
        ItemKind::FunctionInput => info.function_inputs.last().unwrap().clone(),
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
