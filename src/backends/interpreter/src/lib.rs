mod external;

pub use external::*;

use rust_decimal::Decimal;
use serde::Serialize;
use std::{collections::BTreeMap, rc::Rc};
use wipple_frontend::*;

#[derive(Debug, Serialize)]
pub enum Value<'a> {
    Unit,
    Text(String),
    Number(Decimal),
    Function(&'a Item),
}

pub fn eval(item: &TypecheckedItem, external: ExternalFunctions) -> Value {
    let mut info = Info {
        external,
        variables: Vec::new(),
        function_inputs: Vec::new(),
    };

    let value = eval_item(&item.0, &mut info);

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
        ItemKind::Error => unreachable!(),
        ItemKind::Unit(_) => Rc::new(Value::Unit),
        ItemKind::Constant(constant_item) => match constant_item.kind {
            ConstantItemKind::Number(number) => Rc::new(Value::Number(*number)),
            ConstantItemKind::Text(text) => Rc::new(Value::Text(text.to_string())),
        },
        ItemKind::Block(block_item) => {
            info.variables.push(BTreeMap::new());

            let value = block_item
                .statements
                .iter()
                .map(|statement| eval_item(statement, info))
                .last()
                .unwrap_or_else(|| Rc::new(Value::Unit));

            info.variables.pop();

            value
        }
        ItemKind::Apply(apply_item) => match eval_item(&apply_item.function, info).as_ref() {
            Value::Function(body) => {
                let input = eval_item(&apply_item.input, info);
                info.function_inputs.push(input);

                let value = eval_item(body, info);

                info.function_inputs.pop();

                value
            }
            _ => unreachable!(),
        },
        ItemKind::Initialize(initialize_item) => {
            let value = eval_item(&initialize_item.value, info);

            info.variables
                .last_mut()
                .unwrap()
                .insert(initialize_item.variable, value);

            Rc::new(Value::Unit)
        }
        ItemKind::Variable(variable_item) => {
            let mut index = info.variables.len() - 1;
            let mut value = None;

            while let Some(scope) = info.variables.get(index) {
                if let Some(v) = scope.get(&variable_item.variable) {
                    value = Some(v);
                    break;
                } else {
                    index -= 1;
                }
            }

            value.unwrap().clone()
        }
        ItemKind::Function(function_item) => Rc::new(Value::Function(&function_item.body)),
        ItemKind::FunctionInput(_) => info.function_inputs.last().unwrap().clone(),
        ItemKind::External(external_item) => {
            let inputs = external_item
                .inputs
                .iter()
                .map(|item| eval_item(item, info))
                .collect();

            info.external
                .get(&external_item.namespace, &external_item.identifier)
                .call(inputs)
        }
    }
}
