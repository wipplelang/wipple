use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::prelude::*;
use wipple::*;
use wipple_parser::{convert, parse};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ShownValue {
    pub input: String,
    pub output: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InterpreterResult {
    pub success: bool,
    pub output: Option<Vec<ShownValue>>,
    pub error: Option<String>,
}

#[wasm_bindgen]
pub fn run(code: &str) -> JsValue {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    let result = run_code(code);

    JsValue::from_serde(&result).unwrap()
}

fn run_code(code: &str) -> InterpreterResult {
    let ast = match parse(code) {
        Ok(value) => value,
        Err(error) => {
            return InterpreterResult {
                success: false,
                output: None,
                error: Some(
                    Error::new(
                        &error.message,
                        &Stack::new().add_location(
                            || String::from("Parsing input"),
                            &SourceLocation {
                                file: None,
                                line: error.line,
                                column: error.column,
                            },
                        ),
                    )
                    .to_string(),
                ),
            }
        }
    };

    let value = convert(&ast, None);

    let output = Rc::new(RefCell::new(Vec::<ShownValue>::new()));

    let env = wipple::prelude().into_ref();
    init_playground(output.clone(), &env);

    let env = Environment::child_of(&env).into_ref();

    let stack = Stack::new();

    match value.evaluate(&env, &stack) {
        Ok(_) => InterpreterResult {
            success: true,
            output: Some(output.as_ref().clone().get_mut().clone()),
            error: None,
        },
        Err(error) => InterpreterResult {
            success: false,
            output: None,
            error: Some(error.to_string()),
        },
    }
}

fn init_playground(output: Rc<RefCell<Vec<ShownValue>>>, env: &EnvironmentRef) {
    env.borrow_mut().variables().insert(
        String::from("show"),
        Value::of(Function::new(move |value, env, stack| {
            macro_rules! text {
                ($value:expr) => {
                    $value
                        .get_primitive_if_present::<Text>(env, stack)
                        .map(|text| match text {
                            Some(text) => text.text,
                            None => String::from("<value>"),
                        })
                };
            }

            let source_text = text!(value)?;
            let output_text = text!(value.evaluate(env, stack)?)?;

            output.borrow_mut().push(ShownValue {
                input: source_text,
                output: output_text,
            });

            Ok(Value::empty())
        })),
    );
}
