use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::prelude::*;
use wipple::*;
use wipple_parser::{convert, lex, parse_file};

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
    let (tokens, lookup) = lex(code);
    let ast = match parse_file(&mut tokens.iter().peekable(), &lookup) {
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
                            error.location.map(|location| SourceLocation {
                                file: None,
                                line: location.line,
                                column: location.column,
                            }),
                        ),
                    )
                    .to_string(),
                ),
            }
        }
    };

    let value = convert(&ast, None);

    let output = Rc::new(RefCell::new(Vec::<ShownValue>::new()));

    wipple::setup();
    setup_playground(&output);

    let env = Environment::child_of(&Environment::global()).into_ref();
    let stack = Stack::new();

    match value.evaluate(&env, &stack) {
        Ok(_) => InterpreterResult {
            success: true,
            output: Some(output.as_ref().clone().get_mut().clone()),
            error: None,
        },
        Err(state) => InterpreterResult {
            success: false,
            output: None,
            error: Some(state.into_error(&stack).to_string()),
        },
    }
}

fn setup_playground(output: &Rc<RefCell<Vec<ShownValue>>>) {
    let env = Environment::global();
    let env = &mut env.borrow_mut();

    let output = output.clone();

    *env.show() = ShowFn::new(move |value, env, stack| {
        let source_text = value.format(env, stack)?;
        let output_text = value.evaluate(env, stack)?.format(env, stack)?;

        output.borrow_mut().push(ShownValue {
            input: source_text,
            output: output_text,
        });

        Ok(())
    });
}
