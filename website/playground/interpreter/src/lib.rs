use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::prelude::*;
use wipple::*;

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

    let result = match run_code(code) {
        Ok(output) => InterpreterResult {
            success: true,
            output: Some(output),
            error: None,
        },
        Err(state) => InterpreterResult {
            success: false,
            output: None,
            error: Some(state.as_error().to_string()),
        },
    };

    JsValue::from_serde(&result).unwrap()
}

fn run_code(code: &str) -> wipple::Result<Vec<ShownValue>> {
    let output = Rc::new(RefCell::new(Vec::<ShownValue>::new()));

    wipple::setup();

    let mut stack = Stack::new();

    wipple_stdlib::setup(&Environment::global(), &stack)?;

    let env = Environment::child_of(&Environment::global()).into_ref();

    let program = wipple_loading::load_string(code, None, &stack)?;

    setup_module_block(&env);
    setup_playground(&output, &mut stack);
    wipple_loading::include_program(program, &env, &stack)?;

    Ok(output.as_ref().clone().get_mut().clone())
}

fn setup_playground(output: &Rc<RefCell<Vec<ShownValue>>>, stack: &mut Stack) {
    let output = output.clone();

    *wipple_stdlib::show_mut_in(stack) = wipple_stdlib::ShowFn::new(move |value, env, stack| {
        let source_text = value.format(env, stack)?;
        let output_text = value.evaluate(env, stack)?.format(env, stack)?;

        output.borrow_mut().push(ShownValue {
            input: source_text,
            output: output_text,
        });

        Ok(())
    });
}
