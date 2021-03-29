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

    let stack = Stack::empty();

    let result = match run_code(code, stack.clone()) {
        Ok(output) => InterpreterResult {
            success: true,
            output: Some(output),
            error: None,
        },
        Err(state) => InterpreterResult {
            success: false,
            output: None,
            error: Some(state.into_error(stack).to_string()),
        },
    };

    JsValue::from_serde(&result).unwrap()
}

fn run_code(code: &str, stack: Stack) -> wipple::Result<Vec<ShownValue>> {
    let output = Rc::new(RefCell::new(Vec::<ShownValue>::new()));

    wipple::setup();
    setup_playground(&output);

    // Load the standard library
    (*wipple_stdlib::_wipple_plugin(&Environment::global(), stack.clone()))?;

    let env = Environment::child_of(&Environment::global()).into_ref();

    let program = wipple_projects::load_string(code, None, stack.clone())?;

    let stack = setup_module_block(stack);
    wipple_projects::include_program(program, &env, stack)?;

    Ok(output.as_ref().clone().get_mut().clone())
}

fn setup_playground(output: &Rc<RefCell<Vec<ShownValue>>>) {
    let env = Environment::global();
    let env = &mut env.borrow_mut();

    let output = output.clone();

    *env.show() = ShowFn::new(move |value, env, stack| {
        let source_text = value.format(env, stack.clone())?;
        let output_text = value.evaluate(env, stack.clone())?.format(env, stack)?;

        output.borrow_mut().push(ShownValue {
            input: source_text,
            output: output_text,
        });

        Ok(())
    });
}
