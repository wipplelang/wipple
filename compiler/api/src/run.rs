use serde::Serialize;
use std::sync::Arc;
use wasm_bindgen::{convert::TryFromJsValue, prelude::*};
use wipple_core::{codegen, span::Location};
use wipple_interpreter::Span;

#[wasm_bindgen]
pub fn run(bytes: &[u8], env: JsValue) -> Result<(), JsValue> {
    let mir = rmp_serde::from_slice::<codegen::mir::Program>(bytes).map_err(JsError::from)?;

    struct Env(JsValue);

    impl Env {
        fn call(&self, name: &str, input: JsValue) -> Result<JsValue, JsValue> {
            let func = js_sys::Reflect::get(&self.0, &JsValue::from_str(name))?;
            let func = js_sys::Function::<fn(JsValue) -> JsValue>::try_from_js_value(func)?;
            func.call1(&JsValue::NULL, &input)
        }
    }

    let env = Env(env);

    let debugger = wipple_interpreter::Debugger::new(|event| {
        if let wipple_interpreter::DebugEvent::Span(span) = event {
            env.call("trace", serialize_span(&span))?;
        }

        Ok(())
    });

    let interpreter = wipple_interpreter::Interpreter::new(|name, input| {
        let input = js_value_from_handle(input)?;
        let output = env.call(name, input)?;
        Ok(handle_from_js_value(output))
    })
    .with_debugger(debugger);

    interpreter.run(&mir)
}

fn js_value_from_handle(
    handle: wipple_interpreter::Handle<'_, JsValue>,
) -> Result<JsValue, JsValue> {
    use wipple_interpreter::{Handle, Primitive};

    Ok(match handle {
        Handle::Primitive(primitive) => match primitive {
            Primitive::String(string) => JsValue::from_str(&string),
            Primitive::Number(number) => JsValue::from_f64(number),
            Primitive::List(elements) => elements
                .iter()
                .map(|element| js_value_from_handle(element.clone()))
                .collect::<Result<js_sys::Array, _>>()?
                .into(),
        },
        Handle::External(value) => value,
        Handle::Unit => JsValue::NULL,
        Handle::Value(_) => return Err(JsError::new("unsupported value").into()),
    })
}

fn handle_from_js_value<'a>(value: JsValue) -> wipple_interpreter::Handle<'a, JsValue> {
    use wipple_interpreter::{Handle, Primitive};

    if value.is_null_or_undefined() {
        Handle::Unit
    } else if let Some(string) = value.as_string() {
        Handle::Primitive(Primitive::String(Arc::from(string)))
    } else if let Some(number) = value.as_f64() {
        Handle::Primitive(Primitive::Number(number))
    } else if js_sys::Array::is_array(&value) {
        let array = js_sys::Array::from(&value);
        let elements = array.iter().map(handle_from_js_value).collect::<Vec<_>>();
        Handle::Primitive(Primitive::List(elements))
    } else {
        Handle::External(value)
    }
}

fn serialize_span(span: &Span) -> JsValue {
    #[derive(Serialize)]
    struct SerializedSpan<'a> {
        path: &'a str,
        start: &'a Location,
        end: &'a Location,
    }

    let span = SerializedSpan {
        path: &span.path,
        start: &span.start,
        end: &span.end,
    };

    serde_wasm_bindgen::to_value(&span).unwrap()
}
