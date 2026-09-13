use serde::Serialize;
use std::sync::Arc;
use wasm_bindgen::{convert::TryFromJsValue, prelude::*};
use wipple_core::{anyhow, codegen, span::Location};
use wipple_interpreter::Span;

#[wasm_bindgen]
pub fn run(bytes: &[u8], env: JsValue) -> Result<(), JsError> {
    let mir = rmp_serde::from_slice::<codegen::mir::Program>(bytes)?;

    struct Env(JsValue);

    impl Env {
        fn call(&self, name: &str, input: JsValue) -> Result<JsValue, anyhow::Error> {
            let func = js_sys::Reflect::get(&self.0, &JsValue::from_str(name))
                .map_err(|_| anyhow::format_err!("unsupported external {name:?}"))?;

            let func = js_sys::Function::<fn(JsValue) -> JsValue>::try_from_js_value(func)
                .map_err(|_| anyhow::format_err!("external {name:?} is not a function"))?;

            func.call1(&JsValue::NULL, &input)
                .map_err(|_| anyhow::format_err!("external {name:?} failed"))
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
        let input =
            js_value_from_handle(input).map_err(|_| anyhow::format_err!("unsupported input"))?;

        let output = env.call(name, input)?;

        handle_from_js_value(output)
    })
    .with_debugger(debugger);

    interpreter
        .run(&mir)
        .map_err(|e| JsError::new(&e.to_string()))?;

    Ok(())
}

fn js_value_from_handle(
    handle: wipple_interpreter::Handle<'_, JsValue>,
) -> Result<JsValue, anyhow::Error> {
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
        Handle::Value(_) => return Err(anyhow::format_err!("unsupported value")),
    })
}

fn handle_from_js_value<'a>(
    value: JsValue,
) -> Result<wipple_interpreter::Handle<'a, JsValue>, anyhow::Error> {
    use wipple_interpreter::{Handle, Primitive};

    if let Some(string) = value.as_string() {
        Ok(Handle::Primitive(Primitive::String(Arc::from(string))))
    } else if let Some(number) = value.as_f64() {
        Ok(Handle::Primitive(Primitive::Number(number)))
    } else if js_sys::Array::is_array(&value) {
        let array = js_sys::Array::from(&value);

        let elements = array
            .iter()
            .map(handle_from_js_value)
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Handle::Primitive(Primitive::List(elements)))
    } else {
        Ok(Handle::External(value))
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
