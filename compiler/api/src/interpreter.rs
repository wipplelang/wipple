use ouroboros::self_referencing;
use serde::Serialize;
use std::{ops::ControlFlow, sync::Arc};
use wasm_bindgen::{convert::TryFromJsValue, prelude::*};
use wipple_core::{codegen::mir, span::Location};
use wipple_interpreter::Span;

#[derive(Debug, Clone, Serialize)]
struct JsValueWrapper(#[serde(with = "serde_wasm_bindgen::preserve")] JsValue);

#[wasm_bindgen]
pub struct Interpreter {
    env: Env,
    inner: InterpreterInner,
}

#[self_referencing]
struct InterpreterInner {
    program: mir::Program,

    #[borrows(program)]
    #[not_covariant]
    interpreter: wipple_interpreter::Interpreter<'this, 'static, JsValueWrapper, JsValue>,
}

#[derive(Debug, Clone)]
struct Env(Arc<JsValue>);

impl Env {
    fn call(&self, name: &str, input: JsValue) -> Result<JsValue, JsValue> {
        let func = js_sys::Reflect::get(&self.0, &JsValue::from_str(name))?;
        let func = js_sys::Function::<fn(JsValue) -> JsValue>::try_from_js_value(func)?;
        func.call1(&JsValue::NULL, &input)
    }
}

#[wasm_bindgen]
impl Interpreter {
    #[wasm_bindgen(constructor)]
    pub fn new(bytes: Vec<u8>, env: JsValue) -> Result<Self, JsValue> {
        let program = rmp_serde::from_slice::<mir::Program>(&bytes).map_err(JsError::from)?;

        let env = Env(Arc::new(env));

        Ok(Interpreter {
            env: env.clone(),
            inner: InterpreterInner::new(program, move |program| {
                wipple_interpreter::Interpreter::new(program, move |name, input| {
                    js_value_from_handle(input)
                        .and_then(|input| env.call(name, input))
                        .map(handle_from_js_value)
                })
            }),
        })
    }

    #[wasm_bindgen(setter, js_name = "debug")]
    pub fn set_debug(&mut self, enabled: bool) {
        if enabled {
            self.inner.with_interpreter_mut(|interpreter| {
                interpreter.enable_debugging();
            });
        }
    }

    #[wasm_bindgen(js_name = "run")]
    pub fn run(&mut self) -> Result<(), JsValue> {
        while self.step()? {}
        Ok(())
    }

    #[wasm_bindgen(js_name = "step")]
    pub fn step(&mut self) -> Result<bool, JsValue> {
        self.inner.with_interpreter_mut(|interpreter| {
            let ControlFlow::Continue(()) = interpreter.step()? else {
                return Ok(false);
            };

            while let Some(event) = interpreter.next_debug_event() {
                if let wipple_interpreter::DebugEvent::Span(span) = event {
                    self.env.call("trace", serialize_span(&span))?;
                }
            }

            Ok(true)
        })
    }
}

fn js_value_from_handle(
    handle: wipple_interpreter::Handle<'_, JsValueWrapper>,
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
        Handle::External(JsValueWrapper(value)) => value,
        Handle::Unit => JsValue::NULL,
        Handle::Value(_) => return Err(JsError::new("unsupported value").into()),
    })
}

fn handle_from_js_value<'a>(value: JsValue) -> wipple_interpreter::Handle<'a, JsValueWrapper> {
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
        Handle::External(JsValueWrapper(value))
    }
}

fn serialize_span(span: &Span) -> JsValue {
    #[derive(Serialize)]
    struct SerializedSpan {
        path: String,
        start: Location,
        end: Location,
    }

    let span = SerializedSpan {
        path: span.path.to_string(),
        start: span.start.clone(),
        end: span.end.clone(),
    };

    serde_wasm_bindgen::to_value(&span).unwrap()
}
