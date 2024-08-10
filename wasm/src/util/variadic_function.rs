use wasm_bindgen::prelude::*;
use web_sys::js_sys;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(extends = js_sys::Function, is_type_of = JsValue::is_function, typescript_type = "Function")]
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub type VariadicFunction;

    #[wasm_bindgen(method, catch, js_name = call, variadic)]
    pub fn call_variadic(
        this: &VariadicFunction,
        context: &JsValue,
        args: &JsValue,
    ) -> Result<JsValue, JsValue>;
}
