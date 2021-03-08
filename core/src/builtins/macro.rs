use crate::*;
use std::rc::Rc;

pub struct MacroParameter(pub String);

#[derive(Clone)]
pub struct DefineMacroParameterFn(
    #[allow(clippy::type_complexity)]
    pub  Rc<dyn Fn(&Value, &EnvironmentRef, &Stack) -> Result<(MacroParameter, Value)>>,
);

impl DefineMacroParameterFn {
    pub fn new(
        define: impl Fn(&Value, &EnvironmentRef, &Stack) -> Result<(MacroParameter, Value)> + 'static,
    ) -> Self {
        DefineMacroParameterFn(Rc::new(define))
    }
}

fundamental_primitive!(pub macro_parameter for DefineMacroParameterFn);

#[derive(Clone)]
pub struct MacroExpandFn(
    pub Rc<dyn Fn(&MacroParameter, &Value, &EnvironmentRef, &Stack) -> Result>,
);

impl MacroExpandFn {
    pub fn new(
        define: impl Fn(&MacroParameter, &Value, &EnvironmentRef, &Stack) -> Result + 'static,
    ) -> Self {
        MacroExpandFn(Rc::new(define))
    }
}

fundamental_primitive!(pub macro_expand for MacroExpandFn);

impl Value {
    pub fn macro_expand(
        &self,
        parameter: &MacroParameter,
        replacement: &Value,
        env: &EnvironmentRef,
        stack: &Stack,
    ) -> Result {
        match self.get_primitive_if_present::<MacroExpandFn>(env, stack)? {
            Some(macro_expand) => macro_expand.0(parameter, replacement, env, stack),
            None => Ok(self.clone()),
        }
    }
}

#[derive(Clone)]
pub struct Macro {
    pub define_parameter: DefineMacroParameterFn,
    pub value_to_expand: Value,
}

fundamental_primitive!(pub r#macro for Macro);

pub(crate) fn setup(env: &mut Environment) {
    env.add_primitive_conformance(|r#macro: Macro| {
        Function::new(move |value, env, stack| {
            let (parameter, replacement) = r#macro.define_parameter.0(value, env, stack)?;

            r#macro
                .value_to_expand
                .macro_expand(&parameter, &replacement, env, stack)?
                .evaluate(env, stack)
        })
    });

    env.add_primitive_conformance(|_: Macro| Text {
        text: String::from("<macro>"),
        location: None,
    })
}
