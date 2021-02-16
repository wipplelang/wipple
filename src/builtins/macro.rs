use crate::builtins::*;
use crate::fundamentals::*;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct MacroParameter(pub Name);

#[derive(Clone)]
pub struct DefineMacroParameterFn(
    pub Rc<dyn Fn(Value, &mut Environment, &ProgramStack) -> Result<(MacroParameter, Value)>>,
);

impl DefineMacroParameterFn {
    pub fn new(
        define_parameter: impl Fn(Value, &mut Environment, &ProgramStack) -> Result<(MacroParameter, Value)>
            + 'static,
    ) -> DefineMacroParameterFn {
        DefineMacroParameterFn(Rc::new(define_parameter))
    }
}

simple_trait! {
    name: macro_parameter,
    type: DefineMacroParameterFn,
    label: "Macro-Parameter",
}

#[derive(Clone)]
pub struct MacroExpandFn(
    pub Rc<dyn Fn(MacroParameter, Value, &mut Environment, &ProgramStack) -> Result>,
);

impl MacroExpandFn {
    pub fn new(
        expand: impl Fn(MacroParameter, Value, &mut Environment, &ProgramStack) -> Result + 'static,
    ) -> MacroExpandFn {
        MacroExpandFn(Rc::new(expand))
    }
}

simple_trait! {
    name: macro_expand,
    type: MacroExpandFn,
    label: "Macro-Expand",
}

impl Value {
    pub fn macro_expand(
        &self,
        parameter: MacroParameter,
        replacement: Value,
        env: &mut Environment,
        stack: &ProgramStack,
    ) -> Result {
        let stack = stack.add(&format!(
            "Expanding '{}', replacing '{}' with '{}'",
            self.format(env, stack),
            parameter.0 .0,
            replacement.format(env, stack)
        ));

        match self.get_trait_if_present(TraitID::macro_expand, env, &stack)? {
            Some(expand) => expand.0(parameter, replacement, env, &stack),
            None => Ok(self.clone()),
        }
    }
}

#[derive(Clone)]
pub struct Macro {
    pub define_parameter: DefineMacroParameterFn,
    pub value_to_expand: Value,
}

simple_trait! {
    name: r#macro,
    type: Macro,
    label: "Macro",
}

pub(crate) fn init(env: &mut Environment) {
    // Macro ::= Function
    env.add_conformance(Conformance::new(
        TraitID::function,
        TraitID::r#macro.validation(),
        |r#macro, _, _| {
            let r#macro = r#macro.clone();

            Ok(Function::new(move |input, env, stack| {
                let (parameter, replacement) = r#macro.define_parameter.0(input, env, stack)?;

                r#macro
                    .value_to_expand
                    .macro_expand(parameter, replacement, env, stack)?
                    .evaluate(env, stack)
            }))
        },
    ));

    // Macro ::= Text
    env.add_conformance(Conformance::new(
        TraitID::text,
        TraitID::r#macro.validation(),
        |_, _, _| Ok(Text(String::from("<macro>"))),
    ));
}
