use crate::*;
use std::{collections::HashMap, rc::Rc};

#[derive(Clone)]
pub struct Name {
    pub name: String,
    pub location: Option<SourceLocation>,
}

primitive!(name for Name);

#[derive(Clone)]
pub struct AssignFn(pub Rc<dyn Fn(&Value, &EnvironmentRef, &Stack) -> Result<()>>);

impl AssignFn {
    pub fn new(assign: impl Fn(&Value, &EnvironmentRef, &Stack) -> Result<()> + 'static) -> Self {
        AssignFn(Rc::new(assign))
    }
}

primitive!(assign for AssignFn);

#[derive(Clone, Copy)]
pub struct Computed;

primitive!(computed for Computed);

pub type Variables = HashMap<String, Value>;

env_key!(variables for Variables {
    EnvironmentKey::new(
        UseFn::new(|parent: &Variables, new| {
            parent.clone().into_iter().chain(new.clone()).collect()
        }),
        true,
    )
});

impl Environment {
    pub fn get_variable(&mut self, name: &str) -> Option<&Value> {
        self.variables().get(name)
    }

    pub fn set_variable(&mut self, name: &str, value: Value) {
        self.variables().insert(String::from(name), value);
    }
}

impl Name {
    pub fn resolve(&self, env: &EnvironmentRef, stack: &Stack) -> Result {
        let variable = self.resolve_without_computing(env, stack)?;

        if variable.has_trait(TraitID::computed(), env, &stack)? {
            variable.evaluate(env, &stack)
        } else {
            Ok(variable)
        }
    }

    pub fn resolve_without_computing(&self, env: &EnvironmentRef, stack: &Stack) -> Result {
        let stack = stack.add(|| format!("Resolving variable '{}'", self.name));

        fn get(name: &Name, env: &EnvironmentRef) -> Option<Value> {
            let variable = env.borrow_mut().variables().get(&name.name).cloned();
            if let Some(variable) = variable {
                return Some(variable);
            }

            let parent = env.borrow_mut().parent.clone();
            parent.and_then(|parent| get(name, &parent))
        }

        get(self, env).ok_or_else(|| Error::new("Name does not refer to a variable", &stack))
    }
}

pub(crate) fn setup(env: &mut Environment) {
    // Name : trait
    env.set_variable(
        "Name",
        Value::of(TraitConstructor {
            id: TraitID::name(),
            validation: Validation::for_primitive::<Name>(),
        }),
    );

    // Name ::= Assign
    env.add_conformance_for_primitive(TraitID::assign(), |name: Name, _, _| {
        Ok(Some(Value::of(AssignFn::new(move |value, env, stack| {
            let value = value.evaluate(env, stack)?;
            env.borrow_mut().set_variable(&name.name, value);
            Ok(())
        }))))
    });

    // Name ::= Evaluate
    env.add_conformance_for_primitive(TraitID::evaluate(), |name: Name, _, _| {
        Ok(Some(Value::of(EvaluateFn::new(move |env, stack| {
            name.resolve(env, stack)
        }))))
    });

    // Name ::= Macro-Parameter
    env.add_conformance_for_primitive(TraitID::macro_parameter(), |name: Name, _, _| {
        Ok(Some(Value::of(DefineMacroParameterFn::new(
            move |value, env, stack| {
                let parameter = MacroParameter(name.name.clone());
                let replacement = value.evaluate(env, stack)?;

                Ok((parameter, replacement))
            },
        ))))
    });

    // Name ::= Macro-Expand
    env.add_conformance_for_primitive(TraitID::macro_expand(), |name: Name, _, _| {
        Ok(Some(Value::of(MacroExpandFn::new(
            move |parameter, replacement, _, _| {
                Ok(if name.name == parameter.0 {
                    replacement.clone()
                } else {
                    Value::of(name.clone())
                })
            },
        ))))
    });

    // Name ::= Text
    env.add_conformance_for_primitive(TraitID::text(), |name: Name, _, _| {
        Ok(Some(Value::of(Text {
            text: name.name,
            location: None,
        })))
    });
}
