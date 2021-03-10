use crate::*;
use std::{collections::HashMap, rc::Rc};

#[derive(Clone)]
pub struct Name {
    pub name: String,
    pub location: Option<SourceLocation>,
}

impl Name {
    pub fn new(name: &str) -> Self {
        Name::new_located(name, None)
    }

    pub fn new_located(name: &str, location: Option<SourceLocation>) -> Self {
        Name {
            name: String::from(name),
            location,
        }
    }
}

fundamental_primitive!(pub name for Name);

#[derive(Clone)]
pub struct AssignFn(pub Rc<dyn Fn(&Value, &EnvironmentRef, &Stack) -> Result<()>>);

impl AssignFn {
    pub fn new(assign: impl Fn(&Value, &EnvironmentRef, &Stack) -> Result<()> + 'static) -> Self {
        AssignFn(Rc::new(assign))
    }
}

fundamental_primitive!(pub assign for AssignFn);

#[derive(Clone, Copy)]
pub struct Computed;

fundamental_primitive!(pub computed for Computed);

#[derive(Clone)]
pub struct Named {
    pub name: String,
}

fundamental_primitive!(pub named for Named);

pub type Variables = HashMap<String, Value>;

fundamental_env_key!(pub variables for Variables {
    EnvironmentKey::new(
        UseFn::new(|parent: &Variables, new| {
            parent.clone().into_iter().chain(new.clone()).collect()
        }),
        true,
    )
});

impl Environment {
    pub fn set_variable(&mut self, name: &str, value: Value) {
        let name = String::from(name);

        // Add a 'Named' trait to the value if it isn't already named
        let value = if value.has_trait_directly(TraitID::named()) {
            value
        } else {
            value.add(&Trait::of_primitive(Named { name: name.clone() }))
        };

        self.variables().insert(name, value);
    }

    pub fn add_text_conformance(&mut self, id: TraitID, value_name: &'static str) {
        self.add_conformance(TraitID::text(), move |value, env, stack| {
            if !value.has_trait(id, env, stack)? {
                return Ok(None);
            }

            let named = value.get_primitive_if_present::<Named>(env, stack)?;

            Ok(Some(Value::of(Text::new(&match named {
                Some(named) => format!("<{} '{}'>", value_name, named.name),
                None => format!("<{}>", value_name),
            }))))
        });
    }
}

impl Name {
    pub fn resolve(&self, env: &EnvironmentRef, stack: &Stack) -> Result {
        self.resolve_in(env, env, stack)
    }

    pub fn resolve_in(
        &self,
        resolve_env: &EnvironmentRef,
        compute_env: &EnvironmentRef,
        stack: &Stack,
    ) -> Result {
        let variable = self.resolve_without_computing(resolve_env, stack)?;

        if variable.has_trait(TraitID::computed(), compute_env, &stack)? {
            variable.evaluate(compute_env, &stack)
        } else {
            Ok(variable)
        }
    }

    pub fn resolve_without_computing(&self, env: &EnvironmentRef, stack: &Stack) -> Result {
        let stack = stack.add(|| format!("Resolving variable '{}'", self.name));

        self.resolve_without_computing_if_present(env)
            .ok_or_else(|| {
                ReturnState::Error(Error::new("Name does not refer to a variable", &stack))
            })
    }

    pub fn resolve_without_computing_if_present(&self, env: &EnvironmentRef) -> Option<Value> {
        fn get(name: &Name, env: &EnvironmentRef) -> Option<Value> {
            let variable = env.borrow_mut().variables().get(&name.name).cloned();
            if let Some(variable) = variable {
                return Some(variable);
            }

            let parent = env.borrow_mut().parent.clone();
            parent.and_then(|parent| get(name, &parent))
        }

        get(self, env)
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

    env.add_primitive_conformance(|name: Name| {
        AssignFn::new(move |value, env, stack| {
            let value = value.evaluate(env, stack)?;
            env.borrow_mut().set_variable(&name.name, value);
            Ok(())
        })
    });

    env.add_primitive_conformance(|name: Name| {
        EvaluateFn::new(move |env, stack| name.resolve(env, stack))
    });

    env.add_primitive_conformance(|name: Name| {
        DefineMacroParameterFn::new(move |value, env, stack| {
            let parameter = MacroParameter(name.name.clone());
            let replacement = value.evaluate(env, stack)?;

            Ok((parameter, replacement))
        })
    });

    env.add_primitive_conformance(|name: Name| {
        MacroExpandFn::new(move |parameter, replacement, _, _| {
            Ok(if name.name == parameter.0 {
                replacement.clone()
            } else {
                Value::of(name.clone())
            })
        })
    });

    env.add_primitive_conformance(|name: Name| Text {
        text: name.name,
        location: None,
    });
}
