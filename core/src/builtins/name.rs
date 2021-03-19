use crate::*;
use std::collections::HashMap;

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

fn_wrapper_struct! {
    pub type AssignFn(&Value, &EnvironmentRef, &Stack) -> Result<()>;
}

fundamental_primitive!(pub assign for AssignFn);

#[derive(Clone)]
pub struct Named {
    pub name: String,
}

fundamental_primitive!(pub named for Named);

#[derive(Clone)]
pub struct Variable {
    pub value: Value,
    pub computed: bool,
}

pub type Variables = HashMap<String, Variable>;

fundamental_env_key!(pub variables for Variables {
    EnvironmentKey::new(
        UseFn::from(|parent: &Variables, new| {
            parent.clone().into_iter().chain(new.clone()).collect()
        }),
        true,
    )
});

fn_wrapper_struct! {
    pub type AssignHandlerFn(&str, Value, &mut Environment, &Stack) -> Result<()>;
}

impl Default for AssignHandlerFn {
    fn default() -> Self {
        AssignHandlerFn::new(|_, _, _, stack| {
            Err(ReturnState::Error(Error::new(
                "Cannot assign to variables inside this block",
                stack,
            )))
        })
    }
}

fundamental_env_key!(pub handle_assign for AssignHandlerFn {
    EnvironmentKey::new(
        UseFn::take_new(),
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

        self.variables().insert(
            name,
            Variable {
                value,
                computed: false,
            },
        );
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
        let variable = self.resolve_variable(resolve_env, stack)?;

        if variable.computed {
            variable.value.evaluate(compute_env, &stack)
        } else {
            Ok(variable.value)
        }
    }

    pub fn resolve_variable(&self, env: &EnvironmentRef, stack: &Stack) -> Result<Variable> {
        let stack = stack.add(|| format!("Resolving variable '{}'", self.name));

        self.resolve_variable_if_present(env).ok_or_else(|| {
            ReturnState::Error(Error::new("Name does not refer to a variable", &stack))
        })
    }

    pub fn resolve_variable_if_present(&self, env: &EnvironmentRef) -> Option<Variable> {
        fn get(name: &Name, env: &EnvironmentRef) -> Option<Variable> {
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
            validation: Validation::of::<Name>(),
        }),
    );

    // Name ::= Assign
    env.add_primitive_conformance(|name: Name| {
        AssignFn::new(move |value, env, stack| {
            let value = value.evaluate(env, stack)?;
            env.borrow_mut().set_variable(&name.name, value);
            Ok(())
        })
    });

    // Name ::= Evaluate
    env.add_primitive_conformance(|name: Name| {
        EvaluateFn::new(move |env, stack| name.resolve(env, stack))
    });

    // Name ::= Replace-In-Template
    env.add_primitive_conformance(|name: Name| {
        ReplaceInTemplateFn::new(move |parameter, replacement, _, _| {
            Ok(if name.name == parameter {
                replacement.clone()
            } else {
                Value::of(name.clone())
            })
        })
    });

    // Name ::= Text
    env.add_primitive_conformance(|name: Name| Text::new(&name.name));
}
