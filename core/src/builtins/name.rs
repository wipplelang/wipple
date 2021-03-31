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
    pub type AssignFn(&Value, bool, &EnvironmentRef, Stack) -> Result<()>;
}

fundamental_primitive!(pub assign for AssignFn);

#[derive(Clone)]
pub struct Named {
    pub name: String,
}

fundamental_primitive!(pub named for Named);

fn_wrapper_struct! {
    pub type ComputeFn(Stack) -> Result;
}

#[derive(Clone)]
pub enum Variable {
    Just(Value),
    Computed(ComputeFn),
}

impl Variable {
    pub fn get_value(&self, stack: Stack) -> Result {
        match self {
            Variable::Just(value) => Ok(value.clone()),
            Variable::Computed(compute) => compute(stack),
        }
    }
}

pub type Variables = HashMap<String, Variable>;

fundamental_env_key!(pub variables for Variables {
    visibility: EnvironmentVisibility::Public(UseFn::from(|parent: &Variables, new| {
        parent.clone().into_iter().chain(new.clone()).collect()
    }))
});

fn_wrapper_struct! {
    pub type HandleAssignFn(&Value, &Value, bool, &EnvironmentRef, Stack) -> Result<()>;
}

impl Default for HandleAssignFn {
    fn default() -> Self {
        HandleAssignFn::new(|_, _, _, _, stack| {
            Err(ReturnState::Error(Error::new(
                "Cannot assign to variables inside this block",
                stack,
            )))
        })
    }
}

fundamental_env_key!(pub handle_assign for HandleAssignFn {
    visibility: EnvironmentVisibility::Private,
});

impl Environment {
    pub fn set_variable(&mut self, name: &str, value: Value) {
        self.variables()
            .insert(name.to_string(), Variable::Just(value));
    }

    pub fn set_computed_variable(name: &str, value: Value, env: &EnvironmentRef) {
        let captured_env = env.clone();

        env.borrow_mut().variables().insert(
            name.to_string(),
            Variable::Computed(ComputeFn::new(move |stack| {
                value.evaluate(&captured_env, stack)
            })),
        );
    }
}

impl Name {
    pub fn resolve(&self, env: &EnvironmentRef, stack: Stack) -> Result {
        let variable = self.resolve_variable(env, stack.clone())?;
        variable.get_value(stack)
    }

    pub fn resolve_variable(&self, env: &EnvironmentRef, stack: Stack) -> Result<Variable> {
        let stack =
            stack.update_evaluation(|e| e.with(|| format!("Resolving variable '{}'", self.name)));

        self.resolve_variable_if_present(env).ok_or_else(|| {
            ReturnState::Error(Error::new("Name does not refer to a variable", stack))
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
            r#trait: Trait::name(),
            validation: Validation::of::<Name>(),
        }),
    );

    // Name ::= Assign
    env.add_primitive_conformance(|name: Name| {
        AssignFn::new(move |value, computed, env, stack| {
            if computed {
                Environment::set_computed_variable(&name.name, value.clone(), env);
            } else {
                let value = value.evaluate(env, stack)?;
                env.borrow_mut().set_variable(&name.name, value);
            }

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
