use crate::*;
use std::collections::HashMap;

#[derive(TypeInfo, Debug, Clone)]
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

core_primitive!(pub name for Name);

fn_wrapper_struct! {
    pub type ComputeFn(&EnvironmentRef, &Stack) -> Result;
}

#[derive(Debug, Clone)]
pub enum Variable {
    Just(Value),
    Computed(ComputeFn),
}

impl Variable {
    pub fn get_value(&self, env: &EnvironmentRef, stack: &Stack) -> Result {
        match self {
            Variable::Just(value) => Ok(value.clone()),
            Variable::Computed(compute) => compute(env, stack),
        }
    }
}

#[derive(TypeInfo, Debug, Clone, Default)]
pub struct Variables(pub HashMap<String, Variable>);

core_env_key!(pub variables for Variables {
    visibility: EnvironmentVisibility::Public(UseFn::from(|parent: &Variables, new| {
        Variables(parent.0.clone().into_iter().chain(new.0.clone()).collect())
    }))
});

fn_wrapper_struct! {
    #[derive(TypeInfo)]
    pub type HandleAssignFn(&Name, &Value, &EnvironmentRef, &Stack) -> Result<()>;
}

impl Default for HandleAssignFn {
    fn default() -> Self {
        HandleAssignFn::new(|_, _, _, stack| {
            Err(ReturnState::Error(Error::new(
                "Cannot assign to variables here",
                stack,
            )))
        })
    }
}

core_env_key!(pub handle_assign for HandleAssignFn {
    visibility: EnvironmentVisibility::Private,
});

fn_wrapper_struct! {
    #[derive(TypeInfo)]
    pub type HandleComputedAssignFn(&Name, &Value, &EnvironmentRef, &Stack) -> Result<()>;
}

impl Default for HandleComputedAssignFn {
    fn default() -> Self {
        HandleComputedAssignFn::new(|_, _, _, stack| {
            Err(ReturnState::Error(Error::new(
                "Cannot assign to computed variables here",
                stack,
            )))
        })
    }
}

core_env_key!(pub handle_computed_assign for HandleComputedAssignFn {
    visibility: EnvironmentVisibility::Private,
});

impl Environment {
    pub fn set_variable(&mut self, name: &str, value: Value) {
        self.variables()
            .0
            .insert(name.to_string(), Variable::Just(value));
    }

    pub fn set_computed_variable(
        &mut self,
        name: &str,
        compute: impl Fn(&EnvironmentRef, &Stack) -> Result + 'static,
    ) {
        self.variables().0.insert(
            name.to_string(),
            Variable::Computed(ComputeFn::new(compute)),
        );
    }
}

impl Name {
    pub fn resolve(&self, env: &EnvironmentRef, stack: &Stack) -> Result {
        let variable = self.resolve_variable(env, stack)?;
        variable.get_value(env, stack)
    }

    pub fn resolve_if_present(&self, env: &EnvironmentRef, stack: &Stack) -> Result<Option<Value>> {
        let mut stack = stack.clone();
        stack
            .evaluation_mut()
            .set(|| format!("Resolving variable '{}'", self.name));

        match self.resolve_variable_if_present(env) {
            Some(variable) => variable.get_value(env, &stack).map(Some),
            None => Ok(None),
        }
    }

    pub fn resolve_variable(&self, env: &EnvironmentRef, stack: &Stack) -> Result<Variable> {
        let mut stack = stack.clone();
        stack
            .evaluation_mut()
            .set(|| format!("Resolving variable '{}'", self.name));

        self.resolve_variable_if_present(env).ok_or_else(|| {
            ReturnState::Error(Error::new(
                &format!("Name '{}' does not refer to a variable", self.name),
                &stack,
            ))
        })
    }

    pub fn resolve_variable_if_present(&self, env: &EnvironmentRef) -> Option<Variable> {
        fn get(name: &Name, env: &EnvironmentRef) -> Option<Variable> {
            let variable = env.borrow_mut().variables().0.get(&name.name).cloned();
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
    env.set_variable("Name", Value::of(Trait::of::<Name>()));

    // Name == Evaluate
    env.add_primitive_conformance(|name: Name| {
        EvaluateFn::new(move |env, stack| name.resolve(env, stack))
    });

    // Name == Replace-In-Template
    env.add_primitive_conformance(|name: Name| {
        ReplaceInTemplateFn::new(move |parameter, replacement, _, _| {
            Ok(if name.name == parameter {
                replacement.clone()
            } else {
                Value::of(name.clone())
            })
        })
    });

    // Name == Text
    env.add_primitive_conformance(|name: Name| Text::new(&name.name));
}
