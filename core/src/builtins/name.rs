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

fn_wrapper! {
    pub struct ComputeFn(&Environment, &Stack) -> Result;
}

#[derive(Debug, Clone)]
pub enum Variable {
    Just(Value),
    Computed(ComputeFn),
}

impl Variable {
    pub fn get_value(&self, env: &Environment, stack: &Stack) -> Result {
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

fn_wrapper! {
    #[derive(TypeInfo)]
    pub struct AssignmentFn(&Name, &Value, &Environment, &Stack) -> Result<()>;
}

impl Default for AssignmentFn {
    fn default() -> Self {
        AssignmentFn::new(|left, right, env, stack| {
            let value = right.evaluate(env, stack)?;
            env.borrow_mut().set_variable(&left.name, value);
            Ok(())
        })
    }
}

core_env_key!(pub assignment for AssignmentFn {
    visibility: EnvironmentVisibility::Private,
});

fn_wrapper! {
    #[derive(TypeInfo)]
    pub struct ComputedAssignmentFn(&Name, &Value, &Environment, &Stack) -> Result<()>;
}

impl Default for ComputedAssignmentFn {
    fn default() -> Self {
        ComputedAssignmentFn::new(|left, right, env, _| {
            let right = right.clone();
            let captured_env = env::child_of(env).into();

            env.borrow_mut()
                .set_computed_variable(&left.name, move |_, stack| {
                    right.evaluate(&captured_env, stack)
                });

            Ok(())
        })
    }
}

core_env_key!(pub computed_assignment for ComputedAssignmentFn {
    visibility: EnvironmentVisibility::Private,
});

impl EnvironmentInner {
    pub fn set_variable(&mut self, name: &str, value: Value) {
        self.variables()
            .0
            .insert(name.to_string(), Variable::Just(value));
    }

    pub fn set_computed_variable(
        &mut self,
        name: &str,
        compute: impl Fn(&Environment, &Stack) -> Result + 'static,
    ) {
        self.variables().0.insert(
            name.to_string(),
            Variable::Computed(ComputeFn::new(compute)),
        );
    }
}

impl Name {
    pub fn resolve(&self, env: &Environment, stack: &Stack) -> Result {
        let variable = self.resolve_variable(env, stack)?;
        variable.get_value(env, stack)
    }

    pub fn resolve_if_present(&self, env: &Environment, stack: &Stack) -> Result<Option<Value>> {
        let mut stack = stack.clone();
        stack
            .evaluation_mut()
            .add(|| format!("Resolving variable '{}'", self.name));

        match self.resolve_variable_if_present(env) {
            Some(variable) => variable.get_value(env, &stack).map(Some),
            None => Ok(None),
        }
    }

    pub fn resolve_variable(&self, env: &Environment, stack: &Stack) -> Result<Variable> {
        let mut stack = stack.clone();
        stack
            .evaluation_mut()
            .add(|| format!("Resolving variable '{}'", self.name));

        self.resolve_variable_if_present(env).ok_or_else(|| {
            Return::error(
                &format!("Name '{}' does not refer to a variable", self.name),
                &stack,
            )
        })
    }

    pub fn resolve_variable_if_present(&self, env: &Environment) -> Option<Variable> {
        fn get_recursive(name: &Name, env: &Environment) -> Option<Variable> {
            let variable = env.borrow_mut().variables().0.get(&name.name).cloned();
            if let Some(variable) = variable {
                return Some(variable);
            }

            let parent = env.borrow_mut().parent.clone();
            parent.and_then(|parent| get_recursive(name, &parent))
        }

        get_recursive(self, env)
    }

    pub fn update_variable(
        &self,
        update: impl FnOnce(&mut Value),
        env: &Environment,
        stack: &Stack,
    ) -> Result<()> {
        let mut stack = stack.clone();
        stack
            .evaluation_mut()
            .add(|| format!("Resolving variable '{}'", self.name));

        fn update_recursive(
            name: &Name,
            update: impl FnOnce(&mut Value),
            env: &Environment,
            stack: &Stack,
        ) -> Result<()> {
            let mut env = env.borrow_mut();
            let variable = env.variables().0.get_mut(&name.name);

            if let Some(variable) = variable {
                match variable {
                    Variable::Just(value) => {
                        update(value);
                        Ok(())
                    }
                    Variable::Computed(_) => {
                        Err(Return::error("Cannot modify a computed variable", stack))
                    }
                }
            } else if let Some(parent) = env.parent.clone() {
                update_recursive(name, update, &parent, stack)
            } else {
                Err(Return::error(
                    &format!("Name '{}' does not refer to a variable", name.name),
                    &stack,
                ))
            }
        }

        update_recursive(self, update, env, &stack)
    }
}

pub(crate) fn setup(env: &mut EnvironmentInner) {
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
