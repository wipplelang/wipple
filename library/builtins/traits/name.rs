use crate::*;
use std::collections::HashMap;
use wipple::*;

#[derive(TypeInfo, Debug, Clone)]
pub struct Name {
    pub name: String,
    pub location: Option<SourceLocation>,
}

impl Primitive for Name {}

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

stored_closure!(pub struct ComputeFn(&Env, &Stack) -> Result<Value>);

#[derive(Debug, Clone)]
pub enum Variable {
    Local(Value),
    Constant(Value),
    Computed(ComputeFn),
}

impl Variable {
    pub fn get_value(&self, env: &Env, stack: &Stack) -> Result<Value> {
        match self {
            Variable::Local(value) | Variable::Constant(value) => Ok(value.clone()),
            Variable::Computed(compute) => compute(env, stack),
        }
    }
}

#[derive(TypeInfo, Debug, Clone, Default)]
pub struct Variables(pub HashMap<String, Variable>);

env_key!(pub variables: Variables {
    visibility: EnvKeyVisibility::Public(UseMergeFn::merging(|parent: &mut Variables, new, _, _| {
        for (name, variable) in &new.0 {
            parent.0.entry(name.clone()).or_insert_with(|| variable.clone());
        }

        Ok(())
    })),
});

stored_closure!(pub struct AssignmentFn(VariadicInput, VariadicInput, &Env, &Stack) -> Result<()>);

impl Default for AssignmentFn {
    fn default() -> Self {
        AssignmentFn::new(|left, right, env, stack| {
            let left = Value::from(left);
            let right = Value::from(right);

            let name = left.get_or::<Name>("Expected name", env, stack)?;
            let value = right.evaluate(env, stack)?;

            env.set_variable(stack, &name.name, value.into_owned())?;

            Ok(())
        })
    }
}

env_key!(pub assignment: AssignmentFn {
    visibility: EnvKeyVisibility::Private,
});

stored_closure!(pub struct ComputedAssignmentFn(VariadicInput, VariadicInput, &Env, &Stack) -> Result<()>);

impl Default for ComputedAssignmentFn {
    fn default() -> Self {
        ComputedAssignmentFn::new(|left, right, env, stack| {
            let left = Value::from(left);
            let right = Value::from(right);

            let name = left.get_or::<Name>("Expected name", env, stack)?;

            let captured_env = env.child();

            env.set_computed_variable(
                stack,
                &name.name,
                ComputeFn::new(move |_, stack| {
                    catch!(Return in right.evaluate(&captured_env, stack).map(Cow::into_owned))
                }),
            )?;

            Ok(())
        })
    }
}

env_key!(pub computed_assignment: ComputedAssignmentFn {
    visibility: EnvKeyVisibility::Private,
});

macro_rules! set_variable {
    ($name:expr, $variable:expr, $env:expr, $stack:expr) => {{
        if let Some(variable) = get_variable_recursive($name, $env) {
            if matches!(variable, Variable::Constant(_)) {
                return Ok(()); // assignments to constants are ignored
            }
        }

        if $env.variables().0.contains_key($name) {
            return Err(error(
                &format!(
                    "Cannot assign to the same variable ('{}') more than once per scope",
                    $name,
                ),
                $stack,
            ));
        }

        $env.update_variables(|variables| {
            variables.0.insert($name.to_string(), $variable);
        });

        Ok(())
    }};
}

#[ext(pub, name = EnvVariablesExt)]
impl Env {
    fn set_variable(&self, stack: &Stack, name: &str, value: Value) -> Result<()> {
        set_variable!(name, Variable::Local(value), self, stack)
    }

    fn set_constant_variable(&self, stack: &Stack, name: &str, value: Value) -> Result<()> {
        set_variable!(name, Variable::Constant(value), self, stack)
    }

    fn set_computed_variable(&self, stack: &Stack, name: &str, compute: ComputeFn) -> Result<()> {
        set_variable!(name, Variable::Computed(compute), self, stack)
    }
}

impl Name {
    pub fn resolve(&self, env: &Env, stack: &Stack) -> Result<Value> {
        let variable = self.resolve_variable(env, stack)?;
        variable.get_value(env, stack)
    }

    pub fn resolve_if_present(&self, env: &Env, stack: &Stack) -> Result<Option<Value>> {
        let mut stack = stack.clone();
        stack
            .diagnostics_mut()
            .add(|| format!("Resolving variable '{}'", self.name));

        match self.resolve_variable_if_present(env) {
            Some(variable) => variable.get_value(env, &stack).map(Some),
            None => Ok(None),
        }
    }

    pub fn resolve_variable<'a>(&self, env: &'a Env, stack: &Stack) -> Result<Variable> {
        let mut stack = stack.clone();
        stack
            .diagnostics_mut()
            .add(|| format!("Resolving variable '{}'", self.name));

        self.resolve_variable_if_present(env).ok_or_else(|| {
            error(
                &format!("'{}' does not refer to a variable", self.name),
                &stack,
            )
        })
    }

    pub fn resolve_variable_if_present(&self, env: &Env) -> Option<Variable> {
        get_variable_recursive(&self.name, env)
    }
}

fn get_variable_recursive(name: &str, env: &Env) -> Option<Variable> {
    env.variables().0.remove(name).or_else(|| {
        env.parent()
            .and_then(|parent| get_variable_recursive(name, parent))
    })
}

pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    // Name : trait
    env.set_variable(stack, "Name", Value::of(Trait::of::<Name>()))?;

    // Name == Evaluate
    env.add_relation_between(stack, |name: Name| {
        EvaluateFn::new(move |env, stack| name.resolve(env, stack))
    })?;

    // Name == Text
    env.add_relation_between(stack, |name: Name| Text::new(name.name))?;

    Ok(())
}
