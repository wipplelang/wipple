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
    Mutable(Value),
    Constant(Value),
    Computed(ComputeFn),
}

impl Variable {
    pub fn get_value(&self, env: &Env, stack: &Stack) -> Result<Value> {
        match self {
            Variable::Mutable(value) | Variable::Constant(value) => Ok(value.clone()),
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

            env.set_variable(&name.name, value.into_owned());

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
                &name.name,
                ComputeFn::new(move |_, stack| {
                    Ok(right.evaluate(&captured_env, stack)?.into_owned())
                }),
            );

            Ok(())
        })
    }
}

env_key!(pub computed_assignment: ComputedAssignmentFn {
    visibility: EnvKeyVisibility::Private,
});

macro_rules! set_variable {
    ($self:expr, $name:expr, $value:expr, $kind:ident) => {
        $self.update_variables(|variables| {
            variables
                .0
                .insert($name.to_string(), Variable::$kind($value));
        })
    };
}

#[ext(pub, name = EnvVariablesExt)]
impl Env {
    fn set_variable(&self, name: &str, value: Value) {
        if let Some(variable) = get_variable_recursive(name, self) {
            if matches!(variable, Variable::Constant(_)) {
                return; // assignments to constants are ignored
            }
        };

        set_variable!(self, name, value, Mutable);
    }

    fn set_constant_variable(&self, name: &str, value: Value) {
        set_variable!(self, name, value, Constant);
    }

    fn set_computed_variable(&self, name: &str, compute: ComputeFn) {
        set_variable!(self, name, compute, Computed);
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
            .evaluation_mut()
            .add(|| format!("Resolving variable '{}'", self.name));

        match self.resolve_variable_if_present(env) {
            Some(variable) => variable.get_value(env, &stack).map(Some),
            None => Ok(None),
        }
    }

    pub fn resolve_variable<'a>(&self, env: &'a Env, stack: &Stack) -> Result<Variable> {
        let mut stack = stack.clone();
        stack
            .evaluation_mut()
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
    let mut variables = env.variables();

    if variables.0.contains_key(name) {
        return Some(variables.0.remove(name).unwrap());
    }

    env.parent()
        .and_then(|parent| get_variable_recursive(name, parent))
}

pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    // Name : trait
    env.set_variable("Name", Value::of(Trait::of::<Name>()));

    // Name == Evaluate
    env.add_relation_between(stack, |name: Name| {
        EvaluateFn::new(move |env, stack| name.resolve(env, stack))
    })?;

    // Name == Text
    env.add_relation_between(stack, |name: Name| Text::new(name.name))?;

    Ok(())
}
