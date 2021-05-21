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
    Mutable(Value),
    Constant(Value),
    Computed(ComputeFn),
}

impl Variable {
    pub fn get_value(&self, env: &Environment, stack: &Stack) -> Result {
        match self {
            Variable::Mutable(value) | Variable::Constant(value) => Ok(value.clone()),
            Variable::Computed(compute) => compute(env, stack),
        }
    }
}

#[derive(TypeInfo, Debug, Clone, Default)]
pub struct Variables(pub HashMap<String, Variable>);

core_env_key!(pub variables for Variables {
    visibility: EnvironmentKeyVisibility::Public(UseMergeFn::of(|parent: &mut Variables, new| {
        for (key, value) in new.0 {
            parent.0.entry(key).or_insert(value);
        }
    })),
});

fn_wrapper! {
    #[derive(TypeInfo)]
    pub struct AssignmentFn(VariadicOperatorInput, Value, &Environment, &Stack) -> Result<()>;
}

impl Default for AssignmentFn {
    fn default() -> Self {
        AssignmentFn::new(|left, right, env, stack| {
            let name = left
                .into_value()
                .get_or::<Name>("Expected name", env, stack)?
                .name;

            let value = right.evaluate(env, stack)?;

            env.borrow_mut().set_variable(&name, value);

            Ok(())
        })
    }
}

core_env_key!(pub assignment for AssignmentFn {
    visibility: EnvironmentKeyVisibility::Private,
});

fn_wrapper! {
    #[derive(TypeInfo)]
    pub struct ComputedAssignmentFn(VariadicOperatorInput, Value, &Environment, &Stack) -> Result<()>;
}

impl Default for ComputedAssignmentFn {
    fn default() -> Self {
        ComputedAssignmentFn::new(|left, right, env, stack| {
            let name = left
                .into_value()
                .get_or::<Name>("Expected name", env, stack)?
                .name;

            let captured_env = env::child_of(env).into();

            env.borrow_mut()
                .set_computed_variable(&name, move |_, stack| right.evaluate(&captured_env, stack));

            Ok(())
        })
    }
}

core_env_key!(pub computed_assignment for ComputedAssignmentFn {
    visibility: EnvironmentKeyVisibility::Private,
});

impl EnvironmentInner {
    pub fn set_variable(&mut self, name: &str, value: Value) {
        if let Some(variable) = get_variable_recursive(name, self) {
            if matches!(variable, Variable::Constant(_)) {
                return; // assignments to constants are ignored
            }
        }

        self.variables()
            .0
            .insert(name.to_string(), Variable::Mutable(value));
    }

    pub fn set_constant_variable(&mut self, name: &str, value: Value) {
        self.variables()
            .0
            .insert(name.to_string(), Variable::Constant(value));
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
        get_variable_recursive(&self.name, &mut env.borrow_mut())
    }
}

fn get_variable_recursive(name: &str, env: &mut EnvironmentInner) -> Option<Variable> {
    fn get_recursive(name: &str, env: &mut EnvironmentInner) -> Option<Variable> {
        let variable = env.variables().0.get(name).cloned();
        if let Some(variable) = variable {
            return Some(variable);
        }

        let parent = env.parent.clone();
        parent.and_then(|parent| get_recursive(name, &mut parent.borrow_mut()))
    }

    get_recursive(name, env)
}

pub(crate) fn setup(env: &mut EnvironmentInner) {
    // Name : trait
    env.set_variable("Name", Value::of(Trait::of::<Name>()));

    // Name == Evaluate
    env.add_primitive_relation(|name: Name| {
        EvaluateFn::new(move |env, stack| name.resolve(env, stack))
    });

    // Name == Replace-In-Template
    env.add_primitive_relation(|name: Name| {
        ReplaceInTemplateFn::new(move |parameter, replacement, _, _| {
            Ok(if name.name == parameter {
                replacement.clone()
            } else {
                Value::of(name.clone())
            })
        })
    });

    // Name == Text
    env.add_primitive_relation(|name: Name| Text::new(&name.name));
}
