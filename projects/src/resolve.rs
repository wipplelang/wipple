use std::path::PathBuf;
use wipple::*;

env_key!(inner_project_root for Option<PathBuf> {
    EnvironmentKey::new(
        UseFn::take_new(),
        true,
    )
});

pub fn get_project_root(env: &EnvironmentRef, stack: &Stack) -> Result<PathBuf> {
    let env = &mut env.borrow_mut();

    if let Some(path) = get_inner_project_root(env).as_mut() {
        Ok(path.clone())
    } else if let Some(parent) = &env.parent {
        get_project_root(&parent, stack)
    } else {
        Err(ReturnState::Error(Error::new(
            "Project root is not set",
            stack,
        )))
    }
}

pub fn set_project_root(env: &mut Environment, path: Option<PathBuf>) {
    *get_inner_project_root(env) = path;
}

env_key!(inner_current_file for Option<PathBuf> {
    EnvironmentKey::new(
        UseFn::take_new(),
        true,
    )
});

pub fn get_current_file(env: &EnvironmentRef, stack: &Stack) -> Result<PathBuf> {
    let env = &mut env.borrow_mut();

    if let Some(path) = get_inner_current_file(env).as_mut() {
        Ok(path.clone())
    } else if let Some(parent) = &env.parent {
        get_current_file(&parent, stack)
    } else {
        Err(ReturnState::Error(Error::new(
            "Current file is not set",
            stack,
        )))
    }
}

pub fn set_current_file(env: &mut Environment, path: Option<PathBuf>) {
    *get_inner_current_file(env) = path;
}

/// Resolve a module name into a path.
pub fn resolve(module_name: &str, env: &EnvironmentRef, stack: &Stack) -> Result<PathBuf> {
    let base = if module_name.starts_with("./") || module_name.starts_with("../") {
        get_current_file(env, stack)
    } else {
        get_project_root(env, stack)
    }?;

    let path = base.join(module_name);

    Ok(path)
}
