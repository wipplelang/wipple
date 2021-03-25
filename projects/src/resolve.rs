use crate::*;
use std::path::PathBuf;
use wipple::*;

const TARGET: &str = env!("TARGET");

/// Resolve a module name into a path.
pub fn resolve(module_name: &str, stack: Stack) -> Result<PathBuf> {
    let path = get_path(module_name, stack.clone())?;

    if path.is_dir() {
        Ok(path)
    } else if let Some(path) = Some(path.with_extension("wpl")).filter(|p| p.exists()) {
        Ok(path)
    } else {
        Err(ReturnState::Error(Error::new(
            &format!("Cannot find module '{}'", module_name),
            stack,
        )))
    }
}

/// Resolve a module name into a path to a plugin file.
pub fn resolve_plugin(module_name: &str, stack: Stack) -> Result<PathBuf> {
    let path = get_path(module_name, stack.clone())?
        .join(TARGET)
        .with_extension("wplplugin");

    if path.exists() {
        Ok(path)
    } else {
        Err(ReturnState::Error(Error::new(
            &format!("Cannot find plugin at '{}'", path.to_string_lossy()),
            stack,
        )))
    }
}

fn get_path(module_name: &str, stack: Stack) -> Result<PathBuf> {
    let base = if module_name.starts_with("./") || module_name.starts_with("../") {
        get_current_file_in(stack.clone())
            .0
            .ok_or_else(|| ReturnState::Error(Error::new("Current file is not set", stack)))
    } else {
        get_project_root_in(stack.clone())
            .0
            .ok_or_else(|| ReturnState::Error(Error::new("Project root is not set", stack)))
    }?;

    let path = base.join(module_name);

    Ok(path)
}
