use crate::*;
use std::path::PathBuf;
use wipple::*;

/// Resolve a module name into a path.
pub fn resolve_module(module_name: &str, stack: &Stack) -> Result<PathBuf> {
    let base = if module_name.starts_with("./") || module_name.starts_with("../") {
        current_file_in(stack)
            .0
            .ok_or_else(|| Return::error("Current file is not set", stack))
    } else {
        project_root_in(stack)
            .0
            .ok_or_else(|| Return::error("Project root is not set", stack))
    }?;

    let path = base.join(module_name);

    if path.is_dir() {
        Ok(path)
    } else if let Some(path) = Some(path.with_extension("wpl")).filter(|p| p.exists()) {
        Ok(path)
    } else {
        Err(Return::error(
            &format!("Cannot find module '{}'", module_name),
            stack,
        ))
    }
}

pub fn resolve(path_string: &str, stack: &Stack) -> Result<PathBuf> {
    let base = if path_string.starts_with("./") || path_string.starts_with("../") {
        current_file_in(stack)
            .0
            .ok_or_else(|| Return::error("Current file is not set", stack))
    } else {
        project_root_in(stack)
            .0
            .ok_or_else(|| Return::error("Project root is not set", stack))
    }?;

    let path = base.join(path_string);

    if path.exists() {
        Ok(path)
    } else {
        Err(Return::error(
            &format!("Cannot find '{}'", path_string),
            stack,
        ))
    }
}
