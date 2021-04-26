use crate::*;
use std::path::PathBuf;
use wipple::*;

/// Resolve a module name into a path.
pub fn resolve_module(module_name: &str, stack: &Stack) -> Result<PathBuf> {
    let path = resolve_path(module_name, stack)?;

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

pub fn resolve(path: &str, stack: &Stack) -> Result<PathBuf> {
    let path = resolve_path(path, stack)?;

    if path.exists() {
        Ok(path)
    } else {
        Err(Return::error(
            &format!("Path '{}' does not exist", path.to_string_lossy()),
            stack,
        ))
    }
}

fn resolve_path(path: &str, stack: &Stack) -> Result<PathBuf> {
    let base = if path.starts_with("./") || path.starts_with("../") {
        current_file_in(stack)
            .0
            .ok_or_else(|| Return::error("Current file is not set", stack))
            .map(|p| p.parent().unwrap().to_path_buf())
    } else {
        project_root_in(stack)
            .0
            .ok_or_else(|| Return::error("Project root is not set", stack))
    }?;

    Ok(base.join(path))
}
