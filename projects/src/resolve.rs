use crate::*;
use std::path::PathBuf;
use wipple::*;

/// Resolve a module name into a path.
pub fn resolve(module_name: &str, stack: &Stack) -> Result<PathBuf> {
    let base = if module_name.starts_with("./") || module_name.starts_with("../") {
        current_file_in(stack)
            .0
            .ok_or_else(|| ReturnState::Error(Error::new("Current file is not set", stack)))
    } else {
        project_root_in(stack)
            .0
            .ok_or_else(|| ReturnState::Error(Error::new("Project root is not set", stack)))
    }?;

    let path = base.join(module_name);

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
