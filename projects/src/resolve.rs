use std::path::PathBuf;
use wipple::*;

/// Resolve a module name into a path.
pub fn resolve(module_name: &str, stack: &Stack) -> Result<PathBuf> {
    let base = if module_name.starts_with("./") || module_name.starts_with("../") {
        stack
            .current_file
            .clone()
            .ok_or_else(|| ReturnState::Error(Error::new("Current file is not set", stack)))
    } else {
        stack
            .project_root
            .clone()
            .ok_or_else(|| ReturnState::Error(Error::new("Project root is not set", stack)))
    }?;

    let path = base.join(module_name);

    let path = if path.is_dir() {
        path
    } else {
        path.with_extension("wpl")
    };

    Ok(path)
}
