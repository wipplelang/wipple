use crate::*;
use std::path::Path;

/// Import a file/folder using a module name
pub fn import(module_name: &str, stack: &Stack) -> Result<Module> {
    let path = resolve(module_name, stack)?;
    import_path(&path, stack)
}

/// Import a file/folder using a module name directly in the current environment
pub fn include(module_name: &str, env: &EnvironmentRef, stack: &Stack) -> Result<Module> {
    let path = resolve(module_name, stack)?;
    import_path_with_parent_env(&path, env, stack)
}

/// Import a file/folder using a path
pub fn import_path(path: &Path, stack: &Stack) -> Result<Module> {
    let mut stack = stack.clone();
    stack
        .evaluation_mut()
        .set(|| format!("Importing {}", path.to_string_lossy()));

    if let Some(module) = try_import_folder(path, &stack)? {
        Ok(module)
    } else {
        import_file(path, &stack)
    }
}

pub fn import_path_with_parent_env(
    path: &Path,
    env: &EnvironmentRef,
    stack: &Stack,
) -> Result<Module> {
    let mut stack = stack.clone();
    stack
        .evaluation_mut()
        .set(|| format!("Importing {}", path.to_string_lossy()));

    if let Some(module) = try_import_folder(path, &stack)? {
        Ok(module)
    } else {
        import_file_with_parent_env(path, env, &stack)
    }
}

fn try_import_folder(path: &Path, stack: &Stack) -> Result<Option<Module>> {
    if !path.is_dir() {
        return Ok(None);
    }

    let project_file = path.join("project.wpl");

    if project_file.is_file() {
        let module = load_project(&project_file, stack)?;

        Ok(Some(module))
    } else {
        // 'use' each file in the folder

        let mut stack = stack.clone();
        stack
            .evaluation_mut()
            .set(|| format!("Importing all files in folder {}", path.to_string_lossy()));

        let temp_env = Environment::child_of(&Environment::global()).into_ref();

        let mut files = path
            .read_dir()
            .map_err(|error| {
                ReturnState::Error(Error::new(
                    &format!("Error reading folder {}: {}", path.to_string_lossy(), error),
                    &stack,
                ))
            })?
            .filter_map(|entry| {
                let path = match entry {
                    Ok(file) => file.path(),
                    Err(error) => {
                        return Some(Err(ReturnState::Error(Error::new(
                            &format!("Error reading file {}: {}", path.to_string_lossy(), error),
                            &stack,
                        ))))
                    }
                };

                if path.is_dir()
                    || path.file_name().unwrap().to_string_lossy().starts_with('.')
                    || path
                        .extension()
                        .map(|extension| extension != "wpl")
                        .unwrap_or(false)
                {
                    None
                } else {
                    Some(Ok(path))
                }
            })
            .collect::<std::result::Result<Vec<_>, _>>()?;

        files.sort();

        for file in files {
            let module = import_file(&file, &stack)?;
            temp_env.borrow_mut().r#use(&module.env.borrow());
        }

        let module = Module::new(temp_env.borrow().clone());

        Ok(Some(module))
    }
}

/// Import a file, returning a module. If the file belongs to a project, the
/// file's environment will descend from the project's environment.
pub fn import_file(path: &Path, stack: &Stack) -> Result<Module> {
    import_file_with_parent_env(path, &project_env_in(stack), stack)
}
