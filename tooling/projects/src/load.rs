use crate::*;
use std::path::Path;

/// Import a file/folder using a module name
pub fn import(module_name: &str, stack: &Stack) -> Result<Module> {
    let path = resolve_module(module_name, stack)?;
    import_path(&path, stack)
}

/// Import a file/folder using a module name directly in the current environment
pub fn include(module_name: &str, env: &Environment, stack: &Stack) -> Result {
    let path = resolve_module(module_name, stack)?;

    let mut stack = stack.clone();
    stack
        .evaluation_mut()
        .add(|| format!("Including file '{}'", path.to_string_lossy()));

    include_file(&path, env, &stack)
}

/// Import a file/folder using a path
pub fn import_path(path: &Path, stack: &Stack) -> Result<Module> {
    let mut stack = stack.clone();
    stack
        .evaluation_mut()
        .add(|| format!("Importing {}", path.to_string_lossy()));

    if let Some(module) = try_import_folder(path, &stack)? {
        Ok(module)
    } else {
        import_file(path, &stack)
    }
}

pub fn import_path_with_parent_env(
    path: &Path,
    env: &Environment,
    stack: &Stack,
) -> Result<Module> {
    let mut stack = stack.clone();
    stack
        .evaluation_mut()
        .add(|| format!("Importing {}", path.to_string_lossy()));

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

    // Load the folder as a project if it contains project.wpl

    let project_file = path.join("project.wpl");

    if project_file.is_file() {
        let dependencies = DEPENDENCIES.borrow();

        let project = dependencies.get(&project_file).ok_or_else(|| {
            Return::error("Cannot import a project that is not a dependency", stack)
        })?;

        let module = project.run(stack)?;

        return Ok(Some(module));
    }

    // 'use' each file in the folder

    let mut stack = stack.clone();
    stack
        .evaluation_mut()
        .add(|| format!("Importing all files in folder {}", path.to_string_lossy()));

    let mut temp_env = env::child_of(&env::global());

    let mut files = path
        .read_dir()
        .map_err(|error| {
            Return::error(
                &format!("Error reading folder {}: {}", path.to_string_lossy(), error),
                &stack,
            )
        })?
        .filter_map(|entry| {
            let path = match entry {
                Ok(file) => file.path(),
                Err(error) => {
                    return Some(Err(Return::error(
                        &format!("Error reading file {}: {}", path.to_string_lossy(), error),
                        &stack,
                    )))
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
        temp_env.r#use(&module.env.borrow());
    }

    let module = Module::new(temp_env.into());

    Ok(Some(module))
}

/// Import a file, returning a module. If the file belongs to a project, the
/// file's environment will descend from the project's environment.
pub fn import_file(path: &Path, stack: &Stack) -> Result<Module> {
    import_file_with_parent_env(path, &project_env_in(stack), stack)
}
