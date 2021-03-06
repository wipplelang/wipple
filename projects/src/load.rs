use crate::*;
use std::{fs, path::PathBuf};
use wipple::*;

/// Import a file/folder using a module name.
pub fn import(module_name: &str, env: &EnvironmentRef, stack: &Stack) -> Result<Module> {
    import_path(&resolve(module_name, env, stack)?, stack)
}

/// Import a file/folder using a path
pub fn import_path(path: &PathBuf, stack: &Stack) -> Result<Module> {
    if path.is_dir() {
        let project_file = path.join("project.wpl");

        if project_file.is_file() {
            load_project(&project_file, stack)
        } else {
            // 'use' each file in the folder

            let temp_env = Environment::child_of(&Environment::global()).into_ref();

            let mut files = path
                .read_dir()
                .map_err(|error| Error::new(&format!("Error reading folder: {}", error), stack))?
                .map(|entry| entry.map(|file| file.path()))
                .collect::<std::result::Result<Vec<_>, _>>()
                .map_err(|error| {
                    Error::new(&format!("Error reading file in folder: {}", error), stack)
                })?;

            files.sort();

            for file in files {
                let module = import_file(&file, stack)?;
                temp_env.borrow_mut().r#use(&module.env.borrow());
            }

            let module = Module::new(temp_env);

            Ok(module)
        }
    } else {
        import_file(path, stack)
    }
}

/// Import a file, returning a module.
pub fn import_file(path: &PathBuf, stack: &Stack) -> Result<Module> {
    let env = Environment::child_of(&Environment::global()).into_ref();
    load_file_with_parent_env(path, &env, stack)
}

pub fn load_file_with_parent_env(
    path: &PathBuf,
    env: &EnvironmentRef,
    stack: &Stack,
) -> Result<Module> {
    set_current_file(&mut env.borrow_mut(), Some(path.clone()));
    load_file(path, stack)?.evaluate(env, stack)?;
    let module = Module::new(env.clone());
    Ok(module)
}

/// Load a Wipple file into a value. Does not evaluate the file.
pub fn load_file(path: &PathBuf, stack: &Stack) -> Result {
    let code = fs::read_to_string(path)
        .map_err(|error| Error::new(&format!("Error loading file: {}", error), stack))?;

    let ast = wipple_parser::parse(&code)
        .map_err(|error| Error::new(&format!("Error parsing: {}", error.message), stack))?;

    let program = wipple_parser::convert(&ast, Some(path));

    Ok(program)
}
