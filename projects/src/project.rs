use crate::*;
use std::path::{Path, PathBuf};
use wipple::*;

#[derive(Debug, Clone, Default)]
pub struct ProjectRoot(pub Option<PathBuf>);

stack_key!(pub project_root for ProjectRoot);

pub fn load_project(path: &Path, stack: &Stack) -> Result<Module> {
    let mut env = Environment::child_of(&Environment::global());
    setup_project_file(&mut env);
    let env = env.into_ref();

    let mut stack = stack.clone();
    stack
        .evaluation_mut()
        .set(|| format!("Importing project {}", path.to_string_lossy()));

    *project_root_mut_in(&mut stack) = ProjectRoot(Some(path.parent().unwrap().to_path_buf()));

    let project_module = import_file_with_parent_env(path, &env, &stack)?;

    // TODO: Install dependencies

    let main_file = get_main_file(&project_module, &env, &stack)?;
    let main_module = import(&main_file, &stack)?;

    Ok(main_module)
}

fn setup_project_file(_: &mut Environment) {
    // TODO
}

fn get_main_file(project_module: &Module, env: &EnvironmentRef, stack: &Stack) -> Result<String> {
    let mut stack = stack.clone();
    stack
        .evaluation_mut()
        .set(|| String::from("Resolving main file in project"));

    let variable = Name {
        name: String::from("main"),
        location: None,
    }
    .resolve(&project_module.env, &stack)?;

    let path = variable.get_primitive_or::<Text>(
        "Expected a Text value containing the path to the main file",
        env,
        &stack,
    )?;

    Ok(path.text)
}
