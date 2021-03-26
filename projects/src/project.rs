use crate::*;
use std::path::{Path, PathBuf};
use wipple::*;

#[derive(Clone, Default)]
pub struct ProjectRoot(pub Option<PathBuf>);

stack_key!(pub project_root for ProjectRoot);

pub fn load_project(path: &Path, mut stack: Stack) -> Result<Module> {
    let mut env = Environment::child_of(&Environment::global());
    setup_project_file(&mut env);
    let env = env.into_ref();

    stack = stack
        .update_evaluation(|e| e.with(|| format!("Importing project {}", path.to_string_lossy())));

    stack = with_project_root_in(
        stack,
        ProjectRoot(Some(path.parent().unwrap().to_path_buf())),
    );

    let project_module = import_file_with_parent_env(path, &env, stack.clone())?;

    // TODO: Install dependencies

    let main_file = get_main_file(&project_module, &env, stack.clone())?;
    let main_module = import(&main_file, stack)?;

    Ok(main_module)
}

fn setup_project_file(_: &mut Environment) {
    // TODO
}

fn get_main_file(project_module: &Module, env: &EnvironmentRef, stack: Stack) -> Result<String> {
    let variable = Name {
        name: String::from("main"),
        location: None,
    }
    .resolve(
        &project_module.env,
        stack
            .clone()
            .update_evaluation(|e| e.with(|| String::from("Resolving main file in project"))),
    )?;

    let path = variable.get_primitive_or::<Text>(
        "Expected a Text value containing the path to the main file",
        env,
        stack,
    )?;

    Ok(path.text)
}
