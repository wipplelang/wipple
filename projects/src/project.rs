use crate::*;
use std::path::Path;
use wipple::*;

pub fn load_project(path: &Path, stack: &Stack) -> Result<Module> {
    let mut env = Environment::child_of(&Environment::global());
    setup_project_file(&mut env);
    let env = env.into_ref();

    let mut stack = stack.clone();
    stack.project_root = Some(path.parent().unwrap().to_path_buf());

    let project_module = load_file_with_parent_env(path, &env, &stack)?;

    // TODO: Install dependencies

    let main_file = get_main_file(&project_module, &env, &stack)?;
    let main_module = import(&main_file, &stack)?;

    Ok(main_module)
}

fn setup_project_file(_: &mut Environment) {
    // TODO
}

fn get_main_file(project_module: &Module, env: &EnvironmentRef, stack: &Stack) -> Result<String> {
    let variable = Name {
        name: String::from("main"),
        location: None,
    }
    .resolve_in(
        &project_module.env,
        env,
        &stack.add(|| String::from("Resolving main file in project")),
    )?;

    let path = variable.get_primitive_or::<Text>(
        "Expected a Text value containing the path to the main file",
        env,
        stack,
    )?;

    Ok(path.text)
}
