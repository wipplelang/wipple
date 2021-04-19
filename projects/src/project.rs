use crate::*;
use std::{
    cell::RefCell,
    collections::HashMap,
    path::{Path, PathBuf},
    rc::Rc,
};
use wipple::*;

const TARGET: &str = env!("TARGET");

#[typeinfo]
#[derive(Debug, Clone, Default)]
pub struct ProjectRoot(pub Option<PathBuf>);

stack_key!(pub project_root for ProjectRoot);

thread_local! {
    static PROJECT_ENVS: Rc<RefCell<HashMap<PathBuf, EnvironmentRef>>> = Default::default();
}

pub fn project_env_in(stack: &Stack) -> EnvironmentRef {
    match project_root_in(stack).0 {
        Some(project_root) => PROJECT_ENVS
            .with(|x| x.clone())
            .borrow_mut()
            .entry(project_root)
            .or_insert_with(|| Environment::child_of(&Environment::global()).into_ref())
            .clone(),
        None => Environment::global(),
    }
}

pub fn load_project(path: &Path, stack: &Stack) -> Result<Module> {
    let mut env = Environment::child_of(&Environment::global());
    setup_project_file(path, &mut env);
    let env = env.into_ref();

    let mut stack = stack.clone();
    stack
        .evaluation_mut()
        .set(|| format!("Importing project {}", path.to_string_lossy()));

    *project_root_mut_in(&mut stack) = ProjectRoot(Some(path.parent().unwrap().to_path_buf()));

    let project_env = project_env_in(&stack);

    let project_module = import_file_with_parent_env(path, &env, &stack)?;

    if let Some(dependencies) = get_dependencies(&project_module, &env, &stack)? {
        let paths = update_dependencies(dependencies, || {
            // TODO: Move to CLI
            println!("Installing dependencies");
        })
        .map_err(|error| {
            ReturnState::Error(Error::new(
                &format!("Error installing dependencies: {}", error),
                &stack,
            ))
        })?;

        for (name, path) in paths {
            project_env
                .borrow_mut()
                .set_variable(&name, Value::of(Text::new(&path.to_string_lossy())))
        }
    }

    let main_file = get_main_file(&project_module, &env, &stack)?;
    let main_module = import(&main_file, &stack)?;

    Ok(main_module)
}

fn setup_project_file(project_path: &Path, env: &mut Environment) {
    let project_path = project_path.parent().unwrap().to_path_buf();

    env.set_variable(
        "path",
        Value::of(Function::new(move |value, env, stack| {
            let path =
                value
                    .evaluate(env, stack)?
                    .get_or::<Text>("Expected path text", env, stack)?;

            let path = project_path
                .join(PathBuf::from(path.text))
                .canonicalize()
                .map_err(|error| {
                    ReturnState::Error(Error::new(
                        &format!("Error resolving path: {}", error),
                        stack,
                    ))
                })?;

            Ok(Value::of(Dependency::project(DependencyLocation::Path(
                path,
            ))))
        })),
    );

    env.set_variable(
        "url",
        Value::of(Function::new(|value, env, stack| {
            let url =
                value
                    .evaluate(env, stack)?
                    .get_or::<Text>("Expected URL text", env, stack)?;

            Ok(Value::of(Dependency::project(DependencyLocation::URL(
                url.text,
            ))))
        })),
    );

    env.set_variable(
        "git",
        Value::of(Function::new(|value, env, stack| {
            let repo = value
                .evaluate(env, stack)?
                .get_or::<Text>("Expected URL to Git repository", env, stack)?
                .text;

            let git = if repo.contains(' ') {
                let repo = repo.split(' ').collect::<Vec<_>>();

                if repo.len() != 2 {
                    return Err(ReturnState::Error(Error::new(
                        "Expected a URL and a branch separated by a space",
                        stack,
                    )));
                }

                DependencyLocation::Git {
                    location: repo[0].to_string(),
                    branch: Some(repo[1].to_string()),
                }
            } else {
                DependencyLocation::Git {
                    location: repo,
                    branch: None,
                }
            };

            Ok(Value::of(Dependency::project(git)))
        })),
    );

    env.set_variable(
        "plugin",
        Value::of(Function::new(|value, env, stack| {
            let mut dependency = value.evaluate(env, stack)?.get_or::<Dependency>(
                "Expected dependency",
                env,
                stack,
            )?;

            dependency.r#type = DependencyType::Plugin;

            Ok(Value::of(dependency))
        })),
    );

    env.set_variable("host", Value::of(Text::new(TARGET)));
}

fn get_dependencies(
    project_module: &Module,
    env: &EnvironmentRef,
    stack: &Stack,
) -> Result<Option<HashMap<String, Dependency>>> {
    let mut stack = stack.clone();
    stack
        .evaluation_mut()
        .set(|| String::from("Updating dependencies"));

    let dependencies_value =
        match Name::new("dependencies").resolve_if_present(&project_module.env, &stack)? {
            Some(dependencies) => dependencies,
            None => return Ok(None),
        };

    let dependencies_module = dependencies_value.get_or::<Module>(
        "Expected a module containing dependencies",
        env,
        &stack,
    )?;

    let mut dependencies = HashMap::new();

    for (name, dependency_variable) in dependencies_module.env.borrow_mut().variables().0.clone() {
        let mut stack = stack.clone();
        stack
            .evaluation_mut()
            .set(|| format!("Parsing dependency '{}'", name));

        let dependency = dependency_variable
            .get_value(env, &stack)?
            .get_or::<Dependency>("Expected dependency", env, &stack)?;

        dependencies.insert(name.clone(), dependency);
    }

    Ok(Some(dependencies))
}

fn get_main_file(project_module: &Module, env: &EnvironmentRef, stack: &Stack) -> Result<String> {
    let mut stack = stack.clone();
    stack
        .evaluation_mut()
        .set(|| String::from("Resolving main file in project"));

    let main_value = Name::new("main").resolve(&project_module.env, &stack)?;

    let path = main_value.get_or::<Text>(
        "Expected a Text value containing the path to the main file",
        env,
        &stack,
    )?;

    Ok(path.text)
}
