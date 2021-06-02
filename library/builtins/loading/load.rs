use crate::*;
use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};
use wipple::*;

#[derive(TypeInfo, Debug, Clone, Default)]
pub struct CurrentFile(pub Option<PathBuf>);

stack_key!(pub current_file: CurrentFile);

/// Import a file into an environment. If this file has already been imported,
/// it won't be loaded again and the cached module will be returned.
pub fn import_file_with_parent_env(path: PathBuf, env: &Env, stack: &Stack) -> Result<Module> {
    if let Some(module) = get_cached_import(&path) {
        Ok(module)
    } else {
        let program = load_file(path.clone(), stack)?;
        let module = import_program_with_parent_env(program, Some(path.clone()), env, stack)?;

        cache_import(&path, module.clone());

        Ok(module)
    }
}

pub fn import_program_with_parent_env(
    program: Block,
    path: Option<PathBuf>,
    env: &Env,
    stack: &Stack,
) -> Result<Module> {
    let mut stack = stack.clone();
    *stack.current_file_mut() = CurrentFile(path.clone());
    stack.diagnostics_mut().add(|| {
        if let Some(path) = path {
            format!("Importing {}", path.to_string_lossy())
        } else {
            String::from("Importing program")
        }
    });

    let env = env.child();
    program.reduce(&env, &stack)?;
    let module = Module::new(env);

    Ok(module)
}

pub fn include_file(path: PathBuf, env: &Env, stack: &Stack) -> Result<Value> {
    let mut stack = stack.clone();
    *stack.current_file_mut() = CurrentFile(Some(path.clone()));

    let program = load_file(path.clone(), &stack)?;
    include_program(program, Some(path), env, &stack)
}

pub fn include_string(
    code: &str,
    path: Option<PathBuf>,
    env: &Env,
    stack: &Stack,
) -> Result<Value> {
    let program = load_string(code, path.clone(), stack)?;
    include_program(program, path, env, stack)
}

pub fn include_program(
    program: Block,
    path: Option<PathBuf>,
    env: &Env,
    stack: &Stack,
) -> Result<Value> {
    let mut stack = stack.clone();
    stack.diagnostics_mut().add(|| {
        if let Some(path) = path {
            format!("Including {}", path.to_string_lossy())
        } else {
            String::from("Including program")
        }
    });

    program.reduce(env, &stack)
}

/// Load a Wipple file into a value. Does not evaluate the file.
pub fn load_file(path: PathBuf, stack: &Stack) -> Result<Block> {
    let mut stack = stack.clone();
    stack
        .diagnostics_mut()
        .add(|| format!("Loading file {}", path.to_string_lossy()));

    let code = fs::read_to_string(path.clone()).map_err(|error| {
        wipple::error(
            &format!("Error loading file {}: {}", path.to_string_lossy(), error),
            &stack,
        )
    })?;

    load_string(&code, Some(path), &stack)
}

pub fn load_string(code: &str, path: Option<PathBuf>, stack: &Stack) -> Result<Block> {
    let (tokens, lookup) = wipple_parser::lex(&code);

    let ast =
        wipple_parser::parse_file(&mut tokens.iter().peekable(), &lookup).map_err(|error| {
            wipple::error(&format!("Parse error: {}", error.message), &{
                let mut stack = stack.clone();
                stack.diagnostics_mut().add_location(
                    || match path.clone() {
                        Some(path) => format!("Parsing file {}", path.to_string_lossy()),
                        None => String::from("Parsing input"),
                    },
                    error.location.as_ref().map(|l| location(l, path.clone())),
                );
                stack
            })
        })?;

    let program = crate::convert_ast(&ast, path)
        .into_primitive()
        .try_into_cast::<Block>()
        .expect("Wipple programs are always blocks");

    Ok(program)
}

thread_local! {
    static CACHED_IMPORTS: Rc<RefCell<HashMap<PathBuf, Module>>> = Default::default();
}

fn get_cached_import(path: &Path) -> Option<Module> {
    CACHED_IMPORTS
        .with(Clone::clone)
        .borrow()
        .get(path)
        .cloned()
}

fn cache_import(path: &Path, module: Module) {
    CACHED_IMPORTS
        .with(Clone::clone)
        .borrow_mut()
        .insert(path.to_path_buf(), module);
}
