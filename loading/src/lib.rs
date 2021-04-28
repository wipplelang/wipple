use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
    result::Result,
};
use wipple::*;

#[derive(TypeInfo, Debug, Clone, Default)]
pub struct CurrentFile(pub Option<PathBuf>);

stack_key!(pub current_file for CurrentFile);

ref_thread_local! {
    static managed CACHED_IMPORTS: HashMap<PathBuf, Module> = HashMap::new();
}

fn get_cached_import(path: &Path) -> Option<Module> {
    CACHED_IMPORTS.borrow().get(path).cloned()
}

fn cache_import(path: &Path, module: Module) {
    CACHED_IMPORTS
        .borrow_mut()
        .insert(path.to_path_buf(), module);
}

/// Import a file into an environment. If this file has already been imported,
/// it won't be loaded again and the cached module will be returned.
pub fn import_file_with_parent_env(
    path: &Path,
    env: &EnvironmentRef,
    stack: &Stack,
) -> wipple::Result<Module> {
    if let Some(module) = get_cached_import(path) {
        Ok(module)
    } else {
        let program = load_file(&path, stack)?;
        let module = import_program_with_parent_env(program, Some(&path), env, stack)?;

        cache_import(path, module.clone());

        Ok(module)
    }
}

pub fn import_program_with_parent_env(
    program: Block,
    path: Option<&Path>,
    env: &EnvironmentRef,
    stack: &Stack,
) -> wipple::Result<Module> {
    let mut stack = stack.clone();
    *current_file_mut_in(&mut stack) = CurrentFile(path.map(|p| p.to_path_buf()));
    stack.evaluation_mut().add(|| {
        if let Some(path) = path {
            format!("Importing {}", path.to_string_lossy())
        } else {
            String::from("Importing program")
        }
    });

    let result = Value::of(program).evaluate(&env, &stack)?;

    if !result.is_trait_directly(&Trait::module()) {
        panic!("Could not evaluate file, is the standard library set up correctly?");
    }

    let module = result.into_primitive::<Module>();

    Ok(module)
}

pub fn include_file(path: &Path, env: &EnvironmentRef, stack: &Stack) -> wipple::Result {
    let mut stack = stack.clone();
    *current_file_mut_in(&mut stack) = CurrentFile(Some(path.to_path_buf()));
    stack
        .evaluation_mut()
        .add(|| format!("Including file {}", path.to_string_lossy()));

    let program = load_file(path, &stack)?;
    include_program(program, env, &stack)
}

pub fn include_program(program: Block, env: &EnvironmentRef, stack: &Stack) -> wipple::Result {
    let mut stack = stack.clone();
    stack
        .evaluation_mut()
        .add(|| String::from("Including program"));

    program.do_inline(env, &stack)
}

/// Load a Wipple file into a value. Does not evaluate the file.
pub fn load_file(path: &Path, stack: &Stack) -> wipple::Result<Block> {
    let mut stack = stack.clone();
    stack
        .evaluation_mut()
        .add(|| format!("Loading file {}", path.to_string_lossy()));

    let code = fs::read_to_string(path).map_err(|error| {
        Return::error(
            &format!("Error loading file {}: {}", path.to_string_lossy(), error),
            &stack,
        )
    })?;

    load_string(&code, Some(path), &stack)
}

pub fn load_string(code: &str, path: Option<&Path>, stack: &Stack) -> wipple::Result<Block> {
    let (tokens, lookup) = wipple_parser::lex(&code);

    let ast =
        wipple_parser::parse_file(&mut tokens.iter().peekable(), &lookup).map_err(|error| {
            Return::error(&format!("Parse error: {}", error.message), &{
                let mut stack = stack.clone();
                stack.evaluation_mut().add_location(
                    || match path {
                        Some(path) => format!("Parsing file {}", path.to_string_lossy()),
                        None => String::from("Parsing input"),
                    },
                    error.location.map(|location| SourceLocation {
                        file: path.map(PathBuf::from),
                        line: location.line,
                        column: location.column,
                    }),
                );
                stack
            })
        })?;

    // Wipple programs are always blocks
    let program = wipple_parser::convert(&ast, path).into_primitive::<Block>();

    Ok(program)
}
