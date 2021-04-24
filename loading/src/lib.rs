use std::{
    fs,
    path::{Path, PathBuf},
};
use wipple::*;

#[derive(TypeInfo, Debug, Clone, Default)]
pub struct CurrentFile(pub Option<PathBuf>);

stack_key!(pub current_file for CurrentFile);

pub fn import_file_with_parent_env(
    path: &Path,
    env: &EnvironmentRef,
    stack: &Stack,
) -> Result<Module> {
    let program = load_file(path, stack)?;
    import_program_with_parent_env(program, Some(path), env, stack)
}

pub fn import_program_with_parent_env(
    program: Block,
    path: Option<&Path>,
    env: &EnvironmentRef,
    stack: &Stack,
) -> Result<Module> {
    let mut stack = stack.clone();
    *current_file_mut_in(&mut stack) = CurrentFile(path.map(|p| p.to_path_buf()));
    stack
        .evaluation_mut()
        .set(|| String::from("Importing program"));

    let result = Value::of(program).evaluate(&env, &stack)?;

    if !result.is_trait_directly(&Trait::module()) {
        panic!("Could not evaluate file, is the standard library set up correctly?");
    }

    let module = result.into_primitive::<Module>();

    Ok(module)
}

pub fn include_file(path: &Path, env: &EnvironmentRef, stack: &Stack) -> Result {
    let mut stack = stack.clone();
    *current_file_mut_in(&mut stack) = CurrentFile(Some(path.to_path_buf()));
    stack
        .evaluation_mut()
        .set(|| format!("Including file {}", path.to_string_lossy()));

    let program = load_file(path, &stack)?;
    include_program(program, env, &stack)
}

pub fn include_program(program: Block, env: &EnvironmentRef, stack: &Stack) -> Result {
    let mut stack = stack.clone();
    stack
        .evaluation_mut()
        .set(|| String::from("Including program"));

    program.reduce_inline(env, &stack)
}

/// Load a Wipple file into a value. Does not evaluate the file.
pub fn load_file(path: &Path, stack: &Stack) -> Result<Block> {
    let mut stack = stack.clone();
    stack
        .evaluation_mut()
        .set(|| format!("Loading file {}", path.to_string_lossy()));

    let code = fs::read_to_string(path).map_err(|error| {
        ReturnState::Error(Error::new(
            &format!("Error loading file {}: {}", path.to_string_lossy(), error),
            &stack,
        ))
    })?;

    load_string(&code, Some(path), &stack)
}

pub fn load_string(code: &str, path: Option<&Path>, stack: &Stack) -> Result<Block> {
    let (tokens, lookup) = wipple_parser::lex(&code);

    let ast =
        wipple_parser::parse_file(&mut tokens.iter().peekable(), &lookup).map_err(|error| {
            ReturnState::Error(Error::new(&format!("Parse error: {}", error.message), &{
                let mut stack = stack.clone();
                stack.evaluation_mut().set_location(
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
            }))
        })?;

    // Wipple programs are always blocks
    let program = wipple_parser::convert(&ast, path).into_primitive::<Block>();

    Ok(program)
}
