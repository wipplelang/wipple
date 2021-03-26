use crate::*;
use std::{
    fs,
    path::{Path, PathBuf},
};
use wipple::*;

fn_wrapper_struct! {
    pub type LoadFileFn(&Path, Stack) -> Result<String>;
}

impl Default for LoadFileFn {
    fn default() -> Self {
        LoadFileFn::new(|_, stack| {
            Err(ReturnState::Error(Error::new(
                "This runtime does not support loading other files",
                stack,
            )))
        })
    }
}

stack_key!(pub load_file for LoadFileFn);

#[derive(Clone, Default)]
pub struct CurrentFile(pub Option<PathBuf>);

stack_key!(pub current_file for CurrentFile);

/// Import a file/folder using a module name
pub fn import(module_name: &str, stack: Stack) -> Result<Module> {
    let path = resolve(module_name, stack.clone())?;
    import_path(&path, stack)
}

/// Import a file/folder using a module name directly in the current environment
pub fn include(module_name: &str, env: &EnvironmentRef, stack: Stack) -> Result<Module> {
    let path = resolve(module_name, stack.clone())?;
    import_path_with_parent_env(&path, env, stack)
}

/// Import a file/folder using a path
pub fn import_path(path: &Path, stack: Stack) -> Result<Module> {
    let stack =
        stack.update_evaluation(|e| e.with(|| format!("Importing {}", path.to_string_lossy())));

    if let Some(module) = try_import_folder(path, stack.clone())? {
        Ok(module)
    } else {
        import_file(path, stack)
    }
}

pub fn import_path_with_parent_env(
    path: &Path,
    env: &EnvironmentRef,
    stack: Stack,
) -> Result<Module> {
    let stack =
        stack.update_evaluation(|e| e.with(|| format!("Importing {}", path.to_string_lossy())));

    if let Some(module) = try_import_folder(path, stack.clone())? {
        Ok(module)
    } else {
        import_file_with_parent_env(path, env, stack)
    }
}

fn try_import_folder(path: &Path, stack: Stack) -> Result<Option<Module>> {
    if !path.is_dir() {
        return Ok(None);
    }

    let project_file = path.join("project.wpl");

    if project_file.is_file() {
        let module = load_project(&project_file, stack)?;

        Ok(Some(module))
    } else {
        // 'use' each file in the folder

        let stack = stack.update_evaluation(|e| {
            e.with(|| format!("Importing all files in folder {}", path.to_string_lossy()))
        });

        let temp_env = Environment::child_of(&Environment::global()).into_ref();

        let mut files = path
            .read_dir()
            .map_err(|error| {
                ReturnState::Error(Error::new(
                    &format!("Error reading folder {}: {}", path.to_string_lossy(), error),
                    stack.clone(),
                ))
            })?
            .filter_map(|entry| {
                let path = match entry {
                    Ok(file) => file.path(),
                    Err(error) => {
                        return Some(Err(ReturnState::Error(Error::new(
                            &format!("Error reading file {}: {}", path.to_string_lossy(), error),
                            stack.clone(),
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
            let module = import_file(&file, stack.clone())?;
            temp_env.borrow_mut().r#use(&module.env.borrow());
        }

        let module = Module::new(temp_env.borrow().clone());

        Ok(Some(module))
    }
}

/// Import a file, returning a module.
pub fn import_file(path: &Path, stack: Stack) -> Result<Module> {
    import_file_with_parent_env(path, &Environment::global(), stack)
}

pub fn import_file_with_parent_env(
    path: &Path,
    env: &EnvironmentRef,
    stack: Stack,
) -> Result<Module> {
    let program = load_file(path, stack.clone())?;
    import_program_with_parent_env(program, Some(path), env, stack)
}

pub fn import_program_with_parent_env(
    program: Block,
    path: Option<&Path>,
    env: &EnvironmentRef,
    stack: Stack,
) -> Result<Module> {
    let stack = with_current_file_in(stack, CurrentFile(path.map(|p| p.to_path_buf())));

    let result = Value::of(program).evaluate(&env, stack)?;
    let module = result.cast_primitive::<Module>(); // files always evaluate to modules

    Ok(module)
}

pub fn include_file(path: &Path, env: &EnvironmentRef, stack: Stack) -> Result {
    let stack = with_current_file_in(stack, CurrentFile(Some(path.to_path_buf())));

    let program = load_file(path, stack.clone())?;
    include_program(program, env, stack)
}

pub fn include_program(program: Block, env: &EnvironmentRef, stack: Stack) -> Result {
    program.reduce_inline(env, stack)
}

/// Load a Wipple file into a value. Does not evaluate the file.
pub fn load_file(path: &Path, stack: Stack) -> Result<Block> {
    let code = fs::read_to_string(path).map_err(|error| {
        ReturnState::Error(Error::new(
            &format!("Error loading file {}: {}", path.to_string_lossy(), error),
            stack.clone(),
        ))
    })?;

    load_string(&code, Some(path), stack)
}

pub fn load_string(code: &str, path: Option<&Path>, stack: Stack) -> Result<Block> {
    let (tokens, lookup) = wipple_parser::lex(&code);

    let ast =
        wipple_parser::parse_file(&mut tokens.iter().peekable(), &lookup).map_err(|error| {
            ReturnState::Error(Error::new(
                &format!("Parse error: {}", error.message),
                stack.update_evaluation(|e| {
                    e.with_location(
                        || match path {
                            Some(path) => format!("Parsing file {}", path.to_string_lossy()),
                            None => String::from("Parsing input"),
                        },
                        error.location.map(|location| SourceLocation {
                            file: path.map(PathBuf::from),
                            line: location.line,
                            column: location.column,
                        }),
                    )
                }),
            ))
        })?;

    // Wipple programs are always blocks
    let program = wipple_parser::convert(&ast, path).cast_primitive::<Block>();

    Ok(program)
}
