use crate::*;
use wipple::*;

pub fn default_setup(on_output: impl Fn(RunOutput) + 'static) -> Result<(Env, Stack)> {
    default_setup_with(Rc::new(on_output))
}

fn default_setup_with<F>(on_output: Rc<F>) -> Result<(Env, Stack)>
where
    F: Fn(RunOutput) + 'static,
{
    let mut stack = Stack::default();
    *stack.show_mut() = ShowFn::new(move |value, env, stack| {
        let value = value.evaluate(env, stack)?;
        let text = value.format(env, stack)?;

        on_output(RunOutput::Log(text.to_string()));

        Ok(())
    });

    Env::clear_global();

    let env = Env::global();
    env.r#use(&Env::stdlib(&stack)?, &stack)?;

    Ok((env, stack))
}

pub fn run(
    code: &str,
    prepare: impl FnOnce(&Env, &Stack) -> Result<()>,
    on_output: impl Fn(RunOutput) + 'static,
) {
    let on_output = Rc::new(on_output);

    (|| {
        let (env, stack) = default_setup_with(on_output.clone())?;
        prepare(&env, &stack)?;

        let program = load_string(code, None, &stack)?;

        import_program_with_parent_env(program, None, &env, &stack)
    })()
    .map_or_else(
        |exit: Exit| on_output(RunOutput::Error(exit.into_error())),
        |_| {},
    )
}

pub fn run_and_collect_output(
    code: &str,
    prepare: impl FnOnce(&Env, &Stack) -> Result<()>,
) -> Vec<RunOutput> {
    let output: Rc<RefCell<Vec<RunOutput>>> = Default::default();

    run(code, prepare, {
        let output = output.clone();
        move |o| output.borrow_mut().push(o)
    });

    output.take()
}

#[derive(Debug, Clone)]
pub enum RunOutput {
    Log(String),
    Error(Error),
}

impl RunOutput {
    pub fn into_components(self) -> (bool, String) {
        match self {
            RunOutput::Log(msg) => (true, msg),
            RunOutput::Error(err) => (false, err.to_string()),
        }
    }
}

impl std::fmt::Display for RunOutput {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            RunOutput::Log(msg) => f.write_str(msg),
            RunOutput::Error(err) => err.fmt(f),
        }
    }
}
