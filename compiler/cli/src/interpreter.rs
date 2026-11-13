use wipple_core::codegen::mir;
use wipple_interpreter::Interpreter;

pub fn interpreter<'a, 'ctx>(
    program: &'a mir::Program,
    mut display: impl FnMut(&str) -> anyhow::Result<()> + 'ctx,
) -> Interpreter<'a, 'ctx, !, anyhow::Error> {
    Interpreter::new(program, move |name, input| {
        use wipple_interpreter::{Handle, Primitive};

        match name {
            "display" => {
                let Handle::Primitive(Primitive::String(string)) = input else {
                    return Err(anyhow::format_err!("expected string"));
                };

                display(&string)?;

                Ok(Handle::Unit)
            }
            _ => Err(anyhow::format_err!("unsupported external {name:?}")),
        }
    })
}
