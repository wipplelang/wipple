use std::io;
use wipple_interpreter::Interpreter;

pub fn create_interpreter<'a>(mut out: impl io::Write + 'a) -> Interpreter<'a, ()> {
    Interpreter::new(move |name, input| {
        use wipple_interpreter::{Handle, Primitive};

        match name {
            "display" => {
                let Handle::Primitive(Primitive::String(string)) = input else {
                    return Err(anyhow::format_err!("expected string"));
                };

                writeln!(out, "{string}")?;

                Ok(Handle::Unit)
            }
            _ => Err(anyhow::format_err!("unsupported external {name:?}")),
        }
    })
}
