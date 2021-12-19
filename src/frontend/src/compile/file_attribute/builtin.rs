use crate::{compile::*, *};
use std::collections::HashMap;
use wipple_diagnostics::{Diagnostic, DiagnosticLevel, Note};

pub fn builtin_file_attributes() -> HashMap<InternedString, FileAttribute> {
    macro_rules! builtins {
        ($($name:expr => $value:expr,)*) => {{
            let mut variables = HashMap::default();

            $({
                let name = InternedString::new($name);
                variables.insert(name, $value);
            })*

            variables
        }};
    }

    builtins! {
        "no-prelude" => FileAttribute::no_prelude(),
    }
}

impl FileAttribute {
    fn no_prelude() -> Self {
        FileAttribute::new(Some(0), |_, span, stack, info| {
            let mut parent = Some(stack);

            while let Some(stack) = parent {
                match &stack.file_info {
                    Some(file_info) => {
                        let mut file_info = file_info.borrow_mut();

                        if !file_info.include_prelude {
                            info.diagnostics.add(Diagnostic::new(
                                DiagnosticLevel::Error,
                                "Cannot use 'no-prelude' twice in the same file",
                                vec![Note::primary(span, "This has already been declared above")],
                            ));

                            return None;
                        }

                        file_info.include_prelude = false;
                        return Some(());
                    }
                    None => parent = stack.parent,
                }
            }

            unreachable!()
        })
    }
}
