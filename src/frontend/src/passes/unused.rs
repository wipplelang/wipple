use crate::typecheck::Info;
use wipple_diagnostics::*;

pub fn unused_variables(info: &mut Info) {
    for (&variable, &item_info) in &info.declared_variables {
        if let Some(name) = item_info.declared_name {
            if !info.used_variables.contains(&variable) {
                info.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Warning,
                    "Unused variable",
                    vec![Note::primary(
                        item_info.span,
                        format!("'{}' is not accessed anywhere in the program", name),
                    )],
                ));
            }
        }
    }
}
