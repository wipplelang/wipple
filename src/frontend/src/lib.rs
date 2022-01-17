pub mod compile;
pub mod id;
pub mod project;
pub mod typecheck;
pub mod passes;

pub use id::*;

pub use interned_string::InternedString;
pub use wipple_diagnostics::*;

use std::sync::Arc;

fn prelude<'a>(
    diagnostics: &'a mut Diagnostics,
    project: &'a project::Project,
) -> (Arc<compile::File>, compile::Info<'a>) {
    let mut info = compile::Info::with_files(diagnostics, project, Vec::new());

    let prelude = project::load_string(
        "prelude", // this name shouldn't conflict with other files
        Arc::from(include_str!("../../prelude.wpl")),
        &mut info,
    )
    .expect("Failed to load prelude");

    (prelude, info)
}
