pub mod compile;
pub mod id;
pub mod project;
pub mod typecheck;

pub use id::*;

pub use interned_string::InternedString;
pub use wipple_diagnostics::*;

use std::sync::Arc;

fn prelude() -> Arc<compile::File> {
    thread_local! {
        static PRELUDE: Arc<compile::File> = {
            let mut diagnostics = Diagnostics::new();
            let project = project::Project::default();
            let mut info = compile::Info::with_files(&mut diagnostics, &project, Vec::new());

            project::load_string(
                "prelude", // this name shouldn't conflict with other files
                Arc::from(include_str!("../../prelude.wpl")),
                &mut info
            )
            .expect("Failed to load prelude")
        };
    }

    PRELUDE.with(Clone::clone)
}
