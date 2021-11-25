mod compile;
mod id;
mod project;
mod typecheck;

pub use compile::*;
pub use id::*;
pub use project::*;
pub use typecheck::*;

use interned_string::InternedString;
use std::sync::Arc;
use wipple_diagnostics::*;

fn prelude() -> Arc<File> {
    thread_local! {
        static PRELUDE: Arc<File> = {
            let mut diagnostics = Diagnostics::new();
            let project = Project::default();
            let mut info = Info::with_files(&mut diagnostics, &project, Vec::new());

            load_string(
                "prelude",
                Arc::from(include_str!("../../prelude.wpl")),
                &mut info
            )
            .expect("Failed to load prelude")
        };
    }

    PRELUDE.with(Clone::clone)
}
