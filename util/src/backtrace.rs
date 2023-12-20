#[cfg(debug_assertions)]
use crate::Shared;

#[derive(Clone, Default)]
pub struct Backtrace {
    #[cfg(debug_assertions)]
    trace: Option<Shared<backtrace::Backtrace>>,
}

impl Backtrace {
    pub fn capture() -> Self {
        Backtrace {
            #[cfg(debug_assertions)]
            trace: Some(Shared::new(backtrace::Backtrace::new_unresolved())),
        }
    }

    pub fn empty() -> Self {
        Backtrace {
            #[cfg(debug_assertions)]
            trace: None,
        }
    }

    pub fn has_trace(&self) -> bool {
        #[cfg(debug_assertions)]
        {
            self.trace.is_some()
        }

        #[cfg(not(debug_assertions))]
        {
            false
        }
    }
}

impl From<backtrace::Backtrace> for Backtrace {
    fn from(#[allow(unused)] trace: backtrace::Backtrace) -> Self {
        Backtrace {
            #[cfg(debug_assertions)]
            trace: Some(Shared::new(trace)),
        }
    }
}

impl std::fmt::Debug for Backtrace {
    #[cfg(debug_assertions)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::path::Path;

        if let Some(trace) = &self.trace {
            let mut trace = trace.lock().clone();
            trace.resolve();

            // Only show lines that are part of the Wipple source code, unless
            // RUST_BACKTRACE is enabled
            if std::env::var("RUST_BACKTRACE").is_ok() {
                trace.fmt(f)
            } else {
                let dir = Path::new(env!("CARGO_MANIFEST_DIR"))
                    .parent()
                    .unwrap()
                    .to_str()
                    .unwrap();

                let s = format!("{trace:#?}")
                    .lines()
                    .filter(|line| line.contains("wipple") || line.contains(dir))
                    .collect::<Vec<_>>()
                    .join("\n");

                f.write_str(&s)
            }
        } else {
            f.debug_tuple("Backtrace").finish()
        }
    }

    #[cfg(not(debug_assertions))]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Backtrace").finish()
    }
}

impl PartialEq for Backtrace {
    fn eq(&self, _other: &Self) -> bool {
        true // skip backtrace in types that contain it
    }
}

impl Eq for Backtrace {}

impl PartialOrd for Backtrace {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        Some(std::cmp::Ordering::Equal) // skip backtrace in types that contain it
    }
}

impl Ord for Backtrace {
    fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal // skip backtrace in types that contain it
    }
}

impl std::hash::Hash for Backtrace {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {
        // skip backtrace in types that contain it
    }
}
