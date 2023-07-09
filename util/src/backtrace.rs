#[cfg(debug_assertions)]
use crate::Shared;

#[derive(Clone, Default)]
pub struct Backtrace {
    #[cfg(debug_assertions)]
    pub trace: Option<Shared<backtrace::Backtrace>>,
}

impl Backtrace {
    pub fn empty() -> Self {
        Backtrace {
            #[cfg(debug_assertions)]
            trace: None,
        }
    }

    #[cfg(debug_assertions)]
    pub fn into_inner(self) -> Option<backtrace::Backtrace> {
        self.trace.map(|trace| {
            let mut trace = trace.lock().clone();
            trace.resolve();
            trace
        })
    }

    #[cfg(not(debug_assertions))]
    pub fn into_inner(self) -> Option<backtrace::Backtrace> {
        None
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
        if let Some(trace) = &self.trace {
            let mut trace = trace.lock().clone();
            trace.resolve();
            trace.fmt(f)
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
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        ().hash(state) // skip backtrace in types that contain it
    }
}
