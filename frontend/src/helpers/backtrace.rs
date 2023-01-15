use crate::Compiler;

#[cfg(debug_assertions)]
use crate::helpers::Shared;

#[derive(Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct Backtrace {
    #[cfg(debug_assertions)]
    #[cfg_attr(feature = "serde", serde(skip))]
    trace: Option<Shared<backtrace::Backtrace>>,
}

impl Compiler<'_> {
    pub(crate) fn backtrace(&self) -> Backtrace {
        Backtrace {
            #[cfg(debug_assertions)]
            trace: self
                .backtrace_enabled
                .then(backtrace::Backtrace::new_unresolved)
                .map(Shared::new),
        }
    }
}

impl Backtrace {
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

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for Backtrace {
    fn arbitrary(_: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        // Prevent fuzzing from generating error expressions without
        // accompanying diagnostics
        Err(arbitrary::Error::IncorrectFormat)
    }
}
