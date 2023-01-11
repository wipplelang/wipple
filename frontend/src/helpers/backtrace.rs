use crate::Compiler;

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct Backtrace(#[cfg(debug_assertions)] Option<backtrace::Backtrace>);

impl Compiler<'_> {
    pub(crate) fn backtrace(&self) -> Backtrace {
        Backtrace(
            #[cfg(debug_assertions)]
            self.backtrace_enabled
                .then(backtrace::Backtrace::new_unresolved),
        )
    }
}

impl Backtrace {
    #[cfg(debug_assertions)]
    pub fn into_inner(self) -> Option<backtrace::Backtrace> {
        self.0.map(|mut trace| {
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
        Backtrace(
            #[cfg(debug_assertions)]
            Some(trace),
        )
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
