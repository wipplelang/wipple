use crate::{fix::Fix, Diagnostic, Info, Interface, Library};
use wipple_util::WithInfo;

mod split_function_call;

pub(super) trait Rule {
    /// Return a fix if it can be applied in the context.
    fn apply(
        &self,
        diagnostic: WithInfo<Info, &Diagnostic>,
        interface: &Interface,
        library: &Library,
    ) -> Option<Fix>;

    /// Determine if the fix returned by `apply` actually improved the program.
    /// The info provided has been updated to reflect the fix.
    fn succeeded(
        &self,
        fix: WithInfo<Info, &Fix>,
        interface: &Interface,
        library: &Library,
    ) -> bool;
}

pub(super) fn rules() -> &'static [&'static dyn Rule] {
    &[&split_function_call::SplitFunctionCallRule]
}
