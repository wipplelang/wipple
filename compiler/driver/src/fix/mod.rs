//! Determine potential fixes for invalid Wipple code for which there are no
//! straightforward suggestions. This works by applying the fix (which may
//! transform source code arbitrarily), recompiling, and determining if the new
//! program is improved.

use crate::{Diagnostic, Info, Interface, Library};
use wipple_util::WithInfo;

mod fixes;

/// A potential fix.
#[derive(Debug, Clone)]
pub enum Fix {
    /// Replace the current code with new code.
    ReplaceWith(String),

    /// Join the current line with the next line.
    JoinWithNextLine,
}

/// See the [module-level documentation](self).
#[must_use]
pub fn fix<T>(
    diagnostic: WithInfo<Info, &Diagnostic>,
    interface: &Interface,
    library: &Library,
    mut apply: impl FnMut(&Fix) -> Option<(WithInfo<Info, T>, Interface, Library)>,
) -> Option<WithInfo<Info, (Fix, T)>>
// TEMPORARY
where
    T: std::fmt::Debug,
{
    for rule in fixes::rules() {
        let fix = match rule.apply(diagnostic.as_deref(), interface, library) {
            Some(fix) => fix,
            _ => continue,
        };

        let (fixed_value, fixed_interface, fixed_library) = match apply(&fix) {
            Some((info, interface, library)) => (info, interface, library),
            None => continue,
        };

        if rule.succeeded(fixed_value.replace(&fix), &fixed_interface, &fixed_library) {
            return Some(fixed_value.map(|value| (fix, value)));
        }
    }

    None
}
