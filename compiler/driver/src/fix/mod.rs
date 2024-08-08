//! Determine potential fixes for invalid Wipple code for which there are no
//! straightforward suggestions. This works by applying the fix (which may
//! transform source code arbitrarily), recompiling, and determining if the new
//! program is improved.

use crate::{Diagnostic, Info, Interface, Library};
use std::ops::Range;
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

impl Fix {
    /// Apply a fix to a range of code.
    pub fn apply_to(&self, code: &mut String, range: Range<usize>) -> Range<usize> {
        match self {
            Fix::ReplaceWith(replacement) => replace_with(code, range, replacement),
            Fix::JoinWithNextLine => join_with_next_line(code, range),
        }
    }
}

fn replace_with(s: &mut String, range: Range<usize>, replacement: &str) -> Range<usize> {
    s.replace_range(range.clone(), replacement);
    range.start..(range.start + replacement.len())
}

fn join_with_next_line(s: &mut String, range: Range<usize>) -> Range<usize> {
    // HACK: Needed if the fix is on the second-to-last line. Shouldn't affect
    // the range because it's the last character in the string
    s.push('\n');

    let line_index = line_index::LineIndex::new(s);

    let line = line_index
        .line_col(line_index::TextSize::new(range.end as u32))
        .line;

    let (next_line_start, next_line_end) = match line_index
        .lines(line_index::TextRange::up_to(line_index.len()))
        .nth(line as usize + 1)
    {
        Some(range) => (
            u32::from(range.start()) as usize,
            u32::from(range.end()) as usize,
        ),
        None => return range, // do nothing; no next line to join with
    };

    s.replace_range((next_line_start - 1)..next_line_start, " ");

    // Remove the trailing newline (if there is one)
    let next_line_end = next_line_end.saturating_sub(1);

    range.start..next_line_end
}
