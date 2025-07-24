//! Construct an abstract syntax tree from the concrete syntax tree generated
//! by [`parse`].

mod attribute;
mod expression;
mod pattern;
mod statements;
mod r#type;

use crate::{
    syntax::{Location, parse},
    util::WithInfo,
};
use statements::statements;

/// Convert a [`parse::TopLevel`] into a [`crate::syntax::TopLevel`].
pub fn top_level(top_level_syntax: WithInfo<parse::TopLevel>) -> crate::syntax::Result {
    let mut info = Info::default();

    let top_level = top_level_syntax.map(|top_level_syntax| crate::syntax::TopLevel {
        statements: statements(top_level_syntax.statements, &mut info),
    });

    crate::syntax::Result {
        top_level,
        diagnostics: info.errors,
    }
}

#[derive(Debug)]
struct PartialConstant {
    info: Location,
    attributes: Vec<WithInfo<crate::syntax::Attribute>>,
    name: WithInfo<String>,
    parameters: Vec<WithInfo<crate::syntax::TypeParameter>>,
    bounds: Vec<WithInfo<crate::syntax::Instance>>,
    r#type: WithInfo<crate::syntax::Type>,
}

#[derive(Default)]
struct Info {
    errors: Vec<WithInfo<crate::syntax::Diagnostic>>,
    current_constant: Option<PartialConstant>,
}
