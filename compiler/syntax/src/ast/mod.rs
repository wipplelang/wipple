//! Construct an abstract syntax tree from the concrete syntax tree generated
//! by [`parse`].

mod attribute;
mod expression;
mod pattern;
mod statements;
mod r#type;

use crate::{parse, Driver};
use derivative::Derivative;
use statements::statements;
use wipple_util::WithInfo;

/// Convert a [`parse::TopLevel`] into a [`crate::TopLevel`].
pub fn top_level<D: Driver>(
    top_level_syntax: WithInfo<D::Info, parse::TopLevel<D>>,
) -> crate::Result<D> {
    let mut info = Info::default();

    let top_level = top_level_syntax.map(|top_level_syntax| crate::TopLevel {
        statements: statements(top_level_syntax.statements, &mut info),
    });

    crate::Result {
        top_level,
        diagnostics: info.errors,
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct PartialConstant<D: Driver> {
    info: D::Info,
    attributes: Vec<WithInfo<D::Info, crate::Attribute<D>>>,
    name: WithInfo<D::Info, String>,
    parameters: Vec<WithInfo<D::Info, crate::TypeParameter<D>>>,
    bounds: Vec<WithInfo<D::Info, crate::Instance<D>>>,
    r#type: WithInfo<D::Info, crate::Type<D>>,
}

struct Info<D: Driver> {
    errors: Vec<WithInfo<D::Info, crate::Diagnostic>>,
    current_constant: Option<PartialConstant<D>>,
}

impl<D: Driver> Default for Info<D> {
    fn default() -> Self {
        Info {
            errors: Default::default(),
            current_constant: Default::default(),
        }
    }
}
