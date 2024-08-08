//! Lint compiled Wipple code for potential mistakes.

pub mod lints;

use crate::Info;
use serde::Serialize;
use wipple_util::WithInfo;

/// The list of available lints.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "camelCase", tag = "type", content = "value")]
#[allow(missing_docs)]
pub enum Lint {
    NamingConventions(lints::NamingConventionsLint),
}

/// Lint a list of compiled items.
#[must_use]
pub fn lint(interface: &crate::Interface, library: &crate::Library) -> Vec<WithInfo<Info, Lint>> {
    let rules = lints::rules();

    let mut lints = Vec::new();
    let mut add_lint = |lint| lints.push(lint);

    for (path, item) in &library.items {
        for rule in rules {
            rule.lint_item(path, item, &mut add_lint);
        }
    }

    for (path, declaration) in &interface.type_declarations {
        for rule in rules {
            rule.lint_type_declaration(path, declaration.as_ref(), &mut add_lint);
        }
    }

    for (path, trait_) in &interface.trait_declarations {
        for rule in rules {
            rule.lint_trait_declaration(path, trait_.as_ref(), &mut add_lint);
        }
    }

    for (path, constant) in &interface.constant_declarations {
        for rule in rules {
            rule.lint_constant_declaration(path, constant.as_ref(), &mut add_lint);
        }
    }

    for (path, instance) in &interface.instance_declarations {
        for rule in rules {
            rule.lint_instance_declaration(path, instance.as_ref(), &mut add_lint);
        }
    }

    for (path, type_param) in &interface.type_parameter_declarations {
        for rule in rules {
            rule.lint_type_parameter_declaration(path, type_param.as_ref(), &mut add_lint);
        }
    }

    lints
}
