use crate::QueryCtx;
use wipple_core::{
    db::Node,
    facts::Syntax,
    typecheck::{
        groups::Typed,
        ty::{TyTag, TyTag::*},
    },
    visit::{
        Resolved,
        definitions::{Defined, TraitDefinition},
    },
};
use wipple_syntax::types::{IsType, type_parameter::TypeParameter};

pub fn highlight_type(db: &QueryCtx<'_>, node: Node) -> bool {
    let Some(Syntax(syntax)) = db.get(node) else {
        return false;
    };

    !db.is_hidden(node)
        && db.contains::<IsType>(node)
        && db.ast(syntax).downcast_ref::<TypeParameter>().is_none()
}

pub fn highlight_trait(db: &QueryCtx<'_>, node: Node) -> bool {
    let definition_node = db
        .get::<Resolved>(node)
        .and_then(|resolved| resolved.definitions.first().copied())
        .unwrap_or(node);

    let Some(Defined(definition)) = db.get(definition_node) else {
        return false;
    };

    definition.downcast_ref::<TraitDefinition>().is_some()
}

pub fn highlight_function(db: &QueryCtx<'_>, node: Node) -> bool {
    highlight_typed(db, node, |tag| matches!(tag, Function))
}

pub fn highlight_type_parameter(db: &QueryCtx<'_>, node: Node) -> bool {
    highlight_typed(db, node, |tag| matches!(tag, Parameter(_)))
}

fn highlight_typed(db: &QueryCtx<'_>, node: Node, match_tag: impl FnOnce(TyTag) -> bool) -> bool {
    let Some(Typed(Some(group))) = db.get(node) else {
        return false;
    };

    db.contains::<Resolved>(node) && group.tys.first().is_some_and(|ty| match_tag(ty.tag))
}
