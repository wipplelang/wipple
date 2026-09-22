use crate::QueryCtx;
use wipple_core::{
    db::Node,
    render::Comments,
    typecheck::bounds::{ResolvedBound, ResolvedBounds},
    util::get_links,
    visit::{
        Resolved,
        definitions::{Defined, InstanceDefinition},
    },
};

pub fn comments(db: &QueryCtx<'_>, node: Node) -> Option<Comments> {
    let definition_node = db
        .get::<Resolved>(node)
        .and_then(|resolved| resolved.definitions.first().copied())
        .unwrap_or(node);

    let Defined(definition) = db.get(definition_node)?;

    Some(Comments {
        definition: definition_node,
        comments: definition.comments().to_vec(),
        links: get_links(db, definition_node, node),
    })
}

#[derive(Debug, Clone)]
pub struct ErrorInstance<'a> {
    pub bound: &'a ResolvedBound,
    pub is_default: bool,
    pub comments: Comments,
}

pub fn error_instances<'a>(db: &QueryCtx<'a>, node: Node) -> Vec<ErrorInstance<'a>> {
    let Some(ResolvedBounds(bounds)) = db.get(node) else {
        return Vec::new();
    };

    bounds
        .values()
        .filter_map(|result| {
            let bound = result.as_ref().ok()?;

            let instance = db
                .get(bound.instance.node)
                .and_then(|Defined(definition)| definition.downcast_ref::<InstanceDefinition>())?;

            if !instance.error {
                return None;
            }

            let Defined(definition) = db.get(bound.instance.node)?;
            let instance_definition = definition.downcast_ref::<InstanceDefinition>()?;

            let comments = Comments {
                definition: bound.instance.node,
                comments: instance_definition.comments.clone(),
                links: get_links(db, bound.instance.node, node),
            };

            Some(ErrorInstance {
                bound,
                is_default: instance.default,
                comments,
            })
        })
        .collect()
}
