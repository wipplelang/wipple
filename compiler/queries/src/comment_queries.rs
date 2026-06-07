use wipple_core::{
    db::{Db, Node},
    render::Comments,
    typecheck::{
        bounds::{Bounds, ResolvedBound},
        constraints::ConstraintTrace,
    },
    util::get_links,
    visit::{
        Resolved,
        definitions::{Defined, InstanceDefinition},
    },
};

pub fn comments_without_links(db: &Db, node: Node) -> Option<Comments> {
    let definition_node = db
        .get::<Resolved>(node)
        .and_then(|resolved| resolved.definitions.first().copied())
        .unwrap_or(node);

    let Defined(definition) = db.get(definition_node)?;

    Some(Comments {
        nodes: vec![definition_node],
        comments: definition.comments().to_vec(),
        links: Default::default(),
    })
}

pub fn comments_with_links(db: &Db, node: Node) -> Option<Comments> {
    let definition_node = db
        .get::<Resolved>(node)
        .and_then(|resolved| resolved.definitions.first().copied())
        .unwrap_or(node);

    let Defined(definition) = db.get(definition_node)?;

    Some(Comments {
        nodes: vec![definition_node],
        comments: definition.comments().to_vec(),
        links: get_links(db, definition_node, node),
    })
}

#[derive(Debug, Clone)]
pub struct ErrorInstance<'a> {
    pub bound: &'a ResolvedBound,
    pub is_default: bool,
    pub comments: Comments,
    pub traces: Vec<Box<dyn ConstraintTrace>>,
}

pub fn error_instances<'a>(db: &'a Db, node: Node) -> Vec<ErrorInstance<'a>> {
    let Some(Bounds(bounds)) = db.get(node) else {
        return Vec::new();
    };

    bounds
        .iter()
        .filter_map(|(_, result)| {
            let bound = result.as_ref().ok()?;

            let instance = db
                .get(bound.instance.node)
                .and_then(|Defined(definition)| definition.downcast_ref::<InstanceDefinition>())?;

            if !instance.error {
                return None;
            }

            let Defined(definition) = db.get(bound.instance.node)?;
            let instance_definition = definition.downcast_ref::<InstanceDefinition>()?;

            let mut comments = Comments {
                nodes: vec![bound.instance.node],
                comments: instance_definition.comments.clone(),
                links: get_links(db, bound.instance.node, bound.resolved_node),
            };

            let mut traces = db
                .traces_for(comments.nodes.iter().copied().chain(
                    comments.links.values().flat_map(|link| {
                        [link.node].into_iter().chain(link.related.iter().copied())
                    }),
                ))
                .collect::<Vec<_>>();

            comments.nodes.extend(
                traces
                    .iter_mut()
                    .flat_map(|trace| trace.nodes_mut().into_iter().map(|&mut node| node)),
            );

            Some(ErrorInstance {
                bound,
                is_default: instance.default,
                comments,
                traces,
            })
        })
        .collect()
}
