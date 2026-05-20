use std::collections::HashMap;

use wipple_core::{
    arcstr::Substr,
    db::{Db, Node},
    typecheck::{
        bounds::{Bounds, ResolvedBound},
        constraints::Constraint,
        groups::Typed,
        ty::Ty,
    },
    util::{Link, get_links},
    visit::{
        Resolved,
        definitions::{Defined, InstanceDefinition},
    },
};

#[derive(Debug, Clone)]
pub struct Comments {
    pub nodes: Vec<Node>,
    pub comments: Vec<Substr>,
    pub links: HashMap<Substr, Link>,
}

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
    pub trace: Vec<Box<dyn Constraint>>,
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

            if !instance.attributes.error {
                return None;
            }

            let Defined(definition) = db.get(bound.instance.node)?;
            let instance_definition = definition.downcast_ref::<InstanceDefinition>()?;

            let mut comments = Comments {
                nodes: vec![bound.instance.node],
                comments: instance_definition.comments.clone(),
                links: get_links(db, bound.instance.node, bound.resolved_node),
            };

            let trace = bound
                .instance_parameters
                .iter()
                .filter_map(|(_, ty)| {
                    let Ty::Node(node) = ty else {
                        return None;
                    };

                    let Typed(Some(group)) = db.get(*node)? else {
                        return None;
                    };

                    comments.nodes.extend(group.nodes.iter().copied());

                    Some(&group.trace)
                })
                .flatten()
                .cloned()
                .collect();

            Some(ErrorInstance {
                bound,
                is_default: instance.attributes.default,
                comments,
                trace,
            })
        })
        .collect()
}
