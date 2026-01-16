use crate::{
    database::NodeRef,
    nodes::{InstanceDefinitionNode, TypeParameterNode},
    queries::QueryCtx,
    typecheck::{Bound, Bounds, Constraint, ConstructedType, Instantiated, Type, Typed},
    visit::{Defined, Resolved, TypeParameters},
};
use std::collections::HashMap;

#[derive(Debug)]
pub struct QueriedErrorInstance {
    pub bound: Bound,
    pub comments: QueriedComments,
    pub trace: Vec<Box<dyn Constraint>>,
}

pub fn error_instance(ctx: &QueryCtx<'_>, f: &mut dyn FnMut(QueriedErrorInstance)) {
    let Some(Bounds(bounds)) = ctx.db.get(&ctx.node) else {
        return;
    };

    for item in bounds {
        if let Some(instance) = item.instance
            && instance.error
        {
            let Some(instance_definition) = instance.node.downcast_ref::<InstanceDefinitionNode>()
            else {
                continue;
            };

            let mut comments = QueriedComments {
                nodes: vec![instance.node.clone()],
                comments: instance_definition.comments.clone(),
                links: get_links(ctx, &instance.node),
            };

            let trace = item
                .bound
                .substitutions
                .values()
                .into_iter()
                .filter_map(|ty| {
                    let Type::Node(node) = ty else {
                        return None;
                    };

                    let Some(Typed { group: Some(group) }) = ctx.db.get(&node) else {
                        return None;
                    };

                    comments.nodes.extend(group.nodes.iter().cloned());

                    Some(group.trace.clone())
                })
                .flatten()
                .collect::<Vec<_>>();

            f(QueriedErrorInstance {
                bound: item.bound,
                comments,
                trace,
            });
        }
    }
}

#[derive(Debug)]
pub struct QueriedComments {
    pub nodes: Vec<NodeRef>,
    pub comments: Vec<String>,
    pub links: HashMap<String, QueriedCommentsLink>,
}

pub fn comments(ctx: &QueryCtx<'_>, f: &mut dyn FnMut(QueriedComments)) {
    let definition_node = ctx.db.get::<Resolved>(&ctx.node).map_or_else(
        || ctx.node.clone(),
        |Resolved { definitions, .. }| definitions.into_iter().next().unwrap(),
    );

    let Some(Defined(definition)) = ctx.db.get::<Defined>(&definition_node) else {
        return;
    };

    f(QueriedComments {
        nodes: vec![definition.node()],
        comments: definition.comments().to_vec(),
        links: get_links(ctx, &definition.node()),
    })
}

#[derive(Debug)]
pub struct QueriedCommentsLink {
    pub node: NodeRef,
    pub related: Vec<NodeRef>,
    pub types: Vec<ConstructedType>,
}

fn get_links(
    ctx: &QueryCtx<'_>,
    definition_node: &NodeRef,
) -> HashMap<String, QueriedCommentsLink> {
    let mut links = HashMap::new();

    let Some(TypeParameters(type_parameters)) = ctx.db.get(definition_node) else {
        return links;
    };

    for type_parameter_node in type_parameters {
        let Some(type_parameter) = type_parameter_node.downcast_ref::<TypeParameterNode>() else {
            continue;
        };

        let Some(instantiated) = ctx
            .db
            .iter::<Instantiated>()
            .find_map(|(node, instantiated)| {
                (instantiated.from == type_parameter_node && instantiated.source_node == ctx.node)
                    .then_some(node)
            })
        else {
            continue;
        };

        let Some(Typed { group: Some(group) }) = ctx.db.get(&instantiated) else {
            continue;
        };

        let mut uses = group.nodes.iter().cloned();
        let first = uses.next().unwrap();
        let related = uses.collect::<Vec<_>>();

        links.insert(
            type_parameter.name.clone(),
            QueriedCommentsLink {
                node: first,
                related,
                types: group.types.clone(),
            },
        );
    }

    links
}
