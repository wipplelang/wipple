mod feedback;
mod layers;
mod order;

pub use feedback::*;
pub use layers::*;

use crate::{
    database::{Db, Fact, NodeRef, Render},
    driver::order::GroupOrder,
    nodes::BoundConstraintNode,
    typecheck::{Instances, Instantiated, Solver, Typed},
    visit::{Defined, Definition, DefinitionConstraints, Scope, Visitor},
};
use std::sync::Arc;

#[derive(Debug, Clone, Default)]
pub struct TopLevelScopes(Vec<Scope<Definition>>);

impl Fact for TopLevelScopes {}

impl Render for TopLevelScopes {}

pub fn compile(db: &mut Db, files: &[NodeRef]) {
    let node_is_from_files = |db: &mut Db, node: &NodeRef| {
        let node = db
            .get::<Instantiated>(node)
            .map_or_else(|| node.clone(), |instantiated| instantiated.source_node);

        let span = db.span(&node);

        files.iter().any(|file| db.span(file).path == span.path)
    };

    // Define/resolve names and collect constraints

    let TopLevelScopes(top_level_scopes) = db.get_global().unwrap_or_default();

    let mut visitor = Visitor::new(db, top_level_scopes);
    for file in files {
        visitor.visit(file);
    }

    let visited = visitor.finish();

    db.with_global_fact(|TopLevelScopes(top_level_scopes)| {
        top_level_scopes.push(visited.scope);
    });

    // Solve constraints from each definition, implying all bounds

    for (definition_node, DefinitionConstraints(definition_constraints)) in
        db.iter().collect::<Vec<_>>()
    {
        if !node_is_from_files(db, &definition_node) {
            continue;
        }

        let mut solver = Solver::new();

        // If the definition is an instance, imply it inside itself
        if let Some(instance) = db
            .iter::<Instances>()
            .find_map(|(_, Instances(instances))| {
                instances
                    .into_iter()
                    .find(|instance| instance.node == definition_node)
            })
        {
            solver.imply(instance);
        }

        // Also imply all of the definition's bounds (so they remain generic
        // while resolving the definition)

        for constraint in &definition_constraints {
            if let Some(instance) = &constraint.info().instance {
                // Only imply bounds from constraints, not from inside the
                // definition's value!
                if constraint
                    .info()
                    .node
                    .downcast_ref::<BoundConstraintNode>()
                    .is_some()
                {
                    solver.imply(instance.clone());
                }
            }
        }

        solver.insert_constraints(definition_constraints);
        solver.run(db);

        set_groups(db, solver, node_is_from_files);
    }

    // Solve constraints from top-level expressions

    let mut solver = Solver::new();
    solver.insert_constraints(visited.constraints);
    solver.run(db);

    set_groups(db, solver, node_is_from_files);

    // Check for overlapping instances
    for (trait_node, Instances(instances)) in db.iter::<Instances>().collect::<Vec<_>>() {
        db.check_for_overlapping_instances(&trait_node, instances);
    }

    // Resolve `Mismatched` trait for mismatched types
    if let Some(trait_definition) = visited
        .utilities
        .get("Mismatched")
        .and_then(|mismatched_trait| db.get::<Defined>(mismatched_trait))
        .and_then(|Defined(definition)| match definition {
            Definition::Trait(definition) => Some(definition),
            _ => None,
        })
    {
        for solver in db.run_mismatched_trait(&trait_definition, node_is_from_files) {
            set_groups(db, solver, node_is_from_files);
        }
    }

    db.gc();
}

fn set_groups(db: &mut Db, solver: Solver, filter: impl Fn(&mut Db, &NodeRef) -> bool) {
    for group in solver
        .into_sorted_groups(|node| GroupOrder::new(db, node))
        .collect::<Vec<_>>()
    {
        let group = Arc::new(group);

        let mut nodes = Vec::new();
        for node in &group.nodes {
            if !filter(db, node) {
                continue;
            }

            db.with_fact(node, |fact: &mut Typed| {
                fact.group.get_or_insert_with(|| {
                    nodes.push(node.clone());
                    group.clone()
                });
            });
        }

        if !nodes.is_empty() {
            db.graph.group(nodes, group.types.clone());
        }
    }
}
