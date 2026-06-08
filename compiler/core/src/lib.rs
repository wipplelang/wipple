pub mod ast;
pub mod codegen;
pub mod db;
pub mod facts;
pub mod graph;
pub mod render;
pub mod span;
pub mod typecheck;
pub mod util;
pub mod visit;

pub use anyhow;

use crate::{
    ast::AstKey,
    db::{Db, Node},
    facts::Syntax,
    span::Str,
    typecheck::{
        bounds::{Instance, Instances},
        constraints::bound_constraint::{BoundConstraint, IsBound},
        groups::Typed,
        solver::{Solver, Substitutions},
    },
    visit::{
        DefinitionConstraints, VisitUtilities, Visitor, definitions::Definition,
        exhaustiveness::check_exhaustiveness,
    },
};
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, mem, ops::ControlFlow};

#[derive(Debug, Serialize, Deserialize)]
pub struct LibraryArtifact<T> {
    pub db: T,
    pub top_level: TopLevel,
    pub statements: Vec<Node>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TopLevel {
    pub definitions: Vec<BTreeMap<Str, Vec<(Node, Box<dyn Definition>)>>>,
    pub substitutions: Vec<Substitutions>,
    pub utilities: VisitUtilities,
}

pub fn default_filter(db: &Db, node: Node) -> bool {
    db.owned_nodes().any(|owned| owned == node)
        && !db.is_hidden(node)
        && db.contains::<Syntax>(node)
}

pub fn compile<'a, K: Ord>(
    db: &mut Db,
    top_level: &mut TopLevel,
    files: impl IntoIterator<Item = &'a AstKey>,
    mut checks: impl FnMut(&mut Db, &TopLevel),
    mut group_order: impl FnMut(&Db, Node) -> K,
) -> (Node, Vec<Node>, Vec<Node>) {
    // Define/resolve names and collect constraints

    let root_node = db.node();
    db.hide(root_node);

    let mut visitor = Visitor::new(
        db,
        root_node,
        top_level.definitions.iter().cloned(),
        top_level.substitutions.iter().map(|substitutions| {
            (
                substitutions.nodes.clone(),
                substitutions.parameters.clone(),
            )
        }),
    );

    visitor.push_scope(db, root_node);

    let source_files = files
        .into_iter()
        .map(|file| visitor.visit(db, file))
        .collect::<Vec<_>>();

    let mut visited = visitor.finish(db);

    top_level
        .definitions
        .push(mem::take(&mut visited.definitions));

    top_level.substitutions = mem::take(&mut visited.substitutions);

    top_level.utilities.extend(visited.utilities);

    // Solve constraints from each definition, implying all bounds

    let definition_constraints = db
        .owned_nodes()
        .filter_map(|node| {
            db.get(node)
                .map(|DefinitionConstraints(constraints)| (node, constraints.clone()))
        })
        .collect::<Vec<_>>();

    for (definition_node, definition_constraints) in definition_constraints {
        let mut solver = Solver::default();
        solver.substitutions.extend(top_level.substitutions.clone());

        // If the definition is an instance, imply it inside itself
        db.for_each_fact::<Instances, ()>(&mut |_, _, Instances(instances)| {
            for instance in instances {
                if instance.node == definition_node {
                    solver.imply(instance.clone());
                    break;
                }
            }
            ControlFlow::Continue(())
        });

        // Also imply all of the definition's bounds (so they remain generic
        // while resolving the definition)
        for constraint in &definition_constraints {
            if let Some(constraint) = constraint.downcast_ref::<BoundConstraint>() {
                // Only imply bounds from constraints, not from inside the
                // definition's value!
                if !db.contains::<IsBound>(constraint.node) {
                    continue;
                }

                let (_, parameters) = solver.get_substitutions(constraint.bound.substitutions);

                let instance = Instance {
                    node: constraint.node,
                    trait_node: constraint.bound.trait_node,
                    parameters,
                    is_from_bound: true,
                };

                solver.imply(instance);
            }
        }

        solver.constraints.extend(definition_constraints);
        solver.run(db);

        set_groups(db, solver, &mut group_order);
    }

    // Solve constraints from top-level expressions
    let mut solver = Solver::default();
    solver.trace = true;
    solver.constraints.extend(visited.constraints.drain(..));
    solver.substitutions.extend(top_level.substitutions.clone());
    solver.run(db);
    set_groups(db, solver, &mut group_order);

    // Run checks
    check_exhaustiveness(db);
    checks(db, top_level);

    (root_node, source_files, visited.top_level_statements)
}

pub fn set_groups<K: Ord>(db: &mut Db, solver: Solver, mut key: impl FnMut(&Db, Node) -> K) {
    let groups = solver.into_sorted_groups(|node| key(db, node));

    for group in groups {
        if group.nodes.is_empty() {
            continue;
        }

        let mut nodes = Vec::new();
        for &node in &group.nodes {
            // Don't overwrite existing groups
            if db.get(node).is_some_and(|Typed(group)| group.is_some()) {
                continue;
            }

            db.insert(node, Typed(Some(group.clone())));

            nodes.push(node);
        }

        db.graph.group(nodes, group.tys.clone());
    }
}
