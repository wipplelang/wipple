pub mod connections;
pub mod instances;

use crate::{
    GroupOrder,
    checks::{
        connections::create_connections,
        instances::{check_for_overlapping_instances, run_mismatched_trait},
    },
};
use wipple_core::{
    TopLevel,
    db::Db,
    default_filter, set_groups,
    typecheck::{bounds::Instances, solver::Solver},
    visit::definitions::{Defined, TraitDefinition},
};

pub fn run_checks(db: &mut Db, top_level: &TopLevel) {
    // Check for overlapping instances

    for (trait_node, instances) in db
        .collect_facts()
        .into_iter()
        .map(|(trait_node, Instances(instances))| (trait_node, instances.clone()))
        .collect::<Vec<_>>()
    {
        let mut solver = Solver::default();
        solver.substitutions.extend(top_level.substitutions.clone());
        check_for_overlapping_instances(db, &mut solver, trait_node, instances);
    }

    // Resolved `Mismatched` trait for mismatched types

    if let Some(mismatched_trait_node) = top_level.utilities.get("Mismatched")
        && let Some(Defined(definition)) = db.get::<Defined>(*mismatched_trait_node)
        && let Some(trait_definition) = definition.downcast_ref::<TraitDefinition>().cloned()
    {
        for solver in run_mismatched_trait(
            db,
            *mismatched_trait_node,
            &trait_definition,
            default_filter,
            |solver| solver.substitutions.extend(top_level.substitutions.clone()),
        ) {
            set_groups(db, solver, GroupOrder::new);
        }
    }

    // Add custom connections

    create_connections(db, default_filter);
}
