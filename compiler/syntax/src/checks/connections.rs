use crate::expressions::call_expression::ResolvedCall;
use std::ops::ControlFlow;
use wipple_core::{
    db::{Db, Node},
    util::{get_links, instantiated_node_for},
    visit::{
        Resolved,
        definitions::{ConstantDefinition, Defined},
    },
};

pub fn create_connections(db: &mut Db, mut filter: impl FnMut(&Db, Node) -> bool) {
    let mut calls = Vec::new();
    db.for_each_fact::<ResolvedCall, ()>(&mut |db, node, call| {
        if filter(db, node) {
            calls.push((node, call.clone()));
        }

        ControlFlow::Continue(())
    });

    for (node, call) in calls {
        let mut has_custom_edges = false;
        if let Some(Resolved { definitions, .. }) = db.get(call.function)
            && definitions.len() == 1
        {
            let definition = *definitions.first().unwrap();

            let links = get_links(db, definition, call.function, |parameter| {
                instantiated_node_for(db, parameter, node)
            });

            if let Some(attributes) = db
                .get(definition)
                .and_then(|Defined(definition)| definition.downcast_ref::<ConstantDefinition>())
                .map(|definition| definition.attributes.clone())
            {
                for value in &attributes.connect {
                    let Some(left) = links.get(&value.left) else {
                        continue;
                    };

                    let Some(right) = links.get(&value.right) else {
                        continue;
                    };

                    db.graph
                        .edge(left.node, right.node, value.label.to_string());

                    has_custom_edges = true;
                }
            }
        }

        if !has_custom_edges {
            db.graph.edge(call.function, node, "function");

            for input in call.inputs {
                db.graph.edge(input, node, "input");
            }
        }
    }
}
