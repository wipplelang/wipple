use crate::expressions::call_expression::ResolvedCall;
use std::ops::ControlFlow;
use wipple_core::{
    db::{Db, Node},
    util::{LinkKind, get_links},
    visit::{
        Resolved,
        definitions::{ConstantDefinition, Defined},
    },
};

pub fn create_connections(db: &mut Db, mut filter: impl FnMut(&Db, Node) -> bool) {
    if db.graph.is_none() {
        return;
    }

    let mut calls = Vec::new();
    db.for_each_fact::<ResolvedCall, ()>(false, &mut |db, node, call| {
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

            let links = get_links(db, definition, call.function, &mut filter);

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

                    if let LinkKind::Node(left) = left.kind
                        && let LinkKind::Node(right) = right.kind
                    {
                        db.graph
                            .as_mut()
                            .unwrap()
                            .edge(left, right, value.label.to_string());
                    }

                    has_custom_edges = true;
                }
            }
        }

        let graph = db.graph.as_mut().unwrap();

        if !has_custom_edges {
            graph.edge(call.function, node, "function");

            for input in call.inputs {
                graph.edge(input, node, "input");
            }
        }
    }
}
