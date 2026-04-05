use crate::{
    database::{Db, NodeRef},
    nodes::{CallExpressionNode, ResolvedCall},
    queries::{QueryCtx, get_links},
    visit::{ConstantDefinition, Defined, Definition, Resolved},
};

impl Db {
    pub fn create_connections(&mut self, mut filter: impl FnMut(&mut Self, &NodeRef) -> bool) {
        let mut call_nodes = self
            .iter::<ResolvedCall>()
            .map(|(node, _)| node)
            .collect::<Vec<_>>();

        call_nodes.retain(|node| filter(self, node));

        for node in call_nodes {
            let Some(call) = node.downcast_ref::<CallExpressionNode>() else {
                continue;
            };

            let mut has_custom_edges = false;
            if let Some(Resolved { definitions, .. }) = self.get(&call.function)
                && let [definition] = definitions.as_slice()
            {
                let ctx = QueryCtx {
                    node: call.function.clone(),
                    db: self,
                };

                let links = get_links(&ctx, definition, &call.function);

                if let Some(Defined(Definition::Constant(ConstantDefinition {
                    attributes, ..
                }))) = self.get(definition)
                {
                    for value in attributes.connect {
                        let Some(left) = links.get(&value.left) else {
                            continue;
                        };

                        let Some(right) = links.get(&value.right) else {
                            continue;
                        };

                        self.graph.edge(&left.node, &right.node, &value.label);

                        has_custom_edges = true;
                    }
                }
            }

            if !has_custom_edges {
                self.graph.edge(&call.function, &node, "function");

                for input in &call.inputs {
                    self.graph.edge(input, &node, "input");
                }
            }
        }
    }
}
