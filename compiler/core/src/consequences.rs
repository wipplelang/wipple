use crate::{db::Node, typecheck::constraints::ConstraintConsequence};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Consequences {
    responsible: BTreeMap<Node, BTreeSet<usize>>,
    relevant: BTreeMap<Node, BTreeSet<Node>>,
    consequences: Vec<ConstraintConsequence>,
}

impl Consequences {
    pub fn insert(&mut self, node: Node, consequence: &ConstraintConsequence) {
        let index = self.consequences.len();
        self.consequences.push(consequence.clone());

        self.responsible.entry(node).or_default().insert(index);

        for relevant in consequence.relevant_nodes() {
            self.relevant.entry(relevant).or_default().insert(node);
        }
    }

    pub fn responsible_for(&self, node: Node) -> impl Iterator<Item = &ConstraintConsequence> {
        self.responsible
            .get(&node)
            .map_or_default(|indices| indices.iter().copied())
            .filter_map(|index| self.consequences.get(index))
    }

    pub fn relevant(&self, node: Node) -> impl Iterator<Item = Node> {
        self.relevant
            .get(&node)
            .map_or_default(|nodes| nodes.iter().copied())
    }
}
