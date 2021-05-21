use crate::*;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    result::Result,
};

fn_wrapper! {
    pub struct DeriveValueFn(Value, &Environment, &Stack) -> crate::Result;
}

core_env_key!(relations for RelationGraph {
    visibility: EnvironmentKeyVisibility::Public(
        UseMergeFn::of(|current: &mut RelationGraph, new| {
            for edge in new.edges() {
                current.add_edge(edge.clone());
            }

            // TODO: Detect cycles
        }),
    ),
});

impl EnvironmentInner {
    pub fn add_relation(
        &mut self,
        from_trait: Trait,
        to_trait: Trait,
        derive: DeriveValueFn,
    ) -> bool {
        let relations = self.relations();

        let is_added = relations.add_edge(RelationGraphEdge {
            from: RelationGraphNode::new(from_trait),
            to: RelationGraphNode::new(to_trait),
            derive,
            counter: *COUNTER.borrow(),
        });

        *COUNTER.borrow_mut() += 1;

        is_added
    }

    pub fn try_add_relation(
        &mut self,
        from_trait: Trait,
        to_trait: Trait,
        derive: DeriveValueFn,
        stack: &Stack,
    ) -> crate::Result<()> {
        if from_trait == to_trait {
            return Err(Return::error("Cannot relate a trait to itself", stack));
        }

        let is_added = self.add_relation(from_trait, to_trait, derive);

        if !is_added {
            return Err(Return::error(
                "There is already a relation between these traits",
                stack,
            ));
        }

        self.detect_relation_cycles(stack)
    }

    pub fn add_primitive_relation<A: TypeInfo, B: TypeInfo>(
        &mut self,
        derive_trait_value: impl Fn(A) -> B + 'static,
    ) {
        let is_added = self.add_relation(
            Trait::of::<A>(),
            Trait::of::<B>(),
            DeriveValueFn::new(move |value, _, _| {
                let a = value.into_primitive::<A>().unwrap();
                let b = derive_trait_value(a);
                Ok(Value::of(b))
            }),
        );

        debug_assert!(is_added);
    }

    pub fn add_text_relation<T: TypeInfo>(&mut self, value_name: &'static str) {
        self.add_primitive_relation(move |_: T| Text::new(&format!("<{}>", value_name)));
    }
}

impl EnvironmentInner {
    fn detect_relation_cycles(&mut self, stack: &Stack) -> crate::Result<()> {
        if self.relations().contains_cycle() {
            Err(Return::error("Relation cycle detected", stack))
        } else {
            Ok(())
        }
    }
}

impl Value {
    /// Relations are resolved in the following way:
    ///
    /// - Parent scopes and earlier declarations have priority.
    ///
    /// - Relations between the input's trait and the derived trait take precedence.
    ///   Only one of these relations must be satisfied (ie. only one relation
    ///   directly relating the traits must exist).
    ///
    /// - If none of the direct relations succeed, the first indirect relation to
    ///   succeed is used.
    pub(crate) fn derive(
        &self,
        to_trait: &Trait,
        env: &Environment,
        stack: &Stack,
    ) -> crate::Result<Option<Value>> {
        // Parent scopes have priority
        let parent = env.borrow().parent.clone();
        if let Some(parent) = parent {
            if let Some(value) = self.derive(to_trait, &parent, stack)? {
                return Ok(Some(value));
            }
        }

        let relations = env.borrow_mut().relations().clone();

        let direct_relations = relations
            .edges()
            .into_iter()
            .filter_map(|edge| {
                if self.is_trait_directly(&edge.from.r#trait) && &edge.to.r#trait == to_trait {
                    Some(&edge.derive)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        if direct_relations.len() > 1 {
            return Err(Return::error("Cannot derive this trait because there are multiple relations that derive it from the input's trait directly", stack));
        }

        if let Some(derive) = direct_relations.first() {
            let derived_value = derive(self.stored_value().clone(), env, stack)?;

            let matched_value =
                to_trait.pattern()(derived_value, env, stack)?
                .into_valid()
                .ok_or_else(|| Return::error("Cannot derive this trait because the relation's output cannot be represented by the trait", stack))?;

            return Ok(Some(matched_value));
        }

        // Sort paths by order of declaration
        let paths = match relations.paths(&self.r#trait(), to_trait) {
            Some(paths) => paths,
            None => return Ok(None),
        };

        for path in paths {
            if path.first().unwrap().from.r#trait != self.r#trait() {
                continue;
            }

            let mut result = self.stored_value().clone();

            for edge in path {
                let derived_value = (edge.derive)(result, env, stack)?;

                debug_assert!(derived_value.r#trait() == edge.to.r#trait);

                let matched_value = edge.to.r#trait.pattern()(derived_value, env, stack)?
                    .into_valid()
                    .ok_or_else(|| Return::error("Cannot derive this trait because the relation's output cannot be represented by the trait", stack))?;

                result = matched_value;
            }

            debug_assert!(&result.r#trait() == to_trait);

            return Ok(Some(result));
        }

        Ok(None)
    }
}

ref_thread_local! {
    /// Used to determine which relations were declared first
    static managed COUNTER: usize = 0;
}

#[derive(TypeInfo, Debug, Clone, Default)]
struct RelationGraph {
    nodes: HashMap<Id, RelationGraphNode>,
}

impl RelationGraph {
    fn edges(&self) -> Vec<&RelationGraphEdge> {
        self.nodes.values().fold(Vec::new(), |result, node| {
            result.into_iter().chain(node.edges.iter()).collect()
        })
    }

    fn add_edge(&mut self, edge: RelationGraphEdge) -> bool {
        let id = edge.from.r#trait.id();

        macro_rules! node {
            ($trait:expr) => {
                self.nodes
                    .entry($trait.id())
                    .or_insert_with(|| RelationGraphNode::new($trait.clone()))
                    .clone()
            };
        }

        let from_node = node!(edge.from.r#trait);
        let to_node = node!(edge.to.r#trait);

        if from_node.edge_for_node(&to_node).is_some() {
            return false;
        }

        self.nodes.get_mut(&id).unwrap().add_edge(edge);

        true
    }

    fn paths(&self, from: &Trait, to: &Trait) -> Option<Vec<Vec<&RelationGraphEdge>>> {
        for r#trait in &[from, to] {
            if !self.nodes.values().any(|n| &&n.r#trait == r#trait) {
                return None;
            }
        }

        let mut visited = HashSet::new();
        let mut queue = vec![from.id()];
        let mut path_map = HashMap::<Id, &RelationGraphEdge>::new();
        let mut paths = Vec::new();

        while let Some(id) = queue.pop() {
            if id == to.id() {
                let mut edge = &**path_map.get(&id).unwrap();
                let mut edge_path = vec![edge];

                while &edge.from.r#trait != from {
                    edge = path_map.get(&edge.from.r#trait.id()).unwrap();
                    edge_path.insert(0, edge);
                }

                paths.push(edge_path);
            }

            for edge in &self.nodes.get(&id).unwrap().edges {
                let to_id = edge.to.r#trait.id();

                if !visited.contains(&to_id) {
                    visited.insert(to_id);
                    queue.push(to_id);
                    path_map.insert(to_id, edge);
                }
            }
        }

        Some(paths)
    }

    fn contains_cycle(&self) -> bool {
        #[derive(Clone, Copy, PartialEq, Eq)]
        enum Color {
            White,
            Gray,
            Black,
        }

        struct ColoredNode {
            id: Id,
            color: Color,
        }

        let colored_nodes = self
            .nodes
            .keys()
            .map(|id| {
                let colored_node = RefCell::new(ColoredNode {
                    id: *id,
                    color: Color::White,
                });

                (id, colored_node)
            })
            .collect::<HashMap<_, _>>();

        fn visit(
            node: &RefCell<ColoredNode>,
            nodes: &HashMap<Id, RelationGraphNode>,
            colored_nodes: &HashMap<&Id, RefCell<ColoredNode>>,
        ) -> bool {
            let node_id = node.borrow().id;
            let color = node.borrow().color;

            match color {
                Color::Gray => return true,
                Color::White => {
                    node.borrow_mut().color = Color::Gray;

                    for colored_node in colored_nodes.values() {
                        let colored_node_id = colored_node.borrow().id;

                        let contains_colored_node = nodes
                            .get(&node_id)
                            .unwrap()
                            .edges
                            .iter()
                            .any(|e| e.to.r#trait.id() == colored_node_id);

                        if contains_colored_node && visit(colored_node, nodes, colored_nodes) {
                            return true;
                        }
                    }

                    node.borrow_mut().color = Color::Black;
                }
                Color::Black => {}
            }

            false
        }

        for node in colored_nodes.values() {
            if node.borrow().color == Color::White && visit(node, &self.nodes, &colored_nodes) {
                return true;
            }
        }

        false
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct RelationGraphNode {
    r#trait: Trait,
    edges: Vec<RelationGraphEdge>,
}

impl RelationGraphNode {
    fn new(r#trait: Trait) -> Self {
        RelationGraphNode {
            r#trait,
            edges: Vec::new(),
        }
    }

    fn add_edge(&mut self, edge: RelationGraphEdge) {
        self.edges.push(edge)
    }

    fn edge_for_node(&self, node: &RelationGraphNode) -> Option<&RelationGraphEdge> {
        self.edges.iter().find(|e| &e.to == node)
    }
}

#[derive(Debug, Clone)]
struct RelationGraphEdge {
    from: RelationGraphNode,
    to: RelationGraphNode,
    derive: DeriveValueFn,
    counter: usize,
}

impl PartialEq for RelationGraphEdge {
    fn eq(&self, other: &Self) -> bool {
        self.from == other.from && self.to == other.to
    }
}

impl Eq for RelationGraphEdge {}
