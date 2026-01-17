use crate::database::{Graph, Node, NodeRef, Render, RenderConfig, Span, WeakNodeRef};
use dyn_clone::DynClone;
use std::{
    any::{Any, TypeId},
    collections::{BTreeMap, BTreeSet},
    fmt::{Debug, Display},
};

pub trait Fact: Any + Debug + DynClone + Render + Send + Sync {}

dyn_clone::clone_trait_object!(Fact);

#[derive(Clone, Default)]
pub struct Db {
    pub graph: Graph,
    facts: im::HashMap<TypeId, im::OrdMap<WeakNodeRef, Box<dyn Fact>>>,
    global_facts: im::HashMap<TypeId, Box<dyn Fact>>,
    render: RenderConfig,
}

impl Db {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn contains<T: Fact>(&self, node: &NodeRef) -> bool {
        self.facts
            .get(&TypeId::of::<T>())
            .is_some_and(|facts| facts.contains_key(&node.downgrade()))
    }

    pub fn contains_global<T: Fact>(&self) -> bool {
        self.global_facts.contains_key(&TypeId::of::<T>())
    }

    pub fn get<T: Fact>(&self, node: &NodeRef) -> Option<T> {
        let fact = self
            .facts
            .get(&TypeId::of::<T>())?
            .get(&node.downgrade())?
            .as_ref();

        Some(dyn_clone::clone(
            (fact as &dyn Any).downcast_ref::<T>().unwrap(),
        ))
    }

    pub fn get_global<T: Fact>(&self) -> Option<T> {
        let fact = self.global_facts.get(&TypeId::of::<T>())?.as_ref();

        Some(dyn_clone::clone(
            (fact as &dyn Any).downcast_ref::<T>().unwrap(),
        ))
    }

    pub fn insert<T: Fact>(&mut self, node: &NodeRef, fact: T) {
        self.facts
            .entry(TypeId::of::<T>())
            .or_default()
            .insert(node.downgrade(), Box::new(fact));
    }

    pub fn insert_global<T: Fact>(&mut self, fact: T) {
        self.global_facts.insert(TypeId::of::<T>(), Box::new(fact));
    }

    pub fn with_fact<T: Fact + Default, U>(
        &mut self,
        node: &NodeRef,
        f: impl FnOnce(&mut T) -> U,
    ) -> U {
        let fact = self
            .facts
            .entry(TypeId::of::<T>())
            .or_default()
            .entry(node.downgrade())
            .or_insert_with(|| Box::new(T::default()))
            .as_mut();

        f((fact as &mut dyn Any).downcast_mut::<T>().unwrap())
    }

    pub fn with_global_fact<T: Fact + Default, U>(&mut self, f: impl FnOnce(&mut T) -> U) -> U {
        let fact = self
            .global_facts
            .entry(TypeId::of::<T>())
            .or_insert_with(|| Box::new(T::default()))
            .as_mut();

        f((fact as &mut dyn Any).downcast_mut::<T>().unwrap())
    }

    pub fn iter_nodes(&self) -> impl Iterator<Item = NodeRef> {
        self.facts
            .values()
            .flat_map(|nodes| nodes.keys())
            .filter_map(|node| node.upgrade())
            .collect::<BTreeSet<_>>() // ensure nodes are unique
            .into_iter()
    }

    pub fn iter<T: Fact>(&self) -> impl Iterator<Item = (NodeRef, T)> {
        self.facts
            .get(&TypeId::of::<T>())
            .into_iter()
            .flat_map(|nodes| {
                nodes.iter().filter_map(|(node, fact)| {
                    let node = node.upgrade()?;
                    let fact = (fact.as_ref() as &dyn Any).downcast_ref::<T>().unwrap();
                    Some((node, dyn_clone::clone(fact)))
                })
            })
    }

    pub fn gc(&mut self) {
        for (_, facts) in self.facts.iter_mut() {
            for key in facts.keys().cloned().collect::<Vec<_>>() {
                if key.upgrade().is_none() {
                    facts.remove(&key);
                }
            }
        }
    }

    pub fn render_with(&mut self, config: RenderConfig) {
        self.render = config;
    }

    pub fn node(&mut self, span: Span, node: impl Node) -> NodeRef {
        let node = NodeRef::new(node);
        self.insert(&node, span);
        node
    }

    pub fn span(&self, node: &NodeRef) -> Span {
        self.get::<Span>(node).unwrap()
    }

    pub fn filtered_graph(&self, nodes: impl IntoIterator<Item = NodeRef>) -> Graph {
        let mut graph = self.graph.clone();
        graph.set_mask(nodes);
        graph
    }

    pub fn render<'a>(&'a self, value: &'a dyn Render) -> impl Display + 'a {
        self.render.wrap(self, value)
    }

    pub fn display(&self, filter: impl Fn(&NodeRef) -> bool) -> impl Display {
        struct Display<'a, F> {
            db: &'a Db,
            filter: F,
        }

        impl<'a, F> std::fmt::Display for Display<'a, F>
        where
            F: Fn(&NodeRef) -> bool,
        {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let mut nodes = BTreeMap::<NodeRef, Vec<_>>::new();
                for facts in self.db.facts.values() {
                    for (node, fact) in facts {
                        let Some(node) = node.upgrade() else {
                            continue;
                        };

                        if node.is_hidden() || !(self.filter)(&node) {
                            continue;
                        }

                        nodes.entry(node.clone()).or_default().push(fact.as_ref());
                    }
                }

                for (node, facts) in nodes {
                    writeln!(f, "{} {}", node.type_name(), self.db.render(&node))?;

                    let mut facts = facts
                        .into_iter()
                        .filter_map(|fact| {
                            let mut buf = String::new();
                            Render::write(fact, &mut buf, self.db).ok();
                            (!buf.is_empty()).then_some(buf)
                        })
                        .collect::<Vec<_>>();

                    facts.sort();

                    if facts.is_empty() {
                        writeln!(f, "  (no facts)")?;
                    } else {
                        for entry in facts {
                            writeln!(f, "  {entry}")?;
                        }
                    }
                }

                Ok(())
            }
        }

        Display { db: self, filter }
    }
}

impl Debug for Db {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Db").finish_non_exhaustive()
    }
}
