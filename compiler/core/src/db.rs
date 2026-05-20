use crate::{
    facts::Syntax,
    graph::GraphBuilder,
    render::{Render, RenderCtx},
};
use dyn_clone::DynClone;
use serde::{Deserialize, Serialize};
use std::{
    any::{Any, TypeId},
    collections::BTreeMap,
    fmt::{Debug, Display},
    ops::Deref,
    str::FromStr,
    sync::Arc,
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Node {
    layer: usize,
    index: usize,
}

impl Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Node({})", self.id())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(Node);

impl Node {
    pub fn id(self) -> NodeId {
        NodeId(self)
    }
}

impl Display for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.0.layer, self.0.index)
    }
}

impl FromStr for NodeId {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (layer, index) = s.split_once('.').ok_or(())?;
        let layer = layer.parse().map_err(|_| ())?;
        let index = index.parse().map_err(|_| ())?;

        Ok(NodeId(Node { layer, index }))
    }
}

#[typetag::serde]
pub trait Fact: Debug + DynClone + Any + Send + Sync + Render {}

dyn_clone::clone_trait_object!(Fact);

impl dyn Fact {
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        (self as &dyn Any).downcast_ref()
    }

    pub fn downcast_mut<T: Any>(&mut self) -> Option<&mut T> {
        (self as &mut dyn Any).downcast_mut()
    }
}

#[derive(Default, Serialize, Deserialize)]
pub struct Db {
    parent: Option<DbRef>,
    pub debug_enabled: bool,
    pub graph: GraphBuilder,
    nodes: Vec<NodeInfo>,                // for owned nodes
    overrides: BTreeMap<Node, NodeInfo>, // for parent nodes
}

#[derive(Debug, Clone)]
pub struct DbRef(Arc<Db>);

impl DbRef {
    pub fn new(db: Db) -> Self {
        Self(Arc::new(db))
    }
}

impl Deref for DbRef {
    type Target = Db;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Serialize for DbRef {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for DbRef {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(DbRef(Arc::new(Db::deserialize(deserializer)?)))
    }
}

impl Debug for Db {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Db")
            .field("layer", &self.layer())
            .field("parent", &self.parent)
            .finish_non_exhaustive()
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
struct NodeInfo {
    hidden: bool,
    facts: Facts,
}

#[derive(Debug, Clone, Default)]
struct Facts(BTreeMap<TypeId, Box<dyn Fact>>);

impl Serialize for Facts {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        Vec::from_iter(self.0.values()).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Facts {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Facts(
            Vec::<Box<dyn Fact>>::deserialize(deserializer)?
                .into_iter()
                .map(|fact| ((*fact).type_id(), fact))
                .collect(),
        ))
    }
}

impl Db {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn set_parent(&mut self, parent: DbRef) {
        self.parent = Some(parent);
    }

    pub fn layer(&self) -> usize {
        std::iter::successors(self.parent.as_ref(), |db| db.parent.as_ref()).count()
    }

    pub fn node(&mut self) -> Node {
        let id = self.nodes.len();
        self.nodes.push(NodeInfo::default());

        Node {
            layer: self.layer(),
            index: id,
        }
    }

    fn info(&self, node: Node) -> Option<&NodeInfo> {
        if node.layer == self.layer() {
            Some(&self.nodes[node.index])
        } else if let Some(info) = self.overrides.get(&node) {
            Some(info)
        } else {
            None
        }
    }

    fn info_mut(&mut self, node: Node) -> &mut NodeInfo {
        if node.layer == self.layer() {
            &mut self.nodes[node.index]
        } else {
            self.overrides.entry(node).or_insert_with(|| {
                self.parent
                    .as_ref()
                    .and_then(|parent| parent.info(node))
                    .cloned()
                    .unwrap_or_default()
            })
        }
    }

    pub fn hide(&mut self, node: Node) {
        self.info_mut(node).hidden = true;
    }

    pub fn is_hidden(&self, node: Node) -> bool {
        if let Some(info) = self.info(node) {
            info.hidden
        } else {
            self.parent
                .as_ref()
                .is_some_and(|parent| parent.is_hidden(node))
        }
    }

    pub fn contains<T: Fact>(&self, node: Node) -> bool {
        if let Some(info) = self.info(node) {
            info.facts.0.contains_key(&TypeId::of::<T>())
        } else {
            self.parent
                .as_ref()
                .is_some_and(|parent| parent.contains::<T>(node))
        }
    }

    pub fn get<T: Fact>(&self, node: Node) -> Option<&T> {
        if let Some(info) = self.info(node) {
            info.facts
                .0
                .get(&TypeId::of::<T>())
                .map(|fact| fact.downcast_ref::<T>().unwrap())
        } else {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get::<T>(node))
        }
    }

    pub fn get_mut<T: Fact>(&mut self, node: Node) -> Option<&mut T> {
        self.info_mut(node)
            .facts
            .0
            .get_mut(&TypeId::of::<T>())
            .map(|fact| fact.downcast_mut::<T>().unwrap())
    }

    pub fn get_mut_or_default<T: Fact + Default>(&mut self, node: Node) -> &mut T {
        self.info_mut(node)
            .facts
            .0
            .entry(TypeId::of::<T>())
            .or_insert_with(|| Box::new(T::default()))
            .downcast_mut::<T>()
            .unwrap()
    }

    pub fn insert<T: Fact>(&mut self, node: Node, fact: T) {
        self.info_mut(node)
            .facts
            .0
            .insert(TypeId::of::<T>(), Box::new(fact));
    }

    pub fn collect_facts<T: Fact>(&self) -> BTreeMap<Node, &T> {
        let mut facts = self
            .parent
            .as_ref()
            .map(|parent| parent.collect_facts::<T>())
            .unwrap_or_default();

        for (&node, info) in &self.overrides {
            if let Some(fact) = info.facts.0.get(&TypeId::of::<T>()) {
                facts.insert(node, fact.downcast_ref::<T>().unwrap());
            }
        }

        for (index, info) in self.nodes.iter().enumerate() {
            if let Some(fact) = info.facts.0.get(&TypeId::of::<T>()) {
                let node = Node {
                    layer: self.layer(),
                    index,
                };

                facts.insert(node, fact.downcast_ref::<T>().unwrap());
            }
        }

        facts
    }

    pub fn owned_nodes(&self) -> impl Iterator<Item = Node> {
        self.nodes
            .iter()
            .enumerate()
            .map(|(index, _)| Node {
                layer: self.layer(),
                index,
            })
            .chain(self.overrides.keys().copied())
    }

    pub fn debug(&self, mut filter: impl FnMut(&Self, Node) -> bool) -> impl Display {
        let mut nodes = self
            .owned_nodes()
            .filter_map(|node| {
                let info = self.info(node).unwrap();

                if !self.debug_enabled && (info.hidden || !filter(self, node)) {
                    return None;
                }

                let syntax = info
                    .facts
                    .0
                    .get(&TypeId::of::<Syntax>())
                    .and_then(|fact| fact.downcast_ref::<Syntax>());

                if !self.debug_enabled && syntax.is_none() {
                    return None;
                }

                Some((node, info, syntax))
            })
            .collect::<Vec<_>>();

        nodes.sort_by_key(|(_, _, syntax)| syntax.map(|syntax| syntax.span()));

        let mut ctx = RenderCtx::default();
        for (node, info, syntax) in nodes {
            ctx.node(node);
            if let Some(syntax) = syntax {
                ctx.string(format!(" ({})", syntax.0.type_name()));
            }
            ctx.string(":\n");

            let mut facts = info
                .facts
                .0
                .values()
                .filter_map(|fact| {
                    let mut ctx = RenderCtx::default();
                    fact.render_into(self, &mut ctx);

                    if ctx.is_empty() {
                        return None;
                    }

                    Some(
                        ctx.finish(self, |db, segment| segment.markdown(db, true))
                            .0
                            .to_string(),
                    )
                })
                .collect::<Vec<_>>();

            if facts.is_empty() {
                ctx.string("  (no facts)\n");
            } else {
                facts.sort();

                for fact in facts {
                    ctx.string(format!("  {fact}\n"));
                }
            }

            ctx.string("\n");
        }

        ctx.finish(self, |db, segment| segment.markdown(db, true)).0
    }
}
