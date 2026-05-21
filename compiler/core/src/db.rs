use crate::{
    ast::{Ast, AstKey},
    facts::Syntax,
    graph::GraphBuilder,
    render::{Render, RenderCtx},
    visit::Visit,
};
use dyn_clone::DynClone;
use serde::{Deserialize, Serialize};
use std::{
    any::{Any, TypeId},
    collections::{BTreeMap, BTreeSet},
    fmt::{Debug, Display},
    ops::{ControlFlow, Deref},
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

pub struct Db {
    parent: Option<DbRef>,
    pub debug_enabled: bool,
    pub(crate) ast: Ast,
    pub graph: GraphBuilder,
    nodes: Vec<NodeInfo>,                // for owned nodes
    overrides: BTreeMap<Node, NodeInfo>, // for parent nodes
    cache: BTreeMap<TypeId, BTreeSet<Node>>,
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

#[derive(Serialize, Deserialize)]
struct SerializedDb {
    parent: Option<Box<SerializedDb>>,
    debug_enabled: bool,
    graph: GraphBuilder,
    ast: Ast,
    nodes: Vec<NodeInfo>,
    overrides: BTreeMap<Node, NodeInfo>,
}

impl From<&Db> for SerializedDb {
    fn from(db: &Db) -> Self {
        SerializedDb {
            parent: db
                .parent
                .as_ref()
                .map(|parent| Box::new(SerializedDb::from(parent.deref()))),
            debug_enabled: db.debug_enabled,
            graph: db.graph.clone(),
            ast: db.ast.clone(),
            nodes: db.nodes.clone(),
            overrides: db.overrides.clone(),
        }
    }
}

impl From<SerializedDb> for Db {
    fn from(serialized: SerializedDb) -> Self {
        let mut db = Db {
            parent: serialized
                .parent
                .map(|parent| DbRef::new(Db::from(*parent))),
            debug_enabled: serialized.debug_enabled,
            graph: serialized.graph,
            ast: serialized.ast,
            nodes: serialized.nodes,
            overrides: serialized.overrides,
            cache: Default::default(),
        };

        let mut cache = BTreeMap::<TypeId, BTreeSet<Node>>::new();
        for (node, info) in db.owned_nodes_with_info() {
            for fact in info.facts.0.values() {
                cache
                    .entry(fact.as_ref().type_id())
                    .or_default()
                    .insert(node);
            }
        }

        db.cache = cache;

        db
    }
}

impl Serialize for Db {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerializedDb::from(self).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Db {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Db::from(SerializedDb::deserialize(deserializer)?))
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
    pub fn new(parent: Option<DbRef>) -> Self {
        let layer = parent.as_ref().map_or(0, |p| p.layer() + 1);

        Db {
            parent,
            debug_enabled: false,
            ast: Ast::new(layer),
            graph: Default::default(),
            nodes: Default::default(),
            overrides: Default::default(),
            cache: Default::default(),
        }
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

    #[allow(clippy::borrowed_box, reason = "allow cloning")]
    pub fn ast(&self, key: &AstKey) -> &Box<dyn Visit> {
        self.ast
            .get(key)
            .unwrap_or_else(|| self.parent.as_ref().unwrap().ast(key))
    }

    pub fn in_ast(&mut self, value: Box<dyn Visit>) -> AstKey {
        self.ast.insert(value)
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
        self.cache
            .entry(TypeId::of::<T>())
            .or_default()
            .insert(node);

        self.info_mut(node)
            .facts
            .0
            .entry(TypeId::of::<T>())
            .or_insert_with(|| Box::new(T::default()))
            .downcast_mut::<T>()
            .unwrap()
    }

    pub fn insert<T: Fact>(&mut self, node: Node, fact: T) {
        self.cache
            .entry(TypeId::of::<T>())
            .or_default()
            .insert(node);

        self.info_mut(node)
            .facts
            .0
            .insert(TypeId::of::<T>(), Box::new(fact));
    }

    pub fn for_each_fact<T: Fact, U>(
        &self,
        f: &mut dyn FnMut(&Self, Node, &T) -> ControlFlow<U>,
    ) -> Option<U> {
        if let Some(parent) = self.parent.as_ref()
            && let Some(result) = parent.for_each_fact(f)
        {
            return Some(result);
        }

        let nodes = self.cache.get(&TypeId::of::<T>())?;

        for &node in nodes {
            let fact = self
                .info(node)
                .unwrap()
                .facts
                .0
                .get(&TypeId::of::<T>())
                .unwrap()
                .downcast_ref::<T>()
                .unwrap();

            if let ControlFlow::Break(result) = f(self, node, fact) {
                return Some(result);
            }
        }

        None
    }

    pub fn owned_nodes(&self) -> impl Iterator<Item = Node> {
        self.owned_nodes_with_info().map(|(node, _)| node)
    }

    fn owned_nodes_with_info(&self) -> impl Iterator<Item = (Node, &NodeInfo)> {
        self.nodes
            .iter()
            .enumerate()
            .map(|(index, info)| {
                let node = Node {
                    layer: self.layer(),
                    index,
                };

                (node, info)
            })
            .chain(self.overrides.iter().map(|(&node, info)| (node, info)))
    }

    pub fn gc(&mut self) {
        self.ast.gc();
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

        nodes
            .sort_by_key(|(_, _, syntax)| syntax.map(|Syntax(syntax)| syntax.get(self).span(self)));

        let mut ctx = RenderCtx::default();
        for (node, info, syntax) in nodes {
            ctx.node(node);
            if let Some(Syntax(syntax)) = syntax {
                ctx.string(format!(" ({})", syntax.get(self).type_name()));
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
