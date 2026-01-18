use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Db, Fact, Render},
    visit::{Visit, Visitor},
};
use std::{
    any::Any,
    env,
    fmt::Debug,
    hash::{Hash, Hasher},
    ops::Deref,
    sync::{
        Arc, Weak,
        atomic::{self, AtomicUsize},
    },
};

pub trait Node: Debug + Any + Visit + Codegen + Send + Sync {
    fn is_hidden(&self) -> bool {
        false
    }

    fn type_name(&self) -> String {
        let name = std::any::type_name::<Self>();
        name.split("::").last().unwrap_or(name).to_string()
    }
}

impl dyn Node {
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        (self as &dyn Any).downcast_ref()
    }
}

#[derive(Clone)]
pub struct NodeRef(usize, Arc<dyn Node>);

impl NodeRef {
    pub fn new(node: impl Node) -> Self {
        static ID: AtomicUsize = AtomicUsize::new(0);

        NodeRef(ID.fetch_add(1, atomic::Ordering::Relaxed), Arc::new(node))
    }

    pub fn downgrade(&self) -> WeakNodeRef {
        WeakNodeRef(self.0, Arc::downgrade(&self.1))
    }

    pub fn id(&self) -> usize {
        self.0
    }
}

impl Debug for NodeRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            self.1.fmt(f)
        } else {
            write!(f, "({}) {}", self.id(), self.1.type_name())
        }
    }
}

impl PartialEq for NodeRef {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl Eq for NodeRef {}

impl PartialOrd for NodeRef {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for NodeRef {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id().cmp(&other.id())
    }
}

// NOTE: NodeRef does NOT implement `Hash`, as using nodes in a HashMap makes
// the compiler nondeterministic

impl Deref for NodeRef {
    type Target = dyn Node;

    fn deref(&self) -> &Self::Target {
        self.1.as_ref()
    }
}

impl Render for NodeRef {
    fn write(&self, f: &mut dyn std::fmt::Write, db: &Db) -> std::fmt::Result {
        if env::var("WIPPLE_DEBUG").is_ok() {
            write!(f, "({}) ", self.id())?;
        }

        write!(f, "{}", db.span(self).as_node_source())?;

        Ok(())
    }

    fn link(&self) -> Option<&NodeRef> {
        Some(self)
    }
}

#[derive(Clone)]
pub struct WeakNodeRef(usize, Weak<dyn Node>);

impl WeakNodeRef {
    pub fn upgrade(&self) -> Option<NodeRef> {
        self.1.upgrade().map(|node| NodeRef(self.0, node))
    }

    pub fn id(&self) -> usize {
        self.0
    }
}

impl Debug for WeakNodeRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.id())
    }
}

impl PartialEq for WeakNodeRef {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl Eq for WeakNodeRef {}

impl PartialOrd for WeakNodeRef {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for WeakNodeRef {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id().cmp(&other.id())
    }
}

impl Hash for WeakNodeRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id().hash(state);
    }
}

#[derive(Debug, Default)]
pub struct HiddenNode(pub Option<Arc<dyn Node>>);

impl HiddenNode {
    pub fn new(node: impl Node) -> Self {
        HiddenNode(Some(Arc::new(node)))
    }
}

impl Node for HiddenNode {
    fn is_hidden(&self) -> bool {
        true
    }
}

impl Visit for HiddenNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        if let Some(inner) = &self.0 {
            inner.visit(node, visitor);
        }
    }
}

impl Codegen for HiddenNode {
    fn codegen(&self, codegen: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        if let Some(inner) = &self.0 {
            inner.codegen(codegen)
        } else {
            Err(codegen.error())
        }
    }
}

#[derive(Debug, Clone)]
pub struct Parent(pub NodeRef);

impl Fact for Parent {}

impl Render for Parent {}

#[derive(Debug, Clone, Default)]
pub struct Children(pub Vec<NodeRef>);

impl Fact for Children {}

impl Render for Children {}
