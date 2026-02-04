use crate::database::{Db, NodeRef, Render};
use std::{fmt::Debug, sync::Arc};

#[derive(Clone)]
pub struct ConstructedType {
    pub representative: Option<NodeRef>,
    pub tag: ConstructedTypeTag,
    pub instantiate: Option<NodeRef>,
    pub children: Vec<Type>,
    pub display:
        Arc<dyn Fn(&Db, Vec<Box<dyn Fn(bool) -> String + '_>>, bool) -> String + Send + Sync>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstructedTypeTag {
    Named(NodeRef),
    Function,
    Tuple,
    Block,
    Parameter(NodeRef),
}

impl ConstructedType {
    pub fn new(
        id: ConstructedTypeTag,
        children: Vec<Type>,
        display: impl Fn(&Db, Vec<Box<dyn Fn(bool) -> String + '_>>, bool) -> String
        + Send
        + Sync
        + 'static,
    ) -> Self {
        ConstructedType {
            representative: None,
            tag: id,
            instantiate: None,
            children,
            display: Arc::new(display),
        }
    }

    pub fn instantiate(mut self, node: NodeRef) -> Self {
        self.instantiate = Some(node);
        self
    }
}

impl Debug for ConstructedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ConstructedType")
            .field("id", &self.tag)
            .field("representative", &self.representative)
            .field("instantiate", &self.instantiate)
            .field("children", &self.children)
            .finish()
    }
}

impl PartialEq for ConstructedType {
    fn eq(&self, other: &Self) -> bool {
        self.tag == other.tag
            && self.instantiate == other.instantiate
            && self.children == other.children
    }
}

impl Eq for ConstructedType {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Node(NodeRef),
    Constructed(ConstructedType),
}

impl From<NodeRef> for Type {
    fn from(node: NodeRef) -> Self {
        Type::Node(node)
    }
}

impl From<ConstructedType> for Type {
    fn from(ty: ConstructedType) -> Self {
        Type::Constructed(ty)
    }
}

impl Type {
    pub fn as_node(&self) -> Option<&NodeRef> {
        match self {
            Type::Node(node) => Some(node),
            Type::Constructed(_) => None,
        }
    }

    pub fn render(&self, root: bool) -> impl Render {
        struct Rendered(Type, bool);

        impl Render for Rendered {
            fn write(&self, f: &mut dyn std::fmt::Write, db: &Db) -> std::fmt::Result {
                write!(f, "{}", self.0.to_string(db, self.1))
            }

            fn link(&self) -> Option<&NodeRef> {
                match &self.0 {
                    Type::Node(node) => Some(node),
                    Type::Constructed(ty) => ty.representative.as_ref(),
                }
            }
        }

        Rendered(self.clone(), root)
    }

    pub fn to_string(&self, db: &Db, root: bool) -> String {
        match self {
            Type::Node(_) => String::from("_"),
            Type::Constructed(ty) => {
                let children = ty
                    .children
                    .iter()
                    .map(|child| {
                        let child = child.clone();
                        Box::new(move |root| child.to_string(db, root))
                            as Box<dyn Fn(bool) -> String>
                    })
                    .collect::<Vec<_>>();

                (ty.display)(db, children, root)
            }
        }
    }

    pub fn traverse(&self, mut f: impl FnMut(Type) -> Type) -> Self {
        self.traverse_inner(&mut f, &mut Vec::new())
    }

    fn traverse_inner(&self, f: &mut impl FnMut(Type) -> Type, stack: &mut Vec<Type>) -> Self {
        let ty = f(self.clone());

        if stack.contains(&ty) {
            return ty; // recursive type
        }

        stack.push(ty.clone());

        let ty = match ty {
            Type::Node(node) => Type::Node(node),
            Type::Constructed(ty) => {
                if ty.instantiate.is_some() {
                    // ignore the children of type parameters
                    Type::Constructed(ty.clone())
                } else {
                    Type::Constructed(ty.traverse_children_inner(f, stack))
                }
            }
        };

        stack.pop();

        ty
    }

    pub fn referenced_nodes(&self) -> Vec<NodeRef> {
        let mut nodes = Vec::new();
        self.traverse(|ty| {
            if let Type::Node(node) = &ty {
                nodes.push(node.clone());
            }

            ty
        });

        nodes
    }

    pub fn references_nodes(&self, mut f: impl FnMut(&NodeRef) -> bool) -> bool {
        let mut references_node = false;
        self.traverse(|ty| {
            let Type::Node(node) = &ty else {
                return ty;
            };

            if f(node) {
                references_node = true;
            }

            ty
        });

        references_node
    }
}

impl ConstructedType {
    pub fn traverse_children(&self, mut f: impl FnMut(Type) -> Type) -> Self {
        self.traverse_children_inner(&mut f, &mut Vec::new())
    }

    fn traverse_children_inner(
        &self,
        f: &mut impl FnMut(Type) -> Type,
        stack: &mut Vec<Type>,
    ) -> Self {
        let children = self
            .children
            .iter()
            .map(|child| child.traverse_inner(f, stack))
            .collect();

        ConstructedType {
            children,
            ..self.clone()
        }
    }
}
