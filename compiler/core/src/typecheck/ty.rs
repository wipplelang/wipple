use crate::{
    db::{Db, Node},
    render::{Render, RenderCtx},
    typecheck::groups::types_of,
    visit::definitions::Defined,
};
use dyn_clone::DynClone;
use serde::{Deserialize, Serialize};
use std::{
    fmt::{Debug, Write},
    slice,
};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Ty {
    Node(Node),
    Constructed(ConstructedTy),
}

impl Ty {
    pub fn node(&self) -> Option<Node> {
        match self {
            Ty::Node(node) => Some(*node),
            Ty::Constructed(_) => None,
        }
    }

    pub fn referenced_nodes(&self) -> Vec<Node> {
        match self {
            Ty::Node(node) => vec![*node],
            Ty::Constructed(ty) => ty.children.clone(),
        }
    }

    pub fn referenced_nodes_mut(&mut self) -> Vec<&mut Node> {
        match self {
            Ty::Node(node) => vec![node],
            Ty::Constructed(ty) => ty.children.iter_mut().collect(),
        }
    }
    pub fn display(&self, db: &Db, root: bool) -> String {
        let render_unknown = || String::from("_");

        let render_children = |children: &[Node]| {
            children
                .iter()
                .map(|&node| -> Box<dyn Fn(&Db, bool) -> String> {
                    Box::new(move |db, root| Ty::Node(node).display(db, root))
                })
                .collect()
        };

        let tys = match self {
            Ty::Node(node) => types_of(db, *node),
            Ty::Constructed(ty) => slice::from_ref(ty),
        };

        if tys.is_empty() {
            render_unknown()
        } else {
            let mut s = String::new();

            if !root && tys.len() > 1 {
                write!(s, "(").unwrap();
            }

            for (index, ty) in tys.iter().enumerate() {
                if index > 0 {
                    write!(s, " or ").unwrap();
                }

                let children = render_children(&ty.children);

                write!(s, "{}", ty.display.display(db, children, root)).unwrap();
            }

            if !root && tys.len() > 1 {
                write!(s, ")").unwrap();
            }

            s
        }
    }

    pub fn render_into(&self, db: &Db, ctx: &mut RenderCtx<'_>, root: bool) {
        let description = self.display(db, root);

        let node = match self {
            Ty::Node(node) => Some(*node),
            Ty::Constructed(ty) => ty.representative.or_else(|| ty.definition()),
        };

        if let Some(node) = node {
            ctx.link(description, node);
        } else {
            ctx.code(description);
        }
    }
}

impl Render for Ty {
    fn render_into(&self, db: &Db, ctx: &mut RenderCtx<'_>) {
        self.render_into(db, ctx, true);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TyTag {
    Named(Node),
    Function,
    Tuple,
    Block,
    Parameter(Node),
}

#[typetag::serde]
trait TyDisplay: Debug + DynClone + Send + Sync + 'static {
    fn display(
        &self,
        db: &Db,
        children: Vec<Box<dyn Fn(&Db, bool) -> String>>,
        root: bool,
    ) -> String;
}

dyn_clone::clone_trait_object!(TyDisplay);

#[derive(Clone, Serialize, Deserialize)]
pub struct ConstructedTy {
    pub representative: Option<Node>,
    pub tag: TyTag,
    pub children: Vec<Node>,
    display: Box<dyn TyDisplay>,
}

impl Debug for ConstructedTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ConstructedTy")
            .field("representative", &self.representative)
            .field("tag", &self.tag)
            .field("children", &self.children)
            .finish()
    }
}

impl PartialEq for ConstructedTy {
    fn eq(&self, other: &Self) -> bool {
        self.tag == other.tag && self.children == other.children
    }
}

impl Eq for ConstructedTy {}

impl ConstructedTy {
    fn new(tag: TyTag, children: Vec<Node>, display: impl TyDisplay) -> Self {
        ConstructedTy {
            representative: None,
            tag,
            children,
            display: Box::new(display),
        }
    }

    pub fn definition(&self) -> Option<Node> {
        match self.tag {
            TyTag::Named(node) | TyTag::Parameter(node) => Some(node),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct NamedTyDisplay {
    definition: Node,
}

#[typetag::serde]
impl TyDisplay for NamedTyDisplay {
    fn display(
        &self,
        db: &Db,
        children: Vec<Box<dyn Fn(&Db, bool) -> String>>,
        root: bool,
    ) -> String {
        let wrap = !root && !children.is_empty();

        let ty_name = db
            .get::<Defined>(self.definition)
            .unwrap()
            .0
            .name()
            .unwrap()
            .to_string();

        let mut result = ty_name;
        for child in children {
            result.push(' ');
            result.push_str(&child(db, false));
        }

        if wrap { format!("({result})") } else { result }
    }
}

impl ConstructedTy {
    pub fn named(definition: Node, parameters: Vec<Node>) -> Self {
        ConstructedTy::new(
            TyTag::Named(definition),
            parameters,
            NamedTyDisplay { definition },
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct FunctionTyDisplay;

#[typetag::serde]
impl TyDisplay for FunctionTyDisplay {
    fn display(
        &self,
        db: &Db,
        children: Vec<Box<dyn Fn(&Db, bool) -> String>>,
        root: bool,
    ) -> String {
        let (output, inputs) = children.split_first().unwrap();

        let mut result = String::new();
        for input in inputs {
            result.push_str(&input(db, false));
            result.push(' ');
        }

        result.push_str("-> ");
        result.push_str(&output(db, true));

        if root { result } else { format!("({result})") }
    }
}

impl ConstructedTy {
    pub fn function(inputs: Vec<Node>, output: Node) -> Self {
        ConstructedTy::new(
            TyTag::Function,
            [output].into_iter().chain(inputs).collect(),
            FunctionTyDisplay,
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct TupleTyDisplay;

#[typetag::serde]
impl TyDisplay for TupleTyDisplay {
    fn display(
        &self,
        db: &Db,
        children: Vec<Box<dyn Fn(&Db, bool) -> String>>,
        _root: bool,
    ) -> String {
        match children.len() {
            0 => String::from("()"),
            1 => format!("({};)", children.first().unwrap()(db, false)),
            _ => {
                let mut result = String::from("(");

                for (index, child) in children.into_iter().enumerate() {
                    if index > 0 {
                        result.push_str("; ");
                    }

                    result.push_str(&child(db, true));
                }

                result.push(')');
                result
            }
        }
    }
}

impl ConstructedTy {
    pub fn tuple(elements: Vec<Node>) -> Self {
        ConstructedTy::new(TyTag::Tuple, elements, TupleTyDisplay)
    }

    pub fn unit() -> Self {
        ConstructedTy::tuple(Vec::new())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct BlockTyDisplay;

#[typetag::serde]
impl TyDisplay for BlockTyDisplay {
    fn display(
        &self,
        db: &Db,
        children: Vec<Box<dyn Fn(&Db, bool) -> String>>,
        _root: bool,
    ) -> String {
        let output = children.first().unwrap();
        format!("{{{}}}", output(db, true))
    }
}

impl ConstructedTy {
    pub fn block(output: Node) -> Self {
        ConstructedTy::new(TyTag::Block, vec![output], BlockTyDisplay)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ParameterTyDisplay {
    definition: Node,
}

#[typetag::serde]
impl TyDisplay for ParameterTyDisplay {
    fn display(
        &self,
        db: &Db,
        _children: Vec<Box<dyn Fn(&Db, bool) -> String>>,
        _root: bool,
    ) -> String {
        db.get::<Defined>(self.definition)
            .unwrap()
            .0
            .name()
            .unwrap()
            .to_string()
    }
}

impl ConstructedTy {
    pub fn parameter(definition: Node) -> Self {
        ConstructedTy::new(
            TyTag::Parameter(definition),
            Vec::new(),
            ParameterTyDisplay { definition },
        )
    }
}
