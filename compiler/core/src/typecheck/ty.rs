use crate::{
    db::{Db, Node},
    render::{ExplainOptions, ListBuilder, Render, RenderCtx},
    typecheck::{
        groups::{Prefer, representative_types_of},
        instantiate::Instantiated,
    },
    visit::definitions::Defined,
};
use dyn_clone::DynClone;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Write};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
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

    pub fn display(&self, db: &Db, root: bool, relevant: &[Node], prefer: Prefer) -> String {
        let tys = match self {
            Ty::Node(node) => representative_types_of(db, *node, relevant, prefer),
            Ty::Constructed(ty) => vec![ty],
        };

        if tys.is_empty() {
            String::from("_")
        } else {
            let mut s = String::new();

            if !root && tys.len() > 1 {
                write!(s, "(").unwrap();
            }

            for (index, ty) in tys.iter().enumerate() {
                if index > 0 {
                    write!(s, " or ").unwrap();
                }

                let children = ty
                    .children
                    .iter()
                    .map(|&node| -> Box<dyn FnOnce(&Db, bool) -> String> {
                        let relevant = relevant.to_vec();
                        Box::new(move |db, root| {
                            Ty::Node(node).display(db, root, &relevant, prefer)
                        })
                    })
                    .collect::<Vec<_>>();

                write!(s, "{}", ty.display.display(db, children, root)).unwrap();
            }

            if !root && tys.len() > 1 {
                write!(s, ")").unwrap();
            }

            s
        }
    }

    pub fn render_into(&self, db: &Db, ctx: &mut RenderCtx, root: bool) {
        let description = self.display(db, root, &ctx.options.relevant, ctx.options.prefer);

        let node = match self {
            Ty::Node(node) => Some(*node),
            Ty::Constructed(ty) => ty.definition(),
        };

        if let Some(node) = node {
            ctx.link(description, node);
        } else {
            ctx.code(description);
        }
    }
}

impl Render for Ty {
    fn render_into(&self, db: &Db, ctx: &mut RenderCtx) {
        self.render_into(db, ctx, true);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
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
        children: Vec<Box<dyn FnOnce(&Db, bool) -> String>>,
        root: bool,
    ) -> String;

    fn should_render_consequences(
        &self,
        db: &Db,
        ctx: &mut RenderCtx,
        node: Node,
        ty: &ConstructedTy,
    ) -> bool {
        let _ = db;
        let _ = ctx;
        let _ = node;
        let _ = ty;

        true
    }

    fn render_consequences<'a>(
        &'a self,
        db: &'a Db,
        ctx: &mut RenderCtx,
        list: &mut ListBuilder<'a>,
        node: Node,
        ty: &'a ConstructedTy,
    ) {
        let _ = ctx;

        list.add(move |ctx| {
            ctx.node(node);
            ctx.string(" is a ");
            ctx.ty(db, &Ty::Constructed(ty.clone()), true);
        });
    }
}

dyn_clone::clone_trait_object!(TyDisplay);

#[derive(Clone, Serialize, Deserialize)]
pub struct ConstructedTy {
    pub tag: TyTag,
    pub children: Vec<Node>,
    display: Box<dyn TyDisplay>,
}

impl Debug for ConstructedTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ConstructedTy")
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

impl PartialOrd for ConstructedTy {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ConstructedTy {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.tag
            .cmp(&other.tag)
            .then_with(|| self.children.cmp(&other.children))
    }
}

impl ConstructedTy {
    fn new(tag: TyTag, children: Vec<Node>, display: impl TyDisplay) -> Self {
        ConstructedTy {
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

    pub fn should_render_consequences(&self, db: &Db, ctx: &mut RenderCtx, node: Node) -> bool {
        self.display.should_render_consequences(db, ctx, node, self)
    }

    pub fn render_consequences<'a>(
        &'a self,
        db: &'a Db,
        ctx: &mut RenderCtx,
        list: &mut ListBuilder<'a>,
        node: Node,
    ) {
        self.display.render_consequences(db, ctx, list, node, self);
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
        children: Vec<Box<dyn FnOnce(&Db, bool) -> String>>,
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
        children: Vec<Box<dyn FnOnce(&Db, bool) -> String>>,
        root: bool,
    ) -> String {
        let mut children = children.into_iter();
        let output = children.next().unwrap();
        let inputs = children;

        let mut result = String::new();
        for input in inputs {
            result.push_str(&input(db, false));
            result.push(' ');
        }

        result.push_str("-> ");
        result.push_str(&output(db, true));

        if root { result } else { format!("({result})") }
    }

    fn should_render_consequences(
        &self,
        db: &Db,
        ctx: &mut RenderCtx,
        _node: Node,
        ty: &ConstructedTy,
    ) -> bool {
        let (output, inputs) = ty.children.split_first().unwrap();

        if let [input] = inputs
            && should_render_child_in_consequences(db, ctx, *input)
        {
            return true;
        }

        if should_render_child_in_consequences(db, ctx, *output) {
            return true;
        }

        false
    }

    fn render_consequences<'a>(
        &self,
        db: &'a Db,
        ctx: &mut RenderCtx,
        list: &mut ListBuilder<'a>,
        node: Node,
        ty: &'a ConstructedTy,
    ) {
        let (output, inputs) = ty.children.split_first().unwrap();

        if let [input] = inputs
            && should_render_child_in_consequences(db, ctx, *input)
        {
            list.add(move |ctx| {
                ctx.node(node);
                ctx.string(" accepts a ");
                ctx.ty(db, &Ty::Node(*input), true);
            });
        }

        if should_render_child_in_consequences(db, ctx, *output) {
            list.add(move |ctx| {
                ctx.node(node);
                ctx.string(" returns a ");
                ctx.ty(db, &Ty::Node(*output), true);
            });
        }
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
        children: Vec<Box<dyn FnOnce(&Db, bool) -> String>>,
        _root: bool,
    ) -> String {
        match children.len() {
            0 => String::from("()"),
            1 => format!("({};)", children.into_iter().next().unwrap()(db, false)),
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
        children: Vec<Box<dyn FnOnce(&Db, bool) -> String>>,
        _root: bool,
    ) -> String {
        let output = children.into_iter().next().unwrap();
        format!("{{{}}}", output(db, true))
    }

    fn should_render_consequences(
        &self,
        db: &Db,
        ctx: &mut RenderCtx,
        _node: Node,
        ty: &ConstructedTy,
    ) -> bool {
        let output = ty.children.first().unwrap();

        should_render_child_in_consequences(db, ctx, *output)
    }

    fn render_consequences<'a>(
        &'a self,
        db: &'a Db,
        ctx: &mut RenderCtx,
        list: &mut ListBuilder<'a>,
        node: Node,
        ty: &'a ConstructedTy,
    ) {
        let output = ty.children.first().unwrap();

        if should_render_child_in_consequences(db, ctx, *output) {
            list.add(move |ctx| {
                ctx.node(node);
                ctx.string(" returns a ");
                ctx.ty(db, &Ty::Node(*output), true);
            });
        }
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
        _children: Vec<Box<dyn FnOnce(&Db, bool) -> String>>,
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

fn should_render_child_in_consequences(db: &Db, ctx: &RenderCtx, node: Node) -> bool {
    if db.is_hidden(node) || db.get::<Instantiated>(node).is_some() {
        return false;
    }

    if matches!(ctx.options.explain, ExplainOptions::Full) {
        return true;
    }

    let tys = representative_types_of(db, node, &[], Prefer::DirectType);
    tys.len() > 1
}
