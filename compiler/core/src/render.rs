use crate::{
    db::{Db, Node},
    facts::Syntax,
    span::Str,
    typecheck::{
        bounds::Instance,
        groups::{types_of, update_instance, update_type},
        ty::Ty,
    },
    util::Link,
};
use regex::Regex;
use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::Write,
    sync::LazyLock,
};

pub trait Render {
    fn render_into(&self, db: &Db, ctx: &mut RenderCtx) {
        let _ = db;
        let _ = ctx;
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RenderCtx {
    segments: Vec<RenderSegment>,
    nodes: BTreeSet<Node>,
}

#[derive(Debug, Clone, Copy)]
pub enum TyPlacement {
    InlineMultiple,
    InlineFirst,
    BoundMultiple,
    NestedFirst,
}

#[derive(Debug, Clone)]
pub struct Comments {
    pub nodes: Vec<Node>,
    pub comments: Vec<Str>,
    pub links: BTreeMap<Str, Link>,
}

#[derive(Default)]
pub struct ListBuilder<'a> {
    items: Vec<Box<dyn FnOnce(&mut RenderCtx) + 'a>>,
}

impl<'a> ListBuilder<'a> {
    pub fn add(&mut self, item: impl FnOnce(&mut RenderCtx) + 'a) {
        self.items.push(Box::new(item));
    }
}

impl RenderCtx {
    pub fn line_break(&mut self) {
        self.segments.push(RenderSegment::LineBreak);
    }

    pub fn string(&mut self, s: impl Into<String>) {
        let s = s.into();

        if s.is_empty() {
            return;
        }

        self.segments.push(RenderSegment::String(s));
    }

    pub fn code(&mut self, s: impl Into<String>) {
        self.segments.push(RenderSegment::Code(s.into()));
    }

    pub fn node(&mut self, node: Node) {
        self.segments.push(RenderSegment::Node(node));
        self.nodes.insert(node);
    }

    pub fn ty(&mut self, db: &Db, ty: &Ty, placement: TyPlacement) {
        let (root, multiple) = match placement {
            TyPlacement::InlineMultiple => (true, true),
            TyPlacement::InlineFirst => (true, false),
            TyPlacement::BoundMultiple => (false, true),
            TyPlacement::NestedFirst => (false, false),
        };

        if multiple && let Ty::Node(node) = ty {
            let tys = types_of(db, *node);

            if !tys.is_empty() {
                if !root && tys.len() > 1 {
                    self.string("(");
                }

                for (index, ty) in tys.iter().enumerate() {
                    if index > 0 {
                        self.string(" or ");
                    }

                    Ty::Constructed(ty.clone()).render_into(db, self, root);
                }

                if !root && tys.len() > 1 {
                    self.string(")");
                }

                return;
            }
        }

        let ty = update_type(db, ty, !multiple);
        ty.render_into(db, self, root);
    }

    pub fn instance(&mut self, db: &Db, instance: &Instance) {
        let mut instance = instance.clone();
        update_instance(db, &mut instance);
        instance.render_into(db, self);
    }

    pub fn link(&mut self, label: impl Into<String>, node: Node) {
        self.segments.push(RenderSegment::Link(label.into(), node));
        self.nodes.insert(node);
    }

    pub const LIST_LIMIT: usize = 3;

    pub fn list<'a>(&mut self, separator: &str, build: impl FnOnce(&mut ListBuilder<'a>)) {
        let mut builder = ListBuilder::default();
        build(&mut builder);
        let items = builder.items;

        let len = items.len();
        match len {
            3.. => {
                for (index, item) in items.into_iter().enumerate() {
                    if index >= Self::LIST_LIMIT {
                        let remaining = len - Self::LIST_LIMIT;
                        let trailing = if remaining == 1 { "other" } else { "others" };
                        self.string(format!(", {separator} {remaining} {trailing}"));
                        break;
                    }

                    if index == len - 1 {
                        self.string(format!(", {separator} "));
                    } else if index > 0 {
                        self.string(", ");
                    }

                    item(self);
                }
            }
            2 => {
                let mut items = items.into_iter();
                let first = items.next().unwrap();
                let second = items.next().unwrap();

                first(self);
                self.string(format!(" {separator} "));
                second(self);
            }
            1 => {
                let item = items.into_iter().next().unwrap();
                item(self);
            }
            0 => {}
        }
    }

    pub fn comments(&mut self, db: &Db, definition_node: Node, comments: &Comments) {
        static LINK_REGEX: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r"(?s)\[`([^`]+)`\]").unwrap());

        let mut links = HashMap::<String, Box<dyn Fn(&mut RenderCtx)>>::new();
        for (name, link) in &comments.links {
            links.insert(
                name.to_string(),
                Box::new(|ctx| {
                    if link.node == definition_node {
                        ctx.link(name.to_string(), link.node)
                    } else {
                        ctx.node(link.node)
                    }
                }),
            );

            links.insert(
                format!("{name}@related"),
                Box::new(|ctx| {
                    ctx.list("and", |list| {
                        for &node in &link.related {
                            list.add(move |ctx| ctx.node(node));
                        }
                    });
                }),
            );

            links.insert(
                format!("{name}@type"),
                Box::new(|writer| {
                    writer.list("or", |list| {
                        let Some(ty) = link.tys.first() else {
                            return;
                        };

                        let ty = Ty::Constructed(ty.clone());

                        list.add(move |writer| writer.ty(db, &ty, TyPlacement::InlineFirst));
                    });
                }),
            );
        }

        let comments_string = comments
            .comments
            .iter()
            .map(|comment| comment.trim())
            .collect::<Vec<_>>()
            .join("\n")
            .trim()
            .to_string();

        if comments_string.is_empty() {
            return;
        }

        let mut index = 0;
        for captures in LINK_REGEX.captures_iter(&comments_string) {
            let capture = captures.get(0).unwrap();

            self.string(&comments_string[index..capture.range().start]);
            index = capture.range().end;

            let mut ctx = RenderCtx::default();
            let name = captures.get(1).unwrap().as_str();
            match links.get(name) {
                Some(link) => link(&mut ctx),
                None => ctx.code("_"),
            }

            self.extend([ctx]);
        }

        self.string(&comments_string[index..]);
    }

    pub fn render(&mut self, db: &Db, render: &impl Render) {
        render.render_into(db, self);
    }

    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    pub fn finish<'a>(
        self,
        db: &'a Db,
        mut render_segment: impl FnMut(&Db, &RenderSegment) -> String + 'a,
    ) -> (String, BTreeSet<Node>) {
        let mut string = String::new();
        for segment in &self.segments {
            write!(string, "{}", render_segment(db, segment)).unwrap();
        }

        (string, self.nodes)
    }
}

impl Extend<Self> for RenderCtx {
    fn extend<T: IntoIterator<Item = Self>>(&mut self, iter: T) {
        for other in iter {
            self.segments.extend(other.segments);
            self.nodes.extend(other.nodes);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RenderSegment {
    LineBreak,
    String(String),
    Code(String),
    Node(Node),
    Link(String, Node),
}

impl RenderSegment {
    pub fn plain_text(&self, db: &Db) -> String {
        match self {
            RenderSegment::LineBreak => String::from("\n"),
            RenderSegment::String(s) => s.clone(),
            RenderSegment::Code(s) => s.clone(),
            RenderSegment::Node(node) => match db.get(*node) {
                Some(Syntax(syntax)) => format_source(&db.ast(syntax).span(db).source),
                None => String::from("_"),
            },
            RenderSegment::Link(label, _) => label.clone(),
        }
    }

    pub fn markdown(&self, db: &Db, show_span: bool) -> String {
        match self {
            RenderSegment::LineBreak => String::from("\n\n"),
            RenderSegment::String(s) => s.clone(),
            RenderSegment::Code(s) => format!("`{s}`"),
            RenderSegment::Node(node) => {
                let mut s = String::new();

                if let Some(Syntax(syntax)) = db.get(*node) {
                    let span = db.ast(syntax).span(db);

                    write!(s, "`{}`", format_source(&span.source)).unwrap();

                    if show_span {
                        write!(s, " ({span})").unwrap();
                    }
                } else {
                    write!(s, "`_`").unwrap();
                }

                if db.debug_enabled {
                    write!(s, " ({node:?})").unwrap();
                }

                s
            }
            RenderSegment::Link(label, node) => {
                let mut s = format!("`{label}`");

                if let Some(Syntax(syntax)) = db.get(*node) {
                    let span = db.ast(syntax).span(db);

                    if show_span {
                        write!(s, " ({span})").unwrap();
                    }
                }

                if db.debug_enabled {
                    write!(s, " ({node:?})").unwrap();
                }

                s
            }
        }
    }
}

fn format_source(source: &str) -> String {
    static BLOCK_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(?s)\{.*\n.*\}").unwrap());

    BLOCK_REGEX.replace_all(source, "{⋯}").to_string()
}
