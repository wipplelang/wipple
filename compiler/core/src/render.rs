use crate::{
    db::{Db, Node},
    facts::Syntax,
    span::Str,
    typecheck::{groups::Prefer, ty::Ty},
    util::{Link, LinkKind},
    visit::definitions::Defined,
};
use regex::Regex;
use serde::{Deserialize, Serialize};
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

#[derive(Debug, Clone, Default)]
pub struct RenderOptions {
    pub relevant: Vec<Node>,
    pub prefer: Prefer,
    pub explain: ExplainOptions,
}

#[derive(Debug, Clone, Copy, Default)]
pub enum ExplainOptions {
    #[default]
    None,
    Enabled,
    Full,
}

#[derive(Debug, Default)]
pub struct RenderCtx {
    pub options: RenderOptions,
    segments: Vec<RenderSegment>,
    nodes: BTreeSet<Node>,
}

impl RenderCtx {
    pub fn with_options(options: RenderOptions) -> Self {
        RenderCtx {
            options,
            ..Default::default()
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Comments {
    pub node: Node,
    pub comments: Vec<Str>,
    pub links: BTreeMap<Str, Link>,
}

impl Comments {
    pub fn empty_for(node: Node) -> Self {
        Comments {
            node,
            comments: Default::default(),
            links: Default::default(),
        }
    }

    pub fn builtin(
        node: Node,
        comment: &'static str,
        links: impl IntoIterator<Item = (&'static str, Option<Link>)>,
    ) -> Self {
        Comments {
            node,
            comments: vec![Str::from(comment)],
            links: links
                .into_iter()
                .filter_map(|(name, link)| link.map(|link| (Str::from(name), link)))
                .collect(),
        }
    }
}

#[derive(Default)]
pub struct ListBuilder<'a, 'f> {
    _marker: std::marker::PhantomData<&'f ()>, // FIXME: Temporary
    items: Vec<Box<dyn FnOnce(&mut RenderCtx) + 'a>>,
}

impl<'a> ListBuilder<'a, '_> {
    pub fn add(&mut self, item: impl FnOnce(&mut RenderCtx) + 'a) {
        self.items.push(Box::new(item));
    }
}

impl RenderCtx {
    pub fn line_break(&mut self) {
        self.segments.push(RenderSegment::LineBreak);
    }

    pub fn resetting_relevant<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let prev = self.options.relevant.clone();
        self.options.relevant.clear();
        let result = f(self);
        self.options.relevant = prev;
        result
    }

    pub fn with_relevant<T>(
        &mut self,
        relevant: &[Node],
        prefer: Prefer,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let prev_relevant = self.options.relevant.clone();
        let prev_prefer = self.options.prefer;
        self.options.relevant = relevant.iter().chain(&prev_relevant).copied().collect();
        self.options.prefer = prefer;
        let result = f(self);
        self.options.relevant = prev_relevant;
        self.options.prefer = prev_prefer;
        result
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

    pub fn ty(&mut self, db: &Db, ty: &Ty, root: bool) {
        ty.render_into(db, self, root);
    }

    pub fn link(&mut self, label: impl Into<String>, node: Node) {
        self.segments.push(RenderSegment::Link(label.into(), node));
        self.nodes.insert(node);
    }

    pub fn hover_link(&mut self, node: Node) {
        self.segments.push(RenderSegment::HoverLink(node));
        self.nodes.insert(node);
    }

    pub const LIST_LIMIT: usize = 3;

    pub fn list<'a>(&mut self, separator: &str, build: impl FnOnce(&mut ListBuilder<'a, '_>)) {
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

    pub fn comments(&mut self, db: &Db, comments: &Comments) {
        static LINK_REGEX: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r"(?s)\[`([^`]+)`\]").unwrap());

        let mut links = HashMap::<String, Box<dyn Fn(&mut RenderCtx)>>::new();
        for (name, link) in &comments.links {
            links.insert(
                name.to_string(),
                Box::new(|ctx| match &link.kind {
                    LinkKind::Node(node) => ctx.node(*node),
                    LinkKind::Type(node) => ctx.ty(db, &Ty::Node(*node), true),
                    LinkKind::List { nodes, separator } => {
                        ctx.list(separator, |list| {
                            for &node in nodes {
                                list.add(move |ctx| ctx.node(node));
                            }
                        });
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
                Box::new(|writer| match &link.kind {
                    LinkKind::Node(node) | LinkKind::Type(node) => {
                        writer.ty(db, &Ty::Node(*node), true);
                    }
                    LinkKind::List { nodes, separator } => {
                        writer.list(separator, |list| {
                            for &node in nodes {
                                list.add(move |ctx| ctx.ty(db, &Ty::Node(node), true));
                            }
                        });
                    }
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

            let mut ctx = RenderCtx::with_options(self.options.clone());
            let name = captures.get(1).unwrap().as_str();
            match links.get(name) {
                Some(link) => link(&mut ctx),
                None => ctx.code(name.split_once('@').map_or(name, |(name, _)| name)),
            }

            self.extend([ctx]);
        }

        self.string(&comments_string[index..]);

        self.hover_link(comments.node);
    }

    pub fn render(&mut self, db: &Db, render: &impl Render) {
        render.render_into(db, self);
    }

    pub fn nodes(&self) -> impl Iterator<Item = Node> {
        self.nodes.iter().copied()
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
    HoverLink(Node),
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
            RenderSegment::HoverLink(_) => String::new(),
        }
    }
}

#[non_exhaustive]
#[derive(Debug, Clone, Copy, Default)]
pub struct RenderMarkdownOptions {
    pub rich: bool,
    pub color: bool,
    pub hover_links: bool,
}

impl RenderMarkdownOptions {
    pub fn rich(mut self, rich: bool) -> Self {
        self.rich = rich;
        self
    }

    pub fn color(mut self, color: bool) -> Self {
        self.color = color;
        self
    }

    pub fn hover_links(mut self, hover_links: bool) -> Self {
        self.hover_links = hover_links;
        self
    }
}

fn bold(s: &str) -> String {
    format!("\x1b[1m{s}\x1b[22m")
}

fn dim(s: &str) -> String {
    format!("\x1b[2m{s}\x1b[22m")
}

impl RenderSegment {
    pub fn markdown(&self, db: &Db, options: RenderMarkdownOptions) -> String {
        match self {
            RenderSegment::LineBreak => String::from("\n\n"),
            RenderSegment::String(s) => s.clone(),
            RenderSegment::Code(s) => {
                if options.color {
                    bold(s)
                } else {
                    format!("`{s}`")
                }
            }
            RenderSegment::Node(node) => {
                let mut s = String::new();

                if let Some(Syntax(syntax)) = db.get(*node) {
                    let span = db.ast(syntax).span(db);

                    let source = format_source(&span.source);

                    if options.color {
                        write!(s, "{}", bold(&source)).unwrap();
                    } else {
                        write!(s, "`{source}`").unwrap();
                    }

                    if !options.rich {
                        let span = format!(" ({span})");

                        if options.color {
                            write!(s, "{}", dim(&span)).unwrap();
                        } else {
                            write!(s, "{span}").unwrap();
                        }
                    }
                } else {
                    let placeholder = "_";

                    if options.color {
                        write!(s, "{}", dim(placeholder)).unwrap();
                    } else {
                        write!(s, "`{placeholder}`").unwrap();
                    }
                }

                if db.debug_enabled {
                    write!(s, " ({node:?})").unwrap();
                }

                s
            }
            RenderSegment::Link(label, node) => {
                let mut s = if options.color {
                    bold(label)
                } else {
                    format!("`{label}`")
                };

                if let Some(Syntax(syntax)) = db.get(*node) {
                    let span = db.ast(syntax).span(db);

                    if !options.rich {
                        let span = format!(" ({span})");

                        if options.color {
                            write!(s, "{}", dim(&span)).unwrap();
                        } else {
                            write!(s, "{span}").unwrap();
                        }
                    }
                }

                if db.debug_enabled {
                    write!(s, " ({node:?})").unwrap();
                }

                s
            }
            RenderSegment::HoverLink(node) => {
                let mut s = String::new();

                if options.hover_links
                    && let Some(Defined(definition)) = db.get(*node)
                    && let Some(span) = definition
                        .full_span()
                        .or_else(|| db.get(*node).map(|Syntax(syntax)| db.ast(syntax).span(db)))
                {
                    write!(s, "<wipple-hover-link>{}</wipple-hover-link>", span.source).unwrap();
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
