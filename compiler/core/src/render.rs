use crate::{
    db::{Db, Node},
    facts::Syntax,
    typecheck::{
        bounds::Instance,
        groups::{update_instance, update_type},
        ty::Ty,
    },
};
use regex::Regex;
use std::{collections::BTreeSet, fmt::Write, sync::LazyLock};

pub trait Render {
    fn render_into(&self, db: &Db, ctx: &mut RenderCtx) {
        let _ = db;
        let _ = ctx;
    }
}

#[derive(Default)]
pub struct RenderCtx {
    segments: Vec<RenderSegment>,
    nodes: BTreeSet<Node>,
}

impl RenderCtx {
    pub fn line_break(&mut self) {
        self.segments.push(RenderSegment::LineBreak);
    }

    pub fn string(&mut self, s: impl Into<String>) {
        self.segments.push(RenderSegment::String(s.into()));
    }

    pub fn code(&mut self, s: impl Into<String>) {
        self.segments.push(RenderSegment::Code(s.into()));
    }

    pub fn node(&mut self, node: Node) {
        self.segments.push(RenderSegment::Node(node));
        self.nodes.insert(node);
    }

    pub fn ty(&mut self, db: &Db, ty: &Ty, root: bool) {
        let ty = update_type(db, ty);
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
