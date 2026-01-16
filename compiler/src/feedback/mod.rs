mod items;
mod rank;
mod writer;

pub use rank::*;
pub use writer::*;

use crate::{
    database::{Db, NodeRef},
    queries::QueryCtx,
};
use std::{collections::BTreeSet, fmt, rc::Rc};

#[derive(Clone)]
pub struct FeedbackItem {
    pub id: &'static str,
    pub rank: FeedbackRank,
    pub show_graph: bool,
    pub location: (NodeRef, BTreeSet<NodeRef>),
    pub write: Rc<dyn Fn(&Db, &mut dyn fmt::Write) -> BTreeSet<NodeRef>>,
}

pub fn collect_feedback(db: &Db, filter: impl Fn(&FeedbackItem) -> bool) -> Vec<FeedbackItem> {
    let ctx = FeedbackCtx::new();

    let mut items = Vec::new();
    for node in db.iter_nodes() {
        let query_ctx = QueryCtx { db, node };

        for query in &ctx.queries {
            query(&query_ctx, &mut |item| {
                if filter(&item) {
                    items.push(item);
                }
            });
        }
    }

    sort_by_rank(db, &mut items);

    items
}

struct RegisteredFeedback<T> {
    pub id: &'static str,
    pub rank: FeedbackRank,
    pub show_graph: bool,
    pub query: Rc<dyn Fn(&QueryCtx<'_>, &mut dyn FnMut(T))>,
    pub location: Rc<dyn Fn(&T) -> (NodeRef, BTreeSet<NodeRef>)>,
    pub write: Rc<dyn Fn(&mut FeedbackWriter<'_>, &T)>,
}

impl<T> RegisteredFeedback<T> {
    pub fn new(
        id: &'static str,
        rank: FeedbackRank,
        query: impl Fn(&QueryCtx<'_>, &mut dyn FnMut(T)) + 'static,
        location: impl Fn(&T) -> (NodeRef, BTreeSet<NodeRef>) + 'static,
        write: impl Fn(&mut FeedbackWriter<'_>, &T) + 'static,
    ) -> Self {
        RegisteredFeedback {
            id,
            rank,
            show_graph: false,
            query: Rc::new(query),
            location: Rc::new(location),
            write: Rc::new(write),
        }
    }

    pub fn show_graph(mut self) -> Self {
        self.show_graph = true;
        self
    }
}

struct FeedbackCtx {
    queries: Vec<Rc<dyn Fn(&QueryCtx<'_>, &mut dyn FnMut(FeedbackItem))>>,
}

impl FeedbackCtx {
    pub fn new() -> Self {
        let mut ctx = FeedbackCtx {
            queries: Vec::new(),
        };

        items::register(&mut ctx);

        ctx
    }

    pub fn register<T: 'static>(&mut self, entry: RegisteredFeedback<T>) {
        self.queries.push(Rc::new(move |ctx, f| {
            (entry.query)(ctx, &mut |data| {
                let location = (entry.location)(&data);

                let write: Rc<dyn Fn(&Db, &mut dyn fmt::Write) -> _> = Rc::new({
                    let write = entry.write.clone();
                    move |db, w| {
                        let mut writer = FeedbackWriter::new(db, w);
                        (write)(&mut writer, &data);
                        writer.finish()
                    }
                });

                f(FeedbackItem {
                    id: entry.id,
                    rank: entry.rank,
                    show_graph: entry.show_graph,
                    location,
                    write,
                });
            })
        }));
    }
}
