mod item;
mod items;
mod writer;

pub use item::*;
pub use writer::*;

use std::sync::Arc;
use wipple_core::db::{Db, Node};
use wipple_queries::QueryCtx;

pub fn collect_feedback<'a>(
    db: &'a Db,
    node_filter: impl Fn(&Db, Node) -> bool + 'static,
    feedback_filter: impl FnMut(&FeedbackItem<'a>) -> bool,
) -> Vec<FeedbackItem<'a>> {
    let mut ctx = FeedbackCtx::new(Arc::new(node_filter));
    items::register(&mut ctx);

    let mut items = db
        .owned_nodes()
        .flat_map(|node| ctx.queries.iter().flat_map(move |query| query(db, node)))
        .filter(feedback_filter)
        .collect::<Vec<_>>();

    sort_feedback(db, &mut items);

    items
}

struct FeedbackCtx<'a> {
    queries: Vec<Box<dyn Fn(&'a Db, Node) -> Box<dyn Iterator<Item = FeedbackItem<'a>> + 'a> + 'a>>,
    filter: Arc<dyn Fn(&Db, Node) -> bool>,
}

impl<'a> FeedbackCtx<'a> {
    fn new(filter: Arc<dyn Fn(&Db, Node) -> bool>) -> Self {
        FeedbackCtx {
            queries: Vec::new(),
            filter,
        }
    }
}

trait FeedbackQueryItem<'a> {
    type Item: 'a;

    fn into_iter(self) -> Box<dyn Iterator<Item = Self::Item> + 'a>;
}

impl<'a, T: 'a> FeedbackQueryItem<'a> for Vec<T> {
    type Item = T;

    fn into_iter(self) -> Box<dyn Iterator<Item = Self::Item> + 'a> {
        Box::new(IntoIterator::into_iter(self))
    }
}

impl<'a, T: 'a> FeedbackQueryItem<'a> for Option<T> {
    type Item = T;

    fn into_iter(self) -> Box<dyn Iterator<Item = Self::Item> + 'a> {
        Box::new(IntoIterator::into_iter(self))
    }
}

impl<'a> FeedbackQueryItem<'a> for bool {
    type Item = ();

    fn into_iter(self) -> Box<dyn Iterator<Item = Self::Item> + 'a> {
        if self {
            Box::new(std::iter::once(()))
        } else {
            Box::new(std::iter::empty())
        }
    }
}

#[must_use]
struct FeedbackBuilder<'ctx, 'a, T: 'a> {
    ctx: &'ctx mut FeedbackCtx<'a>,
    id: String,
    query: Option<Box<dyn Fn(&QueryCtx<'a>, Node) -> Box<dyn Iterator<Item = T> + 'a>>>,
    rank: Option<fn(&T) -> FeedbackRank>,
    location: Option<fn(Node, &T) -> FeedbackLocation>,
    display: Option<fn(&Db, &mut FeedbackWriter<'_>, Node, &T)>,
    show_graph: bool,
}

impl<'a, T: 'a> FeedbackBuilder<'_, 'a, T> {
    fn query<I: FeedbackQueryItem<'a, Item = T>>(
        mut self,
        query: impl Fn(&QueryCtx<'a>, Node) -> I + 'static,
    ) -> Self {
        self.query = Some(Box::new(move |ctx, node| query(ctx, node).into_iter()));
        self
    }

    fn rank(mut self, rank: fn(&T) -> FeedbackRank) -> Self {
        self.rank = Some(rank);
        self
    }

    fn location(mut self, location: fn(Node, &T) -> FeedbackLocation) -> Self {
        self.location = Some(location);
        self
    }

    fn display(mut self, display: fn(&Db, &mut FeedbackWriter<'_>, Node, &T)) -> Self {
        self.display = Some(display);
        self
    }

    fn show_graph(mut self) -> Self {
        self.show_graph = true;
        self
    }

    fn register(self) {
        let id = self.id;
        let query = self.query.unwrap();
        let rank = self.rank.unwrap();
        let location = self.location;
        let display = self.display.unwrap();
        let filter = self.ctx.filter.clone();
        let show_graph = self.show_graph;

        self.ctx.queries.push(Box::new(move |db, node| {
            let ctx = QueryCtx::new(db, filter.clone());
            let items = query(&ctx, node);

            Box::new({
                let id = id.clone();
                let filter = filter.clone();
                items.map(move |item| FeedbackItem {
                    id: id.clone(),
                    rank: rank(&item),
                    location: match location {
                        Some(f) => f(node, &item),
                        None => FeedbackLocation::from(node),
                    },
                    display: Box::new({
                        let filter = filter.clone();
                        move |db, render_segment| {
                            let mut writer = FeedbackWriter::with_filter(filter.as_ref());
                            display(db, &mut writer, node, &item);
                            writer.finish(db, render_segment)
                        }
                    }),
                    show_graph,
                })
            })
        }))
    }
}

impl<'a> FeedbackCtx<'a> {
    fn feedback<T: 'a>(&mut self, id: impl Into<String>) -> FeedbackBuilder<'_, 'a, T> {
        FeedbackBuilder {
            ctx: self,
            id: id.into(),
            query: None,
            rank: None,
            location: None,
            display: None,
            show_graph: false,
        }
    }
}
