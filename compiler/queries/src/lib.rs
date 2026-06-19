mod bound_queries;
mod comment_queries;
mod definition_queries;
mod highlight_queries;
mod missing_queries;
mod placeholder_queries;
mod syntax_queries;
mod type_queries;
mod unused_queries;

pub use bound_queries::*;
pub use comment_queries::*;
pub use definition_queries::*;
pub use highlight_queries::*;
pub use missing_queries::*;
pub use placeholder_queries::*;
pub use syntax_queries::*;
pub use type_queries::*;
pub use unused_queries::*;

use std::{ops::Deref, sync::Arc};
use wipple_core::db::{Db, Fact, Node};

#[derive(Clone)]
pub struct QueryCtx<'a> {
    db: &'a Db,
    filter: Arc<dyn Fn(&'a Db, Node) -> bool + 'a>,
}

impl<'a> QueryCtx<'a> {
    pub fn new(db: &'a Db, filter: Arc<dyn Fn(&'a Db, Node) -> bool + 'a>) -> Self {
        QueryCtx { db, filter }
    }

    pub fn filter(&self, node: Node) -> bool {
        (self.filter)(self.db, node)
    }
}

impl<'a> Deref for QueryCtx<'a> {
    type Target = &'a Db;

    fn deref(&self) -> &Self::Target {
        &self.db
    }
}

pub fn fact<'a, T: Fact>(db: &QueryCtx<'a>, node: Node) -> Option<&'a T> {
    db.get(node)
}
