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

use wipple_core::db::{Db, Fact, Node};

pub fn fact<T: Fact>(db: &Db, node: Node) -> Option<&T> {
    db.get(node)
}
