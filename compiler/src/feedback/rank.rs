use crate::{database::Db, feedback::FeedbackItem};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FeedbackRank {
    Syntax,
    Names,
    Custom,
    Conflicts,
    Bounds,
    Unknown,
    Placeholders,
}

pub fn sort_by_rank(db: &Db, items: &mut Vec<FeedbackItem>) {
    // Reduce noise by preferring lower-rank items
    let min_rank = items.iter().map(|item| item.rank).min();

    if let Some(min_rank) = min_rank {
        items.retain(|item| item.rank == min_rank);
    }

    items.sort_by_key(|item| {
        let (node, _) = &item.location;
        db.span(node)
    });
}
