use crate::writer::Feedback;
use std::collections::BTreeSet;
use wipple_core::{
    db::{Db, Node},
    facts::Syntax,
    render::RenderSegment,
};

pub struct FeedbackItem<'a> {
    pub id: String,
    pub rank: FeedbackRank,
    pub location: FeedbackLocation,
    pub display: Box<dyn Fn(&Db, &mut dyn FnMut(&Db, &RenderSegment) -> String) -> Feedback + 'a>,
    pub show_graph: bool,
}

impl FeedbackItem<'_> {
    pub fn display(
        &self,
        db: &Db,
        mut render_segment: impl FnMut(&Db, &RenderSegment) -> String,
    ) -> Feedback {
        (self.display)(db, &mut render_segment)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FeedbackRank {
    Syntax,
    Names,
    Custom,
    DirectConflicts,
    IndirectConflicts,
    Bounds,
    CustomDefault,
    Exhaustiveness,
    Unknown,
    Placeholders,
}

#[derive(Debug, Clone)]
pub struct FeedbackLocation {
    pub primary: Node,
    pub secondary: BTreeSet<Node>,
}

impl From<Node> for FeedbackLocation {
    fn from(node: Node) -> Self {
        FeedbackLocation {
            primary: node,
            secondary: BTreeSet::new(),
        }
    }
}

pub fn sort_feedback(db: &Db, items: &mut Vec<FeedbackItem<'_>>) {
    // Reduce noise by preferring lower-rank items
    let min_rank = items.iter().map(|item| item.rank).min();

    if let Some(min_rank) = min_rank {
        items.retain(|item| item.rank == min_rank);
    }

    items.sort_by_key(|item| {
        db.get(item.location.primary)
            .map(|Syntax(syntax)| db.ast(syntax).span(db))
    });
}
