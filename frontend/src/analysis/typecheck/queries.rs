use crate::{
    analysis::{Expression, ExpressionKind},
    ExpressionId,
};

impl Expression {
    /// Treating `self` as the root expression, find the expression with the
    /// provided ID.
    // TODO: This is probably very slow, maybe store the expressions in a map
    // instead of a tree?
    pub fn as_root_query(&self, id: ExpressionId) -> Option<&Expression> {
        let mut expr = None;
        self.traverse(|e| {
            if e.id == id {
                expr = Some(e);
            }
        });

        expr
    }

    /// Treating `self` as the root expression, find the parent of the
    /// expression with the provided ID.
    pub fn as_root_query_parent_of(&self, id: ExpressionId) -> Option<&Expression> {
        let mut parent = None;
        self.traverse_with_parent(|expr, p| {
            if expr.id == id {
                parent = p;
            }
        });

        parent
    }

    pub fn as_root_query_start_of_call_chain(&self, id: ExpressionId) -> Option<&Expression> {
        let mut parent = self.as_root_query_parent_of(id)?;
        while let ExpressionKind::Call(_, _, first) = &parent.kind {
            if *first {
                return Some(parent);
            }

            parent = self.as_root_query_parent_of(parent.id)?;
        }

        None
    }
}
