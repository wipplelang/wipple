//! Parse Wipple source files.

/// The start and end location of an expression, in terms of byte offsets in the
/// source code.
#[derive(Debug, Clone, Copy)]
pub struct Location {
    /// The starting byte offset of the expression.
    pub start: usize,

    /// The ending byte offset of the expression.
    pub end: usize,
}

parser_expression_kind! {
    /// The kind of a parsed expression.
    pub ExpressionKind of Self;
}

/// A parsed expression.
#[derive(Debug, Clone)]
pub struct Expression {
    /// The expression's location in the source code.
    pub location: Location,

    /// The kind of expression.
    pub kind: ExpressionKind,
}
