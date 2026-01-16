use crate::syntax::{TokenKind, tokenize};
use std::collections::VecDeque;

/// Format source code.
pub fn format(source: &str) -> Option<String> {
    // The formatted code is stored here.
    let mut formatted = String::new();

    // Rather than using an iterator, a VecDeque allows us to perform lookahead
    // without consuming tokens. This is done when determining whether to reset
    // the indentation of a line.
    let mut tokens = VecDeque::from(tokenize("", source).ok()?);

    // We track indentation within groups of parentheses, brackets and braces
    // using a stack. Whenever we encounted an opening grouping symbol, we push
    // `true` (indicating the symbol comes immediately before a new line) or
    // `false` (it doesn't) to the stack. Closing grouping symbols pop from the
    // stack.
    let mut groups = Vec::<bool>::new();

    #[derive(Debug, Clone, Copy)]
    enum LineIndent {
        Group,
        TrailingOperator,
    }

    // Similarly, we track what caused a line to be indented. This is used to
    // reset the line indent only as far as needed. For example...
    //
    // ```wipple
    // {
    //   f :
    //     x ->
    //       42
    //
    //  f ()
    // }
    // ```
    //
    // ...would cause `LineIndent::Group` to be pushed once, and then
    // `LineIndent::TrailingOperator` to be pushed twice (for the definition of
    // `f`). Once the definition of `f` is over, we want to remove the
    // `TrailingOperator` indents, but not the `Group` indent, since we want the
    // call to `f` to remain indented within the block.
    //
    // You'll see this pattern several times below -- we remove
    // `TrailingOperator` indents until we encounter a `Group` indent, and then
    // remove a single `Group` indent.
    let mut line_indents = Vec::<LineIndent>::new();

    // This tracks whether to insert a space before a token. We add padding
    // after non-grouping symbols like names and numbers, but skip padding after
    // parentheses, brackets and braces. Padding is also only respected for non-
    // grouping symbols; we want code like `( x )` to be formatted as `(x)`, not
    // `(x )`.
    let mut pad = true;

    // This tracks whether the previous token was an opening grouping symbol for
    // the purpose of inserting a space before a comment. Even though we don't
    // insert a space in the case of `(x`, we always insert a space before
    // comments (ie. `( -- comment`). When we insert a comment, we check if
    // either `pad` or `first_in_group` are set.
    let mut first_in_group = false;

    // Iterate through the tokens, writing them to `formatted` one at a time.
    while let Some(token) = tokens.pop_front() {
        match &token.kind {
            TokenKind::LeftParenthesis => {
                if pad {
                    formatted.push(' ');
                }

                formatted.push('(');

                pad = false;
                first_in_group = true;

                if let Some(TokenKind::LineBreak) = tokens.front().map(|token| token.kind) {
                    line_indents.push(LineIndent::Group);
                    groups.push(true);
                } else {
                    groups.push(false);
                }
            }
            TokenKind::RightParenthesis => {
                formatted.push(')');

                pad = true;
                first_in_group = false;

                if matches!(groups.pop(), Some(true)) {
                    while let Some(LineIndent::TrailingOperator) = line_indents.last() {
                        line_indents.pop();
                    }

                    if matches!(line_indents.last(), Some(LineIndent::Group)) {
                        line_indents.pop();
                    }
                }
            }
            TokenKind::LeftBracket => {
                if pad {
                    formatted.push(' ');
                }

                formatted.push('[');

                pad = false;
                first_in_group = true;

                if let Some(TokenKind::LineBreak) = tokens.front().map(|token| token.kind) {
                    line_indents.push(LineIndent::Group);
                    groups.push(true);
                } else {
                    groups.push(false);
                }
            }
            TokenKind::RightBracket => {
                formatted.push(']');

                pad = true;
                first_in_group = false;

                if matches!(groups.pop(), Some(true)) {
                    while let Some(LineIndent::TrailingOperator) = line_indents.last() {
                        line_indents.pop();
                    }

                    if matches!(line_indents.last(), Some(LineIndent::Group)) {
                        line_indents.pop();
                    }
                }
            }
            TokenKind::LeftBrace => {
                if pad {
                    formatted.push(' ');
                }

                formatted.push('{');

                pad = false;
                first_in_group = true;

                if let Some(TokenKind::LineBreak) = tokens.front().map(|token| token.kind) {
                    line_indents.push(LineIndent::Group);
                    groups.push(true);
                } else {
                    groups.push(false);
                }
            }
            TokenKind::RightBrace => {
                formatted.push('}');

                pad = true;
                first_in_group = false;
                if matches!(groups.pop(), Some(true)) {
                    while let Some(LineIndent::TrailingOperator) = line_indents.last() {
                        line_indents.pop();
                    }

                    if matches!(line_indents.last(), Some(LineIndent::Group)) {
                        line_indents.pop();
                    }
                }
            }
            TokenKind::LineBreak => {
                // Collapse multiple line breaks into at most two line breaks.
                // For example:
                //
                // ```wipple
                // foo
                //
                //
                // bar
                // ```
                //
                // Is formatted as:
                //
                // ```wipple
                // foo
                //
                // bar
                // ```
                let multiple = token.value.len() > 1;

                // As long as we encounter closing grouping symbols before any
                // other tokens, we need to remove indentation. That way, code
                // like this:
                //
                // ```wipple
                // {
                //   x
                // }
                // ```
                //
                // Isn't formatted as:
                //
                // ```wipple
                // {
                //   x
                //   }
                // ```
                //
                // We store the closing symbols in a queue and write them to
                // `formatted` after removing indentation.
                let mut queue = String::new();

                // This is used to insert leading indentation if the first token
                // is an operator.
                let mut leading_indent = false;

                while let Some(token) = tokens.front() {
                    let c = match token.kind {
                        TokenKind::RightParenthesis => ')',
                        TokenKind::RightBracket => ']',
                        TokenKind::RightBrace => '}',
                        kind if kind.is_operator() => {
                            // Only set the leading indent if the operator is
                            // the first token on the line. Otherwise, code
                            // like:
                            //
                            // ```wipple
                            // {
                            // } -> {}
                            // ```
                            //
                            // Would insert a leading indent because of the
                            // `->` and be formatted as:
                            //
                            // ```wipple
                            // {
                            //   } -> {}
                            // ```
                            leading_indent = queue.is_empty();
                            break;
                        }
                        _ => {
                            // Stop on any other token and so it can be printed
                            // as normal.
                            break;
                        }
                    };

                    tokens.pop_front();
                    queue.push(c);

                    // Make sure to handle the standard resetting of
                    // grouping/indentation, since here, the most recent token
                    // was a closing grouping symbol.
                    if matches!(groups.pop(), Some(true)) {
                        while let Some(LineIndent::TrailingOperator) = line_indents.last() {
                            line_indents.pop();
                        }

                        if matches!(line_indents.last(), Some(LineIndent::Group)) {
                            line_indents.pop();
                        }
                    }
                }

                // Add a single extra new line if there are multiple line
                // breaks.
                if multiple {
                    formatted.push('\n');
                }

                let mut indent = line_indents.len();
                if leading_indent {
                    indent += 1;
                }

                formatted.push('\n');
                for _ in 0..indent {
                    // Wipple uses 2-space indents.
                    formatted.push_str("  ");
                }

                formatted.push_str(&queue);

                pad = !queue.is_empty();
                first_in_group = false;

                // Fially, we determine whether to reset the trailing operator
                // indent. We do this by looking ahead to the end of the next
                // line. The rules for resetting indentation are:
                //
                //   -  If the last token (on the following line) is an opening
                //      grouping symbol or operator, don't reset. In fact, we
                //      indent another level.
                //
                //   -  Any other token (eg. a name) causes a reset because
                //      the operator expression is over.
                let mut reset = !queue.is_empty();
                for token in &tokens {
                    if matches!(token.kind, TokenKind::LineBreak) {
                        break;
                    }

                    reset = !token.kind.is_opening() && !token.kind.is_operator();
                }

                if reset {
                    while let Some(LineIndent::TrailingOperator) = line_indents.last() {
                        line_indents.pop();
                    }
                }
            }
            TokenKind::Comment => {
                if pad || first_in_group {
                    formatted.push(' ');
                }

                formatted.push_str("--");
                formatted.push_str(&token.value);
            }
            kind if kind.is_keyword() => {
                if pad {
                    formatted.push(' ');
                }

                formatted.push_str(&token.value);
                pad = true;
                first_in_group = false;
            }
            kind if kind.is_binary_operator() => {
                if pad {
                    formatted.push(' ');
                }

                formatted.push_str(&token.value);

                pad = true;
                first_in_group = false;

                if let Some(TokenKind::LineBreak) = tokens.front().map(|token| token.kind) {
                    line_indents.push(LineIndent::TrailingOperator);
                }
            }
            kind if kind.is_variadic_operator() => {
                formatted.push_str(&token.value);

                pad = true;
                first_in_group = false;

                if let Some(TokenKind::LineBreak) = tokens.front().map(|token| token.kind) {
                    line_indents.push(LineIndent::TrailingOperator);
                }
            }
            kind if kind.is_non_associative_operator() => {
                if pad {
                    formatted.push(' ');
                }

                formatted.push_str(&token.value);

                pad = true;
                first_in_group = false;

                if let Some(TokenKind::LineBreak) = tokens.front().map(|token| token.kind) {
                    line_indents.push(LineIndent::TrailingOperator);
                }
            }
            TokenKind::LowercaseName | TokenKind::CapitalName => {
                if pad {
                    formatted.push(' ');
                }

                formatted.push_str(&token.value);
                pad = true;
                first_in_group = false;
            }
            TokenKind::String => {
                if pad {
                    formatted.push(' ');
                }

                let quote = if token.value.contains('"') { '\'' } else { '"' };

                formatted.push(quote);
                formatted.push_str(&token.value);
                formatted.push(quote);
                pad = true;
                first_in_group = false;
            }
            TokenKind::Number => {
                if pad {
                    formatted.push(' ');
                }

                formatted.push_str(&token.value);

                pad = true;
                first_in_group = false;
            }
            _ => unreachable!("{token:?}"),
        }
    }

    Some(formatted.trim().to_string())
}
