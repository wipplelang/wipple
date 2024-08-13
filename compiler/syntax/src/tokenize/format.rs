use crate::tokenize::Token;
use std::collections::VecDeque;

/// Format the output of [`tokenize`] to a string.
pub fn format<'a, 'src: 'a>(tokens: Vec<&'a Token<'src>>) -> String {
    // The formatted code is stored here.
    let mut formatted = String::new();

    // Rather than using an iterator, a VecDeque allows us to perform lookahead
    // without consuming tokens. This is done when determining whether to reset
    // the indentation of a line.
    let mut tokens = VecDeque::from(tokens);

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
        match &token {
            Token::LeftParenthesis => {
                if pad {
                    formatted.push(' ');
                }

                formatted.push('(');

                pad = false;
                first_in_group = true;

                if let Some(Token::LineBreak) = tokens.front() {
                    line_indents.push(LineIndent::Group);
                    groups.push(true);
                } else {
                    groups.push(false);
                }
            }
            Token::RightParenthesis => {
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
            Token::LeftBracket => {
                if pad {
                    formatted.push(' ');
                }

                formatted.push('[');

                pad = false;
                first_in_group = true;

                if let Some(Token::LineBreak) = tokens.front() {
                    line_indents.push(LineIndent::Group);
                    groups.push(true);
                } else {
                    groups.push(false);
                }
            }
            Token::RightBracket => {
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
            Token::LeftBrace => {
                if pad {
                    formatted.push(' ');
                }

                formatted.push('{');

                pad = false;
                first_in_group = true;

                if let Some(Token::LineBreak) = tokens.front() {
                    line_indents.push(LineIndent::Group);
                    groups.push(true);
                } else {
                    groups.push(false);
                }
            }
            Token::RightBrace => {
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
            Token::LineBreak => {
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
                let mut multiple = false;
                while tokens
                    .front()
                    .is_some_and(|token| matches!(token, Token::LineBreak))
                {
                    tokens.pop_front();
                    multiple = true;
                }

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

                while let Some(token) = tokens.front().copied() {
                    let c = match token {
                        Token::RightParenthesis => ')',
                        Token::RightBracket => ']',
                        Token::RightBrace => '}',
                        Token::Operator(_)
                        | Token::VariadicOperator(_)
                        | Token::NonAssociativeOperator(_) => {
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
                    match token {
                        Token::LineBreak => break,
                        Token::LeftParenthesis
                        | Token::LeftBracket
                        | Token::LeftBrace
                        | Token::Operator(_)
                        | Token::VariadicOperator(_)
                        | Token::NonAssociativeOperator(_) => {
                            reset = false;
                        }
                        _ => {
                            reset = true;
                        }
                    }
                }

                if reset {
                    while let Some(LineIndent::TrailingOperator) = line_indents.last() {
                        line_indents.pop();
                    }
                }
            }
            Token::Comment(comment) => {
                if pad || first_in_group {
                    formatted.push(' ');
                }

                formatted.push_str("--");
                formatted.push_str(comment);
            }
            Token::Keyword(keyword) => {
                if pad && !keyword.is_suffix() {
                    formatted.push(' ');
                }

                formatted.push_str(&keyword.to_string());
                pad = !keyword.is_prefix();
                first_in_group = false;
            }
            Token::Operator(operator) => {
                if pad {
                    formatted.push(' ');
                }

                formatted.push_str(&operator.to_string());

                pad = true;
                first_in_group = false;

                if let Some(Token::LineBreak) = tokens.front() {
                    line_indents.push(LineIndent::TrailingOperator);
                }
            }
            Token::VariadicOperator(operator) => {
                if pad {
                    formatted.push(' ');
                }

                formatted.push_str(&operator.to_string());

                pad = true;
                first_in_group = false;

                if let Some(Token::LineBreak) = tokens.front() {
                    line_indents.push(LineIndent::TrailingOperator);
                }
            }
            Token::NonAssociativeOperator(operator) => {
                if pad {
                    formatted.push(' ');
                }

                formatted.push_str(&operator.to_string());

                pad = true;
                first_in_group = false;

                if let Some(Token::LineBreak) = tokens.front() {
                    line_indents.push(LineIndent::TrailingOperator);
                }
            }
            Token::Name(name) => {
                if pad {
                    formatted.push(' ');
                }

                formatted.push_str(name);
                pad = true;
                first_in_group = false;
            }
            Token::Text(text) => {
                if pad {
                    formatted.push(' ');
                }

                formatted.push('"');
                formatted.push_str(text);
                formatted.push('"');
                pad = true;
                first_in_group = false;
            }
            Token::Number(number) => {
                if pad {
                    formatted.push(' ');
                }

                formatted.push_str(number);

                pad = true;
                first_in_group = false;
            }
        }
    }

    let mut formatted = formatted.trim().to_string();

    // Add a trailing newline
    formatted.push('\n');

    formatted
}
