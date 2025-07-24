use crate::{syntax::tokenize::Token, util::WithInfo};

/// Prepare a token stream for parsing by removing comments and redundant line
/// breaks (including merging lines with leading and trailing operators).
pub fn to_logical_lines<'a, 'src: 'a>(
    tokens: impl IntoIterator<Item = WithInfo<Token<'src>>> + 'a,
) -> Vec<WithInfo<Token<'src>>> {
    let mut result = Vec::new();

    let mut tokens = tokens.into_iter().peekable();
    while let Some(token) = tokens.next() {
        match &token.item {
            Token::Comment(_) => {
                // Skip comments.
            }
            Token::Operator(_) | Token::VariadicOperator(_) | Token::NonAssociativeOperator(_) => {
                result.push(token);

                // Remove all line breaks between the operator and the next
                // token.
                while let Some(WithInfo {
                    item: Token::LineBreak,
                    ..
                }) = tokens.peek()
                {
                    tokens.next();
                }
            }
            Token::LineBreak => {
                // Remove all additional line breaks.
                while let Some(WithInfo {
                    item: Token::LineBreak,
                    ..
                }) = tokens.peek()
                {
                    tokens.next();
                }

                // Merge the next line with this line if it starts with an
                // operator. (`next_if` consumes the token if it matches.)
                if let Some(operator) = tokens.next_if(|token| {
                    matches!(
                        token.item,
                        Token::Operator(_)
                            | Token::VariadicOperator(_)
                            | Token::NonAssociativeOperator(_)
                    )
                }) {
                    result.push(operator);
                } else {
                    result.push(token);
                }

                // Again, remove all additional line breaks.
                while let Some(WithInfo {
                    item: Token::LineBreak,
                    ..
                }) = tokens.peek()
                {
                    tokens.next();
                }
            }
            _ => {
                // Preserve all other tokens.
                result.push(token);
            }
        }
    }

    // HACK: Insert a final line break so the parser always sees a new line
    // after every statement.
    if let Some(last) = result.last().cloned() {
        result.push(last.replace(Token::LineBreak));
    }

    result
}
