use crate::{tokenize::Token, Driver};
use wipple_util::WithInfo;

pub fn to_logical_lines<'a, 'src: 'a, D: Driver>(
    _driver: &'a D,
    tokens: impl IntoIterator<Item = WithInfo<D::Info, Token<'src>>> + 'a,
) -> Vec<WithInfo<D::Info, Token<'src>>> {
    let mut result = Vec::new();

    let mut tokens = tokens.into_iter().peekable();
    while let Some(token) = tokens.next() {
        match &token.item {
            Token::Comment(_) => {}
            Token::Operator(_) | Token::VariadicOperator(_) | Token::NonAssociativeOperator(_) => {
                result.push(token);

                while let Some(WithInfo {
                    item: Token::LineBreak,
                    ..
                }) = tokens.peek()
                {
                    tokens.next();
                }
            }
            Token::LineBreak => {
                while let Some(WithInfo {
                    item: Token::LineBreak,
                    ..
                }) = tokens.peek()
                {
                    tokens.next();
                }

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

                while let Some(WithInfo {
                    item: Token::LineBreak,
                    ..
                }) = tokens.peek()
                {
                    tokens.next();
                }
            }
            _ => {
                result.push(token);
            }
        }
    }

    // HACK: Allow `TokenTree::from_top_level` to parse the last line
    if let Some(last) = result.last().cloned() {
        result.push(last.replace(Token::LineBreak));
    }

    result
}
