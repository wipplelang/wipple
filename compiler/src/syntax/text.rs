use crate::{syntax::WithInfo, util::DefaultFromInfo};

pub struct ParseFormatExpressionResult<T> {
    pub segments: Vec<crate::syntax::FormatSegment<WithInfo<T>>>,
    pub trailing: String,
}

pub(crate) fn parse_format_expression<T: DefaultFromInfo>(
    raw: WithInfo<&str>,
    mut inputs: Vec<WithInfo<T>>,
    errors: &mut Vec<WithInfo<crate::syntax::Diagnostic>>,
) -> ParseFormatExpressionResult<T> {
    let mut segments = vec![String::new()];
    for ch in raw.item.chars() {
        if ch == '\\' {
            continue;
        } else if ch == '_' {
            segments.push(String::new());
        } else if let Some(segment) = segments.last_mut() {
            segment.push(ch);
        } else {
            segments.push(String::from(ch));
        }
    }

    let trailing = segments.pop().unwrap_or_default();

    if segments.len() != inputs.len() {
        errors.push(WithInfo {
            info: raw.info.clone(),
            item: crate::syntax::Diagnostic::InvalidPlaceholderText {
                expected: segments.len() as u32,
                found: inputs.len() as u32,
            },
        });
    }

    for _ in 0..segments.len().saturating_sub(inputs.len()) {
        inputs.push(T::default_from_info(raw.info.clone()));
    }

    let segments = segments
        .into_iter()
        .zip(inputs)
        .map(|(text, value)| crate::syntax::FormatSegment { text, value })
        .collect();

    ParseFormatExpressionResult { segments, trailing }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TextLiteralError {
    pub start: u32,
    pub end: u32,
    pub error: String,
}
