use crate::{Driver, WithInfo};
use rustc_lexer::unescape::EscapeError;
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use wipple_util::DefaultFromInfo;

pub struct ParseFormatExpressionResult<D: Driver, T> {
    pub segments: Vec<crate::FormatSegment<WithInfo<D::Info, T>>>,
    pub trailing: String,
}

pub(crate) fn parse_format_expression<D: Driver, T: DefaultFromInfo<D::Info>>(
    raw: WithInfo<D::Info, &str>,
    mut inputs: Vec<WithInfo<D::Info, T>>,
    errors: &mut Vec<WithInfo<D::Info, crate::Diagnostic>>,
) -> ParseFormatExpressionResult<D, T> {
    let mut string = String::with_capacity(raw.item.len());
    let mut escaped_underscores = Vec::new();

    rustc_lexer::unescape::unescape_str(raw.item, &mut |range, result| match result {
        Ok(ch) => string.push(ch),
        Err(rustc_lexer::unescape::EscapeError::InvalidEscape)
            if &raw.item[range.clone()] == r"\_" =>
        {
            escaped_underscores.push(string.len() as u32);
            string.push('_');
        }
        Err(e) => errors.push(WithInfo {
            info: raw.info.clone(),
            item: crate::Diagnostic::InvalidTextLiteral(TextLiteralError {
                start: range.start as u32,
                end: range.end as u32,
                error: message_from_rustc(&e).to_string(),
            }),
        }),
    });

    let mut segments = vec![String::new()];
    if !string.is_empty() {
        let is_placeholder =
            |index, ch| ch == '_' && escaped_underscores.binary_search(&(index as u32)).is_err();

        for (index, ch) in string.char_indices() {
            if is_placeholder(index, ch) {
                segments.push(String::new());
            } else if let Some(segment) = segments.last_mut() {
                segment.push(ch);
            } else {
                segments.push(String::from(ch));
            }
        }
    }

    let trailing = segments.pop().unwrap_or_default();

    if segments.len() != inputs.len() {
        errors.push(WithInfo {
            info: raw.info.clone(),
            item: crate::Diagnostic::InvalidPlaceholderText {
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
        .map(|(text, value)| crate::FormatSegment { text, value })
        .collect();

    ParseFormatExpressionResult { segments, trailing }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, TS)]
#[ts(export, rename = "syntax_TextLiteralError")]
pub struct TextLiteralError {
    pub start: u32,
    pub end: u32,
    pub error: String,
}

const fn message_from_rustc(error: &EscapeError) -> &'static str {
    match error {
        EscapeError::ZeroChars => "no characters provided",
        EscapeError::MoreThanOneChar => "more than one character provided",
        EscapeError::LoneSlash => "a lone backslash is not a valid escape sequence",
        EscapeError::InvalidEscape => "invalid escape sequence",
        EscapeError::BareCarriageReturn => "bare carriage return is not allowed",
        EscapeError::BareCarriageReturnInRawString => {
            "bare carriage return in raw string is not allowed"
        }
        EscapeError::EscapeOnlyChar => "only an escape character is not valid",
        EscapeError::TooShortHexEscape => "hex escape sequence too short",
        EscapeError::InvalidCharInHexEscape => "invalid character in hexadecimal escape sequence",
        EscapeError::OutOfRangeHexEscape => "hexadecimal escape out of range",
        EscapeError::NoBraceInUnicodeEscape => "missing braces in Unicode escape",
        EscapeError::InvalidCharInUnicodeEscape => "invalid character in Unicode escape sequence",
        EscapeError::EmptyUnicodeEscape => "empty Unicode escape sequence",
        EscapeError::UnclosedUnicodeEscape => "unclosed Unicode escape sequence",
        EscapeError::LeadingUnderscoreUnicodeEscape => {
            "leading underscore in Unicode escape sequence"
        }
        EscapeError::OverlongUnicodeEscape => "overlong Unicode escape sequence",
        EscapeError::LoneSurrogateUnicodeEscape => "lone surrogate in Unicode escape sequence",
        EscapeError::OutOfRangeUnicodeEscape => "Unicode escape sequence out of range",
        EscapeError::UnicodeEscapeInByte => "Unicode escape in byte string is not allowed",
        EscapeError::NonAsciiCharInByte => "non-ASCII character in byte literal is not allowed",
        EscapeError::NonAsciiCharInByteString => {
            "non-ASCII character in byte string is not allowed"
        }
    }
}
