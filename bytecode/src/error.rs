use crate::Span;
use std::{borrow::Cow, cmp::max, fmt};

#[derive(Debug, Clone)]
pub struct Error<'a> {
    pub message: String,
    pub label: String,
    pub span: Span<'a>,
}

impl<'a> Error<'a> {
    pub fn new(message: impl ToString, label: impl ToString, span: Span<'a>) -> Self {
        Error {
            message: message.to_string(),
            label: label.to_string(),
            span,
        }
    }
}

impl fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut err = chic::Error::new(&self.message);

        if let Some((line, start_col)) = self.span.start_line_col() {
            let start_col = start_col - 1;

            let end_col = match self.span.end_line_col() {
                Some((_, col)) => col - 1,
                None => start_col + 1,
            };

            let code = self
                .span
                .source
                .code
                .lines()
                .skip(max(line - 1, 0))
                .take(3)
                .collect::<Vec<_>>()
                .join("\n");

            err = err.error(line, start_col, end_col, code, &self.label);
        }

        let line_col = self
            .span
            .start_line_col()
            .map(|(line, col)| format!("{}:{}", line, col));

        let link = match (self.span.source.file, line_col) {
            (Some(file), Some(line_col)) => Some(Cow::Owned(format!(
                "{}:{}",
                file.to_string_lossy(),
                line_col
            ))),
            (Some(file), None) => Some(file.to_string_lossy()),
            (None, Some(line_col)) => Some(Cow::Owned(line_col)),
            (None, None) => None,
        };

        match link {
            Some(link) => write!(f, "{}: {}", link, err.to_string()),
            None => f.write_str(&err.to_string()),
        }
    }
}

impl std::error::Error for Error<'_> {}
