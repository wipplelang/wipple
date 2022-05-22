mod grammar;
mod lexer;
mod span;

pub use grammar::*;
pub use lexer::*;
pub use span::*;

use crate::{diagnostics::*, Compiler, FilePath, Loader};

impl<L: Loader> Compiler<L> {
    pub fn parse(&mut self, file: FilePath, code: &str) -> Option<File> {
        let tokens = lex(code)
            .into_iter()
            .map(|(token, span)| (token, Span::new(file, span)))
            .collect::<Vec<_>>();

        let result = grammar::file(&tokens, file, code);

        match result {
            Ok(statements) => Some(statements),
            Err(error) => {
                let (token, span) = (error.location > 0)
                    .then(|| error.location)
                    .and_then(|location| tokens.get(location))
                    .map(|(token, span)| (Some(*token), *span))
                    .unwrap_or_else(|| (None, Span::new(file, 0..0)));

                self.diagnostics.add(Diagnostic::error(
                    "syntax error",
                    vec![Note::primary(
                        span,
                        format!(
                            "unexpected {}; expected {}",
                            token
                                .map(|token| token.to_string())
                                .as_deref()
                                .unwrap_or("token"),
                            error.expected
                        ),
                    )],
                ));

                None
            }
        }
    }
}
