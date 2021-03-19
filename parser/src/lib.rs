mod convert;
mod lexer;
mod parser;

pub use convert::*;
pub use lexer::*;
pub use parser::*;

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    fn test_lexer(code: &str, expected: &[(Token, SourceLocation)]) {
        let (tokens, lookup) = lex(code);

        let tokens = tokens
            .into_iter()
            .map(|(token, offset)| (token, SourceLocation::new(&offset, &lookup)))
            .collect::<Vec<_>>();

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test1() {
        test_lexer(
            "foo : 42",
            &[
                (
                    Name(String::from("foo")),
                    SourceLocation { line: 1, column: 1 },
                ),
                (
                    Name(String::from(":")),
                    SourceLocation { line: 1, column: 5 },
                ),
                (
                    Number("42".parse().unwrap()),
                    SourceLocation { line: 1, column: 7 },
                ),
            ],
        )
    }

    #[test]
    fn test2() {
        test_lexer(
            r"a : 1

{
    a : 2
}

show a",
            &[
                (
                    Name(String::from("a")),
                    SourceLocation { line: 1, column: 1 },
                ),
                (
                    Name(String::from(":")),
                    SourceLocation { line: 1, column: 3 },
                ),
                (
                    Number("1".parse().unwrap()),
                    SourceLocation { line: 1, column: 5 },
                ),
                (Newline, SourceLocation { line: 1, column: 6 }),
                (Newline, SourceLocation { line: 2, column: 1 }),
                (OpenBrace, SourceLocation { line: 3, column: 1 }),
                (Newline, SourceLocation { line: 3, column: 2 }),
                (
                    Name(String::from("a")),
                    SourceLocation { line: 4, column: 5 },
                ),
                (
                    Name(String::from(":")),
                    SourceLocation { line: 4, column: 7 },
                ),
                (
                    Number("2".parse().unwrap()),
                    SourceLocation { line: 4, column: 9 },
                ),
                (
                    Newline,
                    SourceLocation {
                        line: 4,
                        column: 10,
                    },
                ),
                (CloseBrace, SourceLocation { line: 5, column: 1 }),
                (Newline, SourceLocation { line: 5, column: 2 }),
                (Newline, SourceLocation { line: 6, column: 1 }),
                (
                    Name(String::from("show")),
                    SourceLocation { line: 7, column: 1 },
                ),
                (
                    Name(String::from("a")),
                    SourceLocation { line: 7, column: 6 },
                ),
            ],
        )
    }
}
