use crate::{
    tokenize::{Diagnostic, Keyword, NonAssociativeOperator, Operator, Token, VariadicOperator},
    Driver, Location,
};
use logos::Logos;
use std::borrow::Cow;
use wipple_util::WithInfo;

#[derive(Debug, Logos)]
#[logos(skip r#"[\t ]+"#)]
enum RawToken<'src> {
    #[token("(")]
    LeftParenthesis,

    #[token(")")]
    RightParenthesis,

    #[token("[")]
    LeftBracket,

    #[token("]")]
    RightBracket,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    #[token("_", priority = 3)]
    Underscore,

    #[token("do")]
    Do,

    #[token("@")]
    Attribute,

    #[token("!")]
    Mutate,

    #[token("when")]
    When,

    #[token("where")]
    Where,

    #[token("type")]
    Type,

    #[token("trait")]
    Trait,

    #[token("instance")]
    Instance,

    #[token("intrinsic")]
    Intrinsic,

    #[token("as")]
    As,

    #[token("to")]
    To,

    #[token("by")]
    By,

    #[token("^")]
    Power,

    #[token("*")]
    Multiply,

    #[token("/")]
    Divide,

    #[token("%")]
    Remainder,

    #[token("+")]
    Add,

    #[token("-", priority = 3)]
    Subtract,

    #[token("<")]
    LessThan,

    #[token("<=")]
    LessThanOrEqual,

    #[token(">")]
    GreaterThan,

    #[token(">=")]
    GreaterThanOrEqual,

    #[token("=")]
    Equal,

    #[token("/=")]
    NotEqual,

    #[token("is")]
    Is,

    #[token("and")]
    And,

    #[token("or")]
    Or,

    #[token(".")]
    Apply,

    #[token("->")]
    Function,

    #[token(";")]
    Tuple,

    #[token(",")]
    Collection,

    #[token("=>")]
    TypeFunction,

    #[token(":")]
    Assign,

    #[token("::")]
    Annotate,

    #[regex(r#"\n"#)]
    LineBreak,

    #[regex(r#"--.*"#, |lex| Cow::Borrowed(&lex.slice()[2..]))]
    Comment(Cow<'src, str>),

    #[regex(r#"\.\.\.|[A-Za-z0-9\-_]+[?]?"#, |lex| Cow::Borrowed(lex.slice()))]
    Name(Cow<'src, str>),

    #[regex(r#""[^"\\]*(?s:\\.[^"\\]*)*"|'[^'\\]*(?s:\\.[^'\\]*)*'"#, |lex| Cow::Borrowed(&lex.slice()[1..(lex.slice().len() - 1)]))]
    Text(Cow<'src, str>),

    #[regex(r#"-?[0-9]+(\.[0-9]+)?"#, |lex| Cow::Borrowed(lex.slice()), priority = 3)]
    Number(Cow<'src, str>),
}

/// Tokenize a source file.
pub fn tokenize<'a, 'src: 'a, D: Driver>(
    driver: &'a D,
    s: &'src str,
) -> impl Iterator<Item = Result<WithInfo<D::Info, Token<'src>>, WithInfo<D::Info, Diagnostic<D>>>> + 'a
where
    D::Info: From<Location>,
{
    logos::Lexer::new(s).spanned().map(|(result, span)| {
        let span = (span.start as u32)..(span.end as u32);

        match result {
            Ok(raw_token) => {
                let kind = match raw_token {
                    RawToken::LeftParenthesis => Token::LeftParenthesis,
                    RawToken::RightParenthesis => Token::RightParenthesis,
                    RawToken::LeftBracket => Token::LeftBracket,
                    RawToken::RightBracket => Token::RightBracket,
                    RawToken::LeftBrace => Token::LeftBrace,
                    RawToken::RightBrace => Token::RightBrace,
                    RawToken::Underscore => Token::Keyword(Keyword::Underscore),
                    RawToken::Attribute => Token::Keyword(Keyword::Attribute),
                    RawToken::Mutate => Token::Keyword(Keyword::Mutate),
                    RawToken::Do => Token::Keyword(Keyword::Do),
                    RawToken::When => Token::Keyword(Keyword::When),
                    RawToken::Type => Token::Keyword(Keyword::Type),
                    RawToken::Trait => Token::Keyword(Keyword::Trait),
                    RawToken::Instance => Token::Keyword(Keyword::Instance),
                    RawToken::Intrinsic => Token::Keyword(Keyword::Intrinsic),
                    RawToken::As => Token::Operator(Operator::As),
                    RawToken::To => Token::Operator(Operator::To),
                    RawToken::By => Token::Operator(Operator::By),
                    RawToken::Power => Token::Operator(Operator::Power),
                    RawToken::Multiply => Token::Operator(Operator::Multiply),
                    RawToken::Divide => Token::Operator(Operator::Divide),
                    RawToken::Remainder => Token::Operator(Operator::Remainder),
                    RawToken::Add => Token::Operator(Operator::Add),
                    RawToken::Subtract => Token::Operator(Operator::Subtract),
                    RawToken::LessThan => Token::Operator(Operator::LessThan),
                    RawToken::LessThanOrEqual => Token::Operator(Operator::LessThanOrEqual),
                    RawToken::GreaterThan => Token::Operator(Operator::GreaterThan),
                    RawToken::GreaterThanOrEqual => Token::Operator(Operator::GreaterThanOrEqual),
                    RawToken::Equal => Token::Operator(Operator::Equal),
                    RawToken::NotEqual => Token::Operator(Operator::NotEqual),
                    RawToken::Is => Token::Operator(Operator::Is),
                    RawToken::And => Token::Operator(Operator::And),
                    RawToken::Or => Token::Operator(Operator::Or),
                    RawToken::Apply => Token::Operator(Operator::Apply),
                    RawToken::Function => Token::Operator(Operator::Function),
                    RawToken::Tuple => Token::VariadicOperator(VariadicOperator::Tuple),
                    RawToken::Collection => Token::VariadicOperator(VariadicOperator::Collection),
                    RawToken::Where => Token::NonAssociativeOperator(NonAssociativeOperator::Where),
                    RawToken::TypeFunction => {
                        Token::NonAssociativeOperator(NonAssociativeOperator::TypeFunction)
                    }
                    RawToken::Assign => {
                        Token::NonAssociativeOperator(NonAssociativeOperator::Assign)
                    }
                    RawToken::Annotate => {
                        Token::NonAssociativeOperator(NonAssociativeOperator::Annotate)
                    }
                    RawToken::LineBreak => Token::LineBreak,
                    RawToken::Comment(comment) => Token::Comment(comment),
                    RawToken::Name(name) => {
                        if let Ok(keyword) = name.parse::<Keyword>() {
                            Token::Keyword(keyword)
                        } else {
                            Token::Name(name)
                        }
                    }
                    RawToken::Text(text) => Token::Text(text),
                    RawToken::Number(number) => Token::Number(number),
                };

                Ok(WithInfo {
                    info: Location {
                        path: driver.file_path(),
                        visible_path: driver.visible_path(),
                        span,
                    }
                    .into(),
                    item: kind,
                })
            }
            Err(()) => Err(WithInfo {
                info: Location {
                    path: driver.file_path(),
                    visible_path: driver.visible_path(),
                    span,
                }
                .into(),
                item: Diagnostic::InvalidToken,
            }),
        }
    })
}
