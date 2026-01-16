use crate::{
    database::{Location, Span},
    syntax::ParseError,
};
use line_index::{LineCol, LineIndex};
use logos::Logos;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Logos)]
pub enum TokenKind {
    #[regex(r#"[ \t]+"#, logos::skip)]
    Space,
    #[regex(r#"\n+"#)]
    LineBreak,
    #[regex(r#"--[^\n]*"#, allow_greedy = true)]
    Comment,
    #[regex(r#"=>"#)]
    TypeFunctionOperator,
    #[regex(r#"::"#)]
    AnnotateOperator,
    #[regex(r#":"#)]
    AssignOperator,
    #[regex(r#"->"#)]
    FunctionOperator,
    #[regex(r#"<="#)]
    LessThanOrEqualOperator,
    #[regex(r#">="#)]
    GreaterThanOrEqualOperator,
    #[regex(r#"/="#)]
    NotEqualOperator,
    #[regex(r#"\^"#)]
    PowerOperator,
    #[regex(r#"\*"#)]
    MultiplyOperator,
    #[regex(r#"/"#)]
    DivideOperator,
    #[regex(r#"%"#)]
    RemainderOperator,
    #[regex(r#"\+"#)]
    AddOperator,
    #[regex(r#"-"#)]
    SubtractOperator,
    #[regex(r#"<"#)]
    LessThanOperator,
    #[regex(r#">"#)]
    GreaterThanOperator,
    #[regex(r#"="#)]
    EqualOperator,
    #[regex(r#"\."#)]
    ApplyOperator,
    #[regex(r#";"#)]
    TupleOperator,
    #[regex(r#","#)]
    CollectionOperator,
    #[regex(r#"\("#)]
    LeftParenthesis,
    #[regex(r#"\)"#)]
    RightParenthesis,
    #[regex(r#"\["#)]
    LeftBracket,
    #[regex(r#"\]"#)]
    RightBracket,
    #[regex(r#"\{"#)]
    LeftBrace,
    #[regex(r#"\}"#)]
    RightBrace,
    #[regex(r#"_"#, priority = 2)]
    UnderscoreKeyword,
    #[regex(r#"do"#)]
    DoKeyword,
    #[regex(r#"infer"#)]
    InferKeyword,
    #[regex(r#"instance"#)]
    InstanceKeyword,
    #[regex(r#"intrinsic"#)]
    IntrinsicKeyword,
    #[regex(r#"set"#)]
    SetKeyword,
    #[regex(r#"trait"#)]
    TraitKeyword,
    #[regex(r#"type"#)]
    TypeKeyword,
    #[regex(r#"when"#)]
    WhenKeyword,
    #[regex(r#"where"#)]
    WhereKeyword,
    #[regex(r#"as"#)]
    AsOperator,
    #[regex(r#"to"#)]
    ToOperator,
    #[regex(r#"by"#)]
    ByOperator,
    #[regex(r#"is"#)]
    IsOperator,
    #[regex(r#"and"#)]
    AndOperator,
    #[regex(r#"or"#)]
    OrOperator,
    #[regex(r#"[+\-]?\d+(\.\d+)?"#, priority = 2)]
    Number,
    #[regex(r#""[^"]*"|'[^']*'"#)]
    String,
    #[regex(r#"(\d+-)*[A-Z][A-Za-z0-9_]*(-[A-Za-z0-9_]+)*([!?])?"#, priority = 2)]
    CapitalName,
    #[regex(r#"(\d+-)*[A-Za-z0-9_]+(-[A-Za-z0-9_]+)*([!?])?"#, priority = 1)]
    LowercaseName,
}

pub fn tokenize(path: &str, source: &str) -> Result<Vec<Token>, ParseError> {
    let mut lexer = logos::Lexer::<TokenKind>::new(source);
    let line_index = LineIndex::new(source);

    let mut tokens = Vec::new();
    while let Some(result) = lexer.next() {
        let range = lexer.span();
        let token_source = lexer.slice();

        let LineCol {
            line: start_line,
            col: start_column,
        } = line_index.line_col((range.start as u32).into());

        let start = Location {
            line: start_line as usize + 1,
            column: start_column as usize + 1,
            index: range.start,
        };

        let LineCol {
            line: end_line,
            col: end_column,
        } = line_index.line_col((range.end as u32).into());

        let end = Location {
            line: end_line as usize + 1,
            column: end_column as usize + 1,
            index: range.end,
        };

        let kind = match result {
            Ok(kind) => kind,
            Err(()) => {
                return Err(ParseError {
                    message: String::from("Unexpected character"),
                    reason: None,
                    committed: None,
                    span: Span {
                        path: path.to_string(),
                        start,
                        end,
                        source: String::new(),
                    },
                });
            }
        };

        let token = Token {
            kind,
            value: trim(kind, token_source).to_string(),
            span: Span {
                path: path.to_string(),
                start,
                end,
                source: token_source.to_string(),
            },
        };

        tokens.push(token);
    }

    Ok(tokens)
}

fn trim(kind: TokenKind, source: &str) -> &str {
    match kind {
        TokenKind::Comment => &source[2..],
        TokenKind::String => &source[1..source.len() - 1],
        _ => source,
    }
}

impl TokenKind {
    pub fn name(&self) -> &'static str {
        match self {
            TokenKind::Space => "",
            TokenKind::LineBreak => "a line break",
            TokenKind::Comment => "a comment",
            TokenKind::TypeFunctionOperator => "`=>`",
            TokenKind::AnnotateOperator => "`::`",
            TokenKind::AssignOperator => "`:`",
            TokenKind::FunctionOperator => "`->`",
            TokenKind::LessThanOrEqualOperator => "`<=`",
            TokenKind::GreaterThanOrEqualOperator => "`>=`",
            TokenKind::NotEqualOperator => "`/=`",
            TokenKind::PowerOperator => "`^`",
            TokenKind::MultiplyOperator => "`*`",
            TokenKind::DivideOperator => "`/`",
            TokenKind::RemainderOperator => "`%`",
            TokenKind::AddOperator => "`+`",
            TokenKind::SubtractOperator => "`-`",
            TokenKind::LessThanOperator => "`<`",
            TokenKind::GreaterThanOperator => "`>`",
            TokenKind::EqualOperator => "`=`",
            TokenKind::ApplyOperator => "`.`",
            TokenKind::TupleOperator => "`;`",
            TokenKind::CollectionOperator => "`,`",
            TokenKind::LeftParenthesis => "`(`",
            TokenKind::RightParenthesis => "`)`",
            TokenKind::LeftBracket => "`[`",
            TokenKind::RightBracket => "`]`",
            TokenKind::LeftBrace => "`{`",
            TokenKind::RightBrace => "`}`",
            TokenKind::UnderscoreKeyword => "`_`",
            TokenKind::DoKeyword => "`do`",
            TokenKind::InferKeyword => "`infer`",
            TokenKind::InstanceKeyword => "`instance`",
            TokenKind::IntrinsicKeyword => "`intrinsic`",
            TokenKind::SetKeyword => "`set`",
            TokenKind::TraitKeyword => "`trait`",
            TokenKind::TypeKeyword => "`type`",
            TokenKind::WhenKeyword => "`when`",
            TokenKind::WhereKeyword => "`where`",
            TokenKind::AsOperator => "`as`",
            TokenKind::ToOperator => "`to`",
            TokenKind::ByOperator => "`by`",
            TokenKind::IsOperator => "`is`",
            TokenKind::AndOperator => "`and`",
            TokenKind::OrOperator => "`or`",
            TokenKind::Number => "a number",
            TokenKind::String => "a string",
            TokenKind::CapitalName => "a capital name",
            TokenKind::LowercaseName => "a lowercase name",
        }
    }

    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            TokenKind::DoKeyword
                | TokenKind::InferKeyword
                | TokenKind::InstanceKeyword
                | TokenKind::IntrinsicKeyword
                | TokenKind::SetKeyword
                | TokenKind::TraitKeyword
                | TokenKind::TypeKeyword
                | TokenKind::UnderscoreKeyword
                | TokenKind::WhenKeyword
        )
    }

    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            TokenKind::TypeFunctionOperator
                | TokenKind::AnnotateOperator
                | TokenKind::AssignOperator
                | TokenKind::FunctionOperator
                | TokenKind::LessThanOrEqualOperator
                | TokenKind::GreaterThanOrEqualOperator
                | TokenKind::NotEqualOperator
                | TokenKind::PowerOperator
                | TokenKind::MultiplyOperator
                | TokenKind::DivideOperator
                | TokenKind::RemainderOperator
                | TokenKind::AddOperator
                | TokenKind::SubtractOperator
                | TokenKind::LessThanOperator
                | TokenKind::GreaterThanOperator
                | TokenKind::EqualOperator
                | TokenKind::ApplyOperator
                | TokenKind::TupleOperator
                | TokenKind::CollectionOperator
                | TokenKind::AsOperator
                | TokenKind::ToOperator
                | TokenKind::ByOperator
                | TokenKind::IsOperator
                | TokenKind::AndOperator
                | TokenKind::OrOperator
                | TokenKind::WhereKeyword
        )
    }

    pub fn is_binary_operator(&self) -> bool {
        self.is_operator() && !self.is_non_associative_operator() && !self.is_variadic_operator()
    }

    pub fn is_non_associative_operator(&self) -> bool {
        matches!(
            self,
            TokenKind::WhereKeyword
                | TokenKind::TypeFunctionOperator
                | TokenKind::AnnotateOperator
                | TokenKind::AssignOperator
        )
    }

    pub fn is_variadic_operator(&self) -> bool {
        matches!(
            self,
            TokenKind::TupleOperator | TokenKind::CollectionOperator
        )
    }

    pub fn is_opening(&self) -> bool {
        matches!(
            self,
            TokenKind::LeftParenthesis | TokenKind::LeftBracket | TokenKind::LeftBrace
        )
    }

    pub fn is_closing(&self) -> bool {
        matches!(
            self,
            TokenKind::RightParenthesis | TokenKind::RightBracket | TokenKind::RightBrace
        )
    }
}
