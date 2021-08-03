use std::path::Path;
use wipple_parser as parser;

#[derive(Debug, Clone)]
pub enum Expression<'a> {
    Block(BlockExpression<'a>),
    List(ListExpression<'a>),
    Name(NameExpression<'a>),
    Text(TextExpression<'a>),
    Number(NumberExpression<'a>),
    Literal(LiteralExpression<'a>),
    Escaped(EscapedExpression<'a>),
}

#[derive(Debug, Clone)]
pub struct BlockExpression<'a> {
    pub block: Vec<ListExpression<'a>>,
    pub location: Location<'a>,
}

#[derive(Debug, Clone)]
pub struct ListExpression<'a> {
    pub items: Vec<Expression<'a>>,
    pub location: Location<'a>,
}

#[derive(Debug, Clone)]
pub struct NameExpression<'a> {
    pub value: String,
    pub location: Location<'a>,
}

#[derive(Debug, Clone)]
pub struct TextExpression<'a> {
    pub value: String,
    pub location: Location<'a>,
}

#[derive(Debug, Clone)]
pub struct NumberExpression<'a> {
    pub value: String,
    pub location: Location<'a>,
}

#[derive(Debug, Clone)]
pub struct LiteralExpression<'a> {
    pub value: Box<Expression<'a>>,
    pub location: Location<'a>,
}

#[derive(Debug, Clone)]
pub struct EscapedExpression<'a> {
    pub value: Box<Expression<'a>>,
    pub location: Location<'a>,
}

#[derive(Debug, Clone)]
pub struct Location<'a> {
    pub file: Option<&'a Path>,
    pub line: usize,
    pub column: usize,
}

impl<'a> Expression<'a> {
    pub fn from_parser(ast: parser::Ast, file: Option<&'a Path>) -> Self {
        use parser::AstNode;

        match ast.node {
            AstNode::Block(block) => Expression::Block(BlockExpression {
                block: block
                    .into_iter()
                    .map(|ast| ListExpression {
                        items: ast
                            .items
                            .into_iter()
                            .map(|ast| Expression::from_parser(ast, file))
                            .collect(),
                        location: Location::from_parser(ast.location, file),
                    })
                    .collect(),
                location: Location::from_parser(ast.location, file),
            }),
            AstNode::List(list) => Expression::List(ListExpression {
                items: list
                    .into_iter()
                    .map(|ast| Expression::from_parser(ast, file))
                    .collect(),
                location: Location::from_parser(ast.location, file),
            }),
            AstNode::Name(name) => Expression::Name(NameExpression {
                value: name,
                location: Location::from_parser(ast.location, file),
            }),
            AstNode::Text(text) => Expression::Text(TextExpression {
                value: text,
                location: Location::from_parser(ast.location, file),
            }),
            AstNode::Number(number) => Expression::Number(NumberExpression {
                value: number,
                location: Location::from_parser(ast.location, file),
            }),
            AstNode::Literal(literal) => Expression::Literal(LiteralExpression {
                value: Box::new(Expression::from_parser(*literal, file)),
                location: Location::from_parser(ast.location, file),
            }),
            AstNode::Escaped(escaped) => Expression::Escaped(EscapedExpression {
                value: Box::new(Expression::from_parser(*escaped, file)),
                location: Location::from_parser(ast.location, file),
            }),
        }
    }
}

impl<'a> Location<'a> {
    pub fn from_parser(location: parser::Location, file: Option<&'a Path>) -> Self {
        Location {
            file,
            line: location.line,
            column: location.column,
        }
    }
}
