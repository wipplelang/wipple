use std::path::PathBuf;

use crate::*;
use wipple::*;
use wipple_parser::*;

pub fn convert_ast(ast: &Ast, file: Option<PathBuf>) -> Value {
    match &ast.node {
        AstNode::Block(statements) => Value::of(Block::new_located(
            statements
                .iter()
                .map(|statement| List {
                    items: statement
                        .items
                        .iter()
                        .map(|node| convert_ast(node, file.clone()))
                        .collect(),
                    location: Some(location(&statement.location, file.clone())),
                })
                .collect::<Vec<_>>(),
            Some(location(&ast.location, file)),
        )),

        AstNode::List(items) => Value::of(List::new_located(
            items
                .iter()
                .map(|node| convert_ast(node, file.clone()))
                .collect::<Vec<_>>(),
            Some(location(&ast.location, file)),
        )),

        AstNode::Literal(node) => Value::of(Literal::new_located(
            convert_ast(node, file.clone()),
            Some(location(&ast.location, file)),
        )),

        AstNode::Escaped(node) => Value::of(Escaped::new_located(
            convert_ast(node, file.clone()),
            Some(location(&ast.location, file)),
        )),

        AstNode::Name(name) => {
            Value::of(Name::new_located(name, Some(location(&ast.location, file))))
        }

        AstNode::Number(number) => Value::of(Number::new_located(
            number.parse().unwrap(),
            Some(location(&ast.location, file)),
        )),

        AstNode::Text(text) => Value::of(Text::new_located(
            text.to_string(),
            Some(location(&ast.location, file)),
        )),
    }
}

pub(crate) fn location(
    location: &wipple_parser::SourceLocation,
    file: Option<PathBuf>,
) -> wipple::SourceLocation {
    wipple::SourceLocation {
        file,
        line: location.line,
        column: location.column,
    }
}
