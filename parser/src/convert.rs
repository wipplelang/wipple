#![cfg(feature = "convert")]

use crate::*;
use std::path::Path;
use wipple::*;

pub fn convert(ast: &Ast, file: Option<&Path>) -> Value {
    use AstNode::*;

    match &ast.node {
        Block(statements) => Value::of(wipple::Block::new_located(
            &statements
                .iter()
                .map(|statement| wipple::List {
                    items: statement
                        .items
                        .iter()
                        .map(|node| convert(node, file))
                        .collect(),
                    location: Some(location(&statement.location, file)),
                })
                .collect::<Vec<_>>(),
            Some(location(&ast.location, file)),
        )),

        Module(statements) => Value::of(wipple::ModuleBlock::new_located(
            &statements
                .iter()
                .map(|statement| wipple::List {
                    items: statement
                        .items
                        .iter()
                        .map(|node| convert(node, file))
                        .collect(),
                    location: Some(location(&statement.location, file)),
                })
                .collect::<Vec<_>>(),
            Some(location(&ast.location, file)),
        )),

        List(items) => Value::of(wipple::List::new_located(
            &items
                .iter()
                .map(|node| convert(node, file))
                .collect::<Vec<_>>(),
            Some(location(&ast.location, file)),
        )),

        Quoted(node) => Value::of(wipple::Quoted::new_located(
            convert(node, file),
            Some(location(&ast.location, file)),
        )),

        Name(name) => Value::of(wipple::Name::new_located(
            name,
            Some(location(&ast.location, file)),
        )),

        Number(number) => Value::of(wipple::Number::new_located(
            number.clone(),
            Some(location(&ast.location, file)),
        )),

        Text(text) => Value::of(wipple::Text::new_located(
            text,
            Some(location(&ast.location, file)),
        )),
    }
}

fn location(location: &crate::SourceLocation, file: Option<&Path>) -> wipple::SourceLocation {
    wipple::SourceLocation {
        file: file.map(|path| path.to_path_buf()),
        line: location.line,
        column: location.column,
    }
}
