use wipple::*;
use wipple_parser::*;

pub fn convert_ast(ast: &Ast, file: Option<&str>) -> Value {
    use AstNode::*;

    match &ast.node {
        Block(statements) => Value::of(crate::Block::new_located(
            &statements
                .iter()
                .map(|statement| crate::List {
                    items: statement
                        .items
                        .iter()
                        .map(|node| convert_ast(node, file))
                        .collect(),
                    location: Some(location(&statement.location, file)),
                })
                .collect::<Vec<_>>(),
            Some(location(&ast.location, file)),
        )),

        List(items) => Value::of(crate::List::new_located(
            items
                .iter()
                .map(|node| convert_ast(node, file))
                .collect::<Vec<_>>(),
            Some(location(&ast.location, file)),
        )),

        Literal(node) => Value::of(crate::Literal::new_located(
            convert_ast(node, file),
            Some(location(&ast.location, file)),
        )),

        Name(name) => Value::of(crate::Name::new_located(
            name,
            Some(location(&ast.location, file)),
        )),

        Number(number) => Value::of(crate::Number::new_located(
            *number,
            Some(location(&ast.location, file)),
        )),

        Text(text) => Value::of(crate::Text::new_located(
            text.to_string(),
            Some(location(&ast.location, file)),
        )),
    }
}

pub(crate) fn location(
    location: &wipple_parser::SourceLocation,
    file: Option<&str>,
) -> wipple::SourceLocation {
    wipple::SourceLocation {
        file: file.map(String::from),
        line: location.line,
        column: location.column,
    }
}
