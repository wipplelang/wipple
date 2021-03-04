use line_col::LineColLookup;
use peg::parser;
use snailquote::unescape;

#[derive(Clone)]
pub struct AST {
    pub node: ASTNode,
    pub location: SourceLocation,
}

#[derive(Clone)]
pub enum ASTNode {
    Block(Vec<ASTNodeStatement>),
    Module(Vec<ASTNodeStatement>),
    List(Vec<AST>),
    Name(String),
    Text(String),
    Number(f64),
    Quoted(Box<AST>),
}

#[derive(Clone)]
pub struct ASTNodeStatement {
    pub items: Vec<AST>,
    pub location: SourceLocation,
}

#[derive(Clone)]
pub struct SourceLocation {
    pub line: usize,
    pub column: usize,
}

fn source_location(lc: &LineColLookup, offset: usize) -> SourceLocation {
    let (line, column) = lc.get(offset);

    SourceLocation { line, column }
}

fn ast(node: ASTNode, lc: &LineColLookup, offset: usize) -> AST {
    AST {
        node,
        location: source_location(lc, offset),
    }
}

parser! {
    pub grammar grammar(lc: &LineColLookup) for str {
        use ASTNode::*;

        pub rule program() -> AST
            = p:position!() ws() statements:statements() ws() eof()
            { ast(Block(statements), lc, p) }

        rule value() -> AST
            = block() / module() / list() / quoted() / text() / number() / name()

        rule name() -> AST
            = p:position!() name:$(!(" " / "\t" / "\r" / "\n" / "(" / ")" / "[" / "]" / "{" / "}" / "'" / "\"") [_])+
            {
                ast(Name(name.join("")), lc, p)
            }

        rule number() -> AST
            = p:position!() number:$("-"? ['0'..='9']+ ("." ['0'..='9']+)?)
            { ast(Number(number.parse().unwrap()), lc, p) }

        rule text() -> AST
            = p:position!() "\"" text:$((!"\"" [_])*) "\""
            {?
                match unescape(text) {
                    Ok(text) => Ok(ast(Text(text), lc, p)),
                    Err(_) => Err("Invalid escape character")
                }
            }

        rule quoted() -> AST
            = p:position!() "'" value:value()
            { ast(Quoted(Box::new(value)), lc, p) }

        rule list() -> AST
            = p:position!() "(" ws() items:list_item()* ws() ")"
            { ast(List(items), lc, p) }

        rule list_item() -> AST
            = ws() value:value()
            { value }

        rule block() -> AST
            = p:position!() "[" ws() statements:statements() ws() "]"
            { ast(Block(statements), lc, p) }

        rule module() -> AST
            = p:position!() "{" ws() statements:statements() ws() "}"
            { ast(Module(statements), lc, p) }

        rule statements() -> Vec<ASTNodeStatement>
            = statements:statement()*
            { statements.iter().filter(|s| !s.items.is_empty()).cloned().collect() }

        rule statement() -> ASTNodeStatement
            = p:position!() first:value()? rest:statement_item()* sp()* comment()? nl()+
            {
                let mut items = rest;

                if let Some(first) = first {
                    items.insert(0, first);
                }

                ASTNodeStatement {
                    items,
                    location: source_location(lc, p)
                }
            }

        rule statement_item() -> AST
            = sp()* value:value()
            { value }

        rule comment()
            = "--" (!nl() [_])*

        rule ws()
            = (sp()+ / eol())*

        rule sp()
            = " " / "\t"

        rule nl()
            = "\r\n" / "\r" / "\n"

        rule eol()
            = comment()? nl()+

        rule eof()
            = ![_]
    }
}
