use bigdecimal::BigDecimal;
use line_col::LineColLookup;
use peg::parser;
use snailquote::unescape;

#[derive(Clone)]
pub struct Ast {
    pub node: AstNode,
    pub location: SourceLocation,
}

#[derive(Clone)]
pub enum AstNode {
    Block(Vec<AstNodeStatement>),
    Module(Vec<AstNodeStatement>),
    List(Vec<Ast>),
    Name(String),
    Text(String),
    Number(BigDecimal),
    Quoted(Box<Ast>),
}

#[derive(Clone)]
pub struct AstNodeStatement {
    pub items: Vec<Ast>,
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

fn ast(node: AstNode, lc: &LineColLookup, offset: usize) -> Ast {
    Ast {
        node,
        location: source_location(lc, offset),
    }
}

parser! {
    pub grammar grammar(lc: &LineColLookup) for str {
        use AstNode::*;

        pub rule inline_program() -> Ast
            = p:position!() ws() statements:statements() ws() eof()
            { ast(Block(statements), lc, p) }

        pub rule file() -> Ast
            = p:position!() ws() statements:statements() ws() eof()
            { ast(Module(statements), lc, p) }

        rule value() -> Ast
            = block() / module() / list() / quoted() / text() / number() / name()

        rule name() -> Ast
            = p:position!() name:$(!(" " / "\t" / "\r" / "\n" / "(" / ")" / "[" / "]" / "{" / "}" / "'" / "\"") [_])+
            {
                ast(Name(name.join("")), lc, p)
            }

        rule number() -> Ast
            = p:position!() number:$("-"? ['0'..='9']+ ("." ['0'..='9']+)?)
            { ast(Number(number.parse().unwrap()), lc, p) }

        rule text() -> Ast
            = p:position!() "\"" text:$((!"\"" [_])*) "\""
            {?
                match unescape(text) {
                    Ok(text) => Ok(ast(Text(text), lc, p)),
                    Err(_) => Err("Invalid escape character")
                }
            }

        rule quoted() -> Ast
            = p:position!() "'" value:value()
            { ast(Quoted(Box::new(value)), lc, p) }

        rule list() -> Ast
            = p:position!() "(" ws() items:list_item()* ws() ")"
            { ast(List(items), lc, p) }

        rule list_item() -> Ast
            = ws() value:value()
            { value }

        rule block() -> Ast
            = p:position!() "[" ws() statements:statements() ws() "]"
            { ast(Block(statements), lc, p) }

        rule module() -> Ast
            = p:position!() "{" ws() statements:statements() ws() "}"
            { ast(Module(statements), lc, p) }

        rule statements() -> Vec<AstNodeStatement>
            = statements:statement()*
            { statements.iter().filter(|s| !s.items.is_empty()).cloned().collect() }

        rule statement() -> AstNodeStatement
            = p:position!() first:value()? rest:statement_item()* sp()* comment()? nl()+
            {
                let mut items = rest;

                if let Some(first) = first {
                    items.insert(0, first);
                }

                AstNodeStatement {
                    items,
                    location: source_location(lc, p)
                }
            }

        rule statement_item() -> Ast
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
