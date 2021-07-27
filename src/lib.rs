use atoms::{StringValue, Value};
use std::{iter::Peekable, vec};

pub mod parser {
    use super::*;

    pub type Input = Peekable<vec::IntoIter<StringValue>>;

    pub trait Parse
    where
        Self: Sized,
    {
        #[must_use]
        fn parse(input: &mut Input) -> Option<Self>;
    }

    #[must_use]
    pub fn parse_statement(name: &str, input: &mut Input) -> Option<()> {
        match input.peek()? {
            Value::Symbol(s) if s == name => {
                input.next();
                Some(())
            }
            _ => None,
        }
    }

    #[must_use]
    pub fn parse_list(input: StringValue) -> Option<Vec<StringValue>> {
        fn parse(input: StringValue, acc: &mut Vec<StringValue>) -> Option<()> {
            match input {
                Value::Cons(a, b) => {
                    acc.push(*a);
                    parse(*b, acc)
                }
                Value::Nil => Some(()),
                _ => None,
            }
        }

        let mut acc = Vec::new();
        parse(input, &mut acc)?;
        Some(acc)
    }

    #[must_use]
    pub fn parse_end(input: &mut Input) -> Option<()> {
        match input.peek() {
            Some(_) => None,
            None => Some(()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Version(VersionStatement),
    Block(BlockStatement),
    Data(DataStatement),
    Extern(ExternStatement),
    Define(DefineStatement),
}

impl parser::Parse for Statement {
    fn parse(input: &mut parser::Input) -> Option<Self> {
        if let Some(version) = parser::Parse::parse(input) {
            Some(Statement::Version(version))
        } else if let Some(block) = parser::Parse::parse(input) {
            Some(Statement::Block(block))
        } else if let Some(data) = parser::Parse::parse(input) {
            Some(Statement::Data(data))
        } else if let Some(r#extern) = parser::Parse::parse(input) {
            Some(Statement::Extern(r#extern))
        } else if let Some(define) = parser::Parse::parse(input) {
            Some(Statement::Define(define))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct VersionStatement {
    pub version: semver::Version,
}

impl parser::Parse for VersionStatement {
    fn parse(input: &mut parser::Input) -> Option<Self> {
        parser::parse_statement("version", input)?;

        let version = match input.next()? {
            Value::Str(s) => s,
            _ => return None,
        };

        let version = semver::Version::parse(&version).ok()?;

        parser::parse_end(input)?;

        Some(VersionStatement { version })
    }
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub instructions: Vec<Instruction>,
}

impl parser::Parse for BlockStatement {
    fn parse(input: &mut parser::Input) -> Option<Self> {
        parser::parse_statement("block", input)?;

        let instructions = parser::Parse::parse(input)?;

        Some(BlockStatement { instructions })
    }
}

impl parser::Parse for Vec<Instruction> {
    fn parse(input: &mut parser::Input) -> Option<Self> {
        let mut instructions = Vec::new();
        for expr in input {
            let mut list = parser::parse_list(expr)?.into_iter().peekable();
            let instruction = parser::Parse::parse(&mut list)?;
            instructions.push(instruction);
        }

        Some(instructions)
    }
}

#[derive(Debug, Clone)]
pub enum DataStatement {
    Number(f64),
    Decimal(i64),
    Hex(i64),
    Binary(i64),
    String(String),
    Bool(bool),
}

impl parser::Parse for DataStatement {
    fn parse(input: &mut parser::Input) -> Option<Self> {
        parser::parse_statement("data", input)?;
        DataStatement::parse_body(input)
    }
}

impl DataStatement {
    #[must_use]
    fn parse_body(input: &mut parser::Input) -> Option<Self> {
        let kind = match input.next()? {
            Value::Symbol(s) => s,
            _ => return None,
        };

        let value = match kind.as_ref() {
            "num" => match input.next()? {
                Value::Int(n) => DataStatement::Number(n as f64),
                Value::Float(n) => DataStatement::Number(n),
                _ => return None,
            },
            "dec" => match input.next()? {
                Value::Int(n) => DataStatement::Decimal(n),
                _ => return None,
            },
            "hex" => {
                let s = input.next()?.to_string();
                let n = i64::from_str_radix(&s, 16).ok()?;
                DataStatement::Hex(n)
            }
            "bin" => {
                let s = input.next()?.to_string();
                let n = i64::from_str_radix(&s, 2).ok()?;
                DataStatement::Binary(n)
            }
            "str" => match input.next()? {
                Value::Str(s) => DataStatement::String(s),
                _ => return None,
            },
            "true" => DataStatement::Bool(true),
            "false" => DataStatement::Bool(false),
            _ => return None,
        };

        parser::parse_end(input)?;

        Some(value)
    }
}

#[derive(Debug, Clone)]
pub struct ExternStatement {
    pub object: String,
    pub symbol: String,
}

impl parser::Parse for ExternStatement {
    fn parse(input: &mut parser::Input) -> Option<Self> {
        parser::parse_statement("extern", input)?;

        let object = match input.next()? {
            Value::Str(s) => s,
            _ => return None,
        };

        let symbol = match input.next()? {
            Value::Str(s) => s,
            _ => return None,
        };

        parser::parse_end(input)?;

        Some(ExternStatement { object, symbol })
    }
}

#[derive(Debug, Clone)]
pub struct DefineStatement {
    pub name: String,
    pub definition: Definition,
}

impl parser::Parse for DefineStatement {
    fn parse(input: &mut parser::Input) -> Option<Self> {
        parser::parse_statement("def", input)?;
        DefineStatement::parse_body(input)
    }
}

impl DefineStatement {
    fn parse_body(input: &mut parser::Input) -> Option<Self> {
        let name = match input.next()? {
            Value::Symbol(s) => s,
            _ => return None,
        };

        let definition = match input.peek() {
            None => Definition::Auto,
            Some(expr) => {
                if let Some(index) = Reference::parse_index(&expr) {
                    Definition::Variable(index)
                } else if let Some(data) = parser::Parse::parse(input) {
                    Definition::Data(data)
                } else if let Some(block) = parser::Parse::parse(input) {
                    Definition::Block(block)
                } else if let Some(r#extern) = parser::Parse::parse(input) {
                    Definition::Extern(r#extern)
                } else {
                    return None;
                }
            }
        };

        parser::parse_end(input)?;

        Some(DefineStatement { name, definition })
    }
}

#[derive(Debug, Clone)]
pub enum Definition {
    Auto,
    Variable(u64),
    Data(DataStatement),
    Block(BlockStatement),
    Extern(ExternStatement),
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Define(DefineStatement),
    Enter(Reference, Vec<Reference>),
    Exit(Vec<Reference>),
    Call(Reference, Vec<Reference>),
    Use(Reference),
    If(Reference, Vec<Instruction>, Vec<Instruction>),
}

impl parser::Parse for Instruction {
    fn parse(input: &mut parser::Input) -> Option<Self> {
        let kind = match input.next()? {
            Value::Symbol(s) => s,
            _ => return None,
        };

        let instruction = match kind.as_ref() {
            "def" => Instruction::Define(DefineStatement::parse_body(input)?),
            "enter" => Instruction::Enter(
                Reference::parse(input.next()?)?,
                parser::Parse::parse(input)?,
            ),
            "exit" => Instruction::Exit(parser::Parse::parse(input)?),
            "call" => Instruction::Call(
                Reference::parse(input.next()?)?,
                parser::Parse::parse(input)?,
            ),
            "use" => Instruction::Use(Reference::parse(input.next()?)?),
            "if" => {
                macro_rules! branch {
                    () => {{
                        let list = &mut parser::parse_list(input.next()?)?.into_iter().peekable();
                        parser::Parse::parse(list)
                    }};
                }

                Instruction::If(Reference::parse(input.next()?)?, branch!()?, branch!()?)
            }
            _ => return None,
        };

        parser::parse_end(input)?;

        Some(instruction)
    }
}

#[derive(Debug, Clone)]
pub enum Reference {
    Index(u64),
    Variable(String),
    Const(DataStatement),
}

impl Reference {
    fn parse(expr: atoms::StringValue) -> Option<Self> {
        if let Some(index) = Reference::parse_index(&expr) {
            Some(Reference::Index(index))
        } else {
            match expr {
                Value::Symbol(s) => Some(Reference::Variable(s)),
                Value::Cons(_, _) => {
                    let mut input = parser::parse_list(expr)?.into_iter().peekable();

                    let kind = match input.next()? {
                        Value::Symbol(s) => s,
                        _ => return None,
                    };

                    let reference = match kind.as_ref() {
                        "const" => Reference::Const(DataStatement::parse_body(&mut input)?),
                        _ => return None,
                    };

                    Some(reference)
                }
                _ => None,
            }
        }
    }

    fn parse_index(expr: &atoms::StringValue) -> Option<u64> {
        let s = match expr {
            Value::Symbol(s) => s,
            _ => return None,
        };

        if !matches!(s.chars().next(), Some('$')) {
            return None;
        }

        u64::from_str_radix(&s[1..], 10).ok()
    }
}

impl parser::Parse for Vec<Reference> {
    fn parse(input: &mut parser::Input) -> Option<Self> {
        let mut references = Vec::new();
        for expr in input {
            let instruction = Reference::parse(expr)?;
            references.push(instruction);
        }

        Some(references)
    }
}
