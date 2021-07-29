use crate::{text::*, *};
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, collections::HashMap};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct File<'a> {
    pub version: semver::Version,
    pub globals: Vec<Data<'a>>,
    pub externs: Vec<Extern<'a>>,
    pub blocks: Vec<Block>,
}

impl File<'_> {
    pub fn new(version: semver::Version) -> Self {
        File {
            version,
            globals: Vec::new(),
            externs: Vec::new(),
            blocks: Vec::new(),
        }
    }
}

pub type Block = Vec<Instruction>;

pub type Data<'a> = Cow<'a, [u8]>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Extern<'a> {
    pub object: Cow<'a, str>,
    pub symbol: Cow<'a, str>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Instruction {
    Enter(Index, Vec<Index>),
    Exit(Vec<Index>),
    Call(Index, Vec<Index>),
    Use(Index),
    Thunk(Index, Vec<Index>),
}

pub type Index = usize;

impl<'a> File<'a> {
    pub fn compile(mut text_file: TextFile<'a>) -> Result<Self> {
        let version = match text_file
            .next()
            .ok_or_else(|| {
                Error::new(
                    "Expected version statement",
                    "Not provided",
                    Span::eof(text_file.source),
                )
            })??
            .into_parts()
        {
            (Statement::Version(statement), _) => statement.version,
            (_, span) => {
                return Err(Error::new(
                    "Expected version statement",
                    "Version statements must appear at the top of the file",
                    span,
                ))
            }
        };

        let mut bin_file = File::new(version.into_inner());
        let mut defines = HashMap::new();
        let mut blocks_to_resolve = Vec::new();

        for statement in text_file {
            let statement = statement?;

            let (statement, span) = statement.into_parts();

            match statement {
                Statement::Version(_) => {
                    return Err(Error::new(
                        "Unexpected version statement",
                        "Version statements are only allowed at the top of the file",
                        span,
                    ))
                }
                Statement::Block(statement) => {
                    blocks_to_resolve.push(statement);
                }
                Statement::Data(statement) => {
                    let data = parse_data(statement);
                    bin_file.globals.push(data);
                }
                Statement::Extern(statement) => {
                    let r#extern = parse_extern(statement);
                    bin_file.externs.push(r#extern);
                }
                Statement::Define(statement) => {
                    let name = statement.name.into_inner();

                    macro_rules! define {
                        ($value:expr, $section:expr) => {{
                            let index = $section.len();
                            $section.push($value);
                            defines.insert(name, index);
                        }};
                    }

                    match statement.definition {
                        Definition::Auto => return Err(Error::new(
                            "Unexpected variable definition",
                            "Automatic variable definitions are only allowed at the top of a block",
                            span,
                        )),
                        Definition::Variable(index) => {
                            defines.insert(name, index.into_inner());
                        }
                        Definition::Data(statement) => {
                            let data = parse_data(statement.into_inner());
                            define!(data, bin_file.globals)
                        }
                        Definition::Block(statement) => {
                            let index = blocks_to_resolve.len();
                            defines.insert(name, index);
                            blocks_to_resolve.push(statement.into_inner());
                        }
                        Definition::Extern(statement) => {
                            let r#extern = parse_extern(statement.into_inner());
                            define!(r#extern, bin_file.externs)
                        }
                    };
                }
            }
        }

        for statement in blocks_to_resolve {
            let block = parse_block(statement, &mut defines, &mut bin_file)?;
            bin_file.blocks.push(block);
        }

        Ok(bin_file)
    }
}

fn parse_block<'a>(
    statement: BlockStatement<'a>,
    defines: &mut HashMap<Cow<'a, str>, Index>,
    bin_file: &mut File<'a>,
) -> Result<'a, Block> {
    let mut locals = HashMap::new();
    let mut can_parse_define = true;

    macro_rules! resolve {
        ($reference:expr) => {{
            let (reference, span) = $reference.into_parts();

            match reference {
                Reference::Index(index) => Ok(index),
                Reference::Variable(variable) => locals
                    .get(&variable)
                    .or_else(|| defines.get(&variable))
                    .copied()
                    .ok_or_else(|| {
                        Error::new(
                            format!("Variable '{}' not found", variable),
                            "No such variable",
                            span,
                        )
                    }),
                Reference::Const(data) => {
                    bin_file.globals.push(parse_data(data));
                    Ok(bin_file.globals.len() - 1)
                }
            }
        }};
    }

    macro_rules! resolve_all {
        ($references:expr) => {
            $references
                .into_iter()
                .map(|r| resolve!(r))
                .collect::<Result<_>>()
        };
    }

    let mut block = Block::with_capacity(statement.instructions.len());

    for instruction in statement.instructions {
        let (instruction, span) = instruction.into_parts();

        match instruction {
            text::Instruction::Define(statement) => {
                if !can_parse_define {
                    return Err(Error::new(
                        "Unexpected define statement",
                        "Define statement may only be used at beginning of block",
                        span,
                    ));
                }

                let index = match statement.definition {
                    Definition::Auto => locals.len(),
                    _ => {
                        return Err(Error::new(
                            "Unexpected definition",
                            "Only auto definitions are allowed within blocks",
                            span,
                        ))
                    }
                };

                locals.insert(statement.name.into_inner(), index);
            }
            _ => {
                can_parse_define = false;

                let instruction = match instruction {
                    text::Instruction::Define(_) => unreachable!(),
                    text::Instruction::Enter(block, inputs) => {
                        Instruction::Enter(resolve!(block)?, resolve_all!(inputs)?)
                    }
                    text::Instruction::Exit(outputs) => Instruction::Exit(resolve_all!(outputs)?),
                    text::Instruction::Call(r#extern, inputs) => {
                        Instruction::Call(resolve!(r#extern)?, resolve_all!(inputs)?)
                    }
                    text::Instruction::Use(global) => Instruction::Use(resolve!(global)?),
                    text::Instruction::Thunk(block, inputs) => {
                        Instruction::Thunk(resolve!(block)?, resolve_all!(inputs)?)
                    }
                };

                block.push(instruction);
            }
        }
    }

    Ok(block)
}

fn parse_data(statement: DataStatement) -> Cow<[u8]> {
    match statement {
        DataStatement::Number(n) => Cow::Owned(n.into_inner().to_le_bytes().to_vec()),
        DataStatement::Integer(n) => Cow::Owned(n.into_inner().to_le_bytes().to_vec()),
        DataStatement::String(s) => match s.into_inner() {
            Cow::Owned(s) => Cow::Owned(s.into_bytes()),
            Cow::Borrowed(s) => Cow::Borrowed(s.as_bytes()),
        },
        DataStatement::Bool(b) => Cow::Owned((b.into_inner() as u8).to_le_bytes().to_vec()),
        DataStatement::Raw(data) => data.into_inner(),
    }
}

fn parse_extern(statement: ExternStatement) -> Extern {
    Extern {
        object: statement.object.into_inner(),
        symbol: statement.symbol.into_inner(),
    }
}

impl<'a> File<'a> {
    pub fn decompile(self) -> String {
        let mut statements = vec![Statement::Version(VersionStatement {
            version: Loc::new(self.version, Span::default()),
        })];

        for data in self.globals {
            statements.push(Statement::Data(DataStatement::Raw(Loc::new(
                data,
                Span::default(),
            ))))
        }

        for r#extern in self.externs {
            statements.push(Statement::Extern(ExternStatement {
                object: Loc::new(r#extern.object, Span::default()),
                symbol: Loc::new(r#extern.symbol, Span::default()),
            }))
        }

        for block in self.blocks {
            fn to_instruction<'a>(instruction: Instruction) -> Loc<'a, text::Instruction<'a>> {
                let to_reference = |index| Loc::new(Reference::Index(index), Span::default());

                let instruction = match instruction {
                    Instruction::Enter(block, inputs) => text::Instruction::Enter(
                        to_reference(block),
                        inputs.into_iter().map(to_reference).collect(),
                    ),
                    Instruction::Exit(outputs) => {
                        text::Instruction::Exit(outputs.into_iter().map(to_reference).collect())
                    }
                    Instruction::Call(r#extern, inputs) => text::Instruction::Call(
                        to_reference(r#extern),
                        inputs.into_iter().map(to_reference).collect(),
                    ),
                    Instruction::Use(index) => text::Instruction::Use(to_reference(index)),
                    Instruction::Thunk(index, inputs) => text::Instruction::Thunk(
                        to_reference(index),
                        inputs.into_iter().map(to_reference).collect(),
                    ),
                };

                Loc::new(instruction, Span::default())
            }

            statements.push(Statement::Block(BlockStatement {
                instructions: block.into_iter().map(to_instruction).collect(),
            }))
        }

        fn print(expr: ess::Sexp) -> Cow<str> {
            use ess::Sexp::*;

            match expr {
                Sym(s, _) => s,
                Str(s, _) => Cow::Owned(format!(r#""{}""#, s)),
                Char(c, _) => Cow::Owned(c.to_string()),
                Int(n, _) => Cow::Owned(n.to_string()),
                Float(n, _) => Cow::Owned(n.to_string()),
                List(l, _) => Cow::Owned(format!(
                    "({})",
                    l.into_iter().map(print).collect::<Vec<_>>().join(" ")
                )),
            }
        }

        statements
            .into_iter()
            .map(Write::write)
            .map(print)
            .collect::<Vec<_>>()
            .join("\n")
    }
}
