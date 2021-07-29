use crate::*;
use ess::Sexp;
use line_col::LineColLookup;
use std::{borrow::Cow, iter::Peekable, path::Path, vec};

#[derive(Debug, Clone)]
pub enum Statement<'a> {
    Version(VersionStatement<'a>),
    Block(BlockStatement<'a>),
    Data(DataStatement<'a>),
    Extern(ExternStatement<'a>),
    Define(DefineStatement<'a>),
}

#[derive(Debug, Clone)]
pub struct VersionStatement<'a> {
    pub version: Loc<'a, semver::Version>,
}

#[derive(Debug, Clone)]
pub struct BlockStatement<'a> {
    pub instructions: Vec<Loc<'a, Instruction<'a>>>,
}

#[derive(Debug, Clone)]
pub enum DataStatement<'a> {
    Number(Loc<'a, f64>),
    Integer(Loc<'a, i64>),
    String(Loc<'a, Cow<'a, str>>),
    Bool(Loc<'a, bool>),
    Raw(Loc<'a, Cow<'a, [u8]>>),
}

#[derive(Debug, Clone)]
pub struct ExternStatement<'a> {
    pub object: Loc<'a, Cow<'a, str>>,
    pub symbol: Loc<'a, Cow<'a, str>>,
}

#[derive(Debug, Clone)]
pub struct DefineStatement<'a> {
    pub name: Loc<'a, Cow<'a, str>>,
    pub definition: Definition<'a>,
}

#[derive(Debug, Clone)]
pub enum Definition<'a> {
    Auto,
    Variable(Loc<'a, usize>),
    Data(Loc<'a, DataStatement<'a>>),
    Block(Loc<'a, BlockStatement<'a>>),
    Extern(Loc<'a, ExternStatement<'a>>),
}

#[derive(Debug, Clone)]
pub enum Instruction<'a> {
    Define(DefineStatement<'a>),
    Enter(Loc<'a, Reference<'a>>, Vec<Loc<'a, Reference<'a>>>),
    Exit(Vec<Loc<'a, Reference<'a>>>),
    Call(Loc<'a, Reference<'a>>, Vec<Loc<'a, Reference<'a>>>),
    Use(Loc<'a, Reference<'a>>),
    If(
        Loc<'a, Reference<'a>>,
        Vec<Loc<'a, Instruction<'a>>>,
        Vec<Loc<'a, Instruction<'a>>>,
    ),
}

#[derive(Debug, Clone)]
pub enum Reference<'a> {
    Index(usize),
    Variable(Cow<'a, str>),
    Const(DataStatement<'a>),
}

pub struct File<'a> {
    pub source: Source<'a>,
    pub(crate) contents: vec::IntoIter<Sexp<'a>>,
}

impl<'a> File<'a> {
    pub fn parse(source: Source<'a>) -> Result<File<'a>> {
        let exprs = match ess::parse(source.code) {
            (exprs, None) => exprs,
            (_, Some(err)) => {
                use ess::parser::ParseError::*;

                let span = match err {
                    UnexpectedEof => Span::eof(source),
                    List(_, loc)
                    | Sexp(_, loc)
                    | Char(_, loc)
                    | String(_, loc)
                    | Symbol(_, loc)
                    | Number(_, loc) => Span::closed(source, loc),
                    Unexpected(_, loc) => Span::open(source, loc),
                };

                return Err(Error::new(
                    "Parse error",
                    format!("{:?}", err), // TODO: Better description
                    span,
                ));
            }
        };

        Ok(File {
            source,
            contents: exprs.into_iter(),
        })
    }
}

impl<'a> Iterator for File<'a> {
    type Item = Result<'a, Loc<'a, Statement<'a>>>;

    fn next(&mut self) -> Option<Self::Item> {
        let expr = self.contents.next()?;

        let (mut input, span) = match expr {
            Sexp::List(list, loc) => (list.into_iter().peekable(), Span::closed(self.source, loc)),
            _ => {
                return Some(Err(Error::new(
                    "Expected statement",
                    "Statements must be in list form",
                    Span::closed(self.source, *expr.get_loc()),
                )))
            }
        };

        Some(Statement::parse(self.source, &mut input, span))
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Source<'a> {
    pub file: Option<&'a Path>,
    pub code: &'a str,
}

impl<'a> Source<'a> {
    pub fn new(file: Option<&'a Path>, code: &'a str) -> Self {
        Source { file, code }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Span<'a> {
    pub source: Source<'a>,
    pub loc: Option<(usize, Option<usize>)>,
}

impl<'a> Span<'a> {
    pub fn open(source: Source<'a>, start: usize) -> Self {
        Span {
            source,
            loc: Some((start, None)),
        }
    }

    pub fn closed(source: Source<'a>, loc: (usize, usize)) -> Self {
        Span {
            source,
            loc: Some((loc.0, Some(loc.1))),
        }
    }

    pub fn eof(source: Source<'a>) -> Self {
        Span { source, loc: None }
    }

    pub fn start_line_col(&self) -> Option<(usize, usize)> {
        let start = match self.loc {
            Some((start, _)) => start,
            None => return None,
        };

        Some(LineColLookup::new(self.source.code).get(start))
    }

    pub fn end_line_col(&self) -> Option<(usize, usize)> {
        let end = match self.loc {
            Some((_, Some(end))) => end,
            _ => return None,
        };

        Some(LineColLookup::new(self.source.code).get(end))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Loc<'a, T>(T, Span<'a>);

impl<'a, T> Loc<'a, T> {
    pub fn new(inner: T, loc: Span<'a>) -> Self {
        Loc(inner, loc)
    }

    pub fn into_parts(self) -> (T, Span<'a>) {
        (self.0, self.1)
    }

    pub fn into_inner(self) -> T {
        self.0
    }

    pub fn loc(&self) -> Span<'a> {
        self.1
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Loc<'a, U> {
        Loc::new(f(self.0), self.1)
    }
}

type Input<'a> = Peekable<vec::IntoIter<Sexp<'a>>>;

trait Parse<'a>
where
    Self: Sized,
{
    fn parse(
        source: Source<'a>,
        input: &mut Input<'a>,
        span: Span<'a>,
    ) -> Result<'a, Loc<'a, Self>>;
}

trait ParseList<'a>
where
    Self: Sized,
{
    type Item: 'a;

    fn parse_list(
        source: Source<'a>,
        input: &mut Input<'a>,
    ) -> Result<'a, Vec<Loc<'a, Self::Item>>>;
}

trait ParseStatement<'a>
where
    Self: Sized,
{
    const NAME: &'static str;

    fn parse_body(source: Source<'a>, input: &mut Input<'a>, span: Span<'a>) -> Result<'a, Self>;

    fn parse(
        source: Source<'a>,
        input: &mut Input<'a>,
        span: Span<'a>,
    ) -> Result<'a, Option<Loc<'a, Self>>> {
        let expr = input
            .peek()
            .ok_or_else(|| Error::new("Expected statement", "Not provided", span))?;

        if !matches!(expr, Sexp::Sym(s, _) if s == Self::NAME) {
            return Ok(None);
        }

        input.next();
        Ok(Some(Loc::new(Self::parse_body(source, input, span)?, span)))
    }
}

pub(crate) trait Write<'a> {
    fn write(self) -> Sexp<'a>;
}

pub(crate) trait WriteList<'a> {
    fn write_list(self, list: &mut Vec<Sexp<'a>>);
}

impl<'a, T: WriteList<'a>> Write<'a> for T {
    fn write(self) -> Sexp<'a> {
        let mut list = Vec::new();
        self.write_list(&mut list);
        Sexp::List(list, Default::default())
    }
}

impl<'a, T: Write<'a>> WriteList<'a> for Vec<Loc<'a, T>> {
    fn write_list(self, list: &mut Vec<Sexp<'a>>) {
        for x in self {
            list.push(x.into_inner().write())
        }
    }
}

fn parse_end<'a>(source: Source<'a>, input: &mut Input<'a>) -> Result<'a, ()> {
    match input.peek() {
        Some(expr) => Err(Error::new(
            "Expected end of statement",
            "Extra arguments",
            Span::closed(source, *expr.get_loc()),
        )),
        None => Ok(()),
    }
}

impl<'a> Parse<'a> for Statement<'a> {
    fn parse(
        source: Source<'a>,
        input: &mut Input<'a>,
        span: Span<'a>,
    ) -> Result<'a, Loc<'a, Self>> {
        if let Some(version) = VersionStatement::parse(source, input, span)? {
            Ok(version.map(Statement::Version))
        } else if let Some(block) = BlockStatement::parse(source, input, span)? {
            Ok(block.map(Statement::Block))
        } else if let Some(data) = DataStatement::parse(source, input, span)? {
            Ok(data.map(Statement::Data))
        } else if let Some(r#extern) = ExternStatement::parse(source, input, span)? {
            Ok(r#extern.map(Statement::Extern))
        } else if let Some(define) = DefineStatement::parse(source, input, span)? {
            Ok(define.map(Statement::Define))
        } else {
            Err(Error::new(
                "Invalid statement",
                "Expected version, block, data, extern or define statement",
                span,
            ))
        }
    }
}

impl<'a> WriteList<'a> for Statement<'a> {
    fn write_list(self, list: &mut Vec<Sexp<'a>>) {
        let kind = match self {
            Statement::Version(_) => "version",
            Statement::Block(_) => "block",
            Statement::Data(_) => "data",
            Statement::Extern(_) => "extern",
            Statement::Define(_) => "def",
        };

        list.push(Sexp::Sym(Cow::Borrowed(kind), Default::default()));

        match self {
            Statement::Version(statement) => statement.write_list(list),
            Statement::Block(statement) => statement.write_list(list),
            Statement::Data(statement) => statement.write_list(list),
            Statement::Extern(statement) => statement.write_list(list),
            Statement::Define(statement) => statement.write_list(list),
        }
    }
}

impl<'a> ParseStatement<'a> for VersionStatement<'a> {
    const NAME: &'static str = "version";

    fn parse_body(source: Source<'a>, input: &mut Input<'a>, span: Span<'a>) -> Result<'a, Self> {
        let (version, span) = match input
            .next()
            .ok_or_else(|| Error::new("Expected version string", "Not provided", span))?
        {
            Sexp::Str(s, loc) => (s, Span::closed(source, loc)),
            expr => {
                return Err(Error::new(
                    "Expected version string",
                    "Invalid",
                    Span::closed(source, *expr.get_loc()),
                ))
            }
        };

        let version = semver::Version::parse(&version)
            .map_err(|err| Error::new("Invalid version string", err, span))?;

        parse_end(source, input)?;

        Ok(VersionStatement {
            version: Loc::new(version, span),
        })
    }
}

impl<'a> WriteList<'a> for VersionStatement<'a> {
    fn write_list(self, list: &mut Vec<Sexp<'a>>) {
        list.push(Sexp::Str(
            Cow::Owned(self.version.into_inner().to_string()),
            Default::default(),
        ))
    }
}

impl<'a> ParseStatement<'a> for BlockStatement<'a> {
    const NAME: &'static str = "block";

    fn parse_body(source: Source<'a>, input: &mut Input<'a>, _: Span<'a>) -> Result<'a, Self> {
        let instructions = Vec::<Instruction<'a>>::parse_list(source, input)?;

        Ok(BlockStatement { instructions })
    }
}

impl<'a> WriteList<'a> for BlockStatement<'a> {
    fn write_list(self, list: &mut Vec<Sexp<'a>>) {
        self.instructions.write_list(list)
    }
}

impl<'a> ParseList<'a> for Vec<Instruction<'a>> {
    type Item = Instruction<'a>;

    fn parse_list(
        source: Source<'a>,
        input: &mut Input<'a>,
    ) -> Result<'a, Vec<Loc<'a, Self::Item>>> {
        let mut instructions = Vec::new();

        for expr in input {
            let (mut input, span) = match expr {
                Sexp::List(list, loc) => (list.into_iter().peekable(), Span::closed(source, loc)),
                expr => {
                    return Err(Error::new(
                        "Expected instruction",
                        "Instructions must be in list form",
                        Span::closed(source, *expr.get_loc()),
                    ))
                }
            };

            let instruction = Instruction::parse(source, &mut input, span)?;
            instructions.push(instruction);
        }

        Ok(instructions)
    }
}

impl<'a> ParseStatement<'a> for DataStatement<'a> {
    const NAME: &'static str = "data";

    fn parse_body(source: Source<'a>, input: &mut Input<'a>, span: Span<'a>) -> Result<'a, Self> {
        let (kind, span) = match input
            .next()
            .ok_or_else(|| Error::new("Expected data type", "Not provided", span))?
        {
            Sexp::Sym(s, loc) => (s, Span::closed(source, loc)),
            expr => {
                return Err(Error::new(
                    "Expected data type",
                    "Invalid",
                    Span::closed(source, *expr.get_loc()),
                ))
            }
        };

        let value = match kind.as_ref() {
            "num" => match input
                .next()
                .ok_or_else(|| Error::new("Expected number", "Not provided", span))?
            {
                Sexp::Int(n, loc) => {
                    DataStatement::Number(Loc::new(n as f64, Span::closed(source, loc)))
                }
                Sexp::Float(n, loc) => {
                    DataStatement::Number(Loc::new(n, Span::closed(source, loc)))
                }
                expr => {
                    return Err(Error::new(
                        "Expected number",
                        "Invalid",
                        Span::closed(source, *expr.get_loc()),
                    ))
                }
            },
            "int" => match input
                .next()
                .ok_or_else(|| Error::new("Expected integer", "Not provided", span))?
            {
                Sexp::Int(n, loc) => DataStatement::Integer(Loc::new(n, Span::closed(source, loc))),
                expr => {
                    return Err(Error::new(
                        "Expected integer",
                        "Invalid",
                        Span::closed(source, *expr.get_loc()),
                    ))
                }
            },
            "bin" => {
                let (s, span) = match input
                    .next()
                    .ok_or_else(|| Error::new("Expected binary integer", "Not provided", span))?
                {
                    Sexp::Int(n, loc) => (n.to_string(), Span::closed(source, loc)),
                    expr => {
                        return Err(Error::new(
                            "Expected binary integer",
                            "Invalid",
                            Span::closed(source, *expr.get_loc()),
                        ))
                    }
                };

                let n = i64::from_str_radix(&s, 2)
                    .map_err(|err| Error::new("Invalid binary integer", err, span))?;

                DataStatement::Integer(Loc::new(n, span))
            }
            "str" => match input
                .next()
                .ok_or_else(|| Error::new("Expected string", "Not provided", span))?
            {
                Sexp::Str(s, loc) => DataStatement::String(Loc::new(s, Span::closed(source, loc))),
                expr => {
                    return Err(Error::new(
                        "Expected string",
                        "Invalid",
                        Span::closed(source, *expr.get_loc()),
                    ))
                }
            },
            "true" => DataStatement::Bool(Loc::new(true, span)),
            "false" => DataStatement::Bool(Loc::new(false, span)),
            "raw" => {
                let (s, span) = match input
                    .next()
                    .ok_or_else(|| Error::new("Expected data", "Not provided", span))?
                {
                    Sexp::Str(s, loc) => (s, Span::closed(source, loc)),
                    expr => {
                        return Err(Error::new(
                            "Invalid data",
                            "Expected string containing hex data",
                            Span::closed(source, *expr.get_loc()),
                        ))
                    }
                };

                let data =
                    hex::decode(s.as_ref()).map_err(|err| Error::new("Invalid data", err, span))?;

                DataStatement::Raw(Loc::new(Cow::Owned(data), span))
            }
            _ => {
                return Err(Error::new(
                    "Invalid data type",
                    "Expected 'num', 'int', 'bin', 'str', 'true', 'false' or 'raw'",
                    span,
                ))
            }
        };

        parse_end(source, input)?;

        Ok(value)
    }
}

impl<'a> WriteList<'a> for DataStatement<'a> {
    fn write_list(self, list: &mut Vec<Sexp<'a>>) {
        let kind = match self {
            DataStatement::Number(_) => "num",
            DataStatement::Integer(_) => "int",
            DataStatement::String(_) => "str",
            DataStatement::Bool(b) => {
                if b.into_inner() {
                    "true"
                } else {
                    "false"
                }
            }
            DataStatement::Raw(_) => "raw",
        };

        list.push(Sexp::Sym(Cow::Borrowed(kind), Default::default()));

        match self {
            DataStatement::Number(n) => list.push(Sexp::Float(n.into_inner(), Default::default())),
            DataStatement::Integer(n) => list.push(Sexp::Int(n.into_inner(), Default::default())),
            DataStatement::String(s) => list.push(Sexp::Str(s.into_inner(), Default::default())),
            DataStatement::Bool(_) => {}
            DataStatement::Raw(data) => {
                let data = hex::encode(data.into_inner());
                list.push(Sexp::Str(Cow::Owned(data), Default::default()))
            }
        }
    }
}

impl<'a> ParseStatement<'a> for ExternStatement<'a> {
    const NAME: &'static str = "extern";

    fn parse_body(source: Source<'a>, input: &mut Input<'a>, span: Span<'a>) -> Result<'a, Self> {
        let object = match input
            .next()
            .ok_or_else(|| Error::new("Expected object name", "Not provided", span))?
        {
            Sexp::Str(s, loc) => Loc::new(s, Span::closed(source, loc)),
            expr => {
                return Err(Error::new(
                    "Expected object name",
                    "Object name must be a string",
                    Span::closed(source, *expr.get_loc()),
                ))
            }
        };

        let symbol = match input
            .next()
            .ok_or_else(|| Error::new("Expected symbol name", "Not provided", span))?
        {
            Sexp::Str(s, loc) => Loc::new(s, Span::closed(source, loc)),
            expr => {
                return Err(Error::new(
                    "Expected symbol name",
                    "Symbol name must be a string",
                    Span::closed(source, *expr.get_loc()),
                ))
            }
        };

        parse_end(source, input)?;

        Ok(ExternStatement { object, symbol })
    }
}

impl<'a> WriteList<'a> for ExternStatement<'a> {
    fn write_list(self, list: &mut Vec<Sexp<'a>>) {
        list.push(Sexp::Str(self.object.into_inner(), Default::default()));
        list.push(Sexp::Str(self.symbol.into_inner(), Default::default()));
    }
}

impl<'a> ParseStatement<'a> for DefineStatement<'a> {
    const NAME: &'static str = "def";

    fn parse_body(source: Source<'a>, input: &mut Input<'a>, span: Span<'a>) -> Result<'a, Self> {
        let name = match input
            .next()
            .ok_or_else(|| Error::new("Expected variable name", "Not provided", span))?
        {
            Sexp::Sym(s, loc) => Loc::new(s, Span::closed(source, loc)),
            expr => {
                return Err(Error::new(
                    "Expected variable name",
                    "Invalid",
                    Span::closed(source, *expr.get_loc()),
                ))
            }
        };

        let definition = match input.peek() {
            None => Definition::Auto,
            Some(expr) => {
                let span = Span::closed(source, *expr.get_loc());

                if let Some(index) = Reference::parse_index(source, &expr)? {
                    Definition::Variable(index)
                } else {
                    let statement = Statement::parse(source, input, span)?;

                    let (statement, span) = statement.into_parts();

                    match statement {
                        Statement::Data(statement) => Definition::Data(Loc::new(statement, span)),
                        Statement::Block(statement) => Definition::Block(Loc::new(statement, span)),
                        Statement::Extern(statement) => {
                            Definition::Extern(Loc::new(statement, span))
                        }
                        _ => {
                            return Err(Error::new(
                                "Invalid definition",
                                "Expected data, block or extern",
                                span,
                            ))
                        }
                    }
                }
            }
        };

        parse_end(source, input)?;

        Ok(DefineStatement { name, definition })
    }
}

impl<'a> WriteList<'a> for DefineStatement<'a> {
    fn write_list(self, list: &mut Vec<Sexp<'a>>) {
        list.push(Sexp::Sym(self.name.into_inner(), Default::default()));

        match self.definition {
            Definition::Auto => {}
            Definition::Variable(index) => list.push(Reference::Index(index.into_inner()).write()),
            Definition::Data(statement) => {
                list.push(Sexp::Sym(Cow::Borrowed("data"), Default::default()));
                statement.into_inner().write_list(list);
            }
            Definition::Block(statement) => {
                list.push(Sexp::Sym(Cow::Borrowed("block"), Default::default()));
                statement.into_inner().write_list(list);
            }
            Definition::Extern(statement) => {
                list.push(Sexp::Sym(Cow::Borrowed("extern"), Default::default()));
                statement.into_inner().write_list(list);
            }
        }
    }
}

impl<'a> Parse<'a> for Instruction<'a> {
    fn parse(
        source: Source<'a>,
        input: &mut Input<'a>,
        span: Span<'a>,
    ) -> Result<'a, Loc<'a, Self>> {
        let (kind, kind_span) = match input
            .next()
            .ok_or_else(|| Error::new("Expected instruction", "Not provided", span))?
        {
            Sexp::Sym(s, loc) => (s, Span::closed(source, loc)),
            expr => {
                return Err(Error::new(
                    "Expected instruction",
                    "Invalid",
                    Span::closed(source, *expr.get_loc()),
                ))
            }
        };

        let instruction = match kind.as_ref() {
            "def" => Instruction::Define(DefineStatement::parse_body(source, input, kind_span)?),
            "enter" => Instruction::Enter(
                Reference::parse(
                    source,
                    input.next().ok_or_else(|| {
                        Error::new("Expected reference", "Not provided", kind_span)
                    })?,
                )?,
                Vec::<Reference>::parse_list(source, input)?,
            ),
            "exit" => Instruction::Exit(Vec::<Reference>::parse_list(source, input)?),
            "call" => Instruction::Call(
                Reference::parse(
                    source,
                    input.next().ok_or_else(|| {
                        Error::new("Expected reference", "Not provided", kind_span)
                    })?,
                )?,
                Vec::<Reference>::parse_list(source, input)?,
            ),
            "use" => Instruction::Use(Reference::parse(
                source,
                input
                    .next()
                    .ok_or_else(|| Error::new("Expected reference", "Not provided", kind_span))?,
            )?),
            "if" => {
                macro_rules! branch {
                    () => {{
                        let mut input = match input.next().ok_or_else(|| {
                            Error::new("Expected list of instructions", "Not provided", span)
                        })? {
                            Sexp::List(list, _) => list.into_iter().peekable(),
                            expr => {
                                return Err(Error::new(
                                    "Expected list of instructions",
                                    "Invalid",
                                    Span::closed(source, *expr.get_loc()),
                                ))
                            }
                        };

                        Vec::<Instruction>::parse_list(source, &mut input)?
                    }};
                }

                Instruction::If(
                    Reference::parse(
                        source,
                        input.next().ok_or_else(|| {
                            Error::new("Expected reference", "Not provided", kind_span)
                        })?,
                    )?,
                    branch!(),
                    branch!(),
                )
            }
            _ => {
                return Err(Error::new(
                    "Invalid instruction",
                    "Expected 'def', 'enter', 'exit', 'call', 'use', or 'if'",
                    kind_span,
                ))
            }
        };

        parse_end(source, input)?;

        Ok(Loc::new(instruction, span))
    }
}

impl<'a> WriteList<'a> for Instruction<'a> {
    fn write_list(self, list: &mut Vec<Sexp<'a>>) {
        let kind = match self {
            Instruction::Define(_) => "def",
            Instruction::Enter(_, _) => "enter",
            Instruction::Exit(_) => "exit",
            Instruction::Call(_, _) => "call",
            Instruction::Use(_) => "use",
            Instruction::If(_, _, _) => "if",
        };

        list.push(Sexp::Sym(Cow::Borrowed(kind), Default::default()));

        match self {
            Instruction::Define(statement) => statement.write_list(list),
            Instruction::Enter(block, inputs) => {
                list.push(block.into_inner().write());
                inputs.write_list(list);
            }
            Instruction::Exit(outputs) => outputs.write_list(list),
            Instruction::Call(r#extern, inputs) => {
                list.push(r#extern.into_inner().write());
                inputs.write_list(list);
            }
            Instruction::Use(reference) => list.push(reference.into_inner().write()),
            Instruction::If(condition, then, r#else) => {
                list.push(condition.into_inner().write());
                list.push(then.write());
                list.push(r#else.write());
            }
        }
    }
}

impl<'a> Reference<'a> {
    fn parse(source: Source<'a>, expr: ess::Sexp<'a>) -> Result<'a, Loc<'a, Self>> {
        if let Some(index) = Reference::parse_index(source, &expr)? {
            Ok(index.map(Reference::Index))
        } else {
            match expr {
                Sexp::Sym(s, loc) => {
                    Ok(Loc::new(Reference::Variable(s), Span::closed(source, loc)))
                }
                Sexp::List(list, loc) => {
                    let mut input = list.into_iter().peekable();
                    let span = Span::closed(source, loc);

                    let (kind, kind_span) = match input
                        .next()
                        .ok_or_else(|| Error::new("Expected inline data", "Not provided", span))?
                    {
                        Sexp::Sym(s, loc) => (s, Span::closed(source, loc)),
                        expr => {
                            return Err(Error::new(
                                "Expected inline data",
                                "Invalid",
                                Span::closed(source, *expr.get_loc()),
                            ))
                        }
                    };

                    let reference = match kind.as_ref() {
                        "const" => Reference::Const(DataStatement::parse_body(
                            source, &mut input, kind_span,
                        )?),
                        _ => {
                            return Err(Error::new(
                                "Invalid inline data",
                                "Expected 'const'",
                                kind_span,
                            ))
                        }
                    };

                    Ok(Loc::new(reference, span))
                }
                expr => Err(Error::new(
                    "Invalid reference",
                    "Expected index, variable name or inline data",
                    Span::closed(source, *expr.get_loc()),
                )),
            }
        }
    }

    fn parse_index(source: Source<'a>, expr: &ess::Sexp<'a>) -> Result<'a, Option<Loc<'a, usize>>> {
        let (s, span) = match expr {
            Sexp::Sym(s, loc) => (s, Span::closed(source, *loc)),
            _ => return Ok(None),
        };

        if !matches!(s.chars().next(), Some('$')) {
            return Ok(None);
        }

        let index = usize::from_str_radix(&s[1..], 10)
            .map_err(|err| Error::new("Invalid variable index", err, span))?;

        Ok(Some(Loc::new(index, span)))
    }
}

impl<'a> Write<'a> for Reference<'a> {
    fn write(self) -> Sexp<'a> {
        match self {
            Reference::Index(index) => {
                Sexp::Sym(Cow::Owned(format!("${}", index)), Default::default())
            }
            Reference::Variable(name) => Sexp::Sym(name, Default::default()),
            Reference::Const(data) => {
                let mut list = vec![Sexp::Sym(Cow::Borrowed("const"), Default::default())];
                data.write_list(&mut list);
                Sexp::List(list, Default::default())
            }
        }
    }
}

impl<'a> ParseList<'a> for Vec<Reference<'a>> {
    type Item = Reference<'a>;

    fn parse_list(
        source: Source<'a>,
        input: &mut Input<'a>,
    ) -> Result<'a, Vec<Loc<'a, Self::Item>>> {
        let mut references = Vec::new();

        for expr in input {
            let instruction = Reference::parse(source, expr)?;
            references.push(instruction);
        }

        Ok(references)
    }
}
