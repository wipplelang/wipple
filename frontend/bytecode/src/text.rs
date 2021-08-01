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
    Thunk(Loc<'a, Reference<'a>>, Vec<Loc<'a, Reference<'a>>>),
}

#[derive(Debug, Clone)]
pub enum Reference<'a> {
    Index(usize),
    Variable(Cow<'a, str>),
    Const(DataStatement<'a>),
}

pub struct File<'a> {
    pub source: Source<'a>,
    pub(crate) input: Input<'a>,
    pub(crate) errors: Vec<Error<'a>>,
}

impl<'a> File<'a> {
    pub fn parse(source: Source<'a>) -> (File<'a>, Option<ess::parser::ParseError>) {
        let (contents, err) = ess::parse(source.code);

        let file = File {
            source,
            input: contents.into_iter().peekable(),
            errors: Vec::new(),
        };

        (file, err)
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

pub(crate) type Input<'a> = Peekable<vec::IntoIter<Sexp<'a>>>;

trait Parse<'a>
where
    Self: Sized,
{
    fn parse(
        source: Source<'a>,
        input: &mut Input<'a>,
        span: Span<'a>,
        errors: &mut Vec<Error<'a>>,
    ) -> Option<Loc<'a, Self>>;
}

pub(crate) trait ParseExpr<'a>
where
    Self: Sized,
{
    fn parse_expr(
        source: Source<'a>,
        expr: Sexp<'a>,
        errors: &mut Vec<Error<'a>>,
    ) -> Option<Loc<'a, Self>>;
}

trait ParseStatement<'a>
where
    Self: Sized,
{
    const NAME: &'static str;

    fn parse_body(
        source: Source<'a>,
        input: &mut Input<'a>,
        span: Span<'a>,
        errors: &mut Vec<Error<'a>>,
    ) -> Option<Self>;

    fn parse(
        source: Source<'a>,
        input: &mut Input<'a>,
        span: Span<'a>,
        errors: &mut Vec<Error<'a>>,
    ) -> Option<Option<Loc<'a, Self>>> {
        let expr = match input.peek() {
            Some(expr) => expr,
            None => {
                errors.push(Error::new("Expected statement", "Not provided", span));
                return None;
            }
        };

        if !matches!(expr, Sexp::Sym(s, _) if s == Self::NAME) {
            return Some(None);
        }

        input.next();
        Some(Some(Loc::new(
            Self::parse_body(source, input, span, errors)?,
            span,
        )))
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

fn parse_end<'a>(
    source: Source<'a>,
    input: &mut Input<'a>,
    errors: &mut Vec<Error<'a>>,
) -> Option<()> {
    match input.peek() {
        Some(expr) => {
            errors.push(Error::new(
                "Expected end of statement",
                "Extra arguments",
                Span::closed(source, *expr.get_loc()),
            ));
            None
        }
        None => Some(()),
    }
}

impl<'a> Parse<'a> for Statement<'a> {
    fn parse(
        source: Source<'a>,
        input: &mut Input<'a>,
        span: Span<'a>,
        errors: &mut Vec<Error<'a>>,
    ) -> Option<Loc<'a, Self>> {
        if let Some(version) = VersionStatement::parse(source, input, span, errors)? {
            Some(version.map(Statement::Version))
        } else if let Some(block) = BlockStatement::parse(source, input, span, errors)? {
            Some(block.map(Statement::Block))
        } else if let Some(data) = DataStatement::parse(source, input, span, errors)? {
            Some(data.map(Statement::Data))
        } else if let Some(r#extern) = ExternStatement::parse(source, input, span, errors)? {
            Some(r#extern.map(Statement::Extern))
        } else if let Some(define) = DefineStatement::parse(source, input, span, errors)? {
            Some(define.map(Statement::Define))
        } else {
            errors.push(Error::new(
                "Invalid statement",
                "Expected version, block, data, extern or define statement",
                span,
            ));
            None
        }
    }
}

impl<'a> ParseExpr<'a> for Statement<'a> {
    fn parse_expr(
        source: Source<'a>,
        expr: Sexp<'a>,
        errors: &mut Vec<Error<'a>>,
    ) -> Option<Loc<'a, Self>> {
        let (mut input, span) = match expr {
            Sexp::List(list, loc) => (list.into_iter().peekable(), Span::closed(source, loc)),
            _ => {
                errors.push(Error::new(
                    "Expected statement",
                    "Statements must be in list form",
                    Span::closed(source, *expr.get_loc()),
                ));
                return None;
            }
        };

        Statement::parse(source, &mut input, span, errors)
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

    fn parse_body(
        source: Source<'a>,
        input: &mut Input<'a>,
        span: Span<'a>,
        errors: &mut Vec<Error<'a>>,
    ) -> Option<Self> {
        let (version, span) = match input.next() {
            Some(Sexp::Str(s, loc)) => (s, Span::closed(source, loc)),
            Some(expr) => {
                errors.push(Error::new(
                    "Expected version string",
                    "Invalid",
                    Span::closed(source, *expr.get_loc()),
                ));
                return None;
            }
            None => {
                errors.push(Error::new("Expected version string", "Not provided", span));
                return None;
            }
        };

        let version = match semver::Version::parse(&version) {
            Ok(version) => version,
            Err(err) => {
                errors.push(Error::new("Invalid version string", err, span));
                return None;
            }
        };

        parse_end(source, input, errors)?;

        Some(VersionStatement {
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

    fn parse_body(
        source: Source<'a>,
        input: &mut Input<'a>,
        _: Span<'a>,
        errors: &mut Vec<Error<'a>>,
    ) -> Option<Self> {
        let instructions = input
            .filter_map(|expr| ParseExpr::parse_expr(source, expr, errors))
            .collect();

        Some(BlockStatement { instructions })
    }
}

impl<'a> WriteList<'a> for BlockStatement<'a> {
    fn write_list(self, list: &mut Vec<Sexp<'a>>) {
        self.instructions.write_list(list)
    }
}

impl<'a> ParseExpr<'a> for Instruction<'a> {
    fn parse_expr(
        source: Source<'a>,
        expr: Sexp<'a>,
        errors: &mut Vec<Error<'a>>,
    ) -> Option<Loc<'a, Self>> {
        let (mut input, span) = match expr {
            Sexp::List(list, loc) => (list.into_iter().peekable(), Span::closed(source, loc)),
            expr => {
                errors.push(Error::new(
                    "Expected instruction",
                    "Instructions must be in list form",
                    Span::closed(source, *expr.get_loc()),
                ));
                return None;
            }
        };

        Instruction::parse(source, &mut input, span, errors)
    }
}

impl<'a> ParseStatement<'a> for DataStatement<'a> {
    const NAME: &'static str = "data";

    fn parse_body(
        source: Source<'a>,
        input: &mut Input<'a>,
        span: Span<'a>,
        errors: &mut Vec<Error<'a>>,
    ) -> Option<Self> {
        let (kind, span) = match input.next() {
            Some(Sexp::Sym(s, loc)) => (s, Span::closed(source, loc)),
            Some(expr) => {
                errors.push(Error::new(
                    "Expected data type",
                    "Invalid",
                    Span::closed(source, *expr.get_loc()),
                ));
                return None;
            }
            None => {
                errors.push(Error::new("Expected data type", "Not provided", span));
                return None;
            }
        };

        let value = match kind.as_ref() {
            "num" => match input.next() {
                Some(Sexp::Int(n, loc)) => {
                    DataStatement::Number(Loc::new(n as f64, Span::closed(source, loc)))
                }
                Some(Sexp::Float(n, loc)) => {
                    DataStatement::Number(Loc::new(n, Span::closed(source, loc)))
                }
                Some(expr) => {
                    errors.push(Error::new(
                        "Expected number",
                        "Invalid",
                        Span::closed(source, *expr.get_loc()),
                    ));
                    return None;
                }
                None => {
                    errors.push(Error::new("Expected number", "Not provided", span));
                    return None;
                }
            },
            "int" => match input.next() {
                Some(Sexp::Int(n, loc)) => {
                    DataStatement::Integer(Loc::new(n, Span::closed(source, loc)))
                }
                Some(expr) => {
                    errors.push(Error::new(
                        "Expected integer",
                        "Invalid",
                        Span::closed(source, *expr.get_loc()),
                    ));
                    return None;
                }
                None => {
                    errors.push(Error::new("Expected integer", "Not provided", span));
                    return None;
                }
            },
            "bin" => {
                let (s, span) = match input.next() {
                    Some(Sexp::Int(n, loc)) => (n.to_string(), Span::closed(source, loc)),
                    Some(expr) => {
                        errors.push(Error::new(
                            "Expected binary integer",
                            "Invalid",
                            Span::closed(source, *expr.get_loc()),
                        ));
                        return None;
                    }
                    None => {
                        errors.push(Error::new("Expected binary integer", "Not provided", span));
                        return None;
                    }
                };

                let n = match i64::from_str_radix(&s, 2) {
                    Ok(n) => n,
                    Err(err) => {
                        errors.push(Error::new("Invalid binary integer", err, span));
                        return None;
                    }
                };

                DataStatement::Integer(Loc::new(n, span))
            }
            "str" => match input.next() {
                Some(Sexp::Str(s, loc)) => {
                    DataStatement::String(Loc::new(s, Span::closed(source, loc)))
                }
                Some(expr) => {
                    errors.push(Error::new(
                        "Expected string",
                        "Invalid",
                        Span::closed(source, *expr.get_loc()),
                    ));
                    return None;
                }
                None => {
                    errors.push(Error::new("Expected string", "Not provided", span));
                    return None;
                }
            },
            "true" => DataStatement::Bool(Loc::new(true, span)),
            "false" => DataStatement::Bool(Loc::new(false, span)),
            "raw" => {
                let (s, span) = match input.next() {
                    Some(Sexp::Str(s, loc)) => (s, Span::closed(source, loc)),
                    Some(expr) => {
                        errors.push(Error::new(
                            "Invalid data",
                            "Expected string containing hex data",
                            Span::closed(source, *expr.get_loc()),
                        ));
                        return None;
                    }
                    None => {
                        errors.push(Error::new("Expected data", "Not provided", span));
                        return None;
                    }
                };

                let data = match hex::decode(s.as_ref()) {
                    Ok(data) => data,
                    Err(err) => {
                        errors.push(Error::new("Invalid data", err, span));
                        return None;
                    }
                };

                DataStatement::Raw(Loc::new(Cow::Owned(data), span))
            }
            _ => {
                errors.push(Error::new(
                    "Invalid data type",
                    "Expected 'num', 'int', 'bin', 'str', 'true', 'false' or 'raw'",
                    span,
                ));
                return None;
            }
        };

        parse_end(source, input, errors)?;

        Some(value)
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

    fn parse_body(
        source: Source<'a>,
        input: &mut Input<'a>,
        span: Span<'a>,
        errors: &mut Vec<Error<'a>>,
    ) -> Option<Self> {
        let object = match input.next() {
            Some(Sexp::Str(s, loc)) => Loc::new(s, Span::closed(source, loc)),
            Some(expr) => {
                errors.push(Error::new(
                    "Expected object name",
                    "Object name must be a string",
                    Span::closed(source, *expr.get_loc()),
                ));
                return None;
            }
            None => {
                errors.push(Error::new("Expected object name", "Not provided", span));
                return None;
            }
        };

        let symbol = match input.next() {
            Some(Sexp::Str(s, loc)) => Loc::new(s, Span::closed(source, loc)),
            Some(expr) => {
                errors.push(Error::new(
                    "Expected symbol name",
                    "Symbol name must be a string",
                    Span::closed(source, *expr.get_loc()),
                ));
                return None;
            }
            None => {
                errors.push(Error::new("Expected symbol name", "Not provided", span));
                return None;
            }
        };

        parse_end(source, input, errors)?;

        Some(ExternStatement { object, symbol })
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

    fn parse_body(
        source: Source<'a>,
        input: &mut Input<'a>,
        span: Span<'a>,
        errors: &mut Vec<Error<'a>>,
    ) -> Option<Self> {
        let name = match input.next() {
            Some(Sexp::Sym(s, loc)) => Loc::new(s, Span::closed(source, loc)),
            Some(expr) => {
                errors.push(Error::new(
                    "Expected variable name",
                    "Invalid",
                    Span::closed(source, *expr.get_loc()),
                ));
                return None;
            }
            None => {
                errors.push(Error::new("Expected variable name", "Not provided", span));
                return None;
            }
        };

        let definition = match input.peek() {
            None => Definition::Auto,
            Some(expr) => {
                let span = Span::closed(source, *expr.get_loc());

                if let Some(index) = Reference::parse_index(source, expr, errors)? {
                    Definition::Variable(index)
                } else {
                    let statement = Statement::parse(source, input, span, errors)?;

                    let (statement, span) = statement.into_parts();

                    match statement {
                        Statement::Data(statement) => Definition::Data(Loc::new(statement, span)),
                        Statement::Block(statement) => Definition::Block(Loc::new(statement, span)),
                        Statement::Extern(statement) => {
                            Definition::Extern(Loc::new(statement, span))
                        }
                        _ => {
                            errors.push(Error::new(
                                "Invalid definition",
                                "Expected data, block or extern",
                                span,
                            ));
                            return None;
                        }
                    }
                }
            }
        };

        parse_end(source, input, errors)?;

        Some(DefineStatement { name, definition })
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
        errors: &mut Vec<Error<'a>>,
    ) -> Option<Loc<'a, Self>> {
        let (kind, kind_span) = match input.next() {
            Some(Sexp::Sym(s, loc)) => (s, Span::closed(source, loc)),
            Some(expr) => {
                errors.push(Error::new(
                    "Expected instruction",
                    "Invalid",
                    Span::closed(source, *expr.get_loc()),
                ));
                return None;
            }
            None => {
                errors.push(Error::new("Expected instruction", "Not provided", span));
                return None;
            }
        };

        macro_rules! reference {
            () => {
                match input.next() {
                    Some(expr) => Reference::parse(source, expr, errors)?,
                    None => {
                        errors.push(Error::new("Expected reference", "Not provided", kind_span));
                        return None;
                    }
                };
            };
        }

        macro_rules! references {
            () => {
                input
                    .filter_map(|expr| Reference::parse(source, expr, errors))
                    .collect()
            };
        }

        let instruction = match kind.as_ref() {
            "def" => Instruction::Define(DefineStatement::parse_body(
                source, input, kind_span, errors,
            )?),
            "enter" => Instruction::Enter(reference!(), references!()),
            "exit" => Instruction::Exit(references!()),
            "call" => Instruction::Call(reference!(), references!()),
            "use" => Instruction::Use(reference!()),
            "thunk" => Instruction::Thunk(reference!(), references!()),
            _ => {
                errors.push(Error::new(
                    "Invalid instruction",
                    "Expected 'def', 'enter', 'exit', 'call', 'use', or 'follow'",
                    kind_span,
                ));
                return None;
            }
        };

        parse_end(source, input, errors)?;

        Some(Loc::new(instruction, span))
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
            Instruction::Thunk(_, _) => "thunk",
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
            Instruction::Thunk(reference, inputs) => {
                list.push(reference.into_inner().write());
                inputs.write_list(list);
            }
        }
    }
}

impl<'a> Reference<'a> {
    fn parse(
        source: Source<'a>,
        expr: ess::Sexp<'a>,
        errors: &mut Vec<Error<'a>>,
    ) -> Option<Loc<'a, Self>> {
        if let Some(index) = Reference::parse_index(source, &expr, errors)? {
            Some(index.map(Reference::Index))
        } else {
            match expr {
                Sexp::Sym(s, loc) => {
                    Some(Loc::new(Reference::Variable(s), Span::closed(source, loc)))
                }
                Sexp::List(list, loc) => {
                    let mut input = list.into_iter().peekable();
                    let span = Span::closed(source, loc);

                    let (kind, kind_span) = match input.next() {
                        Some(Sexp::Sym(s, loc)) => (s, Span::closed(source, loc)),
                        Some(expr) => {
                            errors.push(Error::new(
                                "Expected inline data",
                                "Invalid",
                                Span::closed(source, *expr.get_loc()),
                            ));
                            return None;
                        }
                        None => {
                            errors.push(Error::new("Expected inline data", "Not provided", span));
                            return None;
                        }
                    };

                    let reference = match kind.as_ref() {
                        "const" => Reference::Const(DataStatement::parse_body(
                            source, &mut input, kind_span, errors,
                        )?),
                        _ => {
                            errors.push(Error::new(
                                "Invalid inline data",
                                "Expected 'const'",
                                kind_span,
                            ));
                            return None;
                        }
                    };

                    Some(Loc::new(reference, span))
                }
                expr => {
                    errors.push(Error::new(
                        "Invalid reference",
                        "Expected index, variable name or inline data",
                        Span::closed(source, *expr.get_loc()),
                    ));
                    None
                }
            }
        }
    }

    fn parse_index(
        source: Source<'a>,
        expr: &ess::Sexp<'a>,
        errors: &mut Vec<Error<'a>>,
    ) -> Option<Option<Loc<'a, usize>>> {
        let (s, span) = match expr {
            Sexp::Sym(s, loc) => (s, Span::closed(source, *loc)),
            _ => return Some(None),
        };

        if !matches!(s.chars().next(), Some('$')) {
            return Some(None);
        }

        let s = &s[1..];

        let index = match s.parse() {
            Ok(index) => index,
            Err(err) => {
                errors.push(Error::new("Invalid variable index", err, span));
                return None;
            }
        };

        Some(Some(Loc::new(index, span)))
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
