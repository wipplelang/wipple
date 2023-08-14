use crate::{
    ast,
    parse::{self, Attribute, Expr, ExprKind, File, ListLine},
    Driver, Fix, SingleFile,
};
use async_trait::async_trait;
use std::{
    fmt,
    sync::{atomic::AtomicBool, Arc},
};
use wipple_util::Backtrace;

pub fn format(code: &str) -> Option<String> {
    #[derive(Debug, Clone)]
    struct FormatDriver {
        valid: Arc<AtomicBool>,
    }

    #[async_trait]
    impl Driver for FormatDriver {
        type InternedString = String;
        type Path = ();
        type Span = ();
        type File = SingleFile;
        type Scope = ();

        fn intern(&self, s: impl AsRef<str>) -> Self::InternedString {
            s.as_ref().to_string()
        }

        fn make_path(&self, _path: Self::InternedString) -> Option<Self::Path> {
            Some(())
        }

        fn make_span(&self, _path: Self::Path, _range: std::ops::Range<usize>) -> Self::Span {}

        fn std_path(&self) -> Option<Self::Path> {
            unimplemented!()
        }

        async fn queue_files(&self, _source_path: Option<Self::Path>, _paths: Vec<Self::Path>) {
            unimplemented!()
        }

        async fn expand_file(
            &self,
            _source_file: Option<(Self::Path, Self::File)>,
            _source_span: Option<Self::Span>,
            _path: Self::Path,
            _expand: impl FnOnce(
                    Self::Path,
                    Self::File,
                ) -> futures::future::BoxFuture<'static, Arc<ast::File<Self>>>
                + Send
                + 'static,
        ) -> Option<Arc<ast::File<Self>>> {
            unimplemented!()
        }

        fn syntax_error_with(
            &self,
            _msgs: impl IntoIterator<Item = (Self::Span, String)>,
            _fix: Option<Fix>,
        ) {
            // TODO: Store syntax errors (right now they will also be reported by the compiler)
            self.valid
                .store(false, std::sync::atomic::Ordering::Relaxed);
        }

        fn backtrace(&self) -> Backtrace {
            Backtrace::empty()
        }
    }

    let driver = FormatDriver {
        valid: Arc::new(AtomicBool::new(true)),
    };

    let file = parse::parse(&driver, (), code);

    let valid = Arc::try_unwrap(driver.valid).unwrap().into_inner();

    valid.then(|| file.to_string())
}

const INDENT: &str = "  ";

impl<D: Driver> fmt::Display for File<D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(shebang) = &self.shebang {
            write!(f, "{}\n\n", shebang.as_ref())?;
        }

        for (index, line) in self.comments.iter().enumerate() {
            line.fmt(f, 0, false, index == 0)?;
            writeln!(f)?;
        }

        for attribute in &self.attributes {
            attribute.fmt(f, ("[[", "]]"), 0)?;
        }

        if !self.attributes.is_empty() {
            writeln!(f)?;
        }

        for (statement_index, statement) in self.statements.iter().enumerate() {
            for (line_index, line) in statement.lines.iter().enumerate() {
                let mut indent = 0;
                if line_index > 0 {
                    indent += 1;
                }

                line.fmt(f, indent, true, statement_index == 0 && line_index == 0)?;

                if line_index + 1 < statement.lines.len() {
                    write!(f, " \\")?;
                }

                writeln!(f)?;
            }
        }

        Ok(())
    }
}

impl<D: Driver> fmt::Display for Expr<D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt(f, 0)
    }
}

impl<D: Driver> Expr<D> {
    fn fmt(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        match &self.kind {
            ExprKind::Underscore => write!(f, "_")?,
            ExprKind::Placeholder(placeholder) => write!(f, "(*{}*)", placeholder.as_ref())?,
            ExprKind::Name(name, _) => write!(f, "{}", name.as_ref())?,
            ExprKind::QuoteName(name) => write!(f, "'{}", name.as_ref())?,
            ExprKind::RepeatName(name) => write!(f, "...{}", name.as_ref())?,
            ExprKind::Text(_, raw) => write!(f, "\"{}\"", raw.as_ref())?,
            ExprKind::Number(number) => write!(f, "{}", number.as_ref())?,
            ExprKind::Asset(raw) => write!(f, "`{}`", raw.as_ref())?,
            ExprKind::List(lines) => {
                write!(f, "(")?;

                if self.is_multiline(false) {
                    writeln!(f)?;

                    for (index, line) in lines.iter().enumerate() {
                        line.fmt(f, indent + 1, true, index == 0)?;
                        writeln!(f)?;
                    }

                    write!(f, "{}", INDENT.repeat(indent))?;
                } else if let Some(line) = lines.first() {
                    line.fmt(f, indent, false, false)?;
                }

                write!(f, ")")?;
            }
            ExprKind::RepeatList(lines) => {
                write!(f, "...(")?;

                if self.is_multiline(false) {
                    writeln!(f)?;

                    for (index, line) in lines.iter().enumerate() {
                        line.fmt(f, indent + 1, true, index == 0)?;
                        writeln!(f)?;
                    }

                    write!(f, "{}", INDENT.repeat(indent))?;
                } else if let Some(line) = lines.first() {
                    line.fmt(f, indent, false, false)?;
                }

                write!(f, ")")?;
            }
            ExprKind::Block(statements) => {
                write!(f, "{{")?;

                if self.is_multiline(true) {
                    writeln!(f)?;

                    for (statement_index, statement) in statements.iter().enumerate() {
                        for (line_index, line) in statement.lines.iter().enumerate() {
                            let mut indent = indent;
                            if line_index > 0 {
                                indent += 1;
                            }

                            line.fmt(f, indent + 1, true, statement_index == 0 && line_index == 0)?;

                            if line_index + 1 < statement.lines.len() {
                                write!(f, " \\")?;
                            }

                            writeln!(f)?;
                        }
                    }

                    write!(f, "{}", INDENT.repeat(indent))?;
                } else if let Some(statement) = statements.first() {
                    debug_assert!(statement.lines.len() == 1);

                    let line = statement.lines.first().unwrap();

                    if !line.exprs.is_empty() || line.comment.is_some() {
                        write!(f, " ")?;

                        statement
                            .lines
                            .first()
                            .unwrap()
                            .fmt(f, indent, false, false)?;

                        write!(f, " ")?;
                    }
                }

                write!(f, "}}")?;
            }
            ExprKind::SourceCode(code) => write!(f, "{}", code)?,
        }

        Ok(())
    }

    fn is_multiline(&self, in_block: bool) -> bool {
        match &self.kind {
            ExprKind::Placeholder(_)
            | ExprKind::Underscore
            | ExprKind::Name(_, _)
            | ExprKind::QuoteName(_)
            | ExprKind::RepeatName(_)
            | ExprKind::Number(_) => false,
            ExprKind::Text(_, raw) | ExprKind::Asset(raw) => raw.as_ref().contains('\n'),
            ExprKind::List(lines) | ExprKind::RepeatList(lines) => match lines.len() {
                0 => false,
                1 => lines.first().unwrap().is_multiline(in_block),
                _ => true,
            },
            ExprKind::Block(statements) => match statements.len() {
                0 => false,
                1 => {
                    let lines = &statements.first().unwrap().lines;

                    match lines.len() {
                        0 => false,
                        1 => {
                            let line = lines.first().unwrap();
                            (!line.exprs.is_empty() || line.comment.is_some())
                                && (line.leading_lines > 0 || line.is_multiline(true))
                        }
                        _ => true,
                    }
                }
                _ => true,
            },
            ExprKind::SourceCode(code) => code.contains('\n'),
        }
    }
}

impl<D: Driver> Attribute<D> {
    fn fmt(
        &self,
        f: &mut fmt::Formatter,
        (start, end): (&str, &str),
        indent: usize,
    ) -> fmt::Result {
        write!(f, "{}{}", INDENT.repeat(indent), start)?;

        for (index, expr) in self.exprs.iter().enumerate() {
            if index > 0 {
                write!(f, " ")?;
            }

            expr.fmt(f, indent)?;
        }

        write!(f, "{}", end)?;

        if let Some(comment) = &self.comment {
            write!(f, " --{}", comment.as_ref())?;
        }

        writeln!(f)?;

        Ok(())
    }
}

impl<D: Driver> ListLine<D> {
    fn fmt(
        &self,
        f: &mut fmt::Formatter,
        indent: usize,
        indent_self: bool,
        first_line: bool,
    ) -> fmt::Result {
        if !first_line && self.leading_lines > 0 {
            writeln!(f)?;
        }

        for attribute in &self.attributes {
            attribute.fmt(f, ("[", "]"), indent)?;
        }

        if indent_self {
            write!(f, "{}", INDENT.repeat(indent))?;
        }

        for (index, expr) in self.exprs.iter().enumerate() {
            if index > 0 {
                write!(f, " ")?;
            }

            expr.fmt(f, indent)?;
        }

        if let Some(comment) = &self.comment {
            if !self.exprs.is_empty() {
                write!(f, " ")?;
            }

            write!(f, "--{}", comment.as_ref())?;
        }

        Ok(())
    }

    fn is_multiline(&self, in_block: bool) -> bool {
        !self.attributes.is_empty()
            || (in_block
                && self
                    .exprs
                    .iter()
                    .filter(|expr| expr.is_multiline(in_block))
                    .count()
                    > 0)
    }
}
