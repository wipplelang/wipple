use crate::{
    ast,
    parse::{self, Attribute, Expr, ExprKind, File, ListLine},
    CharIndex, Driver, Fix, SingleFile,
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
        type Span = ();
        type File = SingleFile;
        type Scope = ();

        fn intern(&self, s: impl AsRef<str>) -> Self::InternedString {
            s.as_ref().to_string()
        }

        fn make_span(
            &self,
            _path: Self::InternedString,
            _range: std::ops::Range<CharIndex>,
        ) -> Self::Span {
        }

        fn implicit_entrypoint_imports(&self) -> Vec<Self::InternedString> {
            unimplemented!()
        }

        fn implicit_dependency_imports(&self) -> Vec<Self::InternedString> {
            unimplemented!()
        }

        async fn queue_files(
            &self,
            _source_path: Option<Self::InternedString>,
            _paths: Vec<Self::InternedString>,
        ) {
            unimplemented!()
        }

        async fn expand_file(
            &self,
            _source_path: Option<Self::InternedString>,
            _source_file: Option<Self::File>,
            _source_span: Option<Self::Span>,
            _path: Self::InternedString,
            _expand: impl FnOnce(
                    Self::InternedString,
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

    let file = parse::parse(&driver, String::new(), code, parse::Options::default());

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
            line.fmt(f, 0, false, index == 0, true)?;
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

                line.fmt(
                    f,
                    indent,
                    true,
                    statement_index == 0 && line_index == 0,
                    true,
                )?;

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
        self.fmt(f, 0, false)
    }
}

impl<D: Driver> Expr<D> {
    fn fmt(&self, f: &mut fmt::Formatter, indent: usize, is_statement: bool) -> fmt::Result {
        match &self.kind {
            ExprKind::Underscore => write!(f, "_")?,
            ExprKind::Placeholder(placeholder) => write!(f, "{{%{}%}}", placeholder.as_ref())?,
            ExprKind::Name(name, _) => write!(f, "{}", name.as_ref())?,
            ExprKind::QuoteName(name) => write!(f, "'{}", name.as_ref())?,
            ExprKind::RepeatName(name) => write!(f, "...{}", name.as_ref())?,
            ExprKind::Text(text) => write!(f, "\"{}\"", text.raw().as_ref())?,
            ExprKind::Number(number) => write!(f, "{}", number.as_ref())?,
            ExprKind::Asset(raw) => write!(f, "`{}`", raw.as_ref())?,
            ExprKind::List(lines) => {
                let exprs = lines
                    .iter()
                    .flat_map(|line| &line.exprs)
                    .collect::<Vec<_>>();

                let single_expr = exprs.len() == 1
                    && !matches!(exprs.first().unwrap().kind, ExprKind::Name(_, _));

                if !is_statement || single_expr {
                    write!(f, "(")?;
                }

                for (index, expr) in exprs.iter().enumerate() {
                    if index > 0 {
                        write!(f, " ")?;
                    }

                    expr.fmt(f, indent, false)?;
                }

                if !is_statement || single_expr {
                    write!(f, ")")?;
                }
            }
            ExprKind::RepeatList(lines) => {
                let exprs = lines
                    .iter()
                    .flat_map(|line| &line.exprs)
                    .collect::<Vec<_>>();

                write!(f, "...(")?;

                for (index, expr) in exprs.iter().enumerate() {
                    if index > 0 {
                        write!(f, " ")?;
                    }

                    expr.fmt(f, indent, false)?;
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

                            line.fmt(
                                f,
                                indent + 1,
                                true,
                                statement_index == 0 && line_index == 0,
                                true,
                            )?;

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
                            .fmt(f, indent, false, false, true)?;

                        write!(f, " ")?;
                    }
                }

                write!(f, "}}")?;
            }
            ExprKind::RepeatBlock(lines) => {
                write!(f, "...{{")?;

                if self.is_multiline(false) {
                    writeln!(f)?;

                    for (index, line) in lines.iter().enumerate() {
                        line.fmt(f, indent + 1, true, index == 0, false)?;
                        writeln!(f)?;
                    }

                    write!(f, "{}", INDENT.repeat(indent))?;
                } else if let Some(line) = lines.first() {
                    write!(f, " ")?;

                    line.fmt(f, indent, false, false, false)?;

                    write!(f, " ")?;
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
            ExprKind::Text(text) => text.raw().as_ref().contains('\n'),
            ExprKind::Asset(raw) => raw.as_ref().contains('\n'),
            ExprKind::List(lines) | ExprKind::RepeatList(lines) | ExprKind::RepeatBlock(lines) => {
                match lines.len() {
                    0 => false,
                    1 => lines.first().unwrap().is_multiline(in_block),
                    _ => true,
                }
            }
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

            expr.fmt(f, indent, false)?;
        }

        write!(f, "{}", end)?;

        if let Some(comment) = &self.comment {
            write!(f, " -- {}", comment.as_ref().trim())?;
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
        in_block: bool,
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

            expr.fmt(f, indent, in_block && self.exprs.len() == 1)?;
        }

        if let Some(comment) = &self.comment {
            if !self.exprs.is_empty() {
                write!(f, " ")?;
            }

            write!(f, "-- {}", comment.as_ref().trim())?;
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
