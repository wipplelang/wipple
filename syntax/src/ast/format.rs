use crate::{ast::SyntaxError, parse, Driver};

pub trait Format<D: Driver> {
    fn format(self) -> Result<String, SyntaxError<D>>;
}

impl<D: Driver> Format<D> for parse::Expr<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(match self.kind {
            parse::ExprKind::Underscore => String::from("_"),
            parse::ExprKind::Name(name, _) => name.as_ref().to_string(),
            parse::ExprKind::QuoteName(name) => format!("'{}", name.as_ref()),
            parse::ExprKind::RepeatName(name) => format!("...{}", name.as_ref()),
            parse::ExprKind::Text(_, raw) => format!("\"{}\"", raw.as_ref()),
            parse::ExprKind::Number(number) => number.as_ref().to_string(),
            parse::ExprKind::List(lines) => format!(
                "({})",
                lines
                    .into_iter()
                    .flat_map(|line| line.exprs)
                    .map(|expr| expr.format())
                    .collect::<Result<Vec<_>, _>>()?
                    .join(" ")
            ),
            parse::ExprKind::RepeatList(lines) => format!(
                "...({})",
                lines
                    .into_iter()
                    .flat_map(|line| line.exprs)
                    .map(|expr| expr.format())
                    .collect::<Result<Vec<_>, _>>()?
                    .join(" ")
            ),
            parse::ExprKind::Block(statements) => format!(
                "{{\n{}\n}}",
                statements
                    .into_iter()
                    .map(|statement| Ok(statement
                        .line
                        .exprs
                        .into_iter()
                        .map(|expr| expr.format())
                        .collect::<Result<Vec<_>, _>>()?
                        .join(" ")))
                    .collect::<Result<Vec<_>, _>>()?
                    .join("\n")
            ),
        })
    }
}
