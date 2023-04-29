#![allow(unused)] // Work in progress (TODO: REMOVE)

use crate::{analysis::ast, Compiler, FilePath};
use std::mem;

pub enum Error {
    Syntax(ast::SyntaxError),
    Io(std::io::Error),
    Other(anyhow::Error),
}

impl From<ast::SyntaxError> for Error {
    fn from(error: ast::SyntaxError) -> Self {
        Error::Syntax(error)
    }
}

impl From<&ast::SyntaxError> for Error {
    fn from(error: &ast::SyntaxError) -> Self {
        Error::Syntax(error.clone())
    }
}

impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Self {
        Error::Io(error)
    }
}

impl From<anyhow::Error> for Error {
    fn from(error: anyhow::Error) -> Self {
        Error::Other(error)
    }
}

pub type Result<T> = std::result::Result<T, Error>;

impl Compiler {
    pub async fn format(&self, file: FilePath, mut writer: impl std::io::Write) -> Result<()> {
        const LINE_WIDTH: usize = 100;

        let resolved_path = self.loader.resolve(file, None)?;
        let code = self.loader.load(resolved_path).await?;

        let prev_diagnostics = mem::take(&mut *self.diagnostics.diagnostics.lock());

        let file = self.parse(resolved_path, &code);

        let file = self
            .build_ast(file, ast::Options::default(), |_, _, _| {
                Box::pin(async move { None })
            })
            .await;

        *self.diagnostics.diagnostics.lock() = prev_diagnostics;

        fn format_file_attributes(attributes: &ast::FileAttributes) -> Result<pretty::RcDoc<()>> {
            todo!()
        }

        let attributes = format_file_attributes(&file.attributes)?;
        let statements = file
            .statements
            .iter()
            .map(|statement| {
                statement
                    .as_ref()
                    .map_err(Error::from)
                    .and_then(format_statement)
            })
            .collect::<Result<Vec<_>>>()?;

        let doc = attributes
            .append(pretty::RcDoc::hardline())
            .append(pretty::RcDoc::intersperse(
                statements,
                pretty::RcDoc::hardline(),
            ));

        doc.render(LINE_WIDTH, &mut writer)?;

        Ok(())
    }
}

fn format_assignment_pattern(pattern: &ast::AssignmentPattern) -> Result<pretty::RcDoc<()>> {
    todo!()
}

fn format_assignment_value(value: &ast::AssignmentValue) -> Result<pretty::RcDoc<()>> {
    todo!()
}

fn format_constant_type_annotation(
    annotation: &ast::ConstantTypeAnnotation,
) -> Result<pretty::RcDoc<()>> {
    todo!()
}

fn format_destructuring(destructuring: &ast::Destructuring) -> Result<pretty::RcDoc<()>> {
    todo!()
}

fn format_expression(expr: &ast::Expression) -> Result<pretty::RcDoc<()>> {
    todo!()
}

fn format_file_attribute(attribute: &ast::FileAttribute) -> Result<pretty::RcDoc<()>> {
    todo!()
}

fn format_pattern(pattern: &ast::Pattern) -> Result<pretty::RcDoc<()>> {
    todo!()
}

fn format_statement(statement: &ast::Statement) -> Result<pretty::RcDoc<()>> {
    fn format_statement_attributes(
        attributes: &ast::StatementAttributes,
    ) -> Result<pretty::RcDoc<()>> {
        todo!()
    }

    match statement {
        ast::Statement::Assign(statement) => {
            let attributes = format_statement_attributes(&statement.attributes)?;
            let assignment_pattern = format_assignment_pattern(statement.pattern.as_ref()?)?;
            let value = format_assignment_value(statement.value.as_ref()?)?;

            Ok(attributes.append(pretty::RcDoc::hardline()).append(
                assignment_pattern
                    .append(pretty::RcDoc::space())
                    .append(pretty::RcDoc::as_string(":"))
                    .append(pretty::RcDoc::line())
                    .append(value)
                    .group(),
            ))
        }
        ast::Statement::Annotate(statement) => {
            let attributes = format_statement_attributes(&statement.attributes)?;

            let value = match &statement.value {
                Ok((_, name)) => pretty::RcDoc::as_string(name.as_str()),
                Err(value) => format_expression(value.as_ref()?)?,
            };

            let annotation = format_constant_type_annotation(statement.annotation.as_ref()?)?;

            Ok(attributes.append(pretty::RcDoc::hardline()).append(
                (value.group())
                    .append(pretty::RcDoc::space())
                    .append(pretty::RcDoc::as_string("::"))
                    .append(pretty::RcDoc::line())
                    .append(annotation.group())
                    .group(),
            ))
        }
        ast::Statement::TypeFunction(statement) => todo!(),
        ast::Statement::Instance(statement) => todo!(),
        ast::Statement::Use(statement) => todo!(),
        ast::Statement::Expression(statement) => todo!(),
    }
}

fn format_statement_attribute(attribute: &ast::StatementAttribute) -> Result<pretty::RcDoc<()>> {
    todo!()
}

fn format_syntax_body(body: &ast::SyntaxBody) -> Result<pretty::RcDoc<()>> {
    todo!()
}

fn format_syntax_pattern(pattern: &ast::SyntaxPattern) -> Result<pretty::RcDoc<()>> {
    todo!()
}

fn format_syntax_rule(rule: &ast::SyntaxRule) -> Result<pretty::RcDoc<()>> {
    todo!()
}

fn format_type(ty: &ast::Type) -> Result<pretty::RcDoc<()>> {
    todo!()
}

fn format_type_body(body: &ast::TypeBody) -> Result<pretty::RcDoc<()>> {
    todo!()
}

fn format_type_member(member: &ast::TypeMember) -> Result<pretty::RcDoc<()>> {
    todo!()
}

fn format_type_pattern(pattern: &ast::TypePattern) -> Result<pretty::RcDoc<()>> {
    todo!()
}

fn format_when_arm(arm: &ast::WhenArm) -> Result<pretty::RcDoc<()>> {
    todo!()
}

fn format_when_body(body: &ast::WhenBody) -> Result<pretty::RcDoc<()>> {
    todo!()
}

fn format_when_pattern(pattern: &ast::WhenPattern) -> Result<pretty::RcDoc<()>> {
    todo!()
}

fn format_with_clause(clause: &ast::WithClause) -> Result<pretty::RcDoc<()>> {
    todo!()
}
