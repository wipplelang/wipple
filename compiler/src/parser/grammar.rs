use super::{lexer::Token, Span};
use crate::helpers::InternedString;
use std::collections::VecDeque;

#[derive(Debug)]
pub struct File {
    pub name: InternedString,
    pub span: Span,
    pub attributes: Vec<FileAttribute>,
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct FileAttribute {
    pub span: Span,
    pub name: InternedString,
}

#[derive(Debug)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

#[derive(Debug)]
pub enum StatementKind {
    Data(InternedString, DataDeclaration),
    Function(InternedString, FunctionDeclaration),
    Template(InternedString, TemplateDeclaration),
    Constant(InternedString, ConstantDeclaration),
    Assign(Pattern, Expression),
    Expression(Expression),
}

#[derive(Debug)]
pub struct Expression {
    pub span: Span,
    pub kind: ExpressionKind,
}

#[derive(Debug)]
pub enum ExpressionKind {
    Unit,
    Path(Path),
    Text(InternedString),
    Number(f64),
    Block(Vec<Statement>),
    Call(Box<Expression>, Box<Expression>),
    Closure(Pattern, Box<Expression>),
    When(Box<Expression>, Vec<Arm>),
    Annotate(Box<Expression>, Type),
}

#[derive(Debug)]
pub struct Path {
    pub base: InternedString,
    pub components: Vec<PathComponent>,
}

#[derive(Debug)]
pub struct PathComponent {
    pub span: Span,
    pub kind: PathComponentKind,
}

#[derive(Debug)]
pub enum PathComponentKind {
    Name(InternedString),
    Number(f64),
}

#[derive(Debug)]
pub struct Arm {
    pub span: Span,
    pub pattern: Pattern,
    pub body: Expression,
}

#[derive(Debug)]
pub struct Type {
    pub span: Span,
    pub kind: TypeKind,
}

#[derive(Debug)]
pub enum TypeKind {
    Placeholder,
    Path(Path, Vec<Type>),
    Function(Box<Type>, Box<Type>),
}

#[derive(Debug)]
pub struct TypeParameter {
    pub span: Span,
    pub kind: TypeParameterKind,
}

#[derive(Debug)]
pub enum TypeParameterKind {
    Named(InternedString),
    Constrained(Vec<InternedString>, InternedString),
}

#[derive(Debug)]
pub struct Pattern {
    pub span: Span,
    pub kind: PatternKind,
}

#[derive(Debug)]
pub enum PatternKind {
    Path(Path),
    Wildcard,
}

#[derive(Debug)]
pub struct DataDeclaration {
    pub parameters: Vec<TypeParameter>,
    pub kind: DataKind,
}

#[derive(Debug)]
pub enum DataKind {
    Marker,
    Structure(Vec<DataField>),
    Enumeration(Vec<DataVariant>),
}

#[derive(Debug)]
pub struct DataField {
    pub name: InternedString,
    pub ty: Type,
}

#[derive(Debug)]
pub struct DataVariant {
    pub name: InternedString,
    pub values: Vec<Type>,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub parameters: Vec<TypeParameter>,
    pub input: Pattern,
    pub body: Expression,
}

#[derive(Debug)]
pub struct TemplateDeclaration {
    pub parameters: Vec<InternedString>,
    pub body: Expression,
}

#[derive(Debug)]
pub struct ConstantDeclaration {
    pub ty: Type,
}

pub use grammar::file;

peg::parser! {
    pub(super) grammar grammar() for [(Token, Span)] {
        pub rule file(name: InternedString, code: &str) -> File
            = attributes:(file_attribute() ** _) statements:statements() ![_]
            {
                File {
                    name,
                    span: Span::new(name, 0..code.len()),
                    attributes,
                    statements,
                }
            }
            / expected!("file")

        rule file_attribute() -> FileAttribute
            = [(Token::LeftColonBracket, left_colon_bracket_span)]
              _
              [(Token::Name(name), _)]
              _
              [(Token::RightColonBracket, right_colon_bracket_span)]
            {
                FileAttribute {
                    span: Span::join(left_colon_bracket_span, right_colon_bracket_span),
                    name,
                }
            }
            / expected!("file attribute")

        rule expression() -> Expression
            = path_expression()
            / number_expression()
            / text_expression()
            / grouped_expression()
            / block_expression()
            / expected!("expression")

        rule non_block_expression() -> Expression
            = path_expression()
            / number_expression()
            / text_expression()
            / grouped_expression()
            / expected!("expression")

        rule non_annotate_expression() -> Expression
            = path_expression()
            / number_expression()
            / text_expression()
            / grouped_expression()
            / block_expression()
            / expected!("expression")

        rule path_expression() -> Expression
            = path:path()
            {
                let (path, span) = path;

                Expression {
                    span,
                    kind: ExpressionKind::Path(path),
                }
            }
            / expected!("name")

        rule path() -> (Path, Span)
            = [(Token::Name(base), base_span)]
              components:(
                  [(Token::Slash, _)]
                  component:(
                        [(Token::Name(name), span)]
                        { PathComponent { span, kind: PathComponentKind::Name(name) } }
                      / [(Token::Number(number), span)]
                        { PathComponent { span, kind: PathComponentKind::Number(number) } }
                  )
                  { component }
              )*
            {
                let span = Span::join(base_span, components.last().map(|c| c.span).unwrap_or(base_span));
                (Path { base, components }, span)
            }

        rule text_expression() -> Expression
            = [(Token::Text(text), span)]
            {
                Expression {
                    span,
                    kind: ExpressionKind::Text(text),
                }
            }
            / expected!("text")

        rule number_expression() -> Expression
            = [(Token::Number(number), span)]
            {
                Expression {
                    span,
                    kind: ExpressionKind::Number(number),
                }
            }
            / expected!("number")

        rule grouped_expression() -> Expression
            = [(Token::LeftParen, left_paren_span)]
              _
              expr:grouped_compound_expression(left_paren_span.with_end(left_paren_span.end + 1))
              _
              [(Token::RightParen, right_paren_span)]
            {
                Expression {
                    span: Span::join(left_paren_span, right_paren_span),
                    kind: expr.kind,
                }
            }
            / expected!("expression")

        rule grouped_compound_expression(span: Span) -> Expression
            = raw_compound_expression() / (exprs:expression() ** _ { parse_compound_expr(exprs, || span) })
            / expected!("expression")

        rule compound_expression() -> Expression
            = raw_compound_expression() / (exprs:expression()+ { parse_compound_expr(exprs, || unreachable!()) })
            / expected!("expression")

        rule raw_compound_expression() -> Expression
            = function_expression()
            / when_expression()
            / annotate_expression()
            / expected!("expression")

        rule function_expression() -> Expression
            = input:pattern() _ [(Token::Arrow, _)] _ body:compound_expression()
            {
                Expression {
                    span: Span::join(input.span, body.span),
                    kind: ExpressionKind::Closure(input, Box::new(body)),
                }
            }
            / expected!("function")

        rule when_expression() -> Expression
            = [(Token::When, when_span)]
              _
              expr:expression()
              _
              arms:(
                  (
                      [(Token::Indent, _)]
                      arms:arms()
                      [(Token::Dedent, dedent_span)]
                      { (arms, dedent_span) }
                  )
                  / ([(Token::LineBreak, _)]* / ![_]) { (Vec::new(), expr.span) }
              )
            {
                let (arms, dedent_span) = arms;

                Expression {
                    span: Span::join(when_span, dedent_span),
                    kind: ExpressionKind::When(Box::new(expr), arms),
                }
            }
            / expected!("'when' expression")

        rule arms() -> Vec<Arm>
            = _ arms:(arm() ** ([(Token::LineBreak, _)]+)) _
            { arms }
            / expected!("'when' arms")

        rule arm() -> Arm
            = pattern:pattern() _ [(Token::Arrow, _)] _ body:compound_expression()
            {
                Arm {
                    span: Span::join(pattern.span, body.span),
                    pattern,
                    body,
                }
            }
            / expected!("'when' arm")

        rule annotate_expression() -> Expression
            = expr:non_annotate_expression()
              [(Token::DoubleColon, _)]
              ty:r#type()
            {
                Expression {
                    span: Span::join(expr.span, ty.span),
                    kind: ExpressionKind::Annotate(Box::new(expr), ty),
                }
            }
            / expected!("type annotation")

        rule r#type() -> Type
            = placeholder_type()
            / path_type()
            / function_type()
            / grouped_type()
            / expected!("type")

        rule unparameterized_path_type() -> Type
            = path:path()
            {
                let (path, span) = path;

                Type {
                    span,
                    kind: TypeKind::Path(path, Vec::new()),
                }
            }
            / placeholder_type()
            / function_type()
            / grouped_type()
            / expected!("type")

        rule non_function_type() -> Type
            = placeholder_type()
            / path_type()
            / grouped_type()
            / expected!("type")

        rule placeholder_type() -> Type
            = [(Token::Underscore, span)]
            {
                Type {
                    span,
                    kind: TypeKind::Placeholder,
                }
            }
            / expected!("placeholder type")

        rule path_type() -> Type
            = path:path()
              parameters:(
                  path:path()
                  {
                      let (path, span) = path;

                      Type {
                          span,
                          kind: TypeKind::Path(path, Vec::new())
                      }
                  }
                  / parameters:unparameterized_path_type()
              )*
            {
                let (path, path_span) = path;

                Type {
                    span: Span::join(path_span, parameters.last().map(|ty| ty.span).unwrap_or(path_span)),
                    kind: TypeKind::Path(path, parameters),
                }
            }
            / expected!("named type")

        rule function_type() -> Type
            = input:non_function_type()
              _
              [(Token::Arrow, _)]
              _
              output:r#type()
            {
                Type {
                    span: Span::join(input.span, output.span),
                    kind: TypeKind::Function(Box::new(input), Box::new(output)),
                }
            }
            / expected!("function type")

        rule grouped_type() -> Type
            = [(Token::LeftParen, left_paren_span)]
              _
              ty:r#type()
              _
              [(Token::RightParen, right_paren_span)]
            {
                Type {
                    span: Span::join(left_paren_span, right_paren_span),
                    kind: ty.kind,
                }
            }
            / expected!("type")

        rule block_expression() -> Expression
            = [(Token::Indent, indent_span)]
              statements:statements()
              [(Token::Dedent, dedent_span)]
            {
                Expression {
                    span: Span::join(indent_span, dedent_span),
                    kind: ExpressionKind::Block(statements)
                }
            }
            / expected!("block")

        rule statements() -> Vec<Statement>
            = _ statements:(statement() ** ([(Token::LineBreak, _)]+)) _
            { statements }
            / expected!("statements")

        rule statement() -> Statement
            = data_statement()
            / template_statement()
            / function_statement()
            / constant_statement()
            / assign_statement()
            / expression_statement()
            / expected!("statement")

        rule data_statement() -> Statement
            = [(Token::Name(name), name_span)]
              [(Token::Colon, _)]
              parameters:type_parameter_introduction()?
              [(Token::Data, data_span)]
              kind:(
                  [(Token::Indent, _)]
                  kind:data_kind()
                  [(Token::Dedent, dedent_span)]
                  { (kind, dedent_span) }
              )?
            {
                Statement {
                    span: Span::join(name_span, kind.as_ref().map(|(_, span)| *span).unwrap_or(data_span)),
                    kind: StatementKind::Data(
                        name,
                        DataDeclaration {
                            parameters: parameters.unwrap_or_default(),
                            kind: kind.map(|(kind, _)| kind).unwrap_or(DataKind::Marker),
                        }
                    ),
                }
            }
            / expected!("data declaration")

        rule type_parameter_introduction() -> Vec<TypeParameter>
            = parameters:type_parameter()+ [(Token::DoubleArrow, _)]
            { parameters }
            / expected!("type parameters")

        rule type_parameter() -> TypeParameter
            = [(Token::LeftParen, left_paren_span)]
              _
              names:(([(Token::Name(name), _)] { name }) ++ _)
              _
              [(Token::RightParen, right_paren_span)]
              {
                  let (name, traits) = names.split_last().unwrap();

                  TypeParameter {
                      span: Span::join(left_paren_span, right_paren_span),
                      kind: TypeParameterKind::Constrained(traits.to_vec(), *name)
                  }
              }
            / [(Token::Name(name), span)] {
                TypeParameter {
                    span,
                    kind: TypeParameterKind::Named(name)
                }
            }
            / expected!("type parameter")

        rule data_kind() -> DataKind
            = _ fields:(data_structure_field() ++ ([(Token::LineBreak, _)]+)) _
              { DataKind::Structure(fields) }
            / _ variants:(data_structure_variant() ++ ([(Token::LineBreak, _)]+)) _
              { DataKind::Enumeration(variants) }
            / expected!("data structure fields or variants")

        rule data_structure_field() -> DataField
            = [(Token::Name(name), _)] [(Token::DoubleColon, _)] ty:r#type()
            {
                DataField {
                    name,
                    ty,
                }
            }
            / expected!("data structure field")

        rule data_structure_variant() -> DataVariant
            = [(Token::Name(name), _)] values:unparameterized_path_type()*
            {
                DataVariant {
                    name,
                    values,
                }
            }
            / expected!("data structure variant")

        rule template_statement() -> Statement
            = [(Token::Name(name), name_span)]
              [(Token::Colon, _)]
              [(Token::Template, _)]
              parameters:(([(Token::Name(name), _)] { name }) ++ _)
              _
              [(Token::Arrow, _)]
              _
              body:compound_expression()
            {
                Statement {
                    span: Span::join(name_span, body.span),
                    kind: StatementKind::Template(
                        name,
                        TemplateDeclaration {
                            parameters,
                            body,
                        }
                    ),
                }
          }
          / expected!("function declaration")

        rule function_statement() -> Statement
            = [(Token::Name(name), name_span)]
            [(Token::Colon, _)]
            parameters:type_parameter_introduction()?
            input:pattern()
            _
            [(Token::Arrow, _)]
            _
            body:compound_expression()
          {
              Statement {
                  span: Span::join(name_span, body.span),
                  kind: StatementKind::Function(
                      name,
                      FunctionDeclaration {
                          parameters: parameters.unwrap_or_default(),
                          input,
                          body,
                      }
                  ),
              }
          }
          / expected!("function declaration")

        rule constant_statement() -> Statement
          = [(Token::Name(name), name_span)]
            [(Token::DoubleColon, _)]
            ty:r#type()
            {
                Statement {
                    span: Span::join(name_span, ty.span),
                    kind: StatementKind::Constant(name, ConstantDeclaration { ty }),
                }
            }

        rule assign_statement() -> Statement
            = pattern:pattern() _ [(Token::Colon, _)] _ value:compound_expression()
            {
                Statement {
                    span: Span::join(pattern.span, value.span),
                    kind: StatementKind::Assign(pattern, value),
                }
            }
            / expected!("assignment")

        rule expression_statement() -> Statement
            = expr:compound_expression()
            {
                Statement {
                    span: expr.span,
                    kind: StatementKind::Expression(expr),
                }
            }
            / expected!("expression")

        rule pattern() -> Pattern
            = path_pattern()
            / wildcard_pattern()
            / expected!("pattern")

        rule path_pattern() -> Pattern
            = path:path()
            {
                let (path, span) = path;

                Pattern {
                    span,
                    kind: PatternKind::Path(path),
                }
            }
            / expected!("name")

        rule wildcard_pattern() -> Pattern
            = [(Token::Underscore, span)]
            {
                Pattern {
                    span,
                    kind: PatternKind::Wildcard,
                }
            }
            / expected!("'_'")

        rule _()
            = quiet! { [(Token::LineBreak, _)]* }
            / expected!("line break")
    }
}

fn parse_compound_expr(exprs: Vec<Expression>, span: impl FnOnce() -> Span) -> Expression {
    if exprs.is_empty() {
        Expression {
            span: span(),
            kind: ExpressionKind::Unit,
        }
    } else {
        // TODO: Operator parsing

        let mut exprs = VecDeque::from(exprs);
        let expr = exprs.pop_front().unwrap();

        exprs.into_iter().fold(expr, |result, next| Expression {
            span: Span::join(result.span, next.span),
            kind: ExpressionKind::Call(Box::new(result), Box::new(next)),
        })
    }
}
