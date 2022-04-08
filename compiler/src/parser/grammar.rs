use super::{lexer::Token, Span};
use crate::{helpers::InternedString, FilePath};

#[derive(Debug, Clone)]
pub struct File {
    pub path: FilePath,
    pub span: Span,
    pub attributes: Vec<FileAttribute>,
    pub dependencies: Vec<Dependency>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct FileAttribute {
    pub span: Span,
    pub name: InternedString,
}

#[derive(Debug, Clone)]
pub struct Dependency {
    pub span: Span,
    pub name: InternedString,
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Type((Span, InternedString), TypeDeclaration),
    Trait((Span, InternedString), TraitDeclaration),
    Constant((Span, InternedString), ConstantDeclaration),
    Instance(Instance),
    Assign(Pattern, Expression),
    Expression(Expression),
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub trait_span: Span,
    pub trait_name: InternedString,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub span: Span,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Unit,
    Name(InternedString),
    Number(Decimal),
    Text(InternedString),
    FunctionInput,
    Block(Vec<Statement>),
    List(Vec<Expression>),
    Function(Pattern, Box<Expression>),
    When(Box<Expression>, Vec<Arm>),
    External(InternedString, InternedString, Vec<Expression>),
    Annotate(Box<Expression>, TypeAnnotation),
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub span: Span,
    pub pattern: Pattern,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub struct TypeAnnotation {
    pub span: Span,
    pub kind: TypeAnnotationKind,
}

#[derive(Debug, Clone)]
pub enum TypeAnnotationKind {
    Placeholder,
    Unit,
    Named(InternedString, Vec<TypeAnnotation>),
    Function(Box<TypeAnnotation>, Box<TypeAnnotation>),
}

#[derive(Debug, Clone)]
pub struct TypeParameter {
    pub span: Span,
    pub name: InternedString,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub span: Span,
    pub kind: PatternKind,
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    Name(InternedString),
    Wildcard,
}

#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    pub parameters: Vec<TypeParameter>,
    pub bounds: Vec<Bound>,
    pub kind: TypeKind,
}

#[derive(Debug, Clone)]
pub struct TraitDeclaration {
    pub parameters: Vec<TypeParameter>,
    pub bounds: Vec<Bound>,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Marker,
    Alias(TypeAnnotation),
    Structure(Vec<DataField>),
    Enumeration(Vec<DataVariant>),
}

#[derive(Debug, Clone)]
pub struct DataField {
    pub name: InternedString,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct DataVariant {
    pub name: InternedString,
    pub values: Vec<TypeAnnotation>,
}

#[derive(Debug, Clone)]
pub struct ConstantDeclaration {
    pub parameters: Vec<TypeParameter>,
    pub bounds: Vec<Bound>,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct Bound {
    pub span: Span,
    pub trait_name: InternedString,
    pub parameters: Vec<TypeAnnotation>,
}

pub use grammar::file;
use rust_decimal::Decimal;

peg::parser! {
    pub(super) grammar grammar() for [(Token, Span)] {
        pub rule file(path: FilePath, code: &str) -> File
            = attributes:(file_attribute() ** _)
              dependencies:(use_dependency() ** _)
              statements:statements()
              ![_]
            {
                File {
                    path,
                    span: Span::new(path, 0..code.len()),
                    attributes,
                    dependencies,
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

        rule use_dependency() -> Dependency
            = [(Token::Use, use_span)]
              _
              [(Token::Text(name), name_span)]
            {
                Dependency {
                    span: Span::join(use_span, name_span),
                    name,
                }
            }

        rule expression() -> Expression
            = name_expression()
            / number_expression()
            / text_expression()
            / grouped_expression()
            / block_expression()
            / expected!("expression")

        rule non_block_expression() -> Expression
            = name_expression()
            / number_expression()
            / text_expression()
            / grouped_expression()
            / expected!("expression")

        rule non_annotate_expression() -> Expression
            = name_expression()
            / number_expression()
            / text_expression()
            / grouped_expression()
            / block_expression()
            / expected!("expression")

        rule name_expression() -> Expression
            = [(Token::Name(name), span)]
            {
                Expression {
                    span,
                    kind: ExpressionKind::Name(name),
                }
            }
            / expected!("name")

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
            / external_expression()
            / annotate_expression()
            / expected!("expression")

        rule function_expression() -> Expression
            = input:pattern() _ [(Token::Arrow, _)] _ body:compound_expression()
            {
                Expression {
                    span: Span::join(input.span, body.span),
                    kind: ExpressionKind::Function(input, Box::new(body)),
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
                      [(Token::LeftBrace, _)]
                      arms:arms()
                      [(Token::RightBrace, right_brace_span)]
                      { (arms, right_brace_span) }
                  )
                  / ([(Token::LineBreak, _)]* / ![_]) { (Vec::new(), expr.span) }
              )
            {
                let (arms, right_brace_span) = arms;

                Expression {
                    span: Span::join(when_span, right_brace_span),
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

        rule external_expression() -> Expression
            = [(Token::External, external_span)]
              [(Token::Text(namespace), _)]
              [(Token::Text(identifier), identifier_span)]
              inputs:expression()+
            {
                Expression {
                    span: Span::join(external_span, identifier_span),
                    kind: ExpressionKind::External(namespace, identifier, inputs),
                }
            }

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

        rule r#type() -> TypeAnnotation
            = placeholder_type()
            / function_type()
            / unit_type()
            / named_type()
            / grouped_type()
            / expected!("type")

        rule unparameterized_path_type() -> TypeAnnotation
            = [(Token::Name(name), span)]
            {
                TypeAnnotation {
                    span,
                    kind: TypeAnnotationKind::Named(name, Vec::new()),
                }
            }
            / placeholder_type()
            / function_type()
            / grouped_type()
            / expected!("type")

        rule non_function_type() -> TypeAnnotation
            = placeholder_type()
            / named_type()
            / grouped_type()
            / expected!("type")

        rule placeholder_type() -> TypeAnnotation
            = [(Token::Underscore, span)]
            {
                TypeAnnotation {
                    span,
                    kind: TypeAnnotationKind::Placeholder,
                }
            }
            / expected!("placeholder type")

        rule unit_type() -> TypeAnnotation
            = [(Token::LeftParen, left_paren_span)] _ [(Token::RightParen, right_paren_span)]
            {
                TypeAnnotation {
                    span: Span::join(left_paren_span, right_paren_span),
                    kind: TypeAnnotationKind::Unit,
                }
            }
            / expected!("placeholder type")

        rule named_type() -> TypeAnnotation
            = [(Token::Name(name), name_span)]
              parameters:(
                  [(Token::Name(name), span)]
                  {
                      TypeAnnotation {
                          span,
                          kind: TypeAnnotationKind::Named(name, Vec::new())
                      }
                  }
                  / parameters:unparameterized_path_type()
              )*
            {
                TypeAnnotation {
                    span: Span::join(name_span, parameters.last().map(|ty| ty.span).unwrap_or(name_span)),
                    kind: TypeAnnotationKind::Named(name, parameters),
                }
            }
            / expected!("named type")

        rule function_type() -> TypeAnnotation
            = input:non_function_type()
              _
              [(Token::Arrow, _)]
              _
              output:r#type()
            {
                TypeAnnotation {
                    span: Span::join(input.span, output.span),
                    kind: TypeAnnotationKind::Function(Box::new(input), Box::new(output)),
                }
            }
            / expected!("function type")

        rule grouped_type() -> TypeAnnotation
            = [(Token::LeftParen, left_paren_span)]
              _
              ty:r#type()
              _
              [(Token::RightParen, right_paren_span)]
            {
                TypeAnnotation {
                    span: Span::join(left_paren_span, right_paren_span),
                    kind: ty.kind,
                }
            }
            / expected!("type")

        rule block_expression() -> Expression
            = [(Token::LeftBrace, left_brace_span)]
              statements:statements()
              [(Token::RightBrace, right_brace_span)]
            {
                Expression {
                    span: Span::join(left_brace_span, right_brace_span),
                    kind: ExpressionKind::Block(statements)
                }
            }
            / expected!("block")

        rule statements() -> Vec<Statement>
            = _ statements:(statement() ** ([(Token::LineBreak, _)]+)) _
            { statements }
            / expected!("statements")

        rule statement() -> Statement
            = type_alias_statement()
            / type_statement()
            / trait_statement()
            / constant_statement()
            / instance_statement()
            / assign_statement()
            / expression_statement()
            / expected!("statement")

        rule type_alias_statement() -> Statement
            = [(Token::Name(name), name_span)]
              [(Token::Colon, _)]
              parameters:type_parameter_introduction()?
              [(Token::Type, _)]
              ty:r#type()
            {
                let (parameters, bounds) = parameters.unwrap_or_default();

                Statement {
                    span: Span::join(name_span, ty.span),
                    kind: StatementKind::Type(
                        (name_span, name),
                        TypeDeclaration {
                            parameters,
                            bounds,
                            kind: TypeKind::Alias(ty),
                        }
                    ),
                }
            }
            / expected!("type declaration")

        rule type_statement() -> Statement
            = [(Token::Name(name), name_span)]
              [(Token::Colon, _)]
              parameters:type_parameter_introduction()?
              [(Token::Type, type_span)]
              kind:(
                  [(Token::LeftBrace, _)]
                  kind:type_kind()
                  [(Token::RightBrace, right_brace_span)]
                  { (kind, right_brace_span) }
              )?
            {
                let (parameters, bounds) = parameters.unwrap_or_default();

                Statement {
                    span: Span::join(name_span, kind.as_ref().map(|(_, span)| *span).unwrap_or(type_span)),
                    kind: StatementKind::Type(
                        (name_span, name),
                        TypeDeclaration {
                            parameters,
                            bounds,
                            kind: kind.map(|(kind, _)| kind).unwrap_or(TypeKind::Marker),
                        }
                    ),
                }
            }
            / expected!("type declaration")

        rule trait_statement() -> Statement
            = [(Token::Name(name), name_span)]
              [(Token::Colon, _)]
              parameters:type_parameter_introduction()?
              [(Token::Trait, _)]
              ty:r#type()
            {
                let (parameters, bounds) = parameters.unwrap_or_default();

                Statement {
                    span: Span::join(name_span, ty.span),
                    kind: StatementKind::Trait(
                        (name_span, name),
                        TraitDeclaration {
                            parameters,
                            bounds,
                            ty,
                        }
                    ),
                }
            }
            / expected!("trait declaration")

        rule type_parameter_introduction() -> (Vec<TypeParameter>, Vec<Bound>)
            = parameters:type_parameter()+
              bounds:bounds()?
              [(Token::DoubleArrow, _)]
            { (parameters, bounds.unwrap_or_default()) }
            / expected!("type parameters")

        rule type_parameter() -> TypeParameter
            = [(Token::Name(name), span)] {
                TypeParameter {
                    span,
                    name,
                }
            }
            / expected!("type parameter")

        rule bounds() -> Vec<Bound>
            = [(Token::Where, _)]
              bounds:bound()+
              { bounds }
            / expected!("bounds")

        rule bound() -> Bound
            = [(Token::LeftParen, left_paren_span)]
              [(Token::Name(trait_name), _)]
              parameters:r#type()*
              [(Token::RightParen, right_paren_span)]
            {
                Bound {
                    span: Span::join(left_paren_span, right_paren_span),
                    trait_name,
                    parameters,
                }
            }
            / expected!("bound")

        rule type_kind() -> TypeKind
            = _ fields:(type_field() ++ ([(Token::LineBreak, _)]+)) _
              { TypeKind::Structure(fields) }
            / _ variants:(type_variant() ++ ([(Token::LineBreak, _)]+)) _
              { TypeKind::Enumeration(variants) }
            / expected!("data structure fields or variants")

        rule type_field() -> DataField
            = [(Token::Name(name), _)] [(Token::DoubleColon, _)] ty:r#type()
            {
                DataField {
                    name,
                    ty,
                }
            }
            / expected!("data structure field")

        rule type_variant() -> DataVariant
            = [(Token::Name(name), _)] values:unparameterized_path_type()*
            {
                DataVariant {
                    name,
                    values,
                }
            }
            / expected!("data structure variant")

        rule constant_statement() -> Statement
          = [(Token::Name(name), name_span)]
            [(Token::DoubleColon, _)]
            parameters:type_parameter_introduction()?
            ty:r#type()
            {
                let (parameters, bounds) = parameters.unwrap_or_default();

                Statement {
                    span: Span::join(name_span, ty.span),
                    kind: StatementKind::Constant(
                        (name_span, name),
                        ConstantDeclaration {
                            parameters,
                            bounds,
                            ty,
                        },
                    ),
                }
            }
            / expected!("constant declaration")

        rule instance_statement() -> Statement
            = [(Token::Instance, instance_span)]
              [(Token::Name(trait_name), trait_span)]
              [(Token::Colon, _)]
              value:compound_expression()
              {
                  Statement {
                      span: Span::join(instance_span, value.span),
                      kind: StatementKind::Instance(Instance {
                          trait_span,
                          trait_name,
                          value,
                      }),
                  }
              }
              / expected!("instance declaration")

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
            = name_pattern()
            / wildcard_pattern()
            / expected!("pattern")

        rule name_pattern() -> Pattern
            = [(Token::Name(name), span)]
            {
                Pattern {
                    span,
                    kind: PatternKind::Name(name),
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
        Expression {
            span: Span::join(exprs.first().unwrap().span, exprs.last().unwrap().span),
            kind: ExpressionKind::List(exprs),
        }
    }
}
