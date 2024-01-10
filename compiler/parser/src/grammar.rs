use crate::reader::{BinaryOperatorInput, Node, Token, TokenKind, VariadicOperatorInput};
use serde::Deserialize;
use std::{collections::HashMap, str::FromStr};

macro_rules! grammar {
    ($(static $name:ident = $grammar:expr;)*) => {
        ::lazy_static::lazy_static! {
            $(
                static ref $name: $crate::grammar::Grammar =
                    $grammar.parse().expect("invalid grammar");
            )*
        }
    };
}

pub(crate) use grammar;

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Grammar {
    Variable(String),
    Match(String, Box<Grammar>),
    Empty,
    Symbol(String),
    RepeatList(Box<Grammar>),
    RepeatBlock(Box<Grammar>),
    List(Vec<Grammar>),
    PrefixList(Vec<Grammar>),
    Block(Vec<Grammar>),
    BinaryOperator(String, BinaryOperatorInputGrammar),
    NonAssociativeBinaryOperator(String, Box<Grammar>, Box<Grammar>),
    VariadicOperator(String, VariadicOperatorInputGrammar),
    Or(Vec<Grammar>),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum BinaryOperatorInputGrammar {
    Unapplied,
    PartiallyAppliedLeft(Box<Grammar>),
    PartiallyAppliedRight(Box<Grammar>),
    Applied(Box<Grammar>, Box<Grammar>),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum VariadicOperatorInputGrammar {
    Unapplied,
    Applied(Box<Grammar>),
}

pub type Error = serde_lexpr::Error;

impl FromStr for Grammar {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        serde_lexpr::from_str(s)
    }
}

impl Grammar {
    pub fn parse<'src>(&self, node: &Node<'src>) -> Option<HashMap<&str, Vec<Node<'src>>>> {
        #[must_use]
        fn parse<'a, 'src>(
            g: &'a Grammar,
            n: &Node<'src>,
            vars: &mut HashMap<&'a str, Vec<Node<'src>>>,
        ) -> Option<()> {
            let previous_vars = vars.clone();

            let result = (|| match (g, n) {
                (Grammar::Variable(var), n) => {
                    vars.entry(var).or_default().push(n.clone());
                    Some(())
                }
                (Grammar::Match(var, g), n) => {
                    parse(g, n, vars)?;
                    vars.entry(var).or_default().push(n.clone());
                    Some(())
                }
                (Grammar::Empty, Node::Empty) => Some(()),
                (
                    Grammar::Symbol(a),
                    Node::Token(Token {
                        kind: TokenKind::Symbol(b),
                        ..
                    }),
                ) if a == b => Some(()),
                (Grammar::List(gs), Node::List(_, ns)) => {
                    if gs.len() != ns.len() {
                        return None;
                    }

                    gs.iter().zip(ns).map(|(g, n)| parse(g, n, vars)).collect()
                }
                (Grammar::RepeatList(g), Node::List(_, ns)) => {
                    ns.iter().map(|n| parse(g, n, vars)).collect()
                }
                (Grammar::PrefixList(gs), Node::List(_, ns)) => {
                    let mut ns = ns.iter();
                    let mut ps = gs.iter().zip(ns.by_ref());

                    let (g, n) = ps.next()?;
                    parse(g, n, vars)?;

                    for (g, n) in ps {
                        let _ = parse(g, n, vars);
                    }

                    let extra = vars.entry("__extra").or_default();
                    for n in ns {
                        extra.push(n.clone());
                    }

                    Some(())
                }
                (Grammar::Block(gs), Node::Block(_, ns)) => {
                    if gs.len() != ns.len() {
                        return None;
                    }

                    gs.iter().zip(ns).map(|(g, n)| parse(g, n, vars)).collect()
                }
                (Grammar::RepeatBlock(g), Node::Block(_, ns)) => {
                    ns.iter().map(|n| parse(g, n, vars)).collect()
                }
                (
                    Grammar::BinaryOperator(a, gi),
                    Node::BinaryOperator(
                        _,
                        Token {
                            kind: TokenKind::Symbol(b),
                            ..
                        },
                        ni,
                    ),
                ) if a == b => match (gi, ni) {
                    (BinaryOperatorInputGrammar::Unapplied, BinaryOperatorInput::Unapplied) => {
                        Some(())
                    }
                    (
                        BinaryOperatorInputGrammar::PartiallyAppliedLeft(g),
                        BinaryOperatorInput::PartiallyAppliedLeft(n),
                    ) => parse(g, n, vars),
                    (
                        BinaryOperatorInputGrammar::PartiallyAppliedRight(g),
                        BinaryOperatorInput::PartiallyAppliedRight(n),
                    ) => parse(g, n, vars),
                    (
                        BinaryOperatorInputGrammar::Applied(ga, gb),
                        BinaryOperatorInput::Applied(na, nb),
                    ) => {
                        parse(ga, na, vars)?;
                        parse(gb, nb, vars)
                    }
                    _ => None,
                },
                (
                    Grammar::NonAssociativeBinaryOperator(a, ga, gb),
                    Node::NonAssociativeBinaryOperator(
                        _,
                        Token {
                            kind: TokenKind::Symbol(b),
                            ..
                        },
                        na,
                        nb,
                    ),
                ) if a == b => {
                    parse(ga, na, vars)?;
                    parse(gb, nb, vars)
                }
                (
                    Grammar::VariadicOperator(a, gi),
                    Node::VariadicOperator(
                        _,
                        Token {
                            kind: TokenKind::Symbol(b),
                            ..
                        },
                        ni,
                    ),
                ) if a == b => match (gi, ni) {
                    (VariadicOperatorInputGrammar::Unapplied, VariadicOperatorInput::Unapplied) => {
                        Some(())
                    }
                    (
                        VariadicOperatorInputGrammar::Applied(g),
                        VariadicOperatorInput::Applied(ns),
                    ) => ns.iter().map(|n| parse(g, n, vars)).collect(),
                    _ => None,
                },
                (Grammar::Or(gs), n) => {
                    for g in gs {
                        if let Some(()) = parse(g, n, vars) {
                            return Some(());
                        }
                    }

                    None
                }
                _ => None,
            })();

            if result.is_none() {
                *vars = previous_vars;
            }

            result
        }

        let mut vars = HashMap::new();
        parse(self, node, &mut vars).map(|()| vars)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_grammar {
        ($grammar:literal, $input:expr, { $($var:literal => $value:expr),* $(,)? }) => {{
            grammar! {
                static GRAMMAR = $grammar;
            }

            let vars = GRAMMAR.parse($input).expect("grammar does not match input");

            let expected = HashMap::from([$(($var, $value)),*]);

            assert_eq!(vars, expected);
        }};
    }

    #[test]
    fn test_variable() {
        let input = Node::Token(Token {
            span: 0..0,
            kind: TokenKind::Text("Hello, world!"),
        });

        test_grammar!(
            r#"
                (variable . "x")
            "#,
            &input,
            {
                "x" => vec![input],
            }
        );
    }

    #[test]
    fn test_assignment() {
        let input = Node::NonAssociativeBinaryOperator(
            0..0,
            Token {
                span: 0..0,
                kind: TokenKind::Symbol(":"),
            },
            Box::new(Node::Token(Token {
                span: 0..0,
                kind: TokenKind::Symbol("number"),
            })),
            Box::new(Node::Token(Token {
                span: 0..0,
                kind: TokenKind::Number("42"),
            })),
        );

        test_grammar!(
            r#"
                (non-associative-binary-operator
                    ":"
                    (variable . "name")
                    (variable . "value"))
            "#,
            &input,
            {
                "name" => vec![Node::Token(Token {
                    span: 0..0,
                    kind: TokenKind::Symbol("number"),
                })],
                "value" => vec![Node::Token(Token {
                    span: 0..0,
                    kind: TokenKind::Number("42"),
                })],
            }
        );
    }
}
