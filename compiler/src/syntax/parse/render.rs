#![allow(dead_code)]

use crate::syntax::parse::{SyntaxKind, rules};
use std::rc::Rc;

#[derive(Clone)]
pub enum RuleToRender {
    Ellipsis,
    Token(&'static str),
    Keyword(String),
    Terminal(SyntaxKind),
    List(Vec<Rc<dyn Fn() -> RuleToRender>>),
    Block(Vec<Rc<dyn Fn() -> RuleToRender>>),
    Switch(Vec<Rc<dyn Fn() -> RuleToRender>>),
}

impl RuleToRender {
    pub const NAME: Self = RuleToRender::Token("name");
    pub const NUMBER: Self = RuleToRender::Token("number");
    pub const TEXT: Self = RuleToRender::Token("text");
    pub const UNDERSCORE: Self = RuleToRender::Token("_");
}

fn render() -> Vec<(&'static str, SyntaxKind, RuleToRender)> {
    vec![
        rules::attribute().render(),
        rules::attribute_value().render(),
        rules::top_level().render(),
        rules::statement().render(),
        rules::syntax_declaration().render(),
        rules::type_declaration().render(),
        rules::trait_declaration().render(),
        rules::default_instance_declaration().render(),
        rules::instance_declaration().render(),
        rules::constant_declaration().render(),
        rules::assignment().render(),
        rules::r#type().render(),
        rules::placeholder_type().render(),
        rules::declared_type().render(),
        rules::function_type().render(),
        rules::tuple_type().render(),
        rules::block_type().render(),
        rules::intrinsic_type().render(),
        rules::message_type().render(),
        rules::equal_type().render(),
        rules::type_function().render(),
        rules::type_parameter().render(),
        rules::type_representation().render(),
        rules::instance().render(),
        rules::pattern().render(),
        rules::wildcard_pattern().render(),
        rules::number_pattern().render(),
        rules::text_pattern().render(),
        rules::variant_pattern().render(),
        rules::destructure_pattern().render(),
        rules::tuple_pattern().render(),
        rules::or_pattern().render(),
        rules::mutate_pattern().render(),
        rules::expression().render(),
        rules::annotate_expression().render(),
        rules::name_expression().render(),
        rules::number_expression().render(),
        rules::text_expression().render(),
        rules::apply_expression().render(),
        rules::binary_operator_expression().render(),
        rules::as_expression().render(),
        rules::is_expression().render(),
        rules::when_expression().render(),
        rules::when_arm().render(),
        rules::intrinsic_expression().render(),
        rules::tuple_expression().render(),
        rules::collection_expression().render(),
        rules::structure_expression().render(),
        rules::block_expression().render(),
        rules::function_expression().render(),
        rules::do_expression().render(),
        rules::call_expression().render(),
    ]
}

/// An HTML-rendered version of the parser's grammar.
struct Grammar {
    rules: Vec<(&'static str, SyntaxKind, RuleToRender)>,
}

impl Grammar {
    /// Render the grammar to HTML.
    fn render_to_html(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        write!(f, "<ul class=\"grammar\">")?;

        for (doc, kind, rule) in &self.rules {
            write!(f, "<li id=\"{}\">", kind)?;
            write!(f, "<a href=\"#{}\"><em>{}</em></a> &rarr; ", kind, kind)?;
            rule.render_to_html(f)?;
            write!(f, "<br>")?;
            write!(f, "<p>{}</p>", doc)?;
            write!(f, "</li>")?;
        }

        write!(f, "</ul>")?;

        Ok(())
    }

    /// Render the grammar to an HTML string.
    fn render_to_html_string(&self) -> String {
        let mut s = String::new();
        self.render_to_html(&mut s).unwrap();
        s
    }
}

impl RuleToRender {
    fn render_to_html(&self, f: &mut dyn std::fmt::Write) -> std::fmt::Result {
        match self {
            RuleToRender::Ellipsis => write!(f, "<em>...</em>")?,
            RuleToRender::Token(token) => write!(f, "<em>{}</em>", token)?,
            RuleToRender::Keyword(keyword) => write!(f, "<code>{}</code>", keyword)?,
            RuleToRender::Terminal(kind) => {
                write!(f, "<a href=\"#{}\"><em>{}</em></a>", kind, kind)?
            }
            RuleToRender::List(rules) => {
                write!(f, "<code>(</code>")?;

                for rule in rules {
                    write!(f, " ")?;
                    rule().render_to_html(f)?;
                }

                if !rules.is_empty() {
                    write!(f, " ")?;
                }

                write!(f, "<code>)</code>")?;
            }
            RuleToRender::Block(rules) => {
                write!(f, "<code>{{</code>")?;

                for rule in rules {
                    write!(f, " ")?;
                    rule().render_to_html(f)?;
                }

                if !rules.is_empty() {
                    write!(f, " ")?;
                }

                write!(f, "<code>}}</code>")?;
            }
            RuleToRender::Switch(rules) => {
                write!(f, "(")?;

                for (index, rule) in rules.iter().enumerate() {
                    if index > 0 {
                        write!(f, "&emsp;|&emsp;")?;
                    }

                    rule().render_to_html(f)?;
                }

                write!(f, ")")?;
            }
        }

        Ok(())
    }
}

/// The parser's syntax rules.
fn grammar() -> Grammar {
    let mut rules = render();
    rules.sort_by_key(|(_, kind, _)| kind.to_string());

    Grammar { rules }
}

#[cfg(test)]
mod render_grammar_to_html {
    use super::*;
    use std::{fs, path::PathBuf};

    #[test]
    fn render_grammar_to_html() {
        let html = grammar().render_to_html_string();

        let output_path = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap())
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("target/debug/grammar.html");

        fs::write(output_path, html).unwrap();
    }
}
