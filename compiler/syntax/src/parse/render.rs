#![allow(dead_code)]

use crate::{
    parse::{rules, SyntaxKind},
    Driver,
};
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

fn render<D: Driver>() -> Vec<(&'static str, SyntaxKind, RuleToRender)> {
    vec![
        rules::attribute::<D>().render(),
        rules::attribute_value::<D>().render(),
        rules::top_level::<D>().render(),
        rules::statement::<D>().render(),
        rules::syntax_declaration::<D>().render(),
        rules::type_declaration::<D>().render(),
        rules::trait_declaration::<D>().render(),
        rules::default_instance_declaration::<D>().render(),
        rules::instance_declaration::<D>().render(),
        rules::constant_declaration::<D>().render(),
        rules::assignment::<D>().render(),
        rules::r#type::<D>().render(),
        rules::placeholder_type::<D>().render(),
        rules::declared_type::<D>().render(),
        rules::function_type::<D>().render(),
        rules::tuple_type::<D>().render(),
        rules::block_type::<D>().render(),
        rules::intrinsic_type::<D>().render(),
        rules::message_type::<D>().render(),
        rules::equal_type::<D>().render(),
        rules::type_function::<D>().render(),
        rules::type_parameter::<D>().render(),
        rules::type_representation::<D>().render(),
        rules::instance::<D>().render(),
        rules::pattern::<D>().render(),
        rules::wildcard_pattern::<D>().render(),
        rules::number_pattern::<D>().render(),
        rules::text_pattern::<D>().render(),
        rules::variant_pattern::<D>().render(),
        rules::destructure_pattern::<D>().render(),
        rules::tuple_pattern::<D>().render(),
        rules::or_pattern::<D>().render(),
        rules::mutate_pattern::<D>().render(),
        rules::expression::<D>().render(),
        rules::annotate_expression::<D>().render(),
        rules::name_expression::<D>().render(),
        rules::number_expression::<D>().render(),
        rules::text_expression::<D>().render(),
        rules::apply_expression::<D>().render(),
        rules::binary_operator_expression::<D>().render(),
        rules::as_expression::<D>().render(),
        rules::is_expression::<D>().render(),
        rules::when_expression::<D>().render(),
        rules::when_arm::<D>().render(),
        rules::intrinsic_expression::<D>().render(),
        rules::tuple_expression::<D>().render(),
        rules::collection_expression::<D>().render(),
        rules::structure_expression::<D>().render(),
        rules::block_expression::<D>().render(),
        rules::function_expression::<D>().render(),
        rules::do_expression::<D>().render(),
        rules::call_expression::<D>().render(),
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
fn grammar<D: Driver>(_driver: &D) -> Grammar {
    let mut rules = render::<D>();
    rules.sort_by_key(|(_, kind, _)| kind.to_string());

    Grammar { rules }
}

#[cfg(test)]
mod render_grammar_to_html {
    use super::*;
    use std::{fs, path::PathBuf, sync::Arc};

    #[test]
    fn render_grammar_to_html() {
        struct TestDriver;

        impl Driver for TestDriver {
            type Info = ();

            fn file_path(&self) -> Arc<str> {
                unimplemented!()
            }

            fn visible_path(&self) -> Arc<str> {
                unimplemented!()
            }

            fn file_size(&self) -> u32 {
                unimplemented!()
            }

            fn merge_info(_left: Self::Info, _right: Self::Info) -> Self::Info {
                unimplemented!()
            }
        }

        let html = grammar(&TestDriver).render_to_html_string();

        let output_path = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap())
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("target/debug/grammar.html");

        fs::write(output_path, html).unwrap();
    }
}
