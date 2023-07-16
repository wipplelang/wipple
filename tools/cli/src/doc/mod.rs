use itertools::Itertools;
use std::{collections::BTreeMap, io};
use wipple_default_loader::is_relative_to_entrypoint;
use wipple_frontend::analysis::{Program, Type};

#[derive(Debug, clap::Parser)]
pub struct Options {
    #[clap(long, value_enum, default_value = "html")]
    pub format: Format,

    #[clap(long)]
    pub title: Option<String>,

    #[clap(long)]
    pub image: Option<String>,

    #[clap(long)]
    pub link: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, clap::ValueEnum)]
pub enum Format {
    Json,
    Html,
}

#[derive(Debug, serde::Serialize)]
struct Content {
    pub title: Option<String>,
    pub image: Option<String>,
    pub links: Vec<String>,
    pub groups: BTreeMap<String, HelpGroup>,
}

#[derive(Debug, Default, serde::Serialize)]
struct HelpGroup {
    pub items: BTreeMap<String, HelpItem>,
}

#[derive(Debug, serde::Serialize)]
struct HelpItem {
    pub kind: String,
    pub help: String,
    pub playground: Option<String>,
}

pub fn document(
    program: &Program,
    options: Options,
    mut output: impl io::Write,
) -> anyhow::Result<()> {
    let entrypoint_path = program
        .items
        .get(
            program
                .entrypoint
                .as_ref()
                .expect("compilation failed; no entrypoint set"),
        )
        .unwrap()
        .read()
        .1
        .span
        .first()
        .path;

    let mut groups = BTreeMap::<String, HelpGroup>::new();

    macro_rules! get_help {
        ($decl:expr) => {{
            if !is_relative_to_entrypoint($decl.span.first().path, entrypoint_path) {
                continue;
            }

            let help = $decl.attributes.decl_attributes.help.iter().join("\n");
            if help.is_empty() {
                continue;
            }

            let help_group = $decl
                .attributes
                .decl_attributes
                .help_group
                .as_ref()
                .map(ToString::to_string)
                .unwrap_or_default();

            let help_playground = $decl
                .attributes
                .decl_attributes
                .help_playground
                .as_ref()
                .map(ToString::to_string);

            (help, help_group, help_playground)
        }};
    }

    for decl in program.declarations.types.values() {
        let (help, group, playground) = get_help!(decl);

        groups.entry(group).or_default().items.insert(
            decl.name.to_string(),
            HelpItem {
                kind: String::from("type"),
                help,
                playground,
            },
        );
    }

    for decl in program.declarations.traits.values() {
        let (help, group, playground) = get_help!(decl);

        groups.entry(group).or_default().items.insert(
            decl.name.to_string(),
            HelpItem {
                kind: String::from("trait"),
                help,
                playground,
            },
        );
    }

    for decl in program.declarations.constants.values() {
        let (help, group, playground) = get_help!(decl);

        groups.entry(group).or_default().items.insert(
            decl.name.to_string(),
            HelpItem {
                kind: if matches!(decl.ty, Type::Function(_, _)) {
                    String::from("function")
                } else {
                    String::from("constant")
                },
                help,
                playground,
            },
        );
    }

    for decl in program.declarations.syntaxes.values() {
        let (help, group, playground) = get_help!(decl);

        groups.entry(group).or_default().items.insert(
            decl.name.to_string(),
            HelpItem {
                kind: if decl.operator {
                    String::from("operator")
                } else if decl.keyword {
                    String::from("keyword")
                } else {
                    String::from("syntax")
                },
                help,
                playground,
            },
        );
    }

    let content = Content {
        title: options.title,
        image: options.image,
        links: options.link,
        groups,
    };

    match options.format {
        Format::Json => {
            serde_json::to_writer(&mut output, &content)?;
        }
        Format::Html => {
            const TEMPLATE: &str = include_str!("./template.html");

            let mut handlebars = handlebars::Handlebars::new();
            handlebars
                .register_template_string("doc", TEMPLATE)
                .expect("malformed template");

            handlebars.render_to_write("doc", &content, &mut output)?;
        }
    }

    Ok(())
}
