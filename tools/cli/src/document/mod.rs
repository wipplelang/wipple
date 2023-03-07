use itertools::Itertools;
use std::{collections::BTreeMap, io};
use wipple_frontend::{
    analysis::{
        typecheck::format::{format_type, Format, TypeFunctionFormat},
        Bound, Program, Type,
    },
    helpers::InternedString,
};

#[derive(Debug, clap::Parser, serde::Serialize)]
pub struct Options {
    #[clap(long)]
    pub title: Option<String>,
    #[clap(long)]
    pub image: Option<String>,
}

#[derive(Debug, serde::Serialize)]
struct Content {
    #[serde(flatten)]
    pub options: Options,
    pub sections: Vec<DocumentationSection>,
}

#[derive(Debug, Default, serde::Serialize)]
struct DocumentationSection {
    pub name: String,
    pub items: Vec<DocumentationItem>,
}

#[derive(Debug, serde::Serialize)]
struct DocumentationItem {
    pub name: String,
    pub kind: String,
    pub decl: String,
    pub help: String,
}

pub fn document(
    program: &Program,
    options: Options,
    mut output: impl io::Write,
) -> anyhow::Result<()> {
    const TEMPLATE: &str = include_str!("./template.html");

    let mut handlebars = handlebars::Handlebars::new();
    handlebars
        .register_template_string("doc", TEMPLATE)
        .expect("malformed template");

    let mut sections = BTreeMap::<String, Vec<DocumentationItem>>::new();

    let format_type = |ty: Type, bounds: &[Bound]| {
        macro_rules! getter {
            ($kind:ident, $f:expr) => {
                |id| $f(program.declarations.$kind.get(&id).unwrap().name)
            };
        }

        format_type(
            ty,
            getter!(types, |name: InternedString| name.to_string()),
            getter!(traits, |name: InternedString| name.to_string()),
            getter!(type_parameters, |name: Option<_>| {
                name.as_ref().map(ToString::to_string)
            }),
            Format {
                type_function: TypeFunctionFormat::Arrow(bounds),
                ..Default::default()
            },
        )
    };

    for decl in program.declarations.constants.values() {
        let help = decl.attributes.decl_attributes.help.iter().join("\n");
        if help.is_empty() {
            continue;
        }

        sections
            .entry(String::new())
            .or_default()
            .push(DocumentationItem {
                name: decl.name.to_string(),
                kind: String::from(if matches!(decl.ty, Type::Function(_, _)) {
                    "function"
                } else {
                    "constant"
                }),
                decl: format!(
                    "{} :: {}",
                    decl.name,
                    format_type(decl.ty.clone(), &decl.bounds)
                ),
                help,
            });
    }

    let content = Content {
        options,
        sections: sections
            .into_iter()
            .map(|(name, items)| DocumentationSection {
                name,
                items: items
                    .into_iter()
                    .sorted_by(|a, b| a.name.to_lowercase().cmp(&b.name.to_lowercase()))
                    .collect(),
            })
            .collect(),
    };

    handlebars.render_to_write("doc", &content, &mut output)?;

    Ok(())
}
