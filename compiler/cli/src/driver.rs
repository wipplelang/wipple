use crate::CompileOptions;
use colored::Colorize;
use std::{
    collections::{BTreeMap, HashMap, HashSet, hash_map::Entry},
    fmt::Write,
    io,
};
use wipple_core::{
    TopLevel,
    ast::AstKey,
    compile,
    db::{Db, Node, NodeId},
    default_filter,
    facts::Syntax,
    render::RenderMarkdownOptions,
};
use wipple_feedback::collect_feedback;
use wipple_syntax::checks::run_checks;

pub struct Driver<'a, Out> {
    pub compile_options: &'a CompileOptions,
    pub files: Vec<AstKey>,
    pub out: Out,
    pub silent: bool,
    pub prefix: &'static str,
    pub progress: Option<(usize, usize)>,
    pub hide_facts: bool,
    pub render_options: RenderMarkdownOptions,
}

impl<'a, Out: io::Write> Driver<'a, Out> {
    pub fn new(compile_options: &'a CompileOptions, files: Vec<AstKey>, out: Out) -> Self {
        Driver {
            compile_options,
            files,
            out,
            silent: false,
            prefix: "",
            progress: None,
            hide_facts: false,
            render_options: Default::default(),
        }
    }

    pub fn run(
        mut self,
        db: &mut Db,
        top_level: &mut TopLevel,
        name: &str,
    ) -> anyhow::Result<Option<(Node, Vec<Node>, Vec<Node>)>> {
        if !self.silent {
            if let Some((index, total)) = self.progress {
                eprint!("\u{001B}[2K\r"); // reset line
                eprint!("({}/{}) ", index + 1, total);
            }

            eprint!("{}{}", self.prefix.bold(), name);
        }

        let (root_node, source_files, statements) = compile(db, top_level, &self.files, run_checks);

        if !self.silent && self.progress.is_none() {
            eprintln!();
        }

        if self.compile_options.facts && !self.hide_facts {
            let filter = |db: &Db, node: Node| {
                if let Some(filters) = &self.compile_options.filter_facts {
                    for filter in filters.split(",") {
                        if let Ok(filter) = filter.parse::<NodeId>() {
                            return node.id() == filter;
                        }
                    }
                }

                db.contains::<Syntax>(node)
            };

            writeln!(self.out, "Facts (layer {}):\n", db.layer())?;
            writeln!(self.out, "{}", db.debug(filter, self.render_options))?;
        }

        if self.compile_options.graph && !self.hide_facts {
            let graph = db.graph.build(db, &db.owned_nodes().collect());

            let mut dot = String::new();
            graph.write_dot(&mut dot)?;
            writeln!(self.out, "{dot}")?;
        }

        let filter = default_filter;

        let mut seen_feedback = BTreeMap::<Node, HashSet<String>>::new();
        let feedback_items = collect_feedback(db, filter, |item| {
            filter(db, item.location.primary)
                && (self.compile_options.filter_feedback.is_empty()
                    || self.compile_options.filter_feedback.contains(&item.id))
                && seen_feedback
                    .entry(item.location.primary)
                    .or_default()
                    .insert(item.id.clone())
        });

        let mut feedback_count = 0;
        let mut feedback_files = codespan_reporting::files::SimpleFiles::new();
        let mut feedback_file_ids = HashMap::new();
        let mut feedback_diagnostics = Vec::new();
        for item in feedback_items {
            let Some(span) = db
                .get(item.location.primary)
                .map(|Syntax(key)| key.get(db).span(db))
            else {
                continue;
            };

            let file_id = match feedback_file_ids.entry(&span.path) {
                Entry::Occupied(entry) => *entry.get(),
                Entry::Vacant(entry) => {
                    let Some(file_span) = self
                        .files
                        .iter()
                        .map(|key| key.get(db).span(db))
                        .find(|other| other.path == span.path)
                    else {
                        continue;
                    };

                    *entry.insert(feedback_files.add(&file_span.path, &file_span.source))
                }
            };

            let mut labels = Vec::new();

            let feedback =
                item.display(db, |db, segment| segment.markdown(db, self.render_options));

            labels.push(
                codespan_reporting::diagnostic::Label::primary(
                    file_id,
                    span.start.index..span.end.index,
                )
                .with_message(feedback.message),
            );

            for (node, trace, consequences) in feedback.traces.into_iter() {
                let Some(span) = db.get(node).map(|Syntax(key)| key.get(db).span(db)) else {
                    continue;
                };

                let mut message = trace;
                for consequence in consequences {
                    write!(message, " {consequence}")?;
                }

                labels.push(
                    codespan_reporting::diagnostic::Label::secondary(
                        file_id,
                        span.start.index..span.end.index,
                    )
                    .with_message(message),
                );
            }

            feedback_diagnostics
                .push(codespan_reporting::diagnostic::Diagnostic::error().with_labels(labels));

            feedback_count += 1;
        }

        let config = codespan_reporting::term::Config {
            chars: codespan_reporting::term::Chars::ascii(),
            ..Default::default()
        };

        for diagnostic in feedback_diagnostics {
            let mut f: Box<dyn codespan_reporting::term::WriteStyle> = if self.render_options.color
            {
                Box::new(codespan_reporting::term::termcolor::Ansi::new(
                    &mut self.out,
                ))
            } else {
                Box::new(codespan_reporting::term::termcolor::NoColor::new(
                    &mut self.out,
                ))
            };

            codespan_reporting::term::emit_to_write_style(
                f.as_mut(),
                &config,
                &feedback_files,
                &diagnostic,
            )?;
        }

        Ok((feedback_count == 0).then_some((root_node, source_files, statements)))
    }
}
