use crate::{CompileOptions, FilterFacts};
use colored::Colorize;
use std::{
    collections::{BTreeMap, HashMap, HashSet, hash_map::Entry},
    fmt::Write,
    io,
    ops::ControlFlow,
};
use wipple_core::{
    TopLevel,
    ast::AstKey,
    compile,
    db::{Db, Node},
    default_filter,
    facts::Syntax,
    render::RenderMarkdownOptions,
    span::Span,
};
use wipple_feedback::collect_feedback;
use wipple_syntax::{checks::run_checks, file::File};

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

        let mut seen_feedback = BTreeMap::<Node, HashSet<String>>::new();
        let feedback_items = collect_feedback(db, default_filter, |item| {
            default_filter(db, item.location.primary)
                && (self.compile_options.filter_feedback.is_empty()
                    || self.compile_options.filter_feedback.contains(&item.id))
                && seen_feedback
                    .entry(item.location.primary)
                    .or_default()
                    .insert(item.id.clone())
        })
        .into_iter()
        .filter_map(|item| {
            let span = db
                .get(item.location.primary)
                .map(|Syntax(key)| key.get(db).span(db))?;

            let feedback =
                item.display(db, |db, segment| segment.markdown(db, self.render_options));

            Some((span, feedback))
        })
        .take(if self.compile_options.all_feedback {
            usize::MAX
        } else {
            1
        })
        .collect::<Vec<_>>();

        if self.compile_options.facts && !self.hide_facts {
            let filter = |db: &Db, node: Node| {
                if !self.compile_options.filter_facts.is_empty() {
                    for filter in &self.compile_options.filter_facts {
                        let matches = match filter {
                            FilterFacts::Feedback => feedback_items
                                .iter()
                                .any(|(_, feedback)| feedback.nodes.contains(&node)),
                            FilterFacts::Node(id) => node.id() == *id,
                        };

                        if matches {
                            return true;
                        }
                    }

                    return false;
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

        let mut feedback_count = 0;
        let mut feedback_files = codespan_reporting::files::SimpleFiles::<String, String>::new();
        let mut feedback_file_ids = HashMap::new();

        let mut sources = HashMap::new();
        db.for_each_fact::<_, ()>(&mut |db, _, Syntax(syntax)| {
            if syntax.get(db).downcast_ref::<File>().is_some() {
                let span = syntax.get(db).span(db);
                sources.insert(span.path.clone(), span.source.clone());
            }

            ControlFlow::Continue(())
        });

        let config = codespan_reporting::term::Config {
            chars: codespan_reporting::term::Chars::ascii(),
            ..Default::default()
        };

        let mut f: Box<dyn codespan_reporting::term::WriteStyle> = if self.render_options.color {
            Box::new(codespan_reporting::term::termcolor::Ansi::new(
                &mut self.out,
            ))
        } else {
            Box::new(codespan_reporting::term::termcolor::NoColor::new(
                &mut self.out,
            ))
        };

        for (span, feedback) in feedback_items {
            if feedback_count == 0 && !self.silent {
                writeln!(f)?;
            }

            let mut emit_label = |span: &Span,
                                  message: &str,
                                  primary: bool|
             -> anyhow::Result<()> {
                let severity = if primary {
                    codespan_reporting::diagnostic::Severity::Error
                } else {
                    codespan_reporting::diagnostic::Severity::Note
                };

                let file_id = match feedback_file_ids.entry(span.path.clone()) {
                    Entry::Occupied(entry) => *entry.get(),
                    Entry::Vacant(entry) => {
                        let Some(source) = sources.get(&span.path) else {
                            return Ok(());
                        };

                        *entry.insert(feedback_files.add(span.path.to_string(), source.to_string()))
                    }
                };

                let range = span.start.index..span.end.index;

                let label = if primary {
                    codespan_reporting::diagnostic::Label::primary(file_id, range.clone())
                } else {
                    codespan_reporting::diagnostic::Label::secondary(file_id, range.clone())
                };

                let message = message
                    .split("\n\n")
                    .map(|lines| {
                        lines
                            .lines()
                            .map(|line| line.trim())
                            .collect::<Vec<_>>()
                            .join(" ")
                    })
                    .filter(|line| !line.is_empty())
                    .collect::<Vec<_>>()
                    .join("\n\n");

                let diagnostic = codespan_reporting::diagnostic::Diagnostic::new(severity)
                    .with_labels(vec![label]);

                codespan_reporting::term::emit_to_write_style(
                    f.as_mut(),
                    &config,
                    &feedback_files,
                    &diagnostic,
                )?;

                writeln!(f, "{message}\n")?;

                Ok(())
            };

            if self.compile_options.explain {
                for (node, trace, consequences) in feedback.traces.into_iter().rev() {
                    let Some(span) = db.get(node).map(|Syntax(key)| key.get(db).span(db)) else {
                        continue;
                    };

                    let mut message = trace;
                    for consequence in consequences {
                        write!(message, " {consequence}")?;
                    }

                    emit_label(span, &message, false)?;
                }
            }

            emit_label(span, &feedback.message, true)?;

            feedback_count += 1;
        }

        if feedback_count > 0 && !self.compile_options.explain {
            writeln!(
                f,
                "{} use `--explain` to show more information",
                "Help:".bold()
            )?;
        }

        Ok((feedback_count == 0).then_some((root_node, source_files, statements)))
    }
}
