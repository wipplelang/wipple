use crate::CompileOptions;
use colored::Colorize;
use std::{
    collections::{BTreeMap, HashSet},
    io,
};
use wipple_core::{
    TopLevel,
    ast::AstKey,
    compile,
    db::{Db, Node, NodeId},
    default_filter,
    facts::Syntax,
    render::{RenderCtx, RenderMarkdownOptions},
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
        for item in feedback_items {
            if feedback_count == 0 {
                writeln!(self.out, "\nFeedback:")?;
            } else {
                writeln!(self.out)?;
            }

            let render_location = |db: &Db, node: Node| {
                let mut render_ctx = RenderCtx::new(&filter, Vec::new());
                render_ctx.node(node);
                let (location, _) =
                    render_ctx.finish(db, |db, segment| segment.markdown(db, self.render_options));
                location
            };

            let rendered_location = render_location(db, item.location.primary);

            writeln!(self.out, "\n{} ({})\n", rendered_location, item.id)?;

            let feedback =
                item.display(db, |db, segment| segment.markdown(db, self.render_options));

            for line in feedback.message.lines() {
                writeln!(self.out, "  {line}")?;
            }

            if !feedback.traces.is_empty() {
                let mut indices = BTreeMap::new();

                for (index, (trace_index, (node, trace, consequences))) in
                    feedback.traces.into_iter().enumerate()
                {
                    indices.insert(trace_index, index);

                    let location = render_location(db, node);
                    write!(self.out, "\n  {}. {}: {}", index + 1, location, trace)?;
                    for consequence in consequences {
                        write!(self.out, " {consequence}")?;
                    }
                    writeln!(self.out)?;
                }

                if !feedback.trace_edges.is_empty() {
                    let mut first = true;
                    for (from, to) in feedback.trace_edges {
                        let Some(from) = indices.get(&from) else {
                            continue;
                        };

                        let to = match to {
                            Some(to) => match indices.get(&to) {
                                Some(to) => Some(to),
                                None => continue,
                            },
                            None => None,
                        };

                        if first {
                            write!(self.out, "\n  (")?;
                            first = false;
                        } else {
                            write!(self.out, ", ")?;
                        }

                        write!(
                            self.out,
                            "{} -> {}",
                            from + 1,
                            to.map_or_else(|| String::from("error"), |to| (to + 1).to_string())
                        )?;
                    }

                    if !first {
                        writeln!(self.out, ")")?;
                    }
                }
            }

            feedback_count += 1;
        }

        Ok((feedback_count == 0).then_some((root_node, source_files, statements)))
    }
}
