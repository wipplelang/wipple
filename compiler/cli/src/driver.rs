use crate::CompileOptions;
use std::{
    collections::{BTreeMap, HashSet},
    io,
    time::Instant,
};
use wipple_core::{
    TopLevel,
    ast::AstKey,
    compile,
    db::{Db, Node, NodeId},
    default_filter,
    facts::Syntax,
    render::RenderCtx,
};
use wipple_feedback::collect_feedback;
use wipple_syntax::{GroupOrder, checks::run_checks};

pub struct Driver<'a, Out> {
    pub compile_options: &'a CompileOptions,
    pub files: Vec<AstKey>,
    pub out: Out,
    pub prefix: &'static str,
    pub time: bool,
    pub progress: Option<(usize, usize)>,
    pub hide_facts: bool,
    pub show_span: bool,
}

impl<'a, Out: io::Write> Driver<'a, Out> {
    pub fn new(compile_options: &'a CompileOptions, files: Vec<AstKey>, out: Out) -> Self {
        Driver {
            compile_options,
            files,
            out,
            prefix: "",
            time: false,
            progress: None,
            hide_facts: false,
            show_span: true,
        }
    }

    pub fn run(
        mut self,
        db: &mut Db,
        top_level: &mut TopLevel,
        name: &str,
    ) -> anyhow::Result<Option<(Node, Vec<Node>, Vec<Node>)>> {
        if let Some((index, total)) = self.progress {
            eprint!("\u{001B}[2K\r"); // reset line
            eprint!("({}/{}) ", index + 1, total);
        }

        eprint!("{}{}", self.prefix, name);
        if self.time {
            eprint!("...");
        }

        let start = Instant::now();

        let (root_node, source_files, statements) =
            compile(db, top_level, &self.files, run_checks, GroupOrder::new);

        if self.time {
            eprint!(" done ({:.0?})", start.elapsed());
        }

        if self.progress.is_none() {
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
            writeln!(self.out, "{}", db.debug(filter, self.show_span))?;
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

            let mut render_ctx = RenderCtx::default();
            render_ctx.node(item.location.primary);
            let (rendered_location, _) =
                render_ctx.finish(db, |db, segment| segment.markdown(db, self.show_span));

            writeln!(self.out, "\n{} ({})\n", rendered_location, item.id)?;

            let (feedback, _) = item.display(db, |db, segment| segment.markdown(db, false));

            for line in feedback.lines() {
                writeln!(self.out, "  {line}")?;
            }

            feedback_count += 1;
        }

        Ok((feedback_count == 0).then_some((root_node, source_files, statements)))
    }
}
