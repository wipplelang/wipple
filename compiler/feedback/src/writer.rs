use regex::Regex;
use std::{
    collections::{BTreeSet, HashMap},
    ops::{Deref, DerefMut},
    sync::LazyLock,
};
use wipple_core::{
    db::{Db, Node},
    facts::Syntax,
    render::{RenderCtx, RenderSegment},
    typecheck::{
        bounds::{Bounds, Instance},
        constraints::{Constraint, bound_constraint::BoundConstraint, ty_constraint::TyConstraint},
        groups::types_of,
        instantiate::Instantiated,
        ty::{ConstructedTy, Ty, TyTag},
    },
};
use wipple_queries::Comments;

#[derive(Default)]
pub struct FeedbackWriter(RenderCtx);

impl Deref for FeedbackWriter {
    type Target = RenderCtx;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for FeedbackWriter {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl FeedbackWriter {
    pub fn singular_plural(&mut self, n: usize, singular: &str, plural: &str) {
        if n == 1 {
            self.0.string(format!("{n} {singular}"));
        } else {
            self.0.string(format!("{n} {plural}"));
        }
    }

    pub fn ordinal(&mut self, n: usize) {
        let suffix = match n % 10 {
            1 => "st",
            2 => "nd",
            3 => "rd",
            _ => "th",
        };

        self.0.string(format!("{n}{suffix}"));
    }

    pub fn comments(&mut self, db: &Db, comments: &Comments) {
        static LINK_REGEX: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r"(?s)\[`([^`]+)`\]").unwrap());

        let mut links = HashMap::<String, Box<dyn Fn(&mut FeedbackWriter)>>::new();
        for (name, link) in &comments.links {
            links.insert(name.to_string(), Box::new(|writer| writer.node(link.node)));

            links.insert(
                format!("{name}@related"),
                Box::new(|writer| {
                    writer.write_list("and", |list| {
                        for &node in &link.related {
                            list.add(move |writer| writer.node(node));
                        }
                    });
                }),
            );

            links.insert(
                format!("{name}@type"),
                Box::new(|writer| {
                    writer.write_list("or", |list| {
                        for ty in &link.tys {
                            let ty = Ty::Constructed(ty.clone());
                            list.add(move |writer| writer.ty(db, &ty, true));
                        }
                    });
                }),
            );
        }

        let comments_string = comments
            .comments
            .iter()
            .map(|comment| comment.trim())
            .collect::<Vec<_>>()
            .join("\n")
            .trim()
            .to_string();

        let mut index = 0;
        for captures in LINK_REGEX.captures_iter(&comments_string) {
            let capture = captures.get(0).unwrap();

            self.string(&comments_string[index..capture.range().start]);
            index = capture.range().end;

            let mut writer = FeedbackWriter::default();
            let name = captures.get(1).unwrap().as_str();
            match links.get(name) {
                Some(link) => link(&mut writer),
                None => writer.code("_"),
            }

            self.extend([writer.0]);
        }

        self.string(&comments_string[index..]);
    }

    pub fn constraints(&mut self, db: &Db, constraints: &[Box<dyn Constraint>]) {
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub enum RenderedConstraint {
            Ty(Node, ConstructedTy),
            Bound(Instance),
        }

        let mut rendered_constraints = Vec::new();
        for constraint in constraints {
            let node = constraint.node();

            if db.contains::<Instantiated>(node) {
                continue; // hide instantiated definitions
            }

            let constraint =
                if let Some(constraint) = constraint.downcast_ref::<TyConstraint>() {
                    RenderedConstraint::Ty(node, constraint.ty.clone())
                } else if let Some(constraint) = constraint.downcast_ref::<BoundConstraint>() {
                    let Bounds(bounds) = db.get(constraint.node).cloned().unwrap_or_default();

                    let Some(instance) = bounds.into_iter().find_map(|(node, bound)| {
                        if node != constraint.bound.bound_node {
                            return None;
                        }

                        let instance = match bound {
                            Ok(resolved) => resolved.instance,
                            Err(unresolved) => Instance {
                                node,
                                trait_node: unresolved.trait_node,
                                parameters: unresolved.parameters,
                                is_from_bound: true,
                            },
                        };

                        if instance.trait_node != constraint.bound.trait_node {
                            return None;
                        }

                        // Hide instances containing conflicting types
                        if instance.parameters.values().any(|ty| {
                            ty.references_nodes_where(|node| types_of(db, node).len() > 1)
                        }) {
                            return None;
                        }

                        Some(instance)
                    }) else {
                        continue;
                    };

                    RenderedConstraint::Bound(instance)
                } else {
                    continue;
                };

            if rendered_constraints.contains(&constraint) {
                continue;
            }

            rendered_constraints.push(constraint);
        }

        rendered_constraints.sort_by_key(|constraint| match constraint {
            RenderedConstraint::Ty(..) => 0,
            RenderedConstraint::Bound(..) => 1,
        });

        for constraint in rendered_constraints {
            match constraint {
                RenderedConstraint::Ty(node, ty) => {
                    if let TyTag::Parameter(_) = ty.tag {
                        continue; // hide type parameters
                    }

                    let Some(Syntax(syntax)) = db.get(node) else {
                        continue;
                    };

                    if db.ast(syntax).span(db).source
                        == Ty::Constructed(ty.clone()).display(db, true)
                    {
                        continue; // don't repeat the type if it is from the source code
                    }

                    self.line_break();
                    self.string("-  ");
                    self.node(node);
                    self.string(" is a ");
                    self.ty(db, &Ty::Constructed(ty.clone()), true);
                    self.string(".");
                }
                RenderedConstraint::Bound(instance) => {
                    self.line_break();
                    self.string("-  ");
                    self.string(" The instance ");
                    self.instance(db, &instance);
                    self.string(" doesn't exist.");
                }
            }
        }
    }
}

#[derive(Default)]
pub struct ListBuilder<'a> {
    items: Vec<Box<dyn FnOnce(&mut FeedbackWriter) + 'a>>,
}

impl<'a> ListBuilder<'a> {
    pub fn add(&mut self, item: impl FnOnce(&mut FeedbackWriter) + 'a) {
        self.items.push(Box::new(item));
    }
}

impl FeedbackWriter {
    pub const LIST_LIMIT: usize = 3;

    pub fn write_list<'a>(&mut self, separator: &str, build: impl FnOnce(&mut ListBuilder<'a>)) {
        let mut builder = ListBuilder::default();
        build(&mut builder);
        let items = builder.items;

        let len = items.len();
        match len {
            3.. => {
                for (index, item) in items.into_iter().enumerate() {
                    if index >= Self::LIST_LIMIT {
                        let remaining = len - Self::LIST_LIMIT;
                        let trailing = if remaining == 1 { "other" } else { "others" };
                        self.string(format!(", {separator} {remaining} {trailing}"));
                        break;
                    }

                    if index == len - 1 {
                        self.string(format!(", {separator} "));
                    } else if index > 0 {
                        self.string(", ");
                    }

                    item(self);
                }
            }
            2 => {
                let mut items = items.into_iter();
                let first = items.next().unwrap();
                let second = items.next().unwrap();

                first(self);
                self.string(format!(" {separator} "));
                second(self);
            }
            1 => {
                let item = items.into_iter().next().unwrap();
                item(self);
            }
            0 => {}
        }
    }
}

impl FeedbackWriter {
    pub fn finish(
        self,
        db: &Db,
        render_segment: impl FnMut(&Db, &RenderSegment) -> String,
    ) -> (String, BTreeSet<Node>) {
        self.0.finish(db, render_segment)
    }
}
