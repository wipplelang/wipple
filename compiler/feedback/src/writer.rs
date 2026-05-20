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
        constraints::{Constraint, ty_constraint::TyConstraint},
        instantiate::Instantiated,
        ty::{Ty, TyTag},
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

    pub fn constraint(&mut self, db: &Db, constraint: &dyn Constraint) {
        let node = constraint.node();

        if let Some(constraint) = constraint.downcast_ref::<TyConstraint>() {
            if let TyTag::Parameter(_) = constraint.ty.tag {
                return; // hide type parameters
            }

            if db.contains::<Instantiated>(node) {
                return; // hide instantiated definitions
            }

            let Some(Syntax(syntax)) = db.get(node) else {
                return;
            };

            if syntax.span().source == Ty::Constructed(constraint.ty.clone()).display(db, true) {
                return; // don't repeat the type if it is from the source code
            }

            self.line_break();
            self.string("-  ");
            self.node(node);
            self.string(" is a ");
            self.ty(db, &Ty::Constructed(constraint.ty.clone()), true);
            self.string(".");
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
