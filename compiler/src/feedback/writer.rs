use crate::{
    database::{Db, NodeRef},
    queries::QueriedComments,
    typecheck::{Bound, BoundConstraint, Constraint, Instantiated, Type, TypeConstraint, Typed},
};
use regex::Regex;
use std::{
    collections::{BTreeSet, HashMap},
    fmt::Write,
    sync::LazyLock,
};

pub struct FeedbackWriter<'a> {
    pub db: &'a Db,
    output: &'a mut (dyn Write + 'a),
    nodes: BTreeSet<NodeRef>,
}

impl<'a> FeedbackWriter<'a> {
    pub fn new(db: &'a Db, output: &'a mut (dyn Write + 'a)) -> Self {
        write!(output, "  ").unwrap();

        FeedbackWriter {
            db,
            output,
            nodes: Default::default(),
        }
    }

    pub fn write_string(&mut self, s: &str) {
        write!(self.output, "{}", s).unwrap();
    }

    pub fn write_break(&mut self) {
        write!(self.output, "\n\n  ").unwrap();
    }

    pub fn write_number(&mut self, n: usize, singular: &str, plural: &str) {
        if n == 1 {
            write!(self.output, "{} {}", n, singular).unwrap();
        } else {
            write!(self.output, "{} {}", n, plural).unwrap();
        }
    }

    pub fn write_ordinal(&mut self, n: usize) {
        let suffix = match n % 10 {
            1 => "st",
            2 => "nd",
            3 => "rd",
            _ => "th",
        };

        write!(self.output, "{}{}", n, suffix).unwrap();
    }

    pub fn write_node(&mut self, node: &NodeRef) {
        write!(self.output, "{}", self.db.render(node)).unwrap();
        self.nodes.insert(node.clone());
    }

    pub fn write_code(&mut self, code: &str) {
        write!(self.output, "`{}`", code).unwrap();
    }

    pub fn write_type(&mut self, ty: impl Into<Type>) {
        // Get the latest type
        let ty = match ty.into() {
            Type::Node(node) => self
                .db
                .get::<Typed>(&node)
                .and_then(|Typed { group }| group)
                .and_then(|group| group.types.first().cloned())
                .map_or(Type::Node(node), Type::Constructed),
            ty => ty,
        };

        write!(self.output, "{}", self.db.render(&ty.render(true))).unwrap()
    }

    pub fn write_bound(&mut self, bound: &Bound) {
        write!(self.output, "{}", self.db.render(bound)).unwrap()
    }
}

impl FeedbackWriter<'_> {
    pub fn write_comments(&mut self, comments: &QueriedComments) {
        static LINK_REGEX: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r"(?s)\[`([^`]+)`\]").unwrap());

        let links = comments
            .links
            .iter()
            .flat_map(
                |(name, link)| -> [(String, Box<dyn Fn(&mut FeedbackWriter<'_>)>); _] {
                    [
                        (
                            name.clone(),
                            Box::new(move |writer| writer.write_node(&link.node)),
                        ),
                        (
                            format!("{name}@related"),
                            Box::new(move |writer| {
                                writer.write_list("and", 3, |list| {
                                    for node in &link.related {
                                        list.add(move |writer| writer.write_node(node));
                                    }
                                });
                            }),
                        ),
                        (
                            format!("{name}@type"),
                            Box::new(move |writer| {
                                writer.write_list("or", 3, |list| {
                                    for ty in &link.types {
                                        list.add(move |writer| writer.write_type(ty.clone()));
                                    }
                                });
                            }),
                        ),
                    ]
                },
            )
            .collect::<HashMap<_, _>>();

        let comments_string = comments.comments.join("\n").trim().to_string();

        let replaced = LINK_REGEX.replace_all(&comments_string, |c: &regex::Captures<'_>| {
            let mut buf = String::new();

            let mut writer = FeedbackWriter::new(self.db, &mut buf);
            let (_, [link]) = c.extract();
            match links.get(link) {
                Some(link) => link(&mut writer),
                None => writer.write_code("_"),
            }

            buf
        });

        write!(self.output, "{}", replaced).unwrap();
    }

    pub fn write_constraint(&mut self, prefix: &str, constraint: &dyn Constraint) {
        let node = constraint.info().node.clone();

        if let Some(constraint) = constraint.downcast_ref::<TypeConstraint>() {
            if constraint.ty.instantiate.is_some() {
                return; // hide type parameters
            }

            if self.db.contains::<Instantiated>(&node) {
                return; // hide instantiated definitions
            }

            let span = self.db.span(&node);

            if span.source == Type::Constructed(constraint.ty.clone()).to_string(self.db, true) {
                return; // don't repeat the type if it is from the source code
            }

            self.write_string(prefix);
            self.write_node(&node);
            self.write_string(" is a ");
            self.write_type(constraint.ty.clone());
            self.write_string(".");
        } else if let Some(constraint) = constraint.downcast_ref::<BoundConstraint>() {
            self.write_string(prefix);
            self.write_node(&constraint.bound.source_node);
            self.write_string(" requires ");
            self.write_bound(&constraint.bound);
            self.write_string(".");
        }
    }

    pub fn finish(self) -> BTreeSet<NodeRef> {
        self.nodes
    }
}

#[derive(Default)]
pub struct ListBuilder<'a> {
    items: Vec<Box<dyn FnOnce(&mut FeedbackWriter<'_>) + 'a>>,
}

impl<'a> ListBuilder<'a> {
    pub fn add(&mut self, item: impl FnOnce(&mut FeedbackWriter<'_>) + 'a) {
        self.items.push(Box::new(item));
    }
}

impl FeedbackWriter<'_> {
    pub fn write_list<'a>(
        &mut self,
        separator: &str,
        limit: impl Into<Option<usize>>,
        build: impl FnOnce(&mut ListBuilder<'a>),
    ) {
        let limit = limit.into();

        let mut builder = ListBuilder::default();
        build(&mut builder);
        let items = builder.items;

        let len = items.len();

        match len {
            3.. => {
                for (index, item) in items.into_iter().enumerate() {
                    if limit.is_none_or(|limit| index >= limit) {
                        let remaining = len - limit.unwrap_or(0);

                        let trailing = if remaining == 1 { "other" } else { "others" };

                        write!(self.output, ", {} {} {}", separator, remaining, trailing).unwrap();

                        break;
                    }

                    if index == len - 1 {
                        write!(self.output, ", {} ", separator).unwrap();
                    } else if index > 0 {
                        write!(self.output, ", ").unwrap();
                    }

                    item(self);
                }
            }
            2 => {
                let mut items = items.into_iter();
                let first = items.next().unwrap();
                let second = items.next().unwrap();

                first(self);
                write!(self.output, " {} ", separator).unwrap();
                second(self);
            }
            1 => {
                let item = items.into_iter().next().unwrap();
                item(self);
            }
            0 => panic!("empty list"),
        }
    }
}
