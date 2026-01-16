use crate::database::{Db, NodeRef};
use std::rc::Rc;

pub type Filter<T> = Rc<dyn Fn(&Db, &T) -> bool>;

pub fn path_filter(path: &str) -> Filter<NodeRef> {
    Rc::new({
        let path = path.to_string();
        move |db, node| {
            let span = db.span(node);
            span.path == path
        }
    })
}

pub fn range_filter(path: &str, start: usize, end: usize) -> Filter<NodeRef> {
    Rc::new({
        let path = path.to_string();
        move |db, node| {
            let span = db.span(node);
            span.path == path && span.start.index >= start && span.end.index <= end
        }
    })
}

pub fn line_filter(path: &str, line: usize) -> Filter<NodeRef> {
    Rc::new({
        let path = path.to_string();
        move |db, node| {
            let span = db.span(node);
            span.path == path && span.start.line == line
        }
    })
}
