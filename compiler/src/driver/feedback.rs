use crate::{database::Db, feedback::collect_feedback};
use std::fmt::Write;

pub fn write_feedback(mut w: impl Write, db: &Db, mask: impl Fn(&str) -> bool) -> usize {
    let items = collect_feedback(db, |item| mask(item.id));

    let count = items.len();

    for (index, item) in items.into_iter().enumerate() {
        let (node, _) = &item.location;

        if index == 0 {
            writeln!(w, "\nFeedback:").unwrap();
        } else {
            writeln!(w).unwrap();
        }

        writeln!(w, "\n{} ({}):\n", db.render(node), item.id).unwrap();

        (item.write)(db, &mut w);
    }

    count
}
