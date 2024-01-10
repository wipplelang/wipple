use crate::WithInfo;

/// Trait for traversing an item.
pub trait Traverse<I, T = Self> {
    /// Called recursively for every member of `self`.
    fn traverse_with(&mut self, f: &mut impl FnMut(WithInfo<I, &mut T>));

    /// Begin traversing with `self` as the root.
    fn traverse(&mut self, mut f: impl FnMut(WithInfo<I, &mut T>)) {
        self.traverse_with(&mut f);
    }
}

impl<I: Clone, T> Traverse<I, T> for WithInfo<I, T> {
    fn traverse_with(&mut self, f: &mut impl FnMut(WithInfo<I, &mut T>)) {
        f(self.as_mut());
    }
}

impl<I: Clone, T> Traverse<I, T> for WithInfo<I, &mut T> {
    fn traverse_with(&mut self, f: &mut impl FnMut(WithInfo<I, &mut T>)) {
        f(self.as_deref_mut());
    }
}
