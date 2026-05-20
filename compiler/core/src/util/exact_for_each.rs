pub fn exact_for_each<'a, 'b, A, B>(
    a: &'a [A],
    b: &'b [B],
    mut f: impl FnMut(&'a A, &'b B),
) -> (&'a [A], &'b [B]) {
    let max = a.len().min(b.len());
    for index in 0..max {
        f(&a[index], &b[index]);
    }

    (&a[max..], &b[max..])
}
