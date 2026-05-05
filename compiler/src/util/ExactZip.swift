func exactForEach<A, B>(_ a: [A], _ b: [B], perform action: (A, B) -> Void) -> (
    missing: ArraySlice<A>, extra: ArraySlice<B>
) {
    for index in 0..<min(a.count, b.count) { action(a[index], b[index]) }
    return (a.dropFirst(b.count), b.dropFirst(a.count))
}
