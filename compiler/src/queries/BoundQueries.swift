extension Queries {
    public static var resolvedBound: Query<Bound, ResolvedBound> {
        { db, node, body in
            guard let bounds = db[node, Bounds.self]?.bounds else { return }

            for case (_, (let bound, let resolved?)) in bounds { body(bound, resolved) }
        }
    }

    public static var unresolvedBound: Query<Bound> {
        { db, node, body in
            guard let bounds = db[node, Bounds.self]?.bounds else { return }

            for case (_, (let bound, nil)) in bounds { body(bound) }
        }
    }
}
