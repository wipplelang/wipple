extension Queries {
    public static var placeholder: Query<Node, [Node], ConstructedType?> {
        { db, node, body in
            guard db.contains(IsPlaceholder.self, for: node),
                let group = db[node, Typed.self]?.group
            else { return }

            let others = group.nodes.filter { $0 != node }
            let type = group.types.isEmpty ? nil : group.types[0]

            body(node, others, type)
        }
    }
}
