extension Queries {
    public static var hasType: Query<ConstructedType> {
        { db, node, body in
            guard let group = db[node, Typed.self]?.group, let type = group.types.first else {
                return
            }

            body(type)
        }
    }

    public static var inGroup: Query<Node> {
        { db, node, body in
            guard let group = db[node, Typed.self]?.group else { return }

            for other in group.nodes { body(other) }
        }
    }

    public struct ConflictingTypes {
        let source: Node?
        let from: Node
        let nodes: [Node]
        let types: [ConstructedType]
        let trace: [Constraint]
    }

    public static var conflictingTypes: Query<ConflictingTypes> {
        { db, node, body in
            guard let group = db[node, Typed.self]?.group, group.types.count > 1,
                group.nodes.first == node
            else { return }

            let source = db[node, Instantiated.self]?.sourceNode
            let nodes = group.nodes.filter { $0 != node }

            body(
                .init(
                    source: source,
                    from: node,
                    nodes: nodes,
                    types: group.types,
                    trace: group.trace,
                )
            )
        }
    }

    public static var incompleteType: Query<Node, ConstructedType> {
        { db, node, body in
            guard let group = db[node, Typed.self]?.group, group.types.count == 1,
                group.nodes.first == node
            else { return }

            let type = group.types[0]

            guard Type.constructed(type).referencesNodes else { return }

            body(node, type)
        }
    }

    public static var unknownType: Query<> {
        { db, node, body in
            guard let typed = db[node, Typed.self] else { return }

            guard let group = typed.group else {
                body()
                return
            }

            guard group.types.isEmpty, group.nodes.first == node else { return }

            body()
        }
    }
}
