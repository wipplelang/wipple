extension Queries {
    public struct Comments {
        public var nodes: [Node]
        public var comments: [String]
        public var links: [String: Link]
    }

    public static func comments(includeLinks: Bool) -> Query<Comments> {
        { db, node, body in
            let definitionNode = db[node, Resolved.self]?.definitions.first ?? node

            guard let definition = db[definitionNode, Defined.self]?.definition else { return }

            body(
                .init(
                    nodes: [definitionNode],
                    comments: definition.comments,
                    links: includeLinks
                        ? getLinks(db: db, definitionNode: definitionNode, sourceNode: node) : [:],
                )
            )
        }
    }

    public struct ErrorInstance {
        public let bound: Bound
        public let isDefault: Bool
        public let comments: Queries.Comments
        public let trace: [Constraint]
    }

    public static var errorInstance: Query<ErrorInstance> {
        { db, node, body in
            guard let bounds = db[node, Bounds.self]?.bounds else { return }

            for case (_, (let bound, let resolved?)) in bounds {
                guard resolved.instance.isError,
                    let instanceDefinition = db[resolved.instance.node, Defined.self]?.definition
                        as? InstanceDefinition
                else { continue }

                var comments = Comments(
                    nodes: [resolved.instance.node],
                    comments: instanceDefinition.comments,
                    links: getLinks(
                        db: db,
                        definitionNode: resolved.instance.node,
                        sourceNode: resolved.resolvedNode,
                    ),
                )

                let trace = bound.substitutions.values
                    .compactMap { type -> [Constraint]? in
                        guard case .node(let node) = type, let group = db[node, Typed.self]?.group
                        else { return nil }

                        comments.nodes.append(contentsOf: group.nodes)

                        return group.trace
                    }
                    .joined()

                body(
                    .init(
                        bound: bound,
                        isDefault: resolved.instance.isDefault,
                        comments: comments,
                        trace: Array(trace),
                    )
                )
            }
        }
    }
}
