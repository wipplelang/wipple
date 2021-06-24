public typealias Derive = (Value, Stack) throws -> Value

private extension Env {
    private struct RelationsKey: EnvKey {
        typealias Value = RelationGraph

        static let visibility = EnvKeyVisibility<Value>.public { old, new, env, stack in
            for edge in new.edges {
                old.addEdge(edge)
            }

            try env.detectRelationCycles(stack)
        }
    }

    var relations: RelationGraph {
        get { self[RelationsKey.self] ?? RelationGraph() }
        set { self[RelationsKey.self] = newValue }
    }
}

public extension Env {
    func addRelation(from: Trait, to: Trait, _ stack: Stack, _ derive: @escaping Derive) throws {
        guard from != to else {
            throw Exit.error("Cannot relate a trait to itself", stack)
        }

        let added = self.addRelation(from: from, to: to, derive: derive)
        guard added else {
            throw Exit.error("There is already a relation between these traits", stack)
        }

        try self.detectRelationCycles(stack)
    }

    func addRelation<A: Primitive, B: Primitive>(
        from: A.Type,
        to: B.Type,
        _ stack: Stack,
        _ derive: @escaping (A, _ derivedFrom: Value, Stack) throws -> B
    ) throws {
        try self.addRelation(from: Trait(from), to: Trait(to), stack) { value, stack in
            try Value(derive(value.primitiveValue as! A, value, stack))
        }
    }

    func addRelation<A: Primitive, B: Primitive>(
        from: A.Type,
        to: B.Type,
        _ stack: Stack,
        _ derive: @escaping (A, _ derivedFrom: Value) -> B
    ) throws {
        try self.addRelation(from: from, to: to, stack) { primitive, value, _ in
            derive(primitive, value)
        }
    }

    func addRelation<A: Primitive, B: Primitive>(
        from: A.Type,
        to: B.Type,
        _ stack: Stack,
        _ derive: @escaping (A) -> B
    ) throws {
        try self.addRelation(from: from, to: to, stack) { primitive, _, _ in
            derive(primitive)
        }
    }
}

private extension RelationGraph {
    /// Used to determine which relations were declared first
    static var counter: UInt = 0
}

private extension Env {
    func addRelation(from: Trait, to: Trait, derive: @escaping Derive) -> Bool {
        let counter = RelationGraph.counter
        RelationGraph.counter += 1

        return self.relations.addEdge(.init(
            from: .init(trait: from),
            to: .init(trait: to),
            derive: derive,
            counter: counter
        ))
    }
}

private extension Env {
    func detectRelationCycles(_ stack: Stack) throws {
        if self.relations.containsCycle {
            throw Exit.error("Relation cycle detected", stack)
        }
    }
}

private struct RelationGraph {
    var nodes: [ID: Node] = [:]

    struct Node: Equatable {
        let trait: Trait
        var edges: [Edge] = []
    }

    struct Edge: Equatable {
        let from: Node
        let to: Node
        let derive: Derive
        let counter: UInt

        static func == (lhs: Self, rhs: Self) -> Bool {
            lhs.from == rhs.from && lhs.to == rhs.to
        }
    }
}

private extension RelationGraph {
    var edges: [Edge] {
        self.nodes.values.flatMap(\.edges)
    }

    @discardableResult
    mutating func addEdge(_ edge: Edge) -> Bool {
        let id = edge.from.trait.id

        func node(_ trait: Trait) -> Node {
            self.nodes[trait.id] ?? {
                let node = Node(trait: trait)
                self.nodes[trait.id] = node
                return node
            }()
        }

        let fromNode = node(edge.from.trait)
        let toNode = node(edge.to.trait)

        guard fromNode.edge(to: toNode) == nil else {
            return false
        }

        self.nodes[id]!.addEdge(edge)

        return true
    }

    func paths(from: Trait, to: Trait) -> [[Edge]]? {
        for trait in [from, to] {
            guard self.nodes.values.contains(where: { $0.trait == trait }) else {
                return nil
            }
        }

        var visited: Set<ID> = []
        var queue = [from.id]
        var pathDict: [ID: Edge] = [:]
        var paths: [[Edge]] = []

        while let id = queue.popLast() {
            if id == to.id {
                var edge = pathDict[id]!
                var edgePath = [edge]

                while edge.from.trait != from {
                    edge = pathDict[edge.from.trait.id]!
                    edgePath.insert(edge, at: 0)
                }

                paths.append(edgePath)
            }

            for edge in self.nodes[id]!.edges {
                let toID = edge.to.trait.id

                if !visited.contains(toID) {
                    visited.insert(toID)
                    queue.append(toID)
                    pathDict[toID] = edge
                }
            }
        }

        return paths
    }

    var containsCycle: Bool {
        enum Color { case white, gray, black }

        class ColoredNode {
            let id: ID
            var color: Color

            init(id: ID) {
                self.id = id
                self.color = .white
            }
        }

        let coloredNodes = Dictionary(uniqueKeysWithValues: self.nodes.keys.map { id in
            (id, ColoredNode(id: id))
        })

        func visit(_ node: ColoredNode, _ nodes: [ID: Node]) -> Bool {
            switch node.color {
            case .gray:
                return true
            case .white:
                node.color = .gray

                for coloredNode in coloredNodes.values {
                    let containsColoredNode = nodes[node.id]!.edges
                        .contains(where: { $0.to.trait.id == coloredNode.id })

                    if containsColoredNode && visit(coloredNode, nodes) {
                        return true
                    }
                }

                node.color = .black

                return false
            case .black:
                return false
            }
        }

        for id in self.nodes.keys {
            let node = coloredNodes[id]!

            if node.color == .white && visit(node, self.nodes) {
                return true
            }
        }

        return false
    }
}

extension RelationGraph: CustomDebugStringConvertible {
    var debugDescription: String {
        self.edges
            .map { "\($0.from.trait) == \($0.to.trait)" }
            .joined(separator: "\n")
    }
}

private extension RelationGraph.Node {
    mutating func addEdge(_ edge: RelationGraph.Edge) {
        self.edges.append(edge)
    }

    func edge(to node: RelationGraph.Node) -> RelationGraph.Edge? {
        self.edges.first(where: { $0.to == node })
    }
}

internal extension Value {
    /// Relations are resolved in the following way:
    ///
    /// - Parent scopes and earlier declarations have priority.
    ///
    /// - Relations between the input's trait and the derived trait take precedence.
    ///   Only one of these relations must be satisfied (ie. only one relation
    ///   directly relating the traits must exist).
    ///
    /// - If none of the direct relations succeed, the first indirect relation to
    ///   succeed is used.
    func derive(_ trait: Trait, _ env: Env, _ stack: Stack) throws -> Value? {
        try self.deriveDirectly(trait, relations: env.relations, env, stack)
            ?? self.deriveIndirectly(trait, relations: env.relations, env, stack)
    }

    private func deriveDirectly(
        _ trait: Trait,
        relations: RelationGraph,
        _ env: Env,
        _ stack: Stack
    ) throws -> Value? {
        if let parent = env.parent,
           let value = try self.deriveDirectly(trait, relations: parent.relations, parent, stack)
        {
            return value
        }

        let directRelations = relations.edges.compactMap { edge in
            edge.from.trait == self.trait && edge.to.trait == trait
                ? edge.derive
                : nil
        }

        if directRelations.count > 1 {
            throw Exit.error(
                "Cannot derive this trait because there are multiple relations that derive it from the input's trait directly",
                stack
            )
        }

        if let derive = directRelations.first {
            let derivedValue = try derive(self.storedValue, stack)

            guard let matchedValue = try trait.pattern.match(derivedValue, env, stack) else {
                throw Exit.error(
                    "Cannot derive this trait because the relation's output cannot be represented by the trait",
                    stack
                )
            }

            return matchedValue
        }

        return nil
    }

    private func deriveIndirectly(
        _ trait: Trait,
        relations: RelationGraph,
        _ env: Env,
        _ stack: Stack
    ) throws -> Value? {
        if let parent = env.parent,
           let value = try self.deriveIndirectly(trait, relations: parent.relations, parent, stack)
        {
            return value
        }

        guard var paths = relations.paths(from: self.trait, to: trait) else {
            return nil
        }

        // Sort paths by order of declaration
        paths.sort(by: { $0.first!.counter < $1.first!.counter })

        for path in paths where path.first!.from.trait == self.trait {
            var result = self.storedValue

            for edge in path {
                let derivedValue = try edge.derive(result, stack)

                assert(derivedValue.trait == edge.to.trait)

                guard let matchedValue = try edge.to.trait.pattern
                    .match(derivedValue, env, stack)
                else {
                    throw Exit.error(
                        "Cannot derive this trait because the relation's output cannot be represented by the trait",
                        stack
                    )
                }

                result = matchedValue
            }

            assert(result.trait == trait)

            return result
        }

        return nil
    }
}
