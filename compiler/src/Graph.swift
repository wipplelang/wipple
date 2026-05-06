import OrderedCollections
import Synchronization

struct Graph {
    struct Edge: Hashable {
        var from: Node
        var to: Node
        var label: String
    }

    struct Group {
        var nodes: [Node]
        var types: [ConstructedType]
    }

    fileprivate var replacements: OrderedDictionary<Node, Node> = [:]
    fileprivate var edges: [Edge] = []
    var groups: [Group] = []

    mutating func replace(_ node: Node, with replacement: Node) {
        self.replacements[node] = replacement
    }

    mutating func edge(from: Node, to: Node, label: String) {
        self.edges.append(.init(from: from, to: to, label: label))
    }

    func serialize(including mask: OrderedSet<Node>, db: DB) -> SerializedGraph {
        // Ensure IDs are stable
        func id(for node: Node) -> String? {
            guard let span = db[node, Syntax.self]?.value.span else { return nil }
            return "node_\(span.description.replacing(/[^A-Za-z0-9]/, with: "_"))"
        }

        let maxIterations = 10

        func canDisplay(_ node: Node) -> Bool {
            (!node.isHidden || db.contains(Instantiated.self, for: node))
                && !db.contains(Defined.self, for: node)
        }

        func replacement(for node: Node) -> Node {
            var node = node
            while let replacement = self.replacements[node] { node = replacement }
            return node
        }

        var reachableNodes = OrderedDictionary<Node, Int?>(
            uniqueKeysWithValues: mask.map { ($0, nil) }
        )

        var inGroup: OrderedSet<String> = []
        var finishedGroups: OrderedSet<Int> = []
        var groups: OrderedDictionary<Int, (nodes: OrderedSet<Node>, types: [ConstructedType])> =
            [:]
        var edges: OrderedSet<Edge> = []
        for _ in 0..<maxIterations {
            var progress = false

            for (index, group) in self.groups.enumerated() {
                guard !finishedGroups.contains(index) else { continue }

                let nodes = OrderedSet(
                    group.nodes.lazy.filter(canDisplay(_:)).map(replacement(for:))
                        .filter { node in
                            guard let existing = reachableNodes[node] else { return false }
                            return existing == nil || existing == index
                        }
                )

                guard !nodes.isEmpty else { continue }

                if let (existingNodes, existingTypes) = groups[index] {
                    if existingNodes == nodes {
                        finishedGroups.append(index)
                    } else {
                        groups[index]!.nodes.formUnion(nodes)
                        progress = true
                    }

                    for type in existingTypes {
                        if !existingTypes.contains(type) { groups[index]!.types.append(type) }
                    }
                } else {
                    groups[index] = (nodes, group.types)
                    progress = true
                }

                inGroup.formUnion(nodes.compactMap(id(for:)))
                for node in nodes { reachableNodes[node] = index }
            }

            for edge in self.edges {
                let from = replacement(for: edge.from)
                let to = replacement(for: edge.to)

                guard reachableNodes[from] != nil || reachableNodes[to] != nil,
                    canDisplay(from) && canDisplay(to)
                else { continue }

                if reachableNodes[from] == nil { reachableNodes[from] = .some(nil) }
                if reachableNodes[to] == nil { reachableNodes[to] = .some(nil) }

                let edge = Edge(from: from, to: to, label: edge.label)
                if edges.append(edge).inserted { progress = true }
            }

            guard progress else { break }
        }

        // Remove groups containing only types

        func containsNonType(_ nodes: OrderedSet<Node>) -> Bool {
            nodes.contains { node in !db.contains(IsType.self, for: node) }
        }

        if groups.values.lazy.map(\.nodes).contains(where: containsNonType(_:)) {
            groups.removeAll { _, group in

                let keep = containsNonType(group.nodes)
                if !keep {
                    for node in group.nodes {
                        guard let id = id(for: node) else { continue }
                        inGroup.remove(id)
                    }
                }
                return !keep
            }
        }

        var result = SerializedGraph()

        var connectedNodes: OrderedSet<String> = []
        for edge in edges {
            guard let from = id(for: edge.from), let to = id(for: edge.to),
                inGroup.contains(from) && inGroup.contains(to)
            else { continue }

            connectedNodes.append(from)
            connectedNodes.append(to)

            result.edges.append(.init(from: from, to: to, label: edge.label))
        }

        // Remove unconnected nodes
        inGroup.removeAll { !connectedNodes.contains($0) }

        // Remove groups containing only function constants
        groups.removeAll { _, group in
            if group.nodes.count != 1 { return false }

            let node = group.nodes[0]
            guard let definitions = db[node, Resolved.self]?.definitions, definitions.count == 1,
                let definition = db[definitions[0], Defined.self]?.definition,
                definition is VariableDefinition, group.types.count == 1
            else { return false }

            if case .function = group.types[0].tag { return true }

            return false
        }

        for node in reachableNodes.keys {
            guard let id = id(for: node), inGroup.contains(id),
                let span = db[node, Syntax.self]?.value.span
            else { continue }

            result.nodes.append(.init(id: id, span: span))
        }

        for (var nodes, types) in groups.values {

            nodes.removeAll {
                guard let id = id(for: $0) else { return true }
                return !inGroup.contains(id)
            }

            guard !nodes.isEmpty else { continue }

            var labels = types.map { Type.constructed($0).display(db: db) }

            let conflict = labels.count != 1

            if labels.isEmpty { labels.append("_") }

            result.groups.append(
                .init(nodes: nodes.compactMap(id(for:)), labels: labels, conflict: conflict)
            )
        }

        return result
    }
}

public struct SerializedGraph {
    public struct Group {
        public var nodes: [String]
        public var labels: [String]
        public var conflict: Bool
    }

    public struct Node {
        public var id: String
        public var span: Span
    }

    public struct Edge {
        public var from: String
        public var to: String
        public var label: String
    }

    public var groups: [Group] = []
    public var nodes: [Node] = []
    public var edges: [Edge] = []

    public func toDOT() -> String {
        var graph = ""

        graph += "digraph G {\n"
        graph += "  node [shape=box, fontname=monospace]\n"

        for node in nodes { graph += "  \(node.id) [label=\(node.span.source.debugDescription)]\n" }

        for edge in edges {
            graph += "  \(edge.from) -> \(edge.to) [label=\(edge.label.debugDescription)]\n"
        }

        graph += "}"

        return graph
    }
}
