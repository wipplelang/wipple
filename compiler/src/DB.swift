import OrderedCollections
import Synchronization

public protocol Fact: Renderable {}

public final class DB: Sendable, CustomDebugStringConvertible {
    private struct Facts: ~Copyable {
        typealias Store = OrderedDictionary<ObjectIdentifier, any Fact>

        let facts: Mutex<[(node: Node, store: Store)?]> = Mutex([])

        subscript(node: Node) -> Store {
            get {
                self.facts.withLock { facts in
                    guard node.id < facts.count else { return [:] }
                    return facts[node.id]?.store ?? [:]
                }
            }
            nonmutating set {
                self.facts.withLock { (facts: inout _) in
                    if node.id >= facts.count {
                        facts.reserveCapacity(node.id + 1)
                        facts.append(
                            contentsOf: repeatElement(nil, count: node.id - facts.count + 1)
                        )
                    }

                    facts[node.id] = (node, newValue)
                }
            }
        }

        func asSequence() -> some Sequence<(node: Node, store: Store)> {
            self.facts.withLock { $0.makeIterator().compactMap(\.self) }
        }
    }

    public let parent: DB?
    public let debugEnabled: Bool
    private let facts = Facts()
    private let graph = Mutex(Graph())

    public init(parent: DB? = nil, debugEnabled: Bool? = nil) {
        self.parent = parent
        self.debugEnabled = debugEnabled ?? parent?.debugEnabled ?? false
    }

    public func node(isHidden: Bool = false) -> Node { Node(in: self, isHidden: isHidden) }

    public func contains<T: Fact>(_ type: T.Type, for node: Node) -> Bool {
        if self.facts[node][ObjectIdentifier(type)] != nil {
            return true
        } else if let parent = self.parent {
            return parent.contains(type, for: node)
        } else {
            return false
        }
    }

    public subscript<T: Fact>(node: Node, type: T.Type) -> T? {
        get {
            if let value = self.facts[node][ObjectIdentifier(type)] {
                return (value as! T)
            } else if let parent = self.parent {
                return parent[node, type]
            } else {
                return nil
            }
        }
        set { self.facts[node][ObjectIdentifier(T.self)] = newValue }
    }

    public subscript<T: Fact>(node: Node, type: T.Type, default defaultValue: T) -> T {
        get {
            if let value = self.facts[node][ObjectIdentifier(type)] {
                return value as! T
            } else if let parent = self.parent, let value = parent[node, type] as T? {
                return value
            }

            self.facts[node][ObjectIdentifier(type)] = defaultValue

            return defaultValue
        }
        set { self[node, type] = newValue }
    }

    public func forEachFact<T: Fact>(_ type: T.Type, perform body: (Node, T) -> Void) {
        self.forEachFact(type) { node, fact in
            body(node, fact)
            return false
        }
    }

    @discardableResult public func forEachFact<T: Fact>(
        _ type: T.Type,
        perform body: (Node, T) -> Bool,
    ) -> Bool {
        for (node, facts) in self.facts.asSequence() {
            if let value = facts[ObjectIdentifier(type)] {
                if body(node, value as! T) { return true }
            }
        }

        if let parent = self.parent { return parent.forEachFact(type, perform: body) }

        return false
    }

    public var ownedNodes: some Sequence<Node> { self.facts.asSequence().lazy.map(\.node) }

    public func edge(from: Node, to: Node, label: String) {
        self.graph.withLock { $0.edge(from: from, to: to, label: label) }
    }

    public func replace(_ node: Node, with other: Node) {
        self.graph.withLock { $0.replace(node, with: other) }
    }

    public func group(nodes: [Node], types: [ConstructedType]) {
        self.graph.withLock { (graph: inout _) in
            graph.groups.append(.init(nodes: nodes, types: types))
        }
    }

    public func graph(including nodes: OrderedSet<Node>) -> SerializedGraph {
        self.graph.withLock { $0.serialize(including: nodes, db: self) }
    }

    public var debugDescription: String { self.debugDescription(filter: { _ in true }) }

    public func debugDescription(filter: (Node) -> Bool) -> String {
        var nodes = OrderedDictionary<Node, ((any Visitable)?, [any Fact])>()
        for (node, facts) in self.facts.asSequence() {
            guard (self.debugEnabled || !node.isHidden) && filter(node) else { continue }

            let syntax = self[node, Syntax.self]?.value
            for (_, fact) in facts { nodes[node, default: (syntax, [])].1.append(fact) }
        }

        let context = RenderContext(db: self)
        for (node, (syntax, facts)) in nodes {
            context.node(node)
            if let syntax { context.string(" (\(Swift.type(of: syntax)))") }
            context.string(":\n")

            var facts: [String] = facts.compactMap { fact in
                let context = RenderContext(db: self)
                context.db = self
                fact.render(into: context)
                guard !context.segments.isEmpty else { return nil }
                let (string, _) = context.render(with: { $0.markdown(db: self) })
                return string
            }

            facts.sort()

            if facts.isEmpty {
                context.string("  (no facts)\n")
            } else {
                for fact in facts { context.string("  \(fact)\n") }
            }

            context.string("\n")
        }

        let (string, _) = context.render(with: { $0.markdown(db: self) })
        return string
    }
}
