import OrderedCollections

public struct InferredParameter: Fact {
    public func render(into context: RenderContext) { context.string("is inferred type parameter") }
}

@dynamicMemberLookup public class NodeMap<T>: Sequence {
    private var map: OrderedDictionary<Node, T> = [:]

    public init() {}

    public init(_ node: Node, _ value: T) { self.map[node] = value }

    public subscript(node: Node) -> T? {
        get { self.map[node] }
        set { self.map[node] = newValue }
    }

    public subscript<U>(dynamicMember keyPath: KeyPath<OrderedDictionary<Node, T>, U>) -> U {
        get { self.map[keyPath: keyPath] }
    }

    public func makeIterator() -> some IteratorProtocol<(key: Node, value: T)> {
        self.map.makeIterator()
    }
}

public typealias Replacements = NodeMap<Node>
public typealias Substitutions = NodeMap<Type>
