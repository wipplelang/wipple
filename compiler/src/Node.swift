import Synchronization

public final class Node: Sendable, Hashable, CustomDebugStringConvertible {
    private static let nextID = Atomic(0)

    private unowned let db: DB?
    public let id: Int
    public let isHidden: Bool

    public init(in db: DB?, isHidden: Bool = false) {
        self.db = db
        self.id = Self.nextID.add(1, ordering: .relaxed).oldValue
        self.isHidden = isHidden
    }

    public func belongs(to db: DB) -> Bool { self.db === db }

    public static func == (lhs: Node, rhs: Node) -> Bool { lhs.id == rhs.id }

    public func hash(into hasher: inout Hasher) { hasher.combine(self.id) }

    public var debugDescription: String { "Node(\(self.id))" }
}

extension Node { public static let topLevel = Node(in: nil) }

public struct Parent: Fact { public let parent: Node }

public struct Children: Fact { public var children: [Node] = [] }
