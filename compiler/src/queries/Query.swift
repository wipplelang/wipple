public typealias Query<each T> = (DB, Node, (repeat each T) -> Void) -> Void

public enum Queries {
    public static func fact<T: Fact>(_ type: T.Type) -> Query<Node, T> {
        { db, node, body in if let fact = db[node, type] { body(node, fact) } }
    }
}
