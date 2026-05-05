public typealias Filter<T> = (DB, T) -> Bool

public enum Filters {
    public static func path(_ path: String) -> Filter<Node> {
        { db, node in
            guard let span = db[node, Syntax.self]?.value.span else { return false }
            return span.path == path
        }
    }

    public static func range(path: String, start: Int, end: Int) -> Filter<Node> {
        { db, node in
            guard let span = db[node, Syntax.self]?.value.span else { return false }
            return span.path == path && span.start.index >= start && span.end.index <= end
        }
    }

    public static func line(path: String, line: Int) -> Filter<Node> {
        { db, node in
            guard let span = db[node, Syntax.self]?.value.span else { return false }
            return span.path == path && span.start.line == line
        }
    }
}
