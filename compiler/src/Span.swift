public struct Span: Sendable, CustomStringConvertible, Comparable {
    public let path: String
    public let start: Location
    public let end: Location
    public let source: Substring

    public init(path: String, start: Location, end: Location, in source: String) {
        let startIndex = source.utf16.index(source.startIndex, offsetBy: start.index)
        let endIndex = source.utf16.index(source.startIndex, offsetBy: end.index)

        self.path = path
        self.start = start
        self.end = end
        self.source = Substring(source.utf16[startIndex..<max(startIndex, endIndex)])
    }

    public static var empty: Span { Span(path: "", start: .empty, end: .empty, in: "") }

    public func joined(with other: Self, in source: String) -> Self {
        .init(path: self.path, start: self.start, end: other.end, in: source)
    }

    public func isInside(_ other: Self) -> Bool {
        self.path == other.path && self.start.index >= other.start.index
            && self.end.index <= other.end.index
    }

    public var description: String {
        "\(self.path):\(self.start.line):\(self.start.column)-\(self.end.line):\(self.end.column)"
    }

    public static func < (lhs: Self, rhs: Self) -> Bool {
        if lhs.path != rhs.path { return lhs.path < rhs.path }
        if lhs.start != rhs.start { return lhs.start < rhs.start }
        return lhs.end < rhs.end
    }
}

public struct Location: Sendable, Encodable, Comparable {
    public let line: Int
    public let column: Int
    public let index: Int

    public static var empty: Location { Location(line: 1, column: 1, index: 0) }

    public static func < (lhs: Self, rhs: Self) -> Bool { lhs.index < rhs.index }
}
