import OrderedCollections

public class RenderContext {
    var db: DB
    var segments: [RenderSegment] = []
    private var nodes: OrderedSet<Node> = []

    public init(db: DB) { self.db = db }

    public func lineBreak() { self.segments.append(.lineBreak) }

    public func string(_ string: String) { self.segments.append(.string(string)) }

    public func code(_ code: String) { self.segments.append(.code(code)) }

    public func node(_ node: Node) { self.segments.append(.node(node)) }

    public func link(_ label: String, _ node: Node) { self.segments.append(.link(label, node)) }

    public func render(with render: (RenderSegment) -> String) -> (String, OrderedSet<Node>) {
        var string = ""
        for segment in self.segments { string += render(segment) }
        return (string, self.nodes)
    }
}

public enum RenderSegment {
    case lineBreak
    case string(String)
    case code(String)
    case node(Node)
    case link(String, Node)

    public func plainText(db: DB) -> String {
        switch self {
        case .lineBreak: return "\n\n"
        case .string(let string): return string
        case .code(let code): return code
        case .node(let node):
            guard let span = db[node, Syntax.self]?.value.span else { return "_" }
            return self.formatSource(String(span.source))
        case .link(let label, _): return label
        }
    }

    public func markdown(db: DB, showSpan: Bool = true) -> String {
        switch self {
        case .lineBreak: return "\n\n"
        case .string(let string): return string
        case .code(let code): return "`\(code)`"
        case .node(let node):
            guard let span = db[node, Syntax.self]?.value.span else { return "`_`" }
            var string = "`\(self.formatSource(String(span.source)))`"
            if showSpan { string += " (\(span))" }
            if db.debugEnabled { string += " (\(node.debugDescription))" }
            return string
        case .link(let label, let node):
            guard let span = db[node, Syntax.self]?.value.span else { return "`\(label)`" }
            var string = "`\(label)`"
            if showSpan { string += " (\(span))" }
            return string
        }
    }

    private func formatSource(_ source: String) -> String {
        source.replacing(/(?s)\{.*\n.*\}/, with: "{⋯}")
    }
}

public protocol Renderable { func render(into context: RenderContext) }

extension Renderable { public func render(into context: RenderContext) {} }
