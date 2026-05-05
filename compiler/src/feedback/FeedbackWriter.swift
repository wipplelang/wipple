import OrderedCollections

public class FeedbackWriter {
    let db: DB
    private var output: [RenderSegment] = []

    public init(db: DB) { self.db = db }

    public func write(_ string: String) { self.output.append(.string(string)) }

    public func writeBreak() { self.output.append(.lineBreak) }

    public func writeLink(_ name: String, _ node: Node) { self.output.append(.link(name, node)) }

    public func write(_ number: Int, singular: String, plural: String) {
        if number == 1 {
            self.output.append(.string("1 \(singular)"))
        } else {
            self.output.append(.string("\(number) \(plural)"))
        }
    }

    public func write(ordinal number: Int) {
        let suffix =
            switch number % 10 {
            case 1: "st"
            case 2: "nd"
            case 3: "rd"
            default: "th"
            }

        self.output.append(.string("\(number)\(suffix)"))
    }

    public func write(_ node: Node) {
        let context = RenderContext(db: self.db)
        context.node(node)
        self.output.append(contentsOf: context.segments)
    }

    public func write(code: String) { self.output.append(.code(code)) }

    public func write(_ type: Type) {
        // Get the latest type
        var type = type
        if case .node(let node) = type, let constructed = self.db.type(of: node) {
            type = .constructed(constructed)
        }

        let context = RenderContext(db: self.db)
        type.render(into: context)

        self.output.append(contentsOf: context.segments)
    }

    public func write(_ renderable: some Renderable) {
        let context = RenderContext(db: self.db)
        renderable.render(into: context)
        self.output.append(contentsOf: context.segments)
    }

    public func write(_ comments: Queries.Comments) {
        let linkRegex = /(?s)\[`([^`]+)`\]/

        var links: [String: (FeedbackWriter) -> Void] = [:]
        for (name, link) in comments.links {
            links[name] = { $0.write(link.node) }

            links["\(name)@related"] = { writer in
                writer.writeList(separator: "and") { list in
                    for node in link.related { list.add { writer.write(node) } }
                }
            }

            links["\(name)@type"] = { writer in
                writer.writeList(separator: "or") { list in
                    for type in link.types { list.add { writer.write(.constructed(type)) } }
                }
            }
        }

        let commentsString = comments.comments
            .map { $0.trimmingCharacters(in: .whitespacesAndNewlines) }.joined(separator: "\n")

        var index = commentsString.startIndex
        for match in commentsString.matches(of: linkRegex) {
            self.output.append(.string(String(commentsString[...][index..<match.range.lowerBound])))
            index = match.range.upperBound

            let writer = FeedbackWriter(db: self.db)
            let linkName = String(match.output.1)
            if let link = links[linkName] { link(writer) } else { writer.write(code: linkName) }

            self.output.append(contentsOf: writer.output)
        }

        self.output.append(.string(String(commentsString[...][index...])))
    }

    public func write(_ constraint: Constraint) {
        switch constraint {
        case let constraint as TypeConstraint:
            if case .parameter(_) = constraint.type.tag {
                return  // hide type parameters
            }

            guard !self.db.contains(Instantiated.self, for: constraint.node) else {
                return  // hide instantiated definitions
            }

            guard let span = self.db[constraint.node, Syntax.self]?.value.span else { return }

            guard span.source != Type.constructed(constraint.type).display(db: self.db) else {
                return  // don't repeat the type if it is from the source code
            }

            self.writeBreak()
            self.write("-  ")
            self.write(constraint.node)
            self.write(" is a ")
            self.write(.constructed(constraint.type))
            self.write(".")
        case let constraint as BoundConstraint:
            self.writeBreak()
            self.write("-  ")
            self.write(constraint.node)
            self.write(" requires ")
            self.write(constraint.bound)
            self.write(".")
        default: break
        }
    }

    public struct ListBuilder {
        var items: [() -> Void] = []

        public mutating func add(_ build: @escaping () -> Void) { self.items.append(build) }
    }

    public func writeList(separator: String, limit: Int = 3, build: (inout ListBuilder) -> Void) {
        var builder = ListBuilder()
        build(&builder)

        switch builder.items.count {
        case 3...:
            for (index, item) in builder.items.enumerated() {
                if index >= limit {
                    let remaining = builder.items.count - index
                    let trailing = remaining == 1 ? "other" : "others"
                    self.write(", \(separator) \(remaining) \(trailing)")
                    break
                }

                if index == builder.items.count - 1 {
                    self.write(", \(separator) ")
                } else if index > 0 {
                    self.write(", ")
                }

                item()
            }
        case 2:
            builder.items[0]()
            self.write(" \(separator) ")
            builder.items[1]()
        case 1: builder.items[0]()
        default: break
        }
    }

    public func finish(with render: (RenderSegment) -> String) -> (String, OrderedSet<Node>) {
        let renderContext = RenderContext(db: self.db)
        renderContext.segments = self.output
        return renderContext.render(with: render)
    }
}
