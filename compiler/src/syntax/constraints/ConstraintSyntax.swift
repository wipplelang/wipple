public struct IsConstraint: Fact {
    public func render(into context: RenderContext) { context.string("is a constraint") }
}

private let constraintCacheToken = Parser.CacheToken()

public func parseConstraints(with parser: Parser) throws(ParseError) -> [any Visitable] {
    try parser.token(.whereKeyword)
    parser.commit(trace: "in these constraints")

    return try parser.parseMany(parseConstraint)
}

public func parseConstraint(with parser: Parser) throws(ParseError) -> any Visitable {
    try parser.parseCached(token: constraintCacheToken) { () throws(ParseError) in
        if let value = try parser.parseOptional(parseBoundConstraint) { return value }
        if let value = try parser.parseOptional(parseDefaultConstraint) { return value }
        throw parser.error("Expected constraint")
    }
}

func visitConstraint(node: Node, with visitor: Visitor) {
    visitor.db[node, IsConstraint.self] = .init()
}
