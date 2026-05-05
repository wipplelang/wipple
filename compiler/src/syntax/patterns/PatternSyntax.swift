import OrderedCollections

public struct Temporaries: Fact { public var temporaries: OrderedSet<Node> = [] }

public struct IsPattern: Fact {
    public func render(into context: RenderContext) { context.string("is a pattern") }
}

public struct Matching: Fact { public let node: Node }

private let patternCacheToken = Parser.CacheToken()
private let patternElementCacheToken = Parser.CacheToken()
private let atomicPatternCacheToken = Parser.CacheToken()

public func parsePattern(with parser: Parser) throws(ParseError) -> any Visitable {
    try parser.parseCached(token: patternCacheToken) { () throws(ParseError) in
        if let value = try parser.parseOptional(parseTuplePattern) { return value }
        if let value = try parser.parseOptional(parseOrPattern) { return value }
        if let value = try parser.parseOptional(parseAnnotatePattern) { return value }
        return try parsePatternElement(with: parser)
    }
}

public func parsePatternElement(with parser: Parser) throws(ParseError) -> any Visitable {
    try parser.parseCached(token: patternElementCacheToken) { () throws(ParseError) in
        if let value = try parser.parseOptional(parseStructurePattern) { return value }
        if let value = try parser.parseOptional(parseParameterizedConstructorPattern) {
            return value
        }
        if let value = try parser.parseOptional(parseSetPattern) { return value }
        return try parseAtomicPattern(with: parser)
    }
}

public func parseAtomicPattern(with parser: Parser) throws(ParseError) -> any Visitable {
    try parser.parseCached(token: atomicPatternCacheToken) { () throws(ParseError) in
        if let value = try parser.parseOptional(parseConstructorPattern) { return value }
        if let value = try parser.parseOptional(parseWildcardPattern) { return value }
        if let value = try parser.parseOptional(parseVariablePattern) { return value }
        if let value = try parser.parseOptional(parseNumberPattern) { return value }
        if let value = try parser.parseOptional(parseStringPattern) { return value }
        if let value = try parser.parseOptional(parseUnitPattern) { return value }
        if let value = try parser.parseOptional(parseParenthesizedPattern) { return value }
        throw parser.error("Expected pattern")
    }
}

public func parseParenthesizedPattern(with parser: Parser) throws(ParseError) -> any Visitable {
    try parser.token(.leftParenthesis, reason: "between these parentheses")
    parser.consumeLineBreaks()
    let value = try parsePattern(with: parser)
    parser.consumeLineBreaks()
    try parser.token(.rightParenthesis)
    return value
}

func visitPattern(node: Node, visitor: Visitor, terminal: MatchPathSegment?) {
    visitor.db[node, IsPattern.self] = .init()
    visitor.db[node, Typed.self] = .init()

    let matching = visitor.setMatches(terminal: terminal)
    visitor.db[node, Matching.self] = Matching(node: matching)

    if visitor.currentMatch?.arm == node {
        visitor.db.edge(from: matching, to: node, label: "value")
    }

    visitor.constraint(GroupConstraint(node, matching))
}
