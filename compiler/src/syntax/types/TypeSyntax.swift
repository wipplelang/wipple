public struct IsType: Fact {
    public func render(into context: RenderContext) { context.string("is a type") }
}

public struct MissingTypes: Fact {
    public let types: [Node]

    public func render(into context: RenderContext) { context.string("missing types") }
}

public struct ExtraType: Fact {
    public func render(into context: RenderContext) { context.string("is extra type") }
}

private let typeCacheToken = Parser.CacheToken()
private let typeElementCacheToken = Parser.CacheToken()
private let atomicTypeCacheToken = Parser.CacheToken()

public func parseType(with parser: Parser) throws(ParseError) -> any Visitable {
    try parser.parseCached(token: typeCacheToken) { () throws(ParseError) in
        if let value = try parser.parseOptional(parseTupleType) { return value }
        if let value = try parser.parseOptional(parseFunctionType) { return value }
        if let value = try parser.parseOptional(parseAnnotatedTypeParameter) { return value }
        return try parseTypeElement(with: parser)
    }
}

public func parseTypeElement(with parser: Parser) throws(ParseError) -> any Visitable {
    try parser.parseCached(token: typeElementCacheToken) { () throws(ParseError) in
        if let value = try parser.parseOptional(parseParameterizedType) { return value }
        return try parseAtomicType(with: parser)
    }
}

public func parseAtomicType(with parser: Parser) throws(ParseError) -> any Visitable {
    try parser.parseCached(token: atomicTypeCacheToken) { () throws(ParseError) in
        if let value = try parser.parseOptional(parsePlaceholderType) { return value }
        if let value = try parser.parseOptional(parseTypeParameter) { return value }
        if let value = try parser.parseOptional(parseNamedType) { return value }
        if let value = try parser.parseOptional(parseBlockType) { return value }
        if let value = try parser.parseOptional(parseUnitType) { return value }
        if let value = try parser.parseOptional(parseParenthesizedType) { return value }
        throw parser.error("Expected type")
    }
}

public func parseParenthesizedType(with parser: Parser) throws(ParseError) -> any Visitable {
    try parser.token(.leftParenthesis, reason: "between these parentheses")
    parser.consumeLineBreaks()
    let value = try parseType(with: parser)
    parser.consumeLineBreaks()
    try parser.token(.rightParenthesis)
    return value
}

func visitType(node: Node, with visitor: Visitor) {
    visitor.db[node, IsType.self] = .init()
    visitor.db[node, Typed.self] = .init()
}
