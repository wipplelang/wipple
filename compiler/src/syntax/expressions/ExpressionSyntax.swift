public struct IsExpression: Fact {
    public func render(into context: RenderContext) { context.string("is an expression") }
}

private let expressionCacheToken = Parser.CacheToken()
private let expressionElementCacheToken = Parser.CacheToken()
private let atomicExpressionCacheToken = Parser.CacheToken()

public func parseExpression(with parser: Parser) throws(ParseError) -> any Visitable {
    try parser.parseCached(token: expressionCacheToken) { () throws(ParseError) in
        if let value = try parser.parseOptional(parseFunctionExpression) { return value }
        if let value = try parser.parseOptional(parseTupleExpression) { return value }
        if let value = try parser.parseOptional(parseEmptyCollectionExpression) { return value }
        if let value = try parser.parseOptional(parseCollectionExpression) { return value }
        if let value = try parser.parseOptional(parseIsExpression) { return value }
        if let value = try parser.parseOptional(parseAsExpression) { return value }
        if let value = try parser.parseOptional(parseAnnotateExpression) { return value }
        if let value = try parser.parseOptional(parseOperatorExpression) { return value }
        return try parseExpressionElement(with: parser)
    }
}

public func parseExpressionElement(with parser: Parser) throws(ParseError) -> any Visitable {
    try parser.parseCached(token: expressionElementCacheToken) { () throws(ParseError) in
        if let value = try parser.parseOptional(parseFormatExpression) { return value }
        if let value = try parser.parseOptional(parseStructureExpression) { return value }
        if let value = try parser.parseOptional(parseCallExpression) { return value }
        if let value = try parser.parseOptional(parseDoExpression) { return value }
        if let value = try parser.parseOptional(parseWhenExpression) { return value }
        if let value = try parser.parseOptional(parseIntrinsicExpression) { return value }
        return try parseAtomicExpression(with: parser)
    }
}

public func parseAtomicExpression(with parser: Parser) throws(ParseError) -> any Visitable {
    try parser.parseCached(token: atomicExpressionCacheToken) { () throws(ParseError) in
        if let value = try parser.parseOptional(parsePlaceholderExpression) { return value }
        if let value = try parser.parseOptional(parseVariableExpression) { return value }
        if let value = try parser.parseOptional(parseConstructorExpression) { return value }
        if let value = try parser.parseOptional(parseNumberExpression) { return value }
        if let value = try parser.parseOptional(parseStringExpression) { return value }
        if let value = try parser.parseOptional(parseBlockExpression) { return value }
        if let value = try parser.parseOptional(parseUnitExpression) { return value }
        if let value = try parser.parseOptional(parseParenthesizedOperatorExpression) {
            return value
        }
        if let value = try parser.parseOptional(parseParenthesizedExpression) { return value }
        throw parser.error("Expected expression")
    }
}

public func parseParenthesizedExpression(with parser: Parser) throws(ParseError) -> any Visitable {
    try parser.token(.leftParenthesis, reason: "between these parentheses")
    parser.consumeLineBreaks()
    let value = try parseExpression(with: parser)
    parser.consumeLineBreaks()
    try parser.token(.rightParenthesis)
    return value
}

func visitExpression(node: Node, with visitor: Visitor) {
    visitor.db[node, IsExpression.self] = .init()
    visitor.db[node, Typed.self] = .init()
}
