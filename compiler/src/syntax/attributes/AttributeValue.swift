public struct StringAttributeValueSyntax: Sendable {
    public let span: Span
    public let value: Substring
}

func parseStringAttributeValue(with parser: Parser) throws(ParseError) -> StringAttributeValueSyntax
{
    let span = parser.spanned()
    let value = try parser.token(.string)
    return StringAttributeValueSyntax(span: span(), value: value)
}

public struct ConnectionAttributeValueSyntax: Sendable {
    public let span: Span
    public let left: Substring
    public let right: Substring
    public let label: Substring
}

func parseConnectionAttributeValue(with parser: Parser) throws(ParseError)
    -> ConnectionAttributeValueSyntax
{
    let span = parser.spanned()
    let left = try parser.token(.lowercaseName)
    try parser.token(.functionOperator)
    let right = try parser.token(.lowercaseName)
    try parser.token(.leftParenthesis)
    let label = try parser.token(.string)
    try parser.token(.rightParenthesis)

    return ConnectionAttributeValueSyntax(span: span(), left: left, right: right, label: label)
}

private let attributeValueCacheToken = Parser.CacheToken()

func parseAttributeValue(with parser: Parser) throws(ParseError) -> any Visitable {
    try parser.parseCached(token: attributeValueCacheToken) { () throws(ParseError) in
        if let value = try parser.parseOptional(parseStringAttributeValue) { return value }

        if let value = try parser.parseOptional(parseConnectionAttributeValue) { return value }

        throw parser.error("Expected an attribute value")
    }
}

extension StringAttributeValueSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {}
}

extension ConnectionAttributeValueSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {}
}
