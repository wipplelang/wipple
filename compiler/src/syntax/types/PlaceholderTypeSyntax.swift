public struct PlaceholderTypeSyntax: Sendable { public let span: Span }

public func parsePlaceholderType(with parser: Parser) throws(ParseError) -> PlaceholderTypeSyntax {
    let span = parser.spanned()
    try parser.token(.underscoreKeyword)
    return PlaceholderTypeSyntax(span: span())
}

extension PlaceholderTypeSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) { visitType(node: node, with: visitor) }
}
