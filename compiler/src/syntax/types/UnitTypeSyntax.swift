public struct UnitTypeSyntax: Sendable { public let span: Span }

public func parseUnitType(with parser: Parser) throws(ParseError) -> UnitTypeSyntax {
    let span = parser.spanned()
    try parser.token(.leftParenthesis, reason: "between these parentheses")
    try parser.token(.rightParenthesis)
    return UnitTypeSyntax(span: span())
}

extension UnitTypeSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitType(node: node, with: visitor)

        visitor.constraint(TypeConstraint(node, .unit))
    }
}
