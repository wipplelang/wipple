public struct UnitPatternSyntax: Sendable { public let span: Span }

public func parseUnitPattern(with parser: Parser) throws(ParseError) -> UnitPatternSyntax {
    let span = parser.spanned()
    try parser.token(.leftParenthesis, reason: "between these parentheses")
    try parser.token(.rightParenthesis)
    return UnitPatternSyntax(span: span())
}

extension UnitPatternSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitPattern(node: node, visitor: visitor, terminal: .match)

        visitor.constraint(TypeConstraint(node, .unit))
        visitor.codegen(node: node, with: UnitPatternCodegen())
    }
}

private struct UnitPatternCodegen: Codegenable {
    func codegen(with context: CodegenContext) throws {}
}
