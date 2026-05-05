public struct UnitExpressionSyntax: Sendable { public let span: Span }

public func parseUnitExpression(with parser: Parser) throws(ParseError) -> UnitExpressionSyntax {
    let span = parser.spanned()
    try parser.token(.leftParenthesis, reason: "between these parentheses")
    try parser.token(.rightParenthesis)
    return UnitExpressionSyntax(span: span())
}

extension UnitExpressionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitExpression(node: node, with: visitor)

        visitor.constraint(TypeConstraint(node, .unit))

        visitor.codegen(node: node, with: UnitExpressionCodegen(node: node))
    }
}

private struct UnitExpressionCodegen: Codegenable {
    let node: Node

    func codegen(with context: CodegenContext) throws {
        context.instruction(.value(node: self.node, value: .tuple([])))
    }
}
