public struct NumberExpressionSyntax: Sendable {
    public let span: Span
    public let value: Substring
}

public func parseNumberExpression(with parser: Parser) throws(ParseError) -> NumberExpressionSyntax
{
    let span = parser.spanned()
    let value = try parser.token(.number)
    return NumberExpressionSyntax(span: span(), value: value)
}

extension NumberExpressionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitExpression(node: node, with: visitor)

        let numberType = visitor.db.node(isHidden: true)
        visitor.visit(
            NamedTypeSyntax(span: self.span, name: "Number", parameters: []),
            as: numberType,
        )

        visitor.constraint(GroupConstraint(node, numberType))

        visitor.codegen(
            node: node,
            with: NumberExpressionCodegen(node: node, value: String(self.value)),
        )
    }
}

private struct NumberExpressionCodegen: Codegenable {
    let node: Node
    let value: String

    func codegen(with context: CodegenContext) throws {
        context.instruction(.value(node: self.node, value: .number(self.value)))
    }
}
