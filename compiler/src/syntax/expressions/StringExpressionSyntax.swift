public struct StringExpressionSyntax: Sendable {
    public let span: Span
    public let value: Substring
}

public func parseStringExpression(with parser: Parser) throws(ParseError) -> StringExpressionSyntax
{
    let span = parser.spanned()
    let value = try parser.token(.string)
    return StringExpressionSyntax(span: span(), value: value)
}

extension StringExpressionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitExpression(node: node, with: visitor)

        let stringType = visitor.db.node(isHidden: true)
        visitor.visit(
            NamedTypeSyntax(span: self.span, name: "String", parameters: []),
            as: stringType,
        )

        visitor.constraint(GroupConstraint(node, stringType))

        visitor.codegen(
            node: node,
            with: StringExpressionCodegen(node: node, value: String(self.value)),
        )
    }
}

private struct StringExpressionCodegen: Codegenable {
    let node: Node
    let value: String

    func codegen(with context: CodegenContext) throws {
        context.instruction(.value(node: self.node, value: .string(self.value)))
    }
}
