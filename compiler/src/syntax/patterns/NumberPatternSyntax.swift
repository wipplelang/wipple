public struct NumberPatternSyntax: Sendable {
    public let span: Span
    public let value: Substring
}

public func parseNumberPattern(with parser: Parser) throws(ParseError) -> NumberPatternSyntax {
    let span = parser.spanned()
    return NumberPatternSyntax(span: span(), value: try parser.token(.number))
}

extension NumberPatternSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitPattern(node: node, visitor: visitor, terminal: .noMatch)

        let numberType = visitor.db.node(isHidden: true)
        visitor.visit(
            NamedTypeSyntax(span: self.span, name: "Number", parameters: []),
            as: numberType,
        )

        visitor.constraint(GroupConstraint(node, numberType))

        visitor.codegen(
            node: node,
            with: NumberPatternCodegen(node: node, value: String(self.value)),
        )
    }
}

private struct NumberPatternCodegen: Codegenable {
    let node: Node
    let value: String

    func codegen(with context: CodegenContext) throws {
        guard let matching = context.db[self.node, Matching.self]?.node else {
            throw CodegenError("unresolved")
        }

        context.condition(.equalToNumber(input: matching, value: self.value))
    }
}
