public struct StringPatternSyntax: Sendable {
    public let span: Span
    public let value: Substring
}

public func parseStringPattern(with parser: Parser) throws(ParseError) -> StringPatternSyntax {
    let span = parser.spanned()
    return StringPatternSyntax(span: span(), value: try parser.token(.string))
}

extension StringPatternSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitPattern(node: node, visitor: visitor, terminal: .noMatch)

        let stringType = visitor.db.node(isHidden: true)
        visitor.visit(
            NamedTypeSyntax(span: self.span, name: "String", parameters: []),
            as: stringType,
        )

        visitor.constraint(GroupConstraint(node, stringType))

        visitor.codegen(
            node: node,
            with: StringPatternCodegen(node: node, value: String(self.value)),
        )
    }
}

private struct StringPatternCodegen: Codegenable {
    let node: Node
    let value: String

    func codegen(with context: CodegenContext) throws {
        guard let matching = context.db[self.node, Matching.self]?.node else {
            throw CodegenError("unresolved")
        }

        context.condition(.equalToString(input: matching, value: self.value))
    }
}
