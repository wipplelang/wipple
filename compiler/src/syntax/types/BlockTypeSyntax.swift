public struct BlockTypeSyntax: Sendable {
    public let span: Span
    public let output: any Visitable
}

public func parseBlockType(with parser: Parser) throws(ParseError) -> BlockTypeSyntax {
    let span = parser.spanned()
    try parser.token(.leftBrace)
    let output = try parseTypeElement(with: parser)
    try parser.token(.rightBrace)
    return BlockTypeSyntax(span: span(), output: output)
}

extension BlockTypeSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitType(node: node, with: visitor)

        let output = visitor.visit(self.output)
        visitor.db.edge(from: output, to: node, label: "output")

        visitor.constraint(TypeConstraint(node, .block(output: .node(output))))
    }
}
