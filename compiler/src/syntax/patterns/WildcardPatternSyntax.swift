public struct WildcardPatternSyntax: Sendable { public let span: Span }

public func parseWildcardPattern(with parser: Parser) throws(ParseError) -> WildcardPatternSyntax {
    let span = parser.spanned()
    try parser.token(.underscoreKeyword)
    parser.commit(trace: "in this wildcard pattern")
    return WildcardPatternSyntax(span: span())
}

extension WildcardPatternSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitPattern(node: node, visitor: visitor, terminal: .match)

        visitor.codegen(node: node, with: WildcardPatternCodegen())
    }
}

private struct WildcardPatternCodegen: Codegenable {
    func codegen(with context: CodegenContext) throws {}
}
