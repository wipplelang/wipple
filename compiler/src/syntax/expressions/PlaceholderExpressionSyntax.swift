public struct PlaceholderExpressionSyntax: Sendable { public let span: Span }

struct IsPlaceholder: Fact {
    func render(into context: RenderContext) { context.string("is a placeholder") }
}

public func parsePlaceholderExpression(with parser: Parser) throws(ParseError)
    -> PlaceholderExpressionSyntax
{
    let span = parser.spanned()
    try parser.token(.underscoreKeyword)
    parser.commit(trace: "in this placeholder expression")
    return PlaceholderExpressionSyntax(span: span())
}

extension PlaceholderExpressionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitExpression(node: node, with: visitor)
        visitor.db[node, IsPlaceholder.self] = .init()
        visitor.codegen(node: node, with: PlaceholderExpressionCodegen())
    }
}

private struct PlaceholderExpressionCodegen: Codegenable {
    func codegen(with context: CodegenContext) throws {}
}
