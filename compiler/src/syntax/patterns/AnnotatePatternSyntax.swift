public struct AnnotatePatternSyntax: Sendable {
    public let span: Span
    public let pattern: any Visitable
    public let type: any Visitable
}

public func parseAnnotatePattern(with parser: Parser) throws(ParseError) -> AnnotatePatternSyntax {
    let span = parser.spanned()
    let pattern = try parsePatternElement(with: parser)
    try parser.token(.annotateOperator)
    parser.commit(trace: "in this type annotation")
    parser.consumeLineBreaks()
    let type = try parseTypeElement(with: parser)
    return AnnotatePatternSyntax(span: span(), pattern: pattern, type: type)
}

extension AnnotatePatternSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitPattern(node: node, visitor: visitor, terminal: nil)

        let pattern = visitor.visit(self.pattern)
        visitor.db.edge(from: pattern, to: node, label: "pattern")

        let type = visitor.visit(self.type)
        visitor.db.edge(from: type, to: node, label: "type")

        visitor.constraint(GroupConstraint(pattern, type))
        visitor.constraint(GroupConstraint(node, pattern))

        visitor.codegen(node: node, with: AnnotatePatternCodegen(pattern: pattern))
    }
}

private struct AnnotatePatternCodegen: Codegenable {
    let pattern: Node

    func codegen(with context: CodegenContext) throws { try context.codegen(self.pattern) }
}
