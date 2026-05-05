public struct AnnotateExpressionSyntax: Sendable {
    public let span: Span
    public let expression: any Visitable
    public let type: any Visitable
}

public func parseAnnotateExpression(with parser: Parser) throws(ParseError)
    -> AnnotateExpressionSyntax
{
    let span = parser.spanned()
    let expression = try parseOperatorExpression(with: parser)
    try parser.token(.annotateOperator)
    parser.commit(trace: "in this type annotation")
    parser.consumeLineBreaks()
    let type = try parseTypeElement(with: parser)
    return AnnotateExpressionSyntax(span: span(), expression: expression, type: type)
}

extension AnnotateExpressionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitExpression(node: node, with: visitor)

        let expression = visitor.visit(self.expression)
        let type = visitor.visit(self.type)

        visitor.db.edge(from: type, to: expression, label: "type")

        visitor.constraint(GroupConstraint(expression, type))
        visitor.constraint(GroupConstraint(node, expression))

        visitor.codegen(
            node: node,
            with: AnnotateExpressionCodegen(node: node, expression: expression),
        )
    }
}

private struct AnnotateExpressionCodegen: Codegenable {
    let node: Node
    let expression: Node

    func codegen(with context: CodegenContext) throws {
        try context.codegen(self.expression)
        context.instruction(.value(node: self.node, value: .variable(self.expression)))
    }
}
