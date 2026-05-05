public struct DoExpressionSyntax: Sendable {
    public let span: Span
    public let input: any Visitable
}

public func parseDoExpression(with parser: Parser) throws(ParseError) -> DoExpressionSyntax {
    let span = parser.spanned()
    try parser.token(.doKeyword)
    parser.commit(trace: "in this `do` expression")
    return DoExpressionSyntax(span: span(), input: try parseAtomicExpression(with: parser))
}

extension DoExpressionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitExpression(node: node, with: visitor)

        let input = visitor.visit(self.input)
        visitor.db.edge(from: input, to: node, label: "input")

        visitor.constraint(TypeConstraint(input, .block(output: .node(node))))

        visitor.codegen(node: node, with: DoExpressionCodegen(node: node, input: input))
    }
}

private struct DoExpressionCodegen: Codegenable {
    let node: Node
    let input: Node

    func codegen(with context: CodegenContext) throws {
        try context.codegen(self.input)
        context.instruction(
            .value(node: self.node, value: .call(function: self.input, inputs: []))
        )
    }
}
