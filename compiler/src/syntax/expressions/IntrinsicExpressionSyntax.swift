public struct IntrinsicExpressionSyntax: Sendable {
    public let span: Span
    public let name: Substring
    public let inputs: [any Visitable]
}

public func parseIntrinsicExpression(with parser: Parser) throws(ParseError)
    -> IntrinsicExpressionSyntax
{
    let span = parser.spanned()
    try parser.token(.intrinsicKeyword)
    parser.commit(trace: "in this `intrinsic` expression")
    let name = try parser.token(.string)
    let inputs = try parser.parseMany(parseAtomicExpression)

    return IntrinsicExpressionSyntax(span: span(), name: name, inputs: inputs)
}

extension IntrinsicExpressionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitExpression(node: node, with: visitor)

        let inputs = self.inputs.map { input in
            let input = visitor.visit(input)
            visitor.db.edge(from: input, to: node, label: "input")
            return input
        }

        visitor.codegen(
            node: node,
            with: IntrinsicExpressionCodegen(node: node, name: String(self.name), inputs: inputs),
        )
    }
}

private struct IntrinsicExpressionCodegen: Codegenable {
    let node: Node
    let name: String
    let inputs: [Node]

    func codegen(with context: CodegenContext) throws {
        for input in self.inputs { try context.codegen(input) }

        context.instruction(
            .value(node: self.node, value: .runtime(name: self.name, inputs: self.inputs))
        )
    }
}
