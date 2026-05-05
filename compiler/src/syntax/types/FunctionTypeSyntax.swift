public struct FunctionTypeSyntax: Sendable {
    public let span: Span
    public let inputs: [any Visitable]
    public let output: any Visitable
}

public func parseFunctionType(with parser: Parser) throws(ParseError) -> FunctionTypeSyntax {
    let span = parser.spanned()
    let inputs = try parseFunctionTypeInputs(with: parser)
    let output = try parseType(with: parser)
    return FunctionTypeSyntax(span: span(), inputs: inputs, output: output)
}

public func parseFunctionTypeInputs(with parser: Parser) throws(ParseError) -> [any Visitable] {
    let inputs = try parser.parseMany(min: 1, parseAtomicType)

    try parser.token(.functionOperator)
    parser.commit(trace: "in this function type")
    parser.consumeLineBreaks()

    return inputs
}

extension FunctionTypeSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitType(node: node, with: visitor)

        let inputs = self.inputs.map { visitor.visit($0) }

        for input in inputs { visitor.db.edge(from: input, to: node, label: "input") }

        let output = visitor.visit(self.output)
        visitor.db.edge(from: output, to: node, label: "output")

        visitor.constraint(
            TypeConstraint(node, .function(inputs: inputs.map(Type.node), output: .node(output)))
        )
    }
}
