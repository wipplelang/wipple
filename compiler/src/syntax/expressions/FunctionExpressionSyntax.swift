public struct FunctionExpressionSyntax: Sendable {
    public let span: Span
    public let inputs: [any Visitable]
    public let output: any Visitable
}

public func parseFunctionExpression(with parser: Parser) throws(ParseError)
    -> FunctionExpressionSyntax
{
    let span = parser.spanned()
    let inputs = try parseFunctionExpressionInputs(with: parser)
    let output = try parseExpression(with: parser)
    return FunctionExpressionSyntax(span: span(), inputs: inputs, output: output)
}

public func parseFunctionExpressionInputs(with parser: Parser) throws(ParseError) -> [any Visitable]
{
    let inputs = try parser.parseMany(min: 1, parseAtomicPattern)

    try parser.token(.functionOperator)
    parser.commit(trace: "in this function")
    parser.consumeLineBreaks()

    return inputs
}

extension FunctionExpressionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitExpression(node: node, with: visitor)

        visitor.pushScope()

        let inputs = self.inputs.map { input in
            let inputNode = visitor.db.node()

            visitor.matching(value: inputNode) {
                visitor.currentMatch!.isMutable = false
                visitor.currentMatch!.root = inputNode
                visitor.visit(input, as: inputNode)
            }

            visitor.db.edge(from: inputNode, to: node, label: "input")

            return inputNode
        }

        let output = visitor.visit(self.output)
        visitor.db.edge(from: output, to: node, label: "output")

        visitor.after(\.allExpressions) {
            visitor.db[node, Captures.self] = .init(
                captures: visitor.currentScope.capturedVariables
            )
        }

        visitor.popScope()

        visitor.constraint(
            TypeConstraint(node, .function(inputs: inputs.map(Type.node), output: .node(output)))
        )

        visitor.codegen(
            node: node,
            with: FunctionExpressionCodegen(node: node, inputs: inputs, output: output),
        )
    }
}

private struct FunctionExpressionCodegen: Codegenable {
    let node: Node
    let inputs: [Node]
    let output: Node

    func codegen(with context: CodegenContext) throws {
        let captures = Array(context.db[self.node, Captures.self]?.captures ?? .init())

        context.pushInstructions()
        context.pushConditions()
        for input in self.inputs { try context.codegen(input) }
        let conditions = context.popConditions()

        context.instruction(.if(node: nil, branches: [(conditions, [], nil)], elseBranch: nil))
        try context.codegen(self.output)
        context.instruction(.return(value: self.output))

        let instructions = context.popInstructions()
        context.instruction(
            .value(
                node: self.node,
                value: .function(
                    inputs: self.inputs,
                    captures: captures,
                    instructions: instructions,
                ),
            )
        )
    }
}
