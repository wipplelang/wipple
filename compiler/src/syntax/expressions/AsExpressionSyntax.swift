public struct AsExpressionSyntax: Sendable {
    public let span: Span
    public let left: any Visitable
    public let right: any Visitable
}

public func parseAsExpression(with parser: Parser) throws(ParseError) -> AsExpressionSyntax {
    let span = parser.spanned()
    let left = try parseExpressionElement(with: parser)
    try parser.token(.asOperator)
    parser.consumeLineBreaks()
    let right = try parseTypeElement(with: parser)
    return AsExpressionSyntax(span: span(), left: left, right: right)
}

extension AsExpressionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitExpression(node: node, with: visitor)

        let left = visitor.visit(self.left)
        visitor.db.edge(from: left, to: node, label: "left")

        let right = visitor.visit(self.right)
        visitor.db.edge(from: right, to: node, label: "right")

        let asFunction = visitor.visit(
            ConstructorExpressionSyntax(span: self.span, constructor: Substring("As"))
        )

        visitor.constraint(
            TypeConstraint(asFunction, .function(inputs: [.node(left)], output: .node(right)))
        )

        visitor.constraint(GroupConstraint(node, right))

        visitor.codegen(
            node: node,
            with: AsExpressionCodegen(node: node, asFunction: asFunction, left: left),
        )
    }
}

private struct AsExpressionCodegen: Codegenable {
    let node: Node
    let asFunction: Node
    let left: Node

    func codegen(with context: CodegenContext) throws {
        try context.codegen(self.asFunction)
        try context.codegen(self.left)

        context.instruction(
            .value(node: self.node, value: .call(function: self.asFunction, inputs: [self.left]))
        )
    }
}
