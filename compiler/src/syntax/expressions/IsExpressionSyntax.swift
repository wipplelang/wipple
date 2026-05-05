public struct IsExpressionSyntax: Sendable {
    public let span: Span
    public let left: any Visitable
    public let right: any Visitable
}

public func parseIsExpression(with parser: Parser) throws(ParseError) -> IsExpressionSyntax {
    let span = parser.spanned()
    let left = try parseExpressionElement(with: parser)
    try parser.token(.isOperator)
    parser.consumeLineBreaks()
    let right = try parsePatternElement(with: parser)
    return IsExpressionSyntax(span: span(), left: left, right: right)
}

extension IsExpressionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitExpression(node: node, with: visitor)

        let left = visitor.visit(self.left)
        visitor.db.edge(from: left, to: node, label: "left")

        let right = visitor.matching(value: left, allowOr: true) {
            visitor.currentMatch!.root = left

            let right = visitor.visit(self.right)
            visitor.db.edge(from: right, to: node, label: "right")

            return right
        }

        let trueNode = visitor.visit(
            ConstructorExpressionSyntax(span: self.span, constructor: Substring("True"))
        )

        let falseNode = visitor.visit(
            ConstructorExpressionSyntax(span: self.span, constructor: Substring("False"))
        )

        visitor.constraint(GroupConstraint(node, trueNode))
        visitor.constraint(GroupConstraint(node, falseNode))

        visitor.codegen(
            node: node,
            with: IsExpressionCodegen(
                node: node,
                left: left,
                right: right,
                trueNode: trueNode,
                falseNode: falseNode,
            ),
        )
    }
}

private struct IsExpressionCodegen: Codegenable {
    let node: Node
    let left: Node
    let right: Node
    let trueNode: Node
    let falseNode: Node

    func codegen(with context: CodegenContext) throws {
        try context.codegen(self.left)

        context.pushConditions()
        try context.codegen(self.right)
        let conditions = context.popConditions()

        context.pushInstructions()
        try context.codegen(self.trueNode)
        let trueInstructions = context.popInstructions()

        context.pushInstructions()
        try context.codegen(self.falseNode)
        let falseInstructions = context.popInstructions()

        context.instruction(
            .if(
                node: self.node,
                branches: [(conditions, trueInstructions, self.trueNode)],
                elseBranch: (falseInstructions, self.falseNode),
            )
        )
    }
}
