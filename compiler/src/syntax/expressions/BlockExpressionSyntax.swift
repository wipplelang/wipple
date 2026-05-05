public struct BlockExpressionSyntax: Sendable {
    public let span: Span
    public let statements: [any Visitable]
}

public func parseBlockExpression(with parser: Parser) throws(ParseError) -> BlockExpressionSyntax {
    let span = parser.spanned()
    try parser.token(.leftBrace)
    let statements = try parseStatements(with: parser)
    _ = try parseComments(with: parser)
    try parser.token(.rightBrace)
    return BlockExpressionSyntax(span: span(), statements: statements)
}

extension BlockExpressionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitExpression(node: node, with: visitor)

        visitor.pushScope()

        let statements = self.statements.map { statement in
            let statement = visitor.visit(statement)
            visitor.db.edge(from: statement, to: node, label: "statement")
            return statement
        }

        visitor.after(\.allExpressions) {
            let captures = visitor.currentScope.capturedVariables
            visitor.db[node, Captures.self] = .init(captures: captures)
        }

        visitor.popScope()

        visitor.constraint(
            TypeConstraint(
                node,
                .block(output: statements.last.map(Type.node) ?? .constructed(.unit)),
            )
        )

        visitor.codegen(
            node: node,
            with: BlockExpressionCodegen(node: node, statements: statements),
        )
    }
}

private struct BlockExpressionCodegen: Codegenable {
    let node: Node
    let statements: [Node]

    func codegen(with context: CodegenContext) throws {
        let captures = Array(context.db[self.node, Captures.self]?.captures ?? .init())

        context.pushInstructions()

        if self.statements.isEmpty {
            let unitTemporary = context.db.node()
            context.instruction(.value(node: unitTemporary, value: .tuple([])))
            context.instruction(.return(value: unitTemporary))
        } else {
            for (index, statement) in self.statements.enumerated() {
                context.instruction(.trace(location: statement))
                try context.codegen(statement)

                if index + 1 == self.statements.count {
                    context.instruction(.return(value: statement))
                }
            }
        }

        let instructions = context.popInstructions()

        context.instruction(
            .value(
                node: self.node,
                value: .function(inputs: [], captures: captures, instructions: instructions),
            )
        )
    }
}
