public struct ExpressionStatementSyntax: Sendable {
    public let span: Span
    public let expression: any Visitable
}

public func parseExpressionStatement(with parser: Parser) throws(ParseError)
    -> ExpressionStatementSyntax
{
    let span = parser.spanned()
    _ = try parseComments(with: parser)
    let expression = try parseExpression(with: parser)
    return ExpressionStatementSyntax(span: span(), expression: expression)
}

extension ExpressionStatementSyntax: Visitable {
    public var isHidden: Bool { true }

    public func visit(node: Node, with visitor: Visitor) {
        visitor.db[node, Typed.self] = .init()

        visitor.after(\.allDefinitions) {
            let expression = visitor.visit(self.expression)
            visitor.db.replace(node, with: expression)
            visitor.constraint(GroupConstraint(node, expression))

            visitor.codegen(
                node: node,
                with: ExpressionStatementCodegen(node: node, expression: expression),
            )
        }
    }
}

private struct ExpressionStatementCodegen: Codegenable {
    let node: Node
    let expression: Node

    func codegen(with context: CodegenContext) throws {
        try context.codegen(self.expression)
        context.instruction(.value(node: self.node, value: .variable(self.expression)))
    }
}
