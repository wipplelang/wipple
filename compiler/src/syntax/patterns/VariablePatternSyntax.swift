public struct VariablePatternSyntax: Sendable {
    public let span: Span
    public let variable: Substring
}

public func parseVariablePattern(with parser: Parser) throws(ParseError) -> VariablePatternSyntax {
    let span = parser.spanned()
    let variable = try parseVariableName(with: parser)
    return VariablePatternSyntax(span: span(), variable: variable)
}

extension VariablePatternSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitPattern(node: node, visitor: visitor, terminal: .match)

        let value = visitor.currentMatch!.value

        visitor.define(
            VariableDefinition(
                node: node,
                name: String(self.variable),
                value: value,
                isMutable: visitor.currentMatch!.isMutable,
            )
        )

        visitor.codegen(node: node, with: VariablePatternCodegen(node: node))
    }
}

private struct VariablePatternCodegen: Codegenable {
    let node: Node

    func codegen(with context: CodegenContext) throws {
        guard let matching = context.db[self.node, Matching.self]?.node else {
            throw CodegenError("unresolved")
        }

        context.condition(
            .initialize(
                variable: self.node,
                node: matching,
                value: .variable(matching),
                mutable: context.db.contains(IsMutated.self, for: self.node),
            )
        )
    }
}
