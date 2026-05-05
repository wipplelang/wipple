public struct SetPatternSyntax: Sendable {
    public let span: Span
    public let variable: Substring
}

public func parseSetPattern(with parser: Parser) throws(ParseError) -> SetPatternSyntax {
    let span = parser.spanned()
    try parser.token(.setKeyword)
    parser.commit(trace: "in this `set` pattern")
    return SetPatternSyntax(span: span(), variable: try parseVariableName(with: parser))
}

enum InvalidSetPattern: Fact {
    case nested
    case immutable(Node)

    public func render(into context: RenderContext) { context.string("is invalid `set` pattern") }
}

extension SetPatternSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitPattern(node: node, visitor: visitor, terminal: .match)

        if !visitor.currentMatch!.allowSet { visitor.db[node, InvalidSetPattern.self] = .nested }

        guard
            let variableDefinition = visitor.resolve(name: String(self.variable), as: node)
                as VariableDefinition?
        else { return }

        if !variableDefinition.isMutable {
            visitor.db[node, InvalidSetPattern.self] = .immutable(variableDefinition.node)
        }

        visitor.constraint(GroupConstraint(node, variableDefinition.node))

        if visitor.capture(variableDefinition.node) {
            visitor.db[variableDefinition.node, IsCaptured.self] = .init()
        }

        visitor.db.replace(node, with: variableDefinition.node)

        visitor.db[variableDefinition.node, IsMutated.self] = .init()

        visitor.codegen(
            node: node,
            with: SetPatternCodegen(node: node, variable: variableDefinition.node),
        )
    }
}

private struct SetPatternCodegen: Codegenable {
    let node: Node
    let variable: Node

    func codegen(with context: CodegenContext) throws {
        guard let matching = context.db[self.node, Matching.self]?.node else {
            throw CodegenError("unresolved")
        }

        context.condition(.mutate(input: matching, variable: self.variable))
    }
}
