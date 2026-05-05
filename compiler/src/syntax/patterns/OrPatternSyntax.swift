public struct OrPatternSyntax: Sendable {
    public let span: Span
    public let patterns: [any Visitable]
}

public func parseOrPattern(with parser: Parser) throws(ParseError) -> OrPatternSyntax {
    let span = parser.spanned()
    let patterns =
        try parser.parseMany(min: 2, parsePatternElement) { parser throws(ParseError) in
            try parser.token(.orOperator)
            parser.consumeLineBreaks()
        }
        .map(\.0)

    return OrPatternSyntax(span: span(), patterns: patterns)
}

struct InvalidOrPattern: Fact {
    public func render(into context: RenderContext) { context.string("is invalid `or` pattern") }
}

extension OrPatternSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitPattern(node: node, visitor: visitor, terminal: nil)

        if !visitor.currentMatch!.allowOr { visitor.db[node, InvalidOrPattern.self] = .init() }

        let previousArm = visitor.currentMatch!.arm.take()
        defer { visitor.currentMatch!.arm = previousArm }

        let patterns = self.patterns.map { pattern in
            let patternNode = visitor.db.node()

            if previousArm != nil {
                // HACK: This only works because `or` is only supported at the
                // top level, effectively making each pattern its own arm
                visitor.currentMatch!.arm = patternNode
            } else {
                // Not checking for exhaustiveness here (e.g. inside an `is` expression)
            }

            visitor.visit(pattern, as: patternNode)
            visitor.db.edge(from: patternNode, to: node, label: "pattern")
            visitor.constraint(GroupConstraint(node, patternNode))

            return patternNode
        }

        visitor.currentMatch!.arm = previousArm

        visitor.codegen(node: node, with: OrPatternCodegen(patterns: patterns))
    }
}

private struct OrPatternCodegen: Codegenable {
    let patterns: [Node]

    func codegen(with context: CodegenContext) throws {
        var conditions: [[IR.Condition]] = []
        for pattern in self.patterns {
            context.pushConditions()
            try context.codegen(pattern)
            conditions.append(context.popConditions())
        }

        context.condition(.or(conditions))
    }
}
