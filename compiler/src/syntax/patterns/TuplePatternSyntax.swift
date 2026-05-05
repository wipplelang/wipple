public struct TuplePatternSyntax: Sendable {
    public let span: Span
    public let elements: [any Visitable]
}

public func parseTuplePattern(with parser: Parser) throws(ParseError) -> TuplePatternSyntax {
    let span = parser.spanned()
    let elements =
        try parser.parseMany(min: 1, parsePatternElement) { parser throws(ParseError) in
            try parser.token(.tupleOperator)
            parser.consumeLineBreaks()
        }
        .map(\.0)

    if elements.count == 1 {
        try parser.token(.tupleOperator)
    } else {
        _ = try parser.parseOptional { parser throws(ParseError) in try parser.token(.tupleOperator)
        }
    }

    return TuplePatternSyntax(span: span(), elements: elements)
}

extension TuplePatternSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitPattern(node: node, visitor: visitor, terminal: nil)

        let elements = self.elements.enumerated()
            .map { index, element in
                let element = visitor.visitMatching(
                    pattern: element,
                    segment: .tupleElement(index: index, count: self.elements.count),
                )

                visitor.db.edge(from: element.pattern, to: node, label: "element")

                return element
            }

        visitor.constraint(
            TypeConstraint(node, .tuple(elements: elements.map { .node($0.temporary) }))
        )

        visitor.db[node, Temporaries.self] = .init(temporaries: .init(elements.map(\.temporary)))

        visitor.codegen(node: node, with: TuplePatternCodegen(node: node, elements: elements))
    }
}

private struct TuplePatternCodegen: Codegenable {
    let node: Node
    let elements: [(pattern: Node, temporary: Node)]

    func codegen(with context: CodegenContext) throws {
        guard let matching = context.db[self.node, Matching.self]?.node else {
            throw CodegenError("unresolved")
        }

        for (index, element) in self.elements.enumerated() {
            context.condition(
                .initialize(
                    variable: element.temporary,
                    node: nil,
                    value: .tupleElement(input: matching, index: index),
                    mutable: false,
                )
            )

            try context.codegen(element.pattern)
        }
    }
}
