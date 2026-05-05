public struct TupleExpressionSyntax: Sendable {
    public let span: Span
    public let elements: [any Visitable]
}

public func parseTupleExpression(with parser: Parser) throws(ParseError) -> TupleExpressionSyntax {
    let span = parser.spanned()
    let elements =
        try parser.parseMany(min: 1, parseExpressionElement) { parser throws(ParseError) in
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

    return TupleExpressionSyntax(span: span(), elements: elements)
}

extension TupleExpressionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitExpression(node: node, with: visitor)

        let elements = self.elements.map { visitor.visit($0) }

        for element in elements { visitor.db.edge(from: element, to: node, label: "element") }

        visitor.constraint(TypeConstraint(node, .tuple(elements: elements.map(Type.node))))

        visitor.codegen(node: node, with: TupleExpressionCodegen(node: node, elements: elements))
    }
}

private struct TupleExpressionCodegen: Codegenable {
    let node: Node
    let elements: [Node]

    func codegen(with context: CodegenContext) throws {
        for element in self.elements { try context.codegen(element) }

        context.instruction(.value(node: self.node, value: .tuple(self.elements)))
    }
}
