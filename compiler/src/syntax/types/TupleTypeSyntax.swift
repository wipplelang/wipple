public struct TupleTypeSyntax: Sendable {
    public let span: Span
    public let elements: [any Visitable]
}

public func parseTupleType(with parser: Parser) throws(ParseError) -> TupleTypeSyntax {
    let span = parser.spanned()
    let elements =
        try parser.parseMany(min: 1, parseTypeElement) { parser throws(ParseError) in
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

    return TupleTypeSyntax(span: span(), elements: elements)
}

extension TupleTypeSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitType(node: node, with: visitor)

        let elements = self.elements.map { visitor.visit($0) }

        for element in elements { visitor.db.edge(from: element, to: node, label: "element") }

        visitor.constraint(TypeConstraint(node, .tuple(elements: elements.map(Type.node))))
    }
}
