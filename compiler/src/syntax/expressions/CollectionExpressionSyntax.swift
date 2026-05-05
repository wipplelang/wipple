public struct CollectionExpressionSyntax: Sendable {
    public let span: Span
    public let elements: [any Visitable]
}

public func parseEmptyCollectionExpression(with parser: Parser) throws(ParseError)
    -> CollectionExpressionSyntax
{
    let span = parser.spanned()
    try parser.token(.collectionOperator)
    return CollectionExpressionSyntax(span: span(), elements: [])
}

public func parseCollectionExpression(with parser: Parser) throws(ParseError)
    -> CollectionExpressionSyntax
{
    let span = parser.spanned()
    let elements =
        try parser.parseMany(min: 1, parseExpressionElement) { parser throws(ParseError) in
            try parser.token(.collectionOperator)
            parser.consumeLineBreaks()
        }
        .map(\.0)

    if elements.count == 1 {
        try parser.token(.collectionOperator)
    } else {
        _ = try parser.parseOptional { parser throws(ParseError) in
            try parser.token(.collectionOperator)
        }
    }

    return CollectionExpressionSyntax(span: span(), elements: elements)
}

extension CollectionExpressionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitExpression(node: node, with: visitor)

        let elementType = visitor.db.node()
        visitor.db[elementType, Typed.self] = .init()

        var collection: any Visitable = ConstructorExpressionSyntax(
            span: self.span,
            constructor: Substring("Initial-Collection"),
        )

        for element in self.elements {
            let elementNode = visitor.db.node()

            visitor.db.edge(from: elementNode, to: node, label: "element")

            visitor.constraint(GroupConstraint(elementNode, elementType))

            collection = CallExpressionSyntax(
                span: element.span,
                function: ConstructorExpressionSyntax(
                    span: element.span,
                    constructor: "Build-Collection",
                ),
                inputs: [VisitAs(element, as: elementNode), collection],
            )
        }

        let collectionNode = visitor.visit(collection)
        visitor.db.edge(from: collectionNode, to: node, label: "collection")
        visitor.constraint(GroupConstraint(collectionNode, node))

        visitor.codegen(
            node: node,
            with: CollectionExpressionCodegen(node: node, collectionNode: collectionNode),
        )
    }
}

private struct CollectionExpressionCodegen: Codegenable {
    let node: Node
    let collectionNode: Node

    func codegen(with context: CodegenContext) throws {
        try context.codegen(self.collectionNode)
        context.instruction(.value(node: self.node, value: .variable(self.collectionNode)))
    }
}
