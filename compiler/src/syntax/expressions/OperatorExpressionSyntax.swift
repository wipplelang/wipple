public struct OperatorExpressionSyntax: Sendable {
    public let span: Span
    public let `operator`: Substring
    public let operatorSpan: Span
    public let left: any Visitable
    public let right: any Visitable
}

private enum Associativity {
    case left
    case right
}

public func parseParenthesizedOperatorExpression(with parser: Parser) throws(ParseError)
    -> ConstructorExpressionSyntax
{
    let span = parser.spanned()
    try parser.token(.leftParenthesis, reason: "between these parentheses")
    parser.consumeLineBreaks()
    let op = try parser.token(name: "an operator", matching: \.isBinaryOperator)
    parser.consumeLineBreaks()
    try parser.token(.rightParenthesis)

    guard let traitName = name(forTraitOperator: op) else {
        throw parser.error("This operator cannot be used as a function")
    }

    return ConstructorExpressionSyntax(span: span(), constructor: Substring(traitName))
}

public func parseOperatorExpression(with parser: Parser) throws(ParseError) -> any Visitable {
    let parseToExpression = { parser throws(ParseError) in
        try parseOperator(
            with: parser,
            operators: [.toOperator],
            associativity: .right,
            parseElement: parseExpressionElement,
        )
    }

    let parseByExpression = { parser throws(ParseError) in
        try parseOperator(
            with: parser,
            operators: [.byOperator],
            associativity: .left,
            parseElement: parseToExpression,
        )
    }

    let parsePowerExpression = { parser throws(ParseError) in
        try parseOperator(
            with: parser,
            operators: [.powerOperator],
            associativity: .right,
            parseElement: parseByExpression,
        )
    }

    let parseMultiplyExpression = { parser throws(ParseError) in
        try parseOperator(
            with: parser,
            operators: [.multiplyOperator, .divideOperator, .remainderOperator],
            associativity: .left,
            parseElement: parsePowerExpression,
        )
    }

    let parseAddExpression = { parser throws(ParseError) in
        try parseOperator(
            with: parser,
            operators: [.addOperator, .subtractOperator],
            associativity: .left,
            parseElement: parseMultiplyExpression,
        )
    }

    let parseCompareExpression = { parser throws(ParseError) in
        try parseOperator(
            with: parser,
            operators: [
                .lessThanOrEqualOperator, .lessThanOperator, .greaterThanOrEqualOperator,
                .greaterThanOperator,
            ],
            associativity: .left,
            parseElement: parseAddExpression,
        )
    }

    let parseEqualExpression = { parser throws(ParseError) in
        try parseOperator(
            with: parser,
            operators: [.equalOperator, .notEqualOperator],
            associativity: .left,
            parseElement: parseCompareExpression,
        )
    }

    let parseAndExpression = { parser throws(ParseError) in
        try parseOperator(
            with: parser,
            operators: [.andOperator],
            associativity: .left,
            parseElement: parseEqualExpression,
        )
    }

    let parseOrExpression = { parser throws(ParseError) in
        try parseOperator(
            with: parser,
            operators: [.orOperator],
            associativity: .left,
            parseElement: parseAndExpression,
        )
    }

    let parseApplyExpression = { parser throws(ParseError) in
        try parseOperator(
            with: parser,
            operators: [.applyOperator],
            associativity: .left,
            parseElement: parseOrExpression,
        )
    }

    return try parseApplyExpression(parser)
}

private func parseOperator(
    with parser: Parser,
    operators: [Token.Kind],
    associativity: Associativity,
    parseElement: @escaping (Parser) throws(ParseError) -> any Visitable,
) throws(ParseError) -> any Visitable {
    let elements = try parser.parseMany(min: 1, parseElement) { parser throws(ParseError) in
        parser.consumeLineBreaks()

        for op in operators {
            if let value = try parser.parseOptional({ parser throws(ParseError) in
                try parser.token(op)
            }) {
                parser.consumeLineBreaks()
                return value
            }
        }

        throw parser.error("Expected operator")
    }

    var (result, _) = elements[0]
    let rest = elements.dropFirst()

    let iter: any Sequence<_> =
        switch associativity {
        case .left: rest
        case .right: rest.reversed()
        }

    for (right, (op, operatorSpan)) in iter {
        result = OperatorExpressionSyntax(
            span: result.span.joined(with: right.span, in: parser.source),
            operator: op!,
            operatorSpan: operatorSpan,
            left: result,
            right: right,
        )
    }

    return result
}

private func name(forTraitOperator op: Substring) -> String? {
    switch op {
    case "to": "To"
    case "by": "By"
    case "^": "Power"
    case "*": "Multiply"
    case "/": "Divide"
    case "%": "Remainder"
    case "+": "Add"
    case "-": "Subtract"
    case "<": "Less-Than"
    case "<=": "Less-Than-Or-Equal"
    case ">": "Greater-Than"
    case ">=": "Greater-Than-Or-Equal"
    case "=": "Equal"
    case "/=": "Not-Equal"
    default: nil
    }
}

extension OperatorExpressionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitExpression(node: node, with: visitor)

        let operatorNode: Node =
            if let traitName = name(forTraitOperator: self.operator) {
                traitOperator(
                    visitor: visitor,
                    operatorSpan: self.operatorSpan,
                    node: node,
                    left: self.left,
                    right: self.right,
                    traitName: traitName,
                )
            } else {
                switch self.operator {
                case "and":
                    logicOperator(
                        visitor: visitor,
                        operatorSpan: self.operatorSpan,
                        node: node,
                        left: self.left,
                        right: self.right,
                        traitName: "And",
                    )
                case "or":
                    logicOperator(
                        visitor: visitor,
                        operatorSpan: self.operatorSpan,
                        node: node,
                        left: self.left,
                        right: self.right,
                        traitName: "Or",
                    )
                case ".":
                    applyOperator(visitor: visitor, node: node, left: self.left, right: self.right)
                default: fatalError("unknown operator: \(self.operator)")
                }
            }

        visitor.db.replace(node, with: operatorNode)
        visitor.constraint(GroupConstraint(node, operatorNode))

        visitor.codegen(
            node: node,
            with: OperatorExpressionCodegen(node: node, resolved: operatorNode),
        )
    }
}

private struct OperatorExpressionCodegen: Codegenable {
    let node: Node
    let resolved: Node

    func codegen(with context: CodegenContext) throws {
        try context.codegen(self.resolved)
        context.instruction(.value(node: self.node, value: .variable(self.resolved)))
    }
}

private func traitOperator(
    visitor: Visitor,
    operatorSpan: Span,
    node: Node,
    left: any Visitable,
    right: any Visitable,
    traitName: String,
) -> Node {
    let operatorNode = visitor.db.node()
    let leftNode = visitor.db.node()
    let rightNode = visitor.db.node()

    visitor.constraint(
        TypeConstraint(
            operatorNode,
            .function(inputs: [.node(leftNode), .node(rightNode)], output: .node(node)),
        )
    )

    return visitor.visit(
        CallExpressionSyntax(
            span: operatorSpan,
            function: VisitAs(
                ConstructorExpressionSyntax(span: operatorSpan, constructor: Substring(traitName)),
                as: operatorNode,
            ),
            inputs: [VisitAs(left, as: leftNode), VisitAs(right, as: rightNode)],
        )
    )
}

private func logicOperator(
    visitor: Visitor,
    operatorSpan: Span,
    node: Node,
    left: any Visitable,
    right: any Visitable,
    traitName: String,
) -> Node {
    let operatorNode = visitor.db.node()
    let leftNode = visitor.db.node()
    let rightNode = visitor.db.node()

    visitor.constraint(
        TypeConstraint(
            operatorNode,
            .function(
                inputs: [.node(leftNode), .constructed(.block(output: .node(rightNode)))],
                output: .node(node),
            ),
        )
    )

    return visitor.visit(
        CallExpressionSyntax(
            span: operatorSpan,
            function: VisitAs(
                ConstructorExpressionSyntax(span: operatorSpan, constructor: Substring(traitName)),
                as: operatorNode,
            ),
            inputs: [
                VisitAs(left, as: leftNode),
                BlockExpressionSyntax(
                    span: right.span,
                    statements: [
                        ExpressionStatementSyntax(
                            span: right.span,
                            expression: VisitAs(right, as: rightNode),
                        )
                    ],
                ),
            ],
        )
    )
}

private func applyOperator(
    visitor: Visitor,
    node: Node,
    left: any Visitable,
    right: any Visitable,
) -> Node {
    let leftNode = visitor.db.node()
    let rightNode = visitor.db.node()

    visitor.constraint(
        TypeConstraint(rightNode, .function(inputs: [.node(leftNode)], output: .node(node)))
    )

    return visitor.visit(
        CallExpressionSyntax(
            span: right.span,
            function: VisitAs(right, as: rightNode),
            inputs: [VisitAs(left, as: leftNode)],
        )
    )
}
