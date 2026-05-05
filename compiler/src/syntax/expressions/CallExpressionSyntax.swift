public struct ResolvedCall: Fact {
    let function: Node
    let inputs: [Node]
    let isUnit: Bool
}

public struct CallExpressionSyntax: Sendable {
    public let span: Span
    public let function: any Visitable
    public let inputs: [any Visitable]
}

public func parseCallExpression(with parser: Parser) throws(ParseError) -> CallExpressionSyntax {
    let span = parser.spanned()
    let function = try parseAtomicExpression(with: parser)
    let inputs = try parser.parseMany(min: 1, parseAtomicExpression)

    return CallExpressionSyntax(span: span(), function: function, inputs: inputs)
}

extension CallExpressionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitExpression(node: node, with: visitor)

        if self.inputs.count == 1, let variable = self.inputs[0] as? VariableExpressionSyntax {
            let unitConstant: ConstantDefinition? = visitor.peek(name: String(variable.variable))
                .first(where: \.attributes.unit)

            if unitConstant != nil {
                let unit = visitor.visit(self.inputs[0])
                let number = visitor.visit(self.function)

                visitor.constraint(
                    TypeConstraint(unit, .function(inputs: [.node(number)], output: .node(node)))
                )

                visitor.db.edge(from: number, to: node, label: "number")
                visitor.db.edge(from: unit, to: node, label: "unit")

                visitor.db[node, ResolvedCall.self] = .init(
                    function: unit,
                    inputs: [number],
                    isUnit: true,
                )

                visitor.codegen(
                    node: node,
                    with: CallExpressionCodegen.unit(node: node, number: number, unit: unit),
                )

                return
            }
        }

        let function = visitor.visit(self.function)
        let inputs = self.inputs.map { visitor.visit($0) }

        visitor.constraint(
            TypeConstraint(
                function,
                .function(inputs: inputs.map(Type.node), output: .node(node)),
            )
        )

        visitor.db[node, ResolvedCall.self] = .init(
            function: function,
            inputs: inputs,
            isUnit: false,
        )

        visitor.codegen(
            node: node,
            with: CallExpressionCodegen.function(node: node, function: function, inputs: inputs),
        )
    }
}

private enum CallExpressionCodegen: Codegenable {
    case function(node: Node, function: Node, inputs: [Node])
    case unit(node: Node, number: Node, unit: Node)

    func codegen(with context: CodegenContext) throws {
        switch self {
        case .function(let node, let function, let inputs):
            try context.codegen(function)
            for input in inputs { try context.codegen(input) }

            context.instruction(
                .value(node: node, value: .call(function: function, inputs: inputs))
            )
        case .unit(let node, let number, let unit):
            try context.codegen(number)
            try context.codegen(unit)

            context.instruction(
                .value(node: node, value: .call(function: unit, inputs: [number]))
            )
        }
    }
}
