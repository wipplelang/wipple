public struct WhenArmSyntax: Sendable {
    public let span: Span
    public let pattern: any Visitable
    public let value: any Visitable
}

public struct WhenExpressionSyntax: Sendable {
    public let span: Span
    public let input: any Visitable
    public let arms: [WhenArmSyntax]
}

public func parseWhenExpression(with parser: Parser) throws(ParseError) -> WhenExpressionSyntax {
    let span = parser.spanned()
    try parser.token(.whenKeyword)
    parser.commit(trace: "in this `when` expression")
    let input = try parseAtomicExpression(with: parser)
    try parser.token(.leftBrace)
    let arms = try parseArms(with: parser)
    try parser.token(.rightBrace)
    return WhenExpressionSyntax(span: span(), input: input, arms: arms)
}

public func parseArm(with parser: Parser) throws(ParseError) -> WhenArmSyntax {
    let span = parser.spanned()
    let pattern = try parseAtomicPattern(with: parser)
    parser.commit(trace: "in this `when` arm")
    try parser.token(.functionOperator)
    parser.consumeLineBreaks()
    let value = try parseExpression(with: parser)
    return WhenArmSyntax(span: span(), pattern: pattern, value: value)
}

public func parseArms(with parser: Parser) throws(ParseError) -> [WhenArmSyntax] {
    try parser.parseLines(requireLineBreaks: true, parseArm)
}

extension WhenExpressionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitExpression(node: node, with: visitor)

        let input = visitor.visit(self.input)
        visitor.db.edge(from: input, to: node, label: "input")

        let arms = self.arms.map { arm in
            visitor.matching(value: input, allowOr: true, allowSet: false) {
                visitor.currentMatch!.root = input

                let pattern = visitor.db.node()
                visitor.currentMatch!.arm = pattern

                visitor.pushScope()

                visitor.visit(arm.pattern, as: pattern)
                visitor.db.edge(from: pattern, to: node, label: "pattern")

                let value = visitor.visit(arm.value)
                visitor.db.edge(from: value, to: node, label: "value")

                visitor.popScope()

                visitor.constraint(GroupConstraint(value, node))

                return (pattern, value)
            }
        }

        visitor.codegen(
            node: node,
            with: WhenExpressionCodegen(node: node, input: input, arms: arms),
        )
    }
}

private struct WhenExpressionCodegen: Codegenable {
    let node: Node
    let input: Node
    let arms: [(pattern: Node, value: Node)]

    func codegen(with context: CodegenContext) throws {
        try context.codegen(self.input)

        let branches = try self.arms.map { arm in
            context.pushConditions()
            try context.codegen(arm.pattern)
            let conditions = context.popConditions()

            context.pushInstructions()
            try context.codegen(arm.value)
            let instructions = context.popInstructions()

            return (conditions, instructions, Optional(arm.value))
        }

        context.instruction(.if(node: self.node, branches: branches, elseBranch: nil))
    }
}
