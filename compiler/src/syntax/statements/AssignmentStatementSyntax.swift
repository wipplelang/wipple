public struct AssignmentStatementSyntax: Sendable {
    public let span: Span
    public let pattern: any Visitable
    public let value: any Visitable
}

public func parseAssignmentStatement(with parser: Parser) throws(ParseError)
    -> AssignmentStatementSyntax
{
    let span = parser.spanned()
    _ = try parseComments(with: parser)
    let pattern = try parsePattern(with: parser)
    try parser.token(.assignOperator)
    parser.commit(trace: "in this variable assignment")
    parser.consumeLineBreaks()
    let value = try parseExpression(with: parser)
    return AssignmentStatementSyntax(span: span(), pattern: pattern, value: value)
}

extension AssignmentStatementSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitor.constraint(TypeConstraint(node, .unit))

        visitor.after(\.allDefinitions) {
            // Try assigning to an existing constant in the current scope if possible
            if let pattern = self.pattern as? VariablePatternSyntax {
                for definition: ConstantDefinition in visitor.currentScope.peek(
                    name: String(pattern.variable)
                ) {
                    guard definition.value == nil else {
                        // TODO: Already assigned
                        return
                    }

                    visitor.defining(definition.node) {
                        visitor.withinConstantValue {
                            let value = visitor.visit(self.value)
                            visitor.constraint(GroupConstraint(value, definition.node))
                            definition.value = value
                        }

                        return definition
                    }

                    visitor.codegen(node: node, with: AssignmentStatementCodegen.constant)
                    return
                }
            }

            let value = visitor.visit(self.value)

            let pattern = visitor.matching(value: value, allowSet: true) {
                visitor.currentMatch!.root = value

                let pattern = visitor.db.node()
                visitor.currentMatch!.arm = pattern

                visitor.visit(self.pattern, as: pattern)

                return pattern
            }

            visitor.constraint(GroupConstraint(pattern, value))
            visitor.codegen(
                node: node,
                with: AssignmentStatementCodegen.variable(
                    node: node,
                    pattern: pattern,
                    value: value,
                ),
            )
        }
    }
}

private enum AssignmentStatementCodegen: Codegenable {
    case constant
    case variable(node: Node, pattern: Node, value: Node)

    func codegen(with context: CodegenContext) throws {
        switch self {
        case .constant: break
        case .variable(let node, let pattern, let value):
            try context.codegen(value)

            context.pushConditions()
            try context.codegen(pattern)
            let conditions = context.popConditions()

            context.instruction(
                .if(node: nil, branches: [(conditions, [], nil)], elseBranch: nil)
            )

            context.instruction(.value(node: node, value: .tuple([])))
        }
    }
}
