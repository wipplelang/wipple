struct IsMutated: Fact { func render(into context: RenderContext) { context.string("is mutated") } }

struct IsCaptured: Fact {
    func render(into context: RenderContext) { context.string("is captured") }
}

public struct VariableExpressionSyntax: Sendable {
    public let span: Span
    public let variable: Substring
}

public func parseVariableExpression(with parser: Parser) throws(ParseError)
    -> VariableExpressionSyntax
{
    let span = parser.spanned()
    let variable = try parseVariableName(with: parser)
    return VariableExpressionSyntax(span: span(), variable: variable)
}

extension VariableExpressionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitExpression(node: node, with: visitor)

        let definition = visitor.resolve(name: String(self.variable), as: node) { definition in
            switch definition {
            case is VariableDefinition, is ConstantDefinition: definition
            default: nil
            }
        }

        guard let definition else { return }

        switch definition {
        case let definition as VariableDefinition:
            if visitor.capture(definition.node) {
                visitor.db[definition.node, IsCaptured.self] = .init()
            }

            visitor.db.replace(node, with: definition.node)
            visitor.constraint(GroupConstraint(node, definition.node))

            visitor.codegen(
                node: node,
                with: VariableExpressionCodegen.variable(node: node, resolved: definition.node),
            )
        case let definition as ConstantDefinition:
            let substitutions = Substitutions()

            visitor.constraint(
                InstantiateConstraint(
                    .init(
                        sourceNode: node,
                        definition: definition.node,
                        replacements: Replacements(definition.node, node),
                        substitutions: substitutions,
                        applySubstitutions: true,
                    )
                )
            )

            visitor.codegen(
                node: node,
                with: VariableExpressionCodegen.constant(
                    node: node,
                    definition: definition.node,
                    substitutions,
                    isGeneric: visitor.currentDefinition != nil,
                ),
            )
        default: break
        }
    }
}

private enum VariableExpressionCodegen: Codegenable {
    case variable(node: Node, resolved: Node)
    case constant(node: Node, definition: Node, Substitutions, isGeneric: Bool)

    func codegen(with context: CodegenContext) throws {
        switch self {
        case .variable(let node, let resolved):
            let isMutated = context.db.contains(IsMutated.self, for: resolved)

            context.instruction(
                .value(
                    node: node,
                    value: isMutated ? IR.Value.mutableVariable(resolved) : .variable(resolved),
                )
            )
        case .constant(let node, let definition, let substitutions, let isGeneric):
            let bounds = context.db[node, Bounds.self] ?? .init()

            let key = try context.codegenConstant(
                definition: definition,
                substitutions: substitutions,
                bounds: bounds,
                isGeneric: isGeneric,
            )

            context.instruction(.value(node: node, value: .constant(key)))
        }
    }
}
