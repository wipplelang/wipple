public struct ConstructorExpressionSyntax: Sendable {
    public let span: Span
    public let constructor: Substring
}

public func parseConstructorExpression(with parser: Parser) throws(ParseError)
    -> ConstructorExpressionSyntax
{
    let span = parser.spanned()
    let constructor = try parseConstructorName(with: parser)
    return ConstructorExpressionSyntax(span: span(), constructor: constructor)
}

extension ConstructorExpressionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitExpression(node: node, with: visitor)

        let definition = visitor.resolve(name: String(self.constructor), as: node) { definition in
            switch definition {
            case is TraitDefinition, is VariantConstructorDefinition,
                is MarkerConstructorDefinition:
                definition
            default: nil
            }
        }

        guard let definition else { return }

        visitor.db.edge(from: definition.node, to: node, label: "instantiated")

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

        switch definition {
        case let definition as TraitDefinition:
            visitor.constraint(
                BoundConstraint(
                    node,
                    .init(
                        sourceNode: node,
                        boundNode: node,
                        traitNode: definition.node,
                        substitutions: substitutions,
                    ),
                )
            )

            visitor.codegen(
                node: node,
                with: ConstructorExpressionCodegen.trait(
                    node: node,
                    isGeneric: visitor.currentDefinition != nil,
                ),
            )
        case let definition as VariantConstructorDefinition:
            let result: Node
            if definition.elements.isEmpty {
                result = node
            } else {
                result = visitor.db.node()
                visitor.db[result, Typed.self] = .init()

                let elements = definition.elements.map { _ in
                    let temporary = visitor.db.node()
                    visitor.db[temporary, Typed.self] = .init()
                    return temporary
                }

                visitor.constraint(
                    TypeConstraint(
                        node,
                        .function(inputs: elements.map(Type.node), output: .node(result)),
                    )
                )
            }

            visitor.codegen(
                node: node,
                with: ConstructorExpressionCodegen.variant(
                    node: node,
                    index: definition.index,
                    elements: definition.elements,
                    result: result,
                ),
            )
        case is MarkerConstructorDefinition:
            visitor.codegen(node: node, with: ConstructorExpressionCodegen.marker(node: node))
        default: fatalError("unreachable")
        }
    }
}

private enum ConstructorExpressionCodegen: Codegenable {
    case trait(node: Node, isGeneric: Bool)
    case variant(node: Node, index: Int, elements: [Node], result: Node)
    case marker(node: Node)

    func codegen(with context: CodegenContext) throws {
        switch self {
        case .marker(let node): context.instruction(.value(node: node, value: .marker))
        case .trait(let node, let isGeneric):
            guard let resolved = context.db[node, Bounds.self]?.bounds[node]?.1 else {
                throw CodegenError("unresolved")
            }

            switch try context.codegenInstance(resolved, isGeneric: isGeneric) {
            case .bound(let bound): context.instruction(.value(node: node, value: .bound(bound)))
            case .definition(let key):
                context.instruction(.value(node: node, value: .constant(key)))
            }
        case .variant(let node, let index, let elements, let result):
            if elements.isEmpty {
                context.instruction(
                    .value(node: node, value: .variant(index: index, elements: []))
                )
            } else {
                context.instruction(
                    .value(
                        node: node,
                        value: .function(
                            inputs: elements,
                            captures: [],
                            instructions: [
                                .value(
                                    node: result,
                                    value: .variant(index: index, elements: elements),
                                ), .return(value: result),
                            ],
                        ),
                    )
                )
            }
        }
    }
}
