public struct ConstructorPatternSyntax: Sendable {
    public let span: Span
    public let constructor: Substring
    public let elements: [any Visitable]
}

public func parseParameterizedConstructorPattern(with parser: Parser) throws(ParseError)
    -> ConstructorPatternSyntax
{
    let span = parser.spanned()
    let constructor = try parseConstructorName(with: parser)
    let elements = try parser.parseMany(parseAtomicPattern)

    return ConstructorPatternSyntax(span: span(), constructor: constructor, elements: elements)
}

public func parseConstructorPattern(with parser: Parser) throws(ParseError)
    -> ConstructorPatternSyntax
{
    let span = parser.spanned()
    let constructor = try parseConstructorName(with: parser)
    return ConstructorPatternSyntax(span: span(), constructor: constructor, elements: [])
}

struct ExtraElement: Fact {
    public func render(into context: RenderContext) { context.string("is extra element") }
}

extension ConstructorPatternSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        enum ConstructorDefinition {
            case marker(MarkerConstructorDefinition)
            case variant(VariantConstructorDefinition)
        }

        let definition: ConstructorDefinition? = visitor.resolve(
            name: String(self.constructor),
            as: node,
        ) { definition in
            switch definition {
            case let definition as MarkerConstructorDefinition:
                (.marker(definition), definition.node)
            case let definition as VariantConstructorDefinition:
                (.variant(definition), definition.node)
            default: nil
            }
        }

        guard let definition else {
            visitPattern(node: node, visitor: visitor, terminal: nil)
            return
        }

        switch definition {
        case .marker(let definition):
            visitPattern(node: node, visitor: visitor, terminal: .match)

            for element in self.elements {
                let element = visitor.visitMatching(pattern: element)
                visitor.db.edge(from: element.pattern, to: node, label: "element")
                visitor.db[element.pattern, ExtraElement.self] = .init()
            }

            visitor.constraint(
                InstantiateConstraint(
                    .init(
                        sourceNode: node,
                        definition: definition.node,
                        replacements: Replacements(definition.node, node),
                        substitutions: Substitutions(),
                        applySubstitutions: true,
                    )
                )
            )

            visitor.codegen(node: node, with: ConstructorPatternCodegen.marker(node))
        case .variant(let definition):
            visitPattern(
                node: node,
                visitor: visitor,
                terminal: self.elements.isEmpty ? .variant(type: definition.variant) : nil,
            )

            let elements = self.elements.enumerated()
                .map { index, element in
                    let element = visitor.visitMatching(
                        pattern: element,
                        segment: .variantElement(
                            type: definition.variant,
                            index: index,
                            count: self.elements.count,
                        ),
                    )

                    visitor.db.edge(from: element.pattern, to: node, label: "element")

                    return element
                }

            if self.elements.isEmpty {
                visitor.constraint(
                    InstantiateConstraint(
                        .init(
                            sourceNode: node,
                            definition: definition.variant,
                            replacements: Replacements(definition.variant, node),
                            substitutions: Substitutions(),
                            applySubstitutions: true,
                        )
                    )
                )
            } else {
                let constructorNode = visitor.db.node()
                visitor.db[constructorNode, Typed.self] = .init()

                visitor.constraint(
                    InstantiateConstraint(
                        .init(
                            sourceNode: node,
                            definition: definition.variant,
                            replacements: Replacements(definition.variant, constructorNode),
                            substitutions: Substitutions(),
                            applySubstitutions: true,
                        )
                    )
                )

                visitor.constraint(
                    TypeConstraint(
                        constructorNode,
                        .function(
                            inputs: elements.map { .node($0.temporary) },
                            output: .node(node),
                        ),
                    )
                )
            }

            visitor.db[node, Temporaries.self] = .init(
                temporaries: .init(elements.map(\.temporary))
            )

            visitor.codegen(
                node: node,
                with: ConstructorPatternCodegen.variant(
                    node,
                    index: definition.index,
                    elements: elements,
                ),
            )
        }
    }
}

private enum ConstructorPatternCodegen: Fact, Codegenable {
    case marker(Node)
    case variant(Node, index: Int, elements: [(pattern: Node, temporary: Node)])

    func codegen(with context: CodegenContext) throws {
        switch self {
        case .marker: break
        case .variant(let node, let variant, let elements):

            guard let matching = context.db[node, Matching.self]?.node else {
                throw CodegenError("unresolved")
            }

            context.condition(.equalToVariant(input: matching, variant: variant))

            for (index, element) in elements.enumerated() {
                context.condition(
                    .initialize(
                        variable: element.temporary,
                        node: nil,
                        value: .variantElement(input: matching, variant: variant, index: index),
                        mutable: false,
                    )
                )

                try context.codegen(element.pattern)
            }
        }
    }
}
