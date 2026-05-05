struct MissingInstanceValue: Fact {
    func render(into context: RenderContext) { context.string("is missing instance value") }
}

struct ExtraInstanceValue: Fact {
    func render(into context: RenderContext) { context.string("is extra instance value") }
}

public struct InstanceDefinitionSyntax: Sendable {
    public let span: Span
    public let comments: [Substring]
    public let attributes: [AttributeSyntax]
    public let bound: BoundConstraintSyntax
    public let constraints: [any Visitable]
    public let value: (any Visitable)?
}

public func parseInstanceDefinitionStatement(with parser: Parser) throws(ParseError)
    -> InstanceDefinitionSyntax
{
    let comments = try parseComments(with: parser)
    let attributes = try parseAttributes(with: parser)
    let span = parser.spanned()
    let (bound, constraints) = try parseInstanceConstraints(with: parser)
    let instanceSpan = span()
    let value = try parser.parseOptional { parser throws(ParseError) in
        try parser.token(.assignOperator)
        parser.commit(trace: "in this instance definition")
        parser.consumeLineBreaks()
        return try parseExpression(with: parser)
    }

    return InstanceDefinitionSyntax(
        span: instanceSpan,
        comments: comments,
        attributes: attributes,
        bound: bound,
        constraints: constraints,
        value: value,
    )
}

public func parseInstanceConstraints(with parser: Parser) throws(ParseError) -> (
    BoundConstraintSyntax, [any Visitable]
) {
    try parser.token(.instanceKeyword)
    parser.commit(trace: "in this instance definition")
    let bound = try parseBoundConstraint(with: parser)
    let constraints = try parser.parseOptional(parseConstraints) ?? []
    return (bound, constraints)
}

extension InstanceDefinitionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        let attributes = self.attributes.map { visitor.visit($0) }

        visitor.defining(node) {
            visitor.pushScope()

            let attributes = InstanceDefinition.Attributes.parse(attributes, db: visitor.db)

            var traitNode: Node?
            let substitutions = Substitutions()
            visitor.after(\.typeDefinitions) {
                guard
                    let definition = visitor.resolve(name: String(self.bound.traitName), as: node)
                        as TraitDefinition?
                else { return }

                traitNode = definition.node

                visitor.withImplicitTypeParameters {
                    let parameters = self.bound.parameters.map { visitor.visit($0) }

                    let (missing, extra) = exactForEach(definition.parameters, parameters) {
                        parameter,
                        substitution in substitutions[parameter] = .node(substitution)
                    }

                    if !missing.isEmpty {
                        visitor.db[node, MissingTypes.self] = .init(types: Array(missing))
                    }

                    for node in extra { visitor.db[node, ExtraType.self] = .init() }

                    for constraint in self.constraints {
                        let constraint = visitor.visit(constraint)
                        visitor.db.edge(from: constraint, to: node, label: "constraint")
                    }

                    visitor.constraint(
                        InstantiateConstraint(
                            .init(
                                sourceNode: node,
                                definition: definition.node,
                                replacements: Replacements(definition.node, node),
                                substitutions: substitutions,
                                applySubstitutions: false,
                            )
                        )
                    )
                }
            }

            let definition = InstanceDefinition(
                node: node,
                comments: self.comments.map(String.init),
                attributes: attributes,
                traitNode: node,
            )

            visitor.after(\.allDefinitions) {
                guard let traitNode else { return }

                if let value = self.value {
                    visitor.withinConstantValue {
                        let value = visitor.visit(value)
                        visitor.db.edge(from: value, to: node, label: "value")
                        visitor.constraint(GroupConstraint(value, node))
                        definition.value = value
                    }

                    if attributes.error { visitor.db[node, ExtraInstanceValue.self] = .init() }
                } else if !attributes.error {
                    visitor.db[node, MissingInstanceValue.self] = .init()
                }

                visitor.db[traitNode, Instances.self, default: .init()].instances
                    .append(
                        .init(
                            node: node,
                            traitNode: traitNode,
                            substitutions: substitutions,
                            isDefault: attributes.default,
                            isError: attributes.error,
                        )
                    )
            }

            visitor.popScope()

            visitor.codegen(node: node, with: InstanceDefinitionCodegen())

            return definition
        }
    }
}

private struct InstanceDefinitionCodegen: Codegenable {
    func codegen(with context: CodegenContext) throws {}
}
