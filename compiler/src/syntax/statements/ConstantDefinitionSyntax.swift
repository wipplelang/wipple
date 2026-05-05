struct MissingConstantValue: Fact {
    func render(into context: RenderContext) { context.string("is missing constant value") }
}

public struct ConstantDefinitionSyntax: Sendable {
    public let span: Span
    public let comments: [Substring]
    public let attributes: [AttributeSyntax]
    public let name: Substring
    public let type: any Visitable
    public let constraints: [any Visitable]
}

public func parseConstantDefinitionStatement(with parser: Parser) throws(ParseError)
    -> ConstantDefinitionSyntax
{
    let comments = try parseComments(with: parser)
    let attributes = try parseAttributes(with: parser)
    let span = parser.spanned()
    let name = try parseVariableName(with: parser)
    let (type, constraints) = try parseConstantConstraints(with: parser)

    return ConstantDefinitionSyntax(
        span: span(),
        comments: comments,
        attributes: attributes,
        name: name,
        type: type,
        constraints: constraints,
    )
}

public func parseConstantConstraints(with parser: Parser) throws(ParseError) -> (
    any Visitable, [any Visitable]
) {
    try parser.token(.annotateOperator)
    parser.commit(trace: "in this constant definition")
    parser.consumeLineBreaks()
    let type = try parseType(with: parser)
    let constraints = try parser.parseOptional(parseConstraints) ?? []
    return (type, constraints)
}

extension ConstantDefinitionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        let attributes = self.attributes.map { visitor.visit($0) }

        visitor.defining(node) {
            visitor.pushScope()

            visitor.after(\.typeDefinitions) {
                visitor.withImplicitTypeParameters {
                    let type = visitor.visit(self.type)
                    visitor.db.edge(from: type, to: node, label: "type")

                    for constraint in self.constraints {
                        let constraint = visitor.visit(constraint)
                        visitor.db.edge(from: constraint, to: node, label: "constraint")
                    }

                    visitor.constraint(GroupConstraint(node, type))
                }
            }

            visitor.popScope()

            let definition = ConstantDefinition(
                node: node,
                name: String(self.name),
                comments: self.comments.map(String.init),
                attributes: .parse(attributes, db: visitor.db),
            )

            visitor.define(definition)

            visitor.after(\.allExpressions) {
                guard
                    let definition = visitor.db[node, Defined.self]?.definition
                        as? ConstantDefinition
                else { return }

                if definition.value == nil {
                    visitor.db[node, MissingConstantValue.self] = .init()
                }
            }

            visitor.codegen(node: node, with: ConstantDefinitionCodegen())

            return definition
        }
    }
}

private struct ConstantDefinitionCodegen: Codegenable {
    func codegen(with context: CodegenContext) throws {}
}
