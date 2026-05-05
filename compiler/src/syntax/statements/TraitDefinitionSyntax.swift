public struct TraitDefinitionSyntax: Sendable {
    public let span: Span
    public let comments: [Substring]
    public let attributes: [AttributeSyntax]
    public let name: Substring
    public let parameters: [any Visitable]
    public let type: any Visitable
    public let constraints: [any Visitable]
}

public func parseTraitDefinitionStatement(with parser: Parser) throws(ParseError)
    -> TraitDefinitionSyntax
{
    let comments = try parseComments(with: parser)
    let attributes = try parseAttributes(with: parser)
    let span = parser.spanned()
    let name = try parseTypeName(with: parser)
    try parser.token(.assignOperator)
    parser.consumeLineBreaks()
    let parameters = try parseTypeParameters(with: parser)
    try parser.token(.traitKeyword)
    parser.commit(trace: "in this trait definition")
    let traitSpan = span()
    let (type, constraints) = try parseTraitConstraints(with: parser)

    return TraitDefinitionSyntax(
        span: traitSpan,
        comments: comments,
        attributes: attributes,
        name: name,
        parameters: parameters,
        type: type,
        constraints: constraints,
    )
}

public func parseTraitConstraints(with parser: Parser) throws(ParseError) -> (
    any Visitable, [any Visitable]
) {
    let type = try parseAtomicType(with: parser)
    let constraints = try parser.parseOptional(parseConstraints) ?? []
    return (type, constraints)
}

extension TraitDefinitionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        let attributes = self.attributes.map { visitor.visit($0) }

        visitor.defining(node) {
            visitor.pushScope()

            let parameters = visitor.withImplicitTypeParameters {
                self.parameters.map { visitor.visit($0) }
            }

            visitor.after(\.allDefinitions) {
                let type = visitor.visit(self.type)
                visitor.db.edge(from: type, to: node, label: "type")

                visitor.constraint(GroupConstraint(node, type))

                for constraint in self.constraints {
                    let constraint = visitor.visit(constraint)
                    visitor.db.edge(from: constraint, to: node, label: "constraint")
                }
            }

            visitor.popScope()

            let definition = TraitDefinition(
                node: node,
                name: String(self.name),
                comments: self.comments.map(String.init),
                attributes: .parse(attributes, db: visitor.db),
                parameters: parameters,
            )

            visitor.define(definition)

            visitor.codegen(node: node, with: TraitDefinitionCodegen())

            return definition
        }
    }
}

private struct TraitDefinitionCodegen: Codegenable {
    func codegen(with context: CodegenContext) throws {}
}
