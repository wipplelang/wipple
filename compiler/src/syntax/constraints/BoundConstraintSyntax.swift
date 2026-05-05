struct IsBound: Fact { func render(into context: RenderContext) {} }

public struct BoundConstraintSyntax: Sendable {
    public let span: Span
    public let traitName: Substring
    public let parameters: [any Visitable]
}

public func parseBoundConstraint(with parser: Parser) throws(ParseError) -> BoundConstraintSyntax {
    let span = parser.spanned()
    try parser.token(.leftParenthesis, reason: "between these parentheses")

    let traitName = try parseTypeName(with: parser)
    let parameters = try parser.parseMany(parseAtomicType)

    try parser.token(.rightParenthesis)

    return BoundConstraintSyntax(span: span(), traitName: traitName, parameters: parameters)
}

extension BoundConstraintSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitConstraint(node: node, with: visitor)
        visitor.db[node, IsBound.self] = .init()

        guard
            let traitDefinition = visitor.resolve(name: String(self.traitName), as: node)
                as TraitDefinition?
        else { return }

        let parameters = self.parameters.map { visitor.visit($0) }

        let substitutions = Substitutions()
        let (missing, extra) = exactForEach(traitDefinition.parameters, parameters) {
            parameter,
            substitution in substitutions[parameter] = .node(substitution)
        }

        if !missing.isEmpty { visitor.db[node, MissingTypes.self] = .init(types: Array(missing)) }

        for node in extra { visitor.db[node, ExtraType.self] = .init() }

        visitor.constraint(
            BoundConstraint(
                node,
                .init(
                    sourceNode: node,
                    boundNode: node,
                    traitNode: traitDefinition.node,
                    substitutions: substitutions,
                ),
            )
        )
    }
}
