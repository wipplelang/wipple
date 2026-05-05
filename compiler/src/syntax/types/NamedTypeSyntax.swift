public struct NamedTypeSyntax: Sendable {
    public let span: Span
    public let name: Substring
    public let parameters: [any Visitable]
}

public func parseNamedType(with parser: Parser) throws(ParseError) -> NamedTypeSyntax {
    let span = parser.spanned()
    let name = try parseTypeName(with: parser)
    return NamedTypeSyntax(span: span(), name: name, parameters: [])
}

public func parseParameterizedType(with parser: Parser) throws(ParseError) -> NamedTypeSyntax {
    let span = parser.spanned()
    let name = try parseTypeName(with: parser)
    let parameters = try parser.parseMany(min: 1, parseAtomicType)
    return NamedTypeSyntax(span: span(), name: name, parameters: parameters)
}

extension NamedTypeSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitType(node: node, with: visitor)

        let parameters = parameters.map { visitor.visit($0) }

        for parameter in parameters {
            visitor.db.edge(from: parameter, to: node, label: "parameter")
        }

        guard
            let typeDefinition = visitor.resolve(name: String(self.name), as: node)
                as TypeDefinition?
        else { return }

        let substitutions = Substitutions()
        let (missing, extra) = exactForEach(typeDefinition.parameters, parameters) {
            parameter,
            substitution in substitutions[parameter] = .node(substitution)
        }

        if !missing.isEmpty { visitor.db[node, MissingTypes.self] = .init(types: Array(missing)) }

        for node in extra { visitor.db[node, ExtraType.self] = .init() }

        visitor.constraint(
            InstantiateConstraint(
                .init(
                    sourceNode: node,
                    definition: typeDefinition.node,
                    replacements: Replacements(typeDefinition.node, node),
                    substitutions: substitutions,
                    applySubstitutions: false,
                )
            )
        )
    }
}
