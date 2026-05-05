public struct TypeParameterSyntax: Sendable {
    public let span: Span
    public let name: Substring
    public let infer: Bool
    public let value: (any Visitable)?
}

public func parseTypeParameter(with parser: Parser) throws(ParseError) -> TypeParameterSyntax {
    if let value = try parser.parseOptional(parseNamedTypeParameter) { return value }
    if let value = try parser.parseOptional(parseInferTypeParameter) { return value }
    throw parser.error("Expected type parameter")
}

public func parseAnnotatedTypeParameter(with parser: Parser) throws(ParseError)
    -> TypeParameterSyntax
{
    let span = parser.spanned()
    let name = try parseTypeParameterName(with: parser)
    try parser.token(.annotateOperator)
    parser.commit(trace: "in this type annotation")
    parser.consumeLineBreaks()
    let value = try parseType(with: parser)

    return TypeParameterSyntax(span: span(), name: name, infer: false, value: value)
}

public func parseNamedTypeParameter(with parser: Parser) throws(ParseError) -> TypeParameterSyntax {
    let span = parser.spanned()
    let name = try parseTypeParameterName(with: parser)
    return TypeParameterSyntax(span: span(), name: name, infer: false, value: nil)
}

public func parseInferTypeParameter(with parser: Parser) throws(ParseError) -> TypeParameterSyntax {
    let span = parser.spanned()
    try parser.token(.leftParenthesis, reason: "between these parentheses")
    try parser.token(.inferKeyword)
    let name = try parseTypeParameterName(with: parser)
    try parser.token(.rightParenthesis)
    return TypeParameterSyntax(span: span(), name: name, infer: true, value: nil)
}

public func parseTypeParameters(with parser: Parser) throws(ParseError) -> [any Visitable] {
    let parameters = try parser.parseMany({ parser throws(ParseError) in
        try parseTypeParameter(with: parser)
    })

    if !parameters.isEmpty {
        try parser.token(.typeFunctionOperator)
        parser.consumeLineBreaks()
    }

    return parameters
}

extension TypeParameterSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitType(node: node, with: visitor)

        if let existing =
            visitor.peek(name: String(self.name), match: { $0 as? TypeParameterDefinition }).first
        {
            visitor.constraint(GroupConstraint(node, existing.node))
        } else if visitor.currentDefinition?.implicitTypeParameters ?? false {
            visitor.defining(node) {
                let definition = TypeParameterDefinition(node: node, name: String(self.name))
                visitor.define(definition)
                return definition
            }

            if let value = self.value {
                let value = visitor.visit(value)
                visitor.db.edge(from: value, to: node, label: "value")

                visitor.constraint(GroupConstraint(node, value))
            } else {
                visitor.constraint(TypeConstraint(node, .parameter(definition: node)))
            }

            if self.infer { visitor.db[node, InferredParameter.self] = .init() }

            visitor.db[visitor.currentDefinition!.node, TypeParameters.self, default: .init()]
                .typeParameters.append(node)
        }

        // Update the `Resolved` fact
        _ = visitor.resolve(name: String(self.name), as: node) as TypeParameterDefinition?
    }
}
