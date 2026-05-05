public struct DefaultConstraintSyntax: Sendable {
    public let span: Span
    public let parameter: TypeParameterSyntax
    public let value: any Visitable
}

public func parseDefaultConstraint(with parser: Parser) throws(ParseError)
    -> DefaultConstraintSyntax
{
    let span = parser.spanned()
    try parser.token(.leftParenthesis, reason: "between these parentheses")
    let parameter = try parseNamedTypeParameter(with: parser)
    try parser.token(.annotateOperator)
    parser.commit(trace: "in this type annotation")
    parser.consumeLineBreaks()
    let value = try parseType(with: parser)
    try parser.token(.rightParenthesis)

    return DefaultConstraintSyntax(span: span(), parameter: parameter, value: value)
}

extension DefaultConstraintSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitConstraint(node: node, with: visitor)

        let parameter = visitor.visit(self.parameter)
        let value = visitor.visit(self.value)

        visitor.db.edge(from: value, to: node, label: "value")

        visitor.constraint(DefaultConstraint(parameter, value))
    }
}
