public struct StructurePatternFieldSyntax: Sendable {
    public let span: Span
    public let name: Substring
    public let pattern: any Visitable
}

public struct StructurePatternSyntax: Sendable {
    public let span: Span
    public let name: Substring
    public let fields: [StructurePatternFieldSyntax]
}

public func parseStructurePattern(with parser: Parser) throws(ParseError) -> StructurePatternSyntax
{
    let span = parser.spanned()
    let name = try parseTypeName(with: parser)
    try parser.token(.leftBrace)
    let fields = try parser.parseLines(min: 1, requireLineBreaks: true, parseStructurePatternField)
    try parser.token(.rightBrace)
    return StructurePatternSyntax(span: span(), name: name, fields: fields)
}

public func parseStructurePatternField(with parser: Parser) throws(ParseError)
    -> StructurePatternFieldSyntax
{
    let span = parser.spanned()
    let name = try parseVariableName(with: parser)
    try parser.token(.assignOperator)
    parser.consumeLineBreaks()
    let pattern = try parsePattern(with: parser)
    return StructurePatternFieldSyntax(span: span(), name: name, pattern: pattern)
}

struct ResolvedStructurePattern: Fact {
    var fields: [(index: Int?, name: String, value: Node)] = []
}

extension StructurePatternSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitPattern(node: node, visitor: visitor, terminal: nil)

        guard
            let definition = visitor.resolve(name: String(self.name), as: node)
                as StructureConstructorDefinition?
        else { return }

        var fieldPatterns: [(Int, String, pattern: Node, temporary: Node)] = []
        for field in self.fields {
            let (pattern, temporary) = visitor.visitMatching(
                pattern: field.pattern,
                segment: definition.fields.first(where: { name, _ in name == field.name })
                    .map { _, type in .field(type: type) },
            )

            visitor.db.edge(from: pattern, to: node, label: "field")

            let fieldIndex = fieldPatterns.count
            fieldPatterns.append((fieldIndex, String(field.name), pattern, temporary))
        }

        let fieldValues = fieldPatterns.map { index, name, pattern, temporary in
            let valueIndex = definition.fields.firstIndex { fieldName, _ in fieldName == name }
            return (valueIndex, name, pattern, temporary)
        }

        let replacements = Replacements(definition.node, node)
        for (fieldName, fieldType) in definition.fields {
            if let (_, _, _, temporary) = fieldPatterns.first(where: { _, name, _, _ in
                name == fieldName
            }) {
                replacements[fieldType] = temporary
            }
        }

        visitor.db[node, Temporaries.self] = .init(
            temporaries: .init(fieldPatterns.map(\.temporary))
        )

        visitor.constraint(
            InstantiateConstraint(
                .init(
                    sourceNode: node,
                    definition: definition.node,
                    replacements: replacements,
                    substitutions: Substitutions(),
                    applySubstitutions: true,
                )
            )
        )

        visitor.codegen(
            node: node,
            with: StructurePatternCodegen(node: node, fields: fieldValues),
        )
    }
}

private struct StructurePatternCodegen: Codegenable {
    let node: Node
    let fields: [(index: Int?, name: String, pattern: Node, temporary: Node)]

    func codegen(with context: CodegenContext) throws {
        guard let matching = context.db[self.node, Matching.self]?.node else {
            throw CodegenError("unresolved")
        }

        for (index, name, pattern, temporary) in self.fields {
            guard let index else { throw CodegenError("unresolved") }

            context.condition(
                .initialize(
                    variable: temporary,
                    node: nil,
                    value: .field(input: matching, fieldName: name, fieldIndex: index),
                    mutable: false,
                )
            )

            try context.codegen(pattern)
        }
    }
}
