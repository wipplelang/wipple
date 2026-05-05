public struct StructureExpressionFieldSyntax: Sendable {
    public let span: Span
    public let name: Substring
    public let value: any Visitable
}

public struct StructureExpressionSyntax: Sendable {
    public let span: Span
    public let name: Substring
    public let fields: [StructureExpressionFieldSyntax]
}

public func parseStructureExpression(with parser: Parser) throws(ParseError)
    -> StructureExpressionSyntax
{
    let span = parser.spanned()
    let name = try parseTypeName(with: parser)
    try parser.token(.leftBrace)
    parser.commit(trace: "in this structure")
    let fields = try parseStructureExpressionFields(with: parser)
    try parser.token(.rightBrace)
    return StructureExpressionSyntax(span: span(), name: name, fields: fields)
}

public func parseStructureExpressionField(with parser: Parser) throws(ParseError)
    -> StructureExpressionFieldSyntax
{
    let span = parser.spanned()
    let name = try parseVariableName(with: parser)
    try parser.token(.assignOperator)
    parser.consumeLineBreaks()
    let value = try parseExpression(with: parser)
    return StructureExpressionFieldSyntax(span: span(), name: name, value: value)
}

public func parseStructureExpressionFields(with parser: Parser) throws(ParseError)
    -> [StructureExpressionFieldSyntax]
{ try parser.parseLines(min: 1, requireLineBreaks: true, parseStructureExpressionField) }

struct MissingField: Fact {
    let name: String

    func render(into context: RenderContext) { context.string("is missing field") }
}

struct ExtraField: Fact {
    let name: String

    func render(into context: RenderContext) { context.string("is extra field") }
}

struct DuplicateField: Fact {
    func render(into context: RenderContext) { context.string("is duplicate field") }
}

extension StructureExpressionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitExpression(node: node, with: visitor)

        guard
            let definition = visitor.resolve(name: String(self.name), as: node)
                as StructureConstructorDefinition?
        else { return }

        var fieldValues: [(Int, String, Node)] = []
        for field in self.fields {
            let value = visitor.visit(field.value)

            visitor.db.edge(from: value, to: node, label: "field")

            let fieldIndex = fieldValues.count
            fieldValues.append((fieldIndex, String(field.name), value))
        }

        let replacements = Replacements(definition.node, node)
        for (fieldName, fieldType) in definition.fields {
            if let (_, _, value) = fieldValues.first(where: { _, name, _ in name == fieldName }) {
                if replacements[fieldType] != nil {
                    visitor.db[node, DuplicateField.self] = .init()
                } else {
                    replacements[fieldType] = value
                }
            } else {
                visitor.db[node, MissingField.self] = .init(name: fieldName)
            }
        }

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
            with: StructureExpressionCodegen(node: node, fields: fieldValues),
        )
    }
}

private struct StructureExpressionCodegen: Codegenable {
    let node: Node
    let fields: [(Int, String, Node)]

    func codegen(with context: CodegenContext) throws {
        var fields = fields
        fields.sort { $0.0 < $1.0 }

        for (_, _, field) in fields { try context.codegen(field) }

        context.instruction(
            .value(
                node: self.node,
                value: .structure(fields.map { _, name, value in (name, value) }),
            )
        )
    }
}
