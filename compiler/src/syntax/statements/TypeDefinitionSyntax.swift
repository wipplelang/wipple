struct DuplicateFieldDefinition: Fact {
    func render(into context: RenderContext) { context.string("duplicate field definition") }
}

struct DuplicateVariantDefinition: Fact {
    func render(into context: RenderContext) { context.string("duplicate variant definition") }
}

struct StructureFields: Fact {
    var fields: [Node]

    func render(into context: RenderContext) { context.string("has structure fields") }
}

struct EnumerationVariants: Fact {
    var variants: [(variant: Node, elements: [Node])] = []

    func render(into context: RenderContext) { context.string("has enumeration variants") }
}

public struct FieldDefinitionSyntax: Visitable {
    public let span: Span
    public let name: Substring
    public let type: any Visitable

    public func visit(node: Node, with visitor: Visitor) {
        // handled in `TypeDefinitionSyntax.visit`
    }
}

public struct VariantDefinitionSyntax: Visitable {
    public let span: Span
    public let name: Substring
    public let elements: [any Visitable]

    public func visit(node: Node, with visitor: Visitor) {
        // handled in `TypeDefinitionSyntax.visit`
    }
}

public struct StructureTypeRepresentationSyntax: Visitable {
    public let span: Span
    public let fields: [FieldDefinitionSyntax]

    public func visit(node: Node, with visitor: Visitor) {
        // handled in `TypeDefinitionSyntax.visit`
    }
}

public struct EnumerationTypeRepresentationSyntax: Visitable {
    public let span: Span
    public let variants: [VariantDefinitionSyntax]

    public func visit(node: Node, with visitor: Visitor) {
        // handled in `TypeDefinitionSyntax.visit`
    }
}

public struct MarkerTypeRepresentationSyntax: Visitable {
    public let span: Span

    public func visit(node: Node, with visitor: Visitor) {
        // handled in `TypeDefinitionSyntax.visit`
    }
}

public struct TypeDefinitionSyntax: Sendable {
    public let span: Span
    public let comments: [Substring]
    public let attributes: [AttributeSyntax]
    public let name: Substring
    public let parameters: [any Visitable]
    public let representation: any Visitable
}

public func parseTypeDefinitionStatement(with parser: Parser) throws(ParseError)
    -> TypeDefinitionSyntax
{
    let comments = try parseComments(with: parser)
    let attributes = try parseAttributes(with: parser)
    let span = parser.spanned()
    let name = try parseTypeName(with: parser)
    try parser.token(.assignOperator)
    parser.consumeLineBreaks()
    let parameters = try parseTypeParameters(with: parser)
    try parser.token(.typeKeyword)
    let typeSpan = span()
    let representation = try parseTypeRepresentation(with: parser)

    return TypeDefinitionSyntax(
        span: typeSpan,
        comments: comments,
        attributes: attributes,
        name: name,
        parameters: parameters,
        representation: representation,
    )
}

public func parseStructureTypeRepresentation(with parser: Parser) throws(ParseError)
    -> StructureTypeRepresentationSyntax
{
    let span = parser.spanned()
    try parser.token(.leftBrace)
    let fields = try parser.parseLines(min: 1, requireLineBreaks: true, parseFieldDefinition)
    try parser.token(.rightBrace)
    return StructureTypeRepresentationSyntax(span: span(), fields: fields)
}

public func parseFieldDefinition(with parser: Parser) throws(ParseError) -> FieldDefinitionSyntax {
    let span = parser.spanned()
    let name = try parseVariableName(with: parser)
    try parser.token(.annotateOperator)
    parser.commit(trace: "in this type annotation")
    parser.consumeLineBreaks()
    let type = try parseType(with: parser)
    return FieldDefinitionSyntax(span: span(), name: name, type: type)
}

public func parseEnumerationTypeRepresentation(with parser: Parser) throws(ParseError)
    -> EnumerationTypeRepresentationSyntax
{
    let span = parser.spanned()
    try parser.token(.leftBrace)
    let variants = try parser.parseLines(min: 1, requireLineBreaks: true, parseVariantDefinition)
    try parser.token(.rightBrace)
    return EnumerationTypeRepresentationSyntax(span: span(), variants: variants)
}

public func parseMarkerTypeRepresentation(with parser: Parser) throws(ParseError)
    -> MarkerTypeRepresentationSyntax
{
    let span = parser.spanned()
    parser.commit(trace: "in this type definition")
    return MarkerTypeRepresentationSyntax(span: span())
}

public func parseVariantDefinition(with parser: Parser) throws(ParseError)
    -> VariantDefinitionSyntax
{
    let span = parser.spanned()
    let name = try parseConstructorName(with: parser)
    let elements = try parser.parseMany(parseAtomicType)

    return VariantDefinitionSyntax(span: span(), name: name, elements: elements)
}

extension TypeDefinitionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        let attributes = self.attributes.map { visitor.visit($0) }

        visitor.defining(node) {
            visitor.pushScope()

            let parameters = visitor.withImplicitTypeParameters {
                self.parameters.map { visitor.visit($0) }
            }

            visitor.constraint(
                TypeConstraint(node, .named(node, parameters: parameters.map(Type.node)))
            )

            let typeConstraints = visitor.currentDefinition!.constraints(db: visitor.db)

            let definition = TypeDefinition(
                node: node,
                name: String(self.name),
                comments: self.comments.map(String.init),
                attributes: .parse(attributes, db: visitor.db),
                parameters: parameters,
            )

            if !definition.attributes.intrinsic {
                visitor.after(\.typeDefinitions) {
                    switch self.representation {
                    case is MarkerTypeRepresentationSyntax:
                        visitor.popScope()

                        visitor.define(
                            MarkerConstructorDefinition(
                                node: definition.node,
                                name: definition.name!,
                                comments: definition.comments,
                            )
                        )
                    case let representation as StructureTypeRepresentationSyntax:
                        var fields: [(String, Node)] = []
                        for field in representation.fields {
                            let type = visitor.visit(field.type)
                            visitor.db.edge(from: type, to: node, label: "field")

                            if fields.contains(where: { name, _ in name == field.name }) {
                                visitor.db[node, DuplicateFieldDefinition.self] = .init()
                                continue
                            }

                            fields.append((String(field.name), type))
                        }

                        visitor.popScope()

                        visitor.db[node, StructureFields.self] = .init(fields: fields.map(\.1))

                        visitor.define(
                            StructureConstructorDefinition(
                                node: definition.node,
                                name: definition.name!,
                                comments: definition.comments,
                                fields: fields,
                            )
                        )
                    case let representation as EnumerationTypeRepresentationSyntax:
                        var constructors: [VariantConstructorDefinition] = []
                        for (index, variant) in representation.variants.enumerated() {
                            if constructors.contains(where: { $0.name! == variant.name }) {
                                visitor.db[node, DuplicateVariantDefinition.self] = .init()
                                continue
                            }

                            let variantNode = visitor.db.node()
                            visitor.db[variantNode, Syntax.self] = .init(value: variant)
                            visitor.db[variantNode, Typed.self] = .init()

                            let constructorDefinition: VariantConstructorDefinition =
                                visitor.defining(variantNode) {
                                    for constraint in typeConstraints {
                                        visitor.constraint(constraint)
                                    }

                                    let elements = variant.elements.map { element in
                                        let element = visitor.visit(element)
                                        visitor.db.edge(
                                            from: element,
                                            to: variantNode,
                                            label: "element",
                                        )
                                        return element
                                    }

                                    if elements.isEmpty {
                                        visitor.constraint(GroupConstraint(variantNode, node))
                                    } else {
                                        visitor.constraint(
                                            TypeConstraint(
                                                variantNode,
                                                .function(
                                                    inputs: elements.map(Type.node),
                                                    output: .node(node),
                                                ),
                                            )
                                        )
                                    }

                                    return .init(
                                        node: variantNode,
                                        name: String(variant.name),
                                        typeDefinition: node,
                                        variant: variantNode,
                                        index: index,
                                        elements: elements,
                                    )
                                }

                            visitor.codegen(node: variantNode, with: VariantDefinitionCodegen())

                            constructors.append(constructorDefinition)
                        }

                        visitor.popScope()

                        for constructor in constructors {
                            visitor.db[node, EnumerationVariants.self, default: .init()].variants
                                .append((constructor.variant, constructor.elements))

                            visitor.define(constructor)
                        }
                    default: fatalError("unreachable")
                    }
                }
            }

            visitor.popScope()

            visitor.define(definition)

            visitor.codegen(node: node, with: TypeDefinitionCodegen())

            return definition
        }
    }
}

private struct TypeDefinitionCodegen: Codegenable {
    func codegen(with context: CodegenContext) throws {}
}

private struct VariantDefinitionCodegen: Codegenable {
    func codegen(with context: CodegenContext) throws {}
}
