public struct ExtraAttributeValue: Fact {
    public func render(into context: RenderContext) { context.string("extra attribute value") }
}

public struct DuplicateAttribute: Fact {
    public func render(into context: RenderContext) { context.string("duplicate attribute") }
}

public struct MismatchedAttributeValue: Fact {
    public func render(into context: RenderContext) { context.string("mismatched attribute value") }
}

public struct MissingAttributeValue: Fact {
    public func render(into context: RenderContext) { context.string("missing attribute value") }
}

public struct AttributeSyntax: Sendable {
    public let span: Span
    public let name: Substring
    public let value: (any Visitable)?
}

public func parseAttributes(with parser: Parser) throws(ParseError) -> [AttributeSyntax] {
    try parser.parseLines(requireLineBreaks: false, parseAttribute)
}

public func parseAttribute(with parser: Parser) throws(ParseError) -> AttributeSyntax {
    let span = parser.spanned()
    try parser.token(.leftBracket)

    let name = try parseAttributeName(with: parser)

    let value = try parser.parseOptional { parser throws(ParseError) in
        try parser.token(.assignOperator)
        parser.consumeLineBreaks()
        return try parseAttributeValue(with: parser)
    }

    try parser.token(.rightBracket)

    return AttributeSyntax(span: span(), name: name, value: value)
}

extension AttributeSyntax: Visitable { public func visit(node: Node, with visitor: Visitor) {} }

func parseAttribute(_ attributes: [Node], named name: String, db: DB) -> Bool {
    var found = false
    for node in attributes {
        guard let attribute = db[node, Syntax.self]?.value as? AttributeSyntax else { continue }

        if attribute.name == name {
            if attribute.value != nil {
                db[node, ExtraAttributeValue.self] = .init()
            } else if found {
                db[node, DuplicateAttribute.self] = .init()
            } else {
                found = true
            }
        }
    }

    return found
}

func parseAttribute<T: Visitable>(_ attributes: [Node], named name: String, value: T.Type, db: DB)
    -> T?
{
    var result: T?
    for node in attributes {
        guard let attribute = db[node, Syntax.self]?.value as? AttributeSyntax else { continue }

        if attribute.name == name {
            if let value = attribute.value {
                if result != nil {
                    db[node, ExtraAttributeValue.self] = .init()
                    continue
                }

                result = value as? T

                if result == nil { db[node, MismatchedAttributeValue.self] = .init() }
            } else {
                db[node, MissingAttributeValue.self] = .init()
            }
        }
    }

    return result
}

func parseAttributes<T: Visitable>(_ attributes: [Node], named name: String, db: DB) -> [T] {
    attributes.compactMap { node in
        guard let attribute = db[node, Syntax.self]?.value as? AttributeSyntax,
            attribute.name == name, let value = attribute.value
        else { return nil }

        guard let result = value as? T else {
            db[node, MismatchedAttributeValue.self] = .init()
            return nil
        }

        return result
    }
}
