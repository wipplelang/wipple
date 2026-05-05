func registerAttributeFeedback(into context: FeedbackContext) {
    context.register(
        id: "extra-attribute-value",
        query: Queries.fact(ExtraAttributeValue.self),
        rank: { _, _ in .syntax },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, node, _ in
        writer.write(node)
        writer.write(" doesn't accept a value.")
        writer.writeBreak()
        writer.write("Try removing the value from this attribute.")
    }

    context.register(
        id: "duplicate-attribute",
        query: Queries.fact(DuplicateAttribute.self),
        rank: { _, _ in .syntax },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, node, _ in
        writer.write(node)
        writer.write(" is defined more than once.")
        writer.writeBreak()
        writer.write("Try removing this attribute.")
    }

    context.register(
        id: "missing-attribute-value",
        query: Queries.fact(MissingAttributeValue.self),
        rank: { _, _ in .syntax },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, node, _ in
        writer.write(node)
        writer.write(" is missing a value.")
        writer.writeBreak()
        writer.write("Try adding a value to this attribute using ")
        writer.write(code: ":")
        writer.write(".")
    }
}
