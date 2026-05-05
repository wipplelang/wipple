func registerTypeDefinitionFeedback(into context: FeedbackContext) {
    context.register(
        id: "duplicate-field-definition",
        query: Queries.fact(DuplicateFieldDefinition.self),
        rank: { _, _ in .syntax },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, node, _ in
        writer.write(node)
        writer.write(" is already defined in this type.")
        writer.writeBreak()
        writer.write("Try renaming this field.")
    }

    context.register(
        id: "duplicate-variant-definition",
        query: Queries.fact(DuplicateVariantDefinition.self),
        rank: { _, _ in .syntax },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, node, _ in
        writer.write(node)
        writer.write(" is already defined in this type.")
        writer.writeBreak()
        writer.write("Try renaming this variant.")
    }
}
