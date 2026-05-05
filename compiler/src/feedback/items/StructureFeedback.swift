func registerStructureFeedback(into context: FeedbackContext) {
    context.register(
        id: "missing-field",
        query: Queries.fact(MissingField.self),
        rank: { _, _ in .syntax },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, node, missing in
        writer.write(node)
        writer.write(" is missing a pattern for the field ")
        writer.write(code: missing.name)
        writer.write(".")
        writer.writeBreak()
        writer.write("Try adding a pattern for this field using ")
        writer.write(code: ":")
        writer.write(".")
    }

    context.register(
        id: "extra-field",
        query: Queries.fact(ExtraField.self),
        rank: { _, _ in .syntax },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, _, extra in
        writer.write(code: extra.name)
        writer.write(" isn't a field on this structure.")
        writer.writeBreak()
        writer.write("Double-check your spelling.")
    }

    context.register(
        id: "duplicate-field",
        query: Queries.fact(DuplicateField.self),
        rank: { _, _ in .syntax },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, node, _ in
        writer.write(node)
        writer.write(" is defined more than once.")
        writer.writeBreak()
        writer.write("Try removing this field.")
    }
}
