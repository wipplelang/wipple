func registerFormatFeedback(into context: FeedbackContext) {
    context.register(
        id: "missing-format-inputs",
        query: Queries.fact(MissingFormatInputs.self),
        rank: { _, _ in .syntax },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, node, missingInputs in
        writer.write(node)
        writer.write(" needs ")
        writer.write(missingInputs.count, singular: "more input", plural: "more inputs")
        writer.write(".")
        writer.writeBreak()
        writer.write("Try adding code after the string.")
    }

    context.register(
        id: "extra-format-input",
        query: Queries.fact(ExtraFormatInput.self),
        rank: { _, _ in .syntax },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, node, _ in
        writer.write(node)
        writer.write(" isn't used in the format string.")
        writer.writeBreak()
        writer.write("Try removing this input or add another ")
        writer.write(code: "_")
        writer.write(" placeholder.")
    }
}
