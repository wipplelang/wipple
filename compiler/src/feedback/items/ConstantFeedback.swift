func registerConstantFeedback(into context: FeedbackContext) {
    context.register(
        id: "missing-constant-value",
        query: Queries.fact(MissingConstantValue.self),
        rank: { _, _ in .syntax },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, node, _ in
        writer.write(node)
        writer.write(" is missing a value.")
        writer.writeBreak()
        writer.write("Try defining a value for this constant using ")
        writer.write(code: ":")
        writer.write(" on the following line.")
    }
}
