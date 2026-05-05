func registerInstanceFeedback(into context: FeedbackContext) {
    context.register(
        id: "missing-instance-value",
        query: Queries.fact(MissingInstanceValue.self),
        rank: { _, _ in .syntax },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, node, _ in
        writer.write(node)
        writer.write(" is missing a value.")
        writer.writeBreak()
        writer.write("Try adding a value for this instance using ")
        writer.write(code: ":")
        writer.write(".")
    }

    context.register(
        id: "extra-instance-value",
        query: Queries.fact(ExtraInstanceValue.self),
        rank: { _, _ in .syntax },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, _, _ in
        writer.write("This instance doesn't need a value because it is marked with ")
        writer.write(code: "[error]")
        writer.write(".")
        writer.writeBreak()
        writer.write("Remove this code.")
    }
}
