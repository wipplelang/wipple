func registerSyntaxFeedback(into context: FeedbackContext) {
    context.register(
        id: "syntax-error",
        query: Queries.fact(ParseError.self),
        rank: { _, _ in .syntax },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, _, error in
        writer.write(error.message)

        if let committed = error.committed {
            writer.write(" ")
            writer.write(committed)
        }

        writer.write(".")

        if let reason = error.reason {
            writer.writeBreak()
            writer.write(reason)
        }

        writer.writeBreak()
        writer.write("Check your spelling.")
    }

    context.register(
        id: "unused-block",
        query: { (db, node, body: (Node) -> Void) in Queries.unusedBlock(db, node) { body(node) } },
        rank: { _ in .syntax },
        location: { node in (primary: node, secondary: []) },
    ) { writer, node in
        writer.write(node)
        writer.write(" is on its own line and will never run.")
        writer.writeBreak()
        writer.write("Did you mean to put the opening ")
        writer.write(code: "{")
        writer.write(" on the line above?")
    }
}
