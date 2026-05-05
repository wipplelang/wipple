import OrderedCollections

func registerPlaceholderFeedback(into context: FeedbackContext) {
    context.register(
        id: "placeholder",
        query: Queries.placeholder,
        rank: { _, _, _ in .placeholders },
        location: { node, others, _ in (primary: node, secondary: OrderedSet(others)) },
        showGraph: true,
    ) { writer, _, _, type in
        if let type {
            writer.write("Found a placeholder of type ")
            writer.write(.constructed(type))
            writer.write(".")
        } else {
            writer.write("Found a placeholder.")
        }

        writer.writeBreak()

        if let type {
            writer.write("Add a ")
            writer.write(.constructed(type))
            writer.write(" value here before running your program.")
        } else {
            writer.write("Add a value here before running your program.")
        }
    }
}
