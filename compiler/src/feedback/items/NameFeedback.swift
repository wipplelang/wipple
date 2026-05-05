import OrderedCollections

func registerNameFeedback(into context: FeedbackContext) {
    context.register(
        id: "unresolved",
        query: { (db, node, body: (Node, String, (node: Node, name: String)?) -> Void) in
            Queries.unresolved(db, node) { name, suggestion in body(node, name, suggestion) }
        },
        rank: { _, _, _ in .names },
        location: { node, _, suggestion in
            let secondary = suggestion.map { OrderedSet([$0.node]) } ?? []
            return (primary: node, secondary: secondary)
        },
    ) { writer, _, name, suggestion in
        writer.write("Can't find ")
        writer.write(code: name)
        writer.write(".")
        writer.writeBreak()

        if let suggestion {
            writer.write("Did you mean ")
            writer.writeLink(suggestion.name, suggestion.node)
            writer.write("?")
        } else {
            writer.write("Double-check your spelling.")
        }
    }

    context.register(
        id: "ambiguous",
        query: Queries.ambiguous,
        rank: { _, _, _ in .names },
        location: { node, _, _ in (primary: node, secondary: []) },
    ) { writer, _, name, definitions in
        writer.write(code: name)
        writer.write(" could refer to ")
        writer.writeList(separator: "or") { list in
            for definition in definitions { list.add { writer.write(definition) } }
        }
        writer.write(".")
        writer.writeBreak()
        writer.write("Rename the extra definitions.")
    }
}
