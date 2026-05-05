func registerPatternFeedback(into context: FeedbackContext) {
    context.register(
        id: "nested-or-pattern",
        query: Queries.fact(InvalidOrPattern.self),
        rank: { _, _ in .syntax },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, node, _ in
        writer.write(node)
        writer.write(" cannot be used inside another pattern.")
        writer.writeBreak()
        writer.write(code: "or")
        writer.write(" can only be used immediately within a ")
        writer.write(code: "when")
        writer.write(" arm.")
    }

    context.register(
        id: "nested-set-pattern",
        query: Queries.fact(InvalidSetPattern.self),
        rank: { _, _ in .syntax },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, node, reason in
        switch reason {
        case .nested:
            writer.write(node)
            writer.write(" cannot be used inside another pattern.")
            writer.writeBreak()
            writer.write(code: "set")
            writer.write(" can only be used immediately before a variable assignment using ")
            writer.write(code: ":")
            writer.write(".")
        case .immutable(let variable):
            writer.write(variable)
            writer.write(" cannot be changed.")
            writer.writeBreak()
            writer.write("Try copying ")
            writer.write(variable)
            writer.write(" into a new variable before using ")
            writer.write(code: "set")
            writer.write(".")
        }
    }

    context.register(
        id: "extra-element",
        query: Queries.fact(ExtraElement.self),
        rank: { _, _ in .syntax },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, node, _ in
        writer.write(node)
        writer.write(
            " can't be used here because this code matches against a marker type, not a variant."
        )
        writer.writeBreak()
        writer.write("Try removing this element.")
    }

    context.register(
        id: "missing-patterns",
        query: Queries.fact(MissingPatterns.self),
        rank: { _, _ in .exhaustiveness },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, node, missing in
        if missing.patterns.count == 1, case .wildcard = missing.patterns[0] {
            writer.write(node)
            writer.write(" could be a different possible value that isn't covered here.")
            writer.writeBreak()
            writer.write("Try assigning it to a variable or ")
            writer.write(code: "_")
            writer.write(".")
        } else {
            writer.write(node)
            writer.write(" could be ")
            writer.writeList(separator: "or") { list in
                for tree in missing.patterns { list.add { writer.write(tree) } }
            }
            writer.write(", but these patterns are missing.")
            writer.writeBreak()
            writer.write("Try adding patterns to cover these cases using ")
            writer.write(code: "or")
            writer.write(", ")
            writer.write(code: "when")
            writer.write(", or a variable.")
        }
    }
}
