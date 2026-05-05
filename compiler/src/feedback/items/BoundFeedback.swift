import OrderedCollections

func registerBoundFeedback(into context: FeedbackContext) {
    context.register(
        id: "unresolved-bound",
        query: Queries.unresolvedBound,
        rank: { _ in .bounds },
        location: { (primary: $0.targetNode ?? $0.sourceNode, secondary: []) },
    ) { writer, bound in
        writer.write(bound.targetNode ?? bound.sourceNode)
        writer.write(" requires the instance ")
        writer.write(bound)
        writer.write(", but this instance isn't defined.")
        writer.writeBreak()
        writer.write("Double-check that these types are correct.")
    }

    context.register(
        id: "error-instance",
        query: Queries.errorInstance,
        rank: { $0.isDefault ? .customDefault : .custom },
        location: { errorInstance in
            var related: OrderedSet<Node> = []
            for link in errorInstance.comments.links.values {
                related.append(link.node)
                related.append(contentsOf: link.related)
            }

            related.append(contentsOf: errorInstance.comments.nodes)

            return (primary: errorInstance.bound.sourceNode, secondary: related)
        },
        showGraph: true,
    ) { writer, errorInstance in
        writer.write(errorInstance.comments)

        for constraint in errorInstance.trace { writer.write(constraint) }
    }
}
