import OrderedCollections

func registerTypeFeedback(into context: FeedbackContext) {
    context.register(
        id: "conflicting-types",
        query: Queries.conflictingTypes,
        rank: { _ in .conflicts },
        location: { data in
            var related = OrderedSet(data.nodes)

            if let source = data.source, data.from != source { related.append(data.from) }

            return (primary: data.source ?? data.from, secondary: related)
        },
        showGraph: true,
    ) { writer, data in
        if let source = data.source {
            writer.write("In ")
            writer.write(source)
            writer.write(", ")
        }

        let hasTypeConstraint = data.trace.contains(where: { $0 is TypeConstraint })

        if hasTypeConstraint, data.nodes.count == 1, data.types.count == 2 {
            writer.write(data.from)
            writer.write(" isn't a ")
            writer.write(.constructed(data.types[1]))
            writer.write(".")
            writer.writeBreak()
        } else {
            writer.write(data.from)
            writer.write(" is a ")
            writer.writeList(separator: "or a") { list in
                for type in data.types { list.add { writer.write(.constructed(type)) } }
            }
            writer.write(", but it can only be one of these.")
        }

        let fromSpan = writer.db[data.from, Syntax.self]?.value.span
        let nodes = data.nodes.filter { writer.db[$0, Syntax.self]?.value.span != fromSpan }

        if nodes.count > 1 {
            writer.writeBreak()
            writer.write(data.from)
            writer.write(" must be the same type as ")
            writer.writeList(separator: "and") { list in
                for node in nodes { list.add { writer.write(node) } }
            }
            writer.write("; double-check these.")
        }

        for constraint in data.trace { writer.write(constraint) }

        var parametersByName: [String: [(source: Node, from: Node)]] = [:]
        for node in data.nodes {
            guard let instantiated = writer.db[node, Instantiated.self],
                let parameter = instantiated.from as Node?,
                let syntax = writer.db[parameter, Syntax.self]?.value as? TypeParameterSyntax
            else { continue }

            parametersByName[String(syntax.name), default: []]
                .append((source: instantiated.sourceNode, from: instantiated.from))
        }

        for name in parametersByName.keys.sorted() {
            let entries = parametersByName[name] ?? []
            guard entries.count > 1, let entry = entries.first else { continue }

            writer.writeBreak()
            writer.write("-  ")
            writer.write(entry.from)
            writer.write(" must have the same type everywhere in this use of ")
            writer.write(entry.source)
            writer.write(".")
        }
    }

    context.register(
        id: "incomplete-type",
        query: Queries.incompleteType,
        rank: { _, _ in .unknown },
        location: { node, _ in (primary: node, secondary: []) },
        showGraph: true,
    ) { writer, node, type in
        writer.write("Missing information for the type of ")
        writer.write(node)
        writer.write(".")
        writer.writeBreak()
        writer.write("Wipple determined this code is ")
        writer.write(.constructed(type))
        writer.write(", but it needs some more information for the ")
        writer.write(code: "_")
        writer.write(" placeholders.")
    }

    context.register(
        id: "unknown-type",
        query: { (db, node, body: (Node) -> Void) in Queries.unknownType(db, node) { body(node) } },
        rank: { _ in .unknown },
        location: { node in (primary: node, secondary: []) },
        showGraph: true,
    ) { writer, node in
        writer.write("Could not determine the type of ")
        writer.write(node)
        writer.write(".")
        writer.writeBreak()
        writer.write(
            "Wipple needs to know the type of this code before running it. Try using a function or assigning it to a variable."
        )
    }

    context.register(
        id: "missing-type",
        query: Queries.fact(MissingTypes.self),
        rank: { _, _ in .syntax },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, node, missing in
        writer.write(node)

        if missing.types.count == 1, let parameter = missing.types.first {
            writer.write(" is missing a type for ")
            writer.write(parameter)
        } else {
            writer.write(" is missing types for ")
            writer.writeList(separator: "and") { list in
                for parameter in missing.types { list.add { writer.write(parameter) } }
            }
        }

        writer.write(".")
        writer.writeBreak()
        writer.write("Try adding another type here, or double-check your parentheses.")
    }

    context.register(
        id: "extra-type",
        query: Queries.fact(ExtraType.self),
        rank: { _, _ in .syntax },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, node, _ in
        writer.write(node)
        writer.write(" doesn't match any parameter of this type.")
        writer.writeBreak()
        writer.write("Try removing this type, or double-check your parentheses.")
    }

    context.register(
        id: "conflicting-instances",
        query: Queries.fact(OverlappingInstances.self),
        rank: { _, _ in .bounds },
        location: { node, _ in (primary: node, secondary: []) },
    ) { writer, node, overlapping in
        writer.write(node)
        writer.write(" has multiple overlapping instances: ")
        writer.writeList(separator: "and") { list in
            for instance in overlapping.instances { list.add { writer.write(instance) } }
        }
        writer.write(".")
        writer.writeBreak()
        writer.write(
            "Only one of these instances can be defined at a time. Try making your instance more specific."
        )
    }
}
