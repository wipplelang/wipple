func createConnections(db: DB, filter: (Node) -> Bool) {
    db.forEachFact(ResolvedCall.self) { node, call in
        guard filter(node) else { return }

        var hasCustomEdges = false
        if let definitions = db[call.function, Resolved.self]?.definitions, definitions.count == 1 {
            let definition = definitions[0]

            let links = getLinks(db: db, definitionNode: definition, sourceNode: call.function)

            if let attributes = (db[definition, Defined.self]?.definition as? ConstantDefinition)?
                .attributes
            {
                for value in attributes.connect {
                    guard let left = links[String(value.left)],
                        let right = links[String(value.right)]
                    else { continue }

                    db.edge(from: left.node, to: right.node, label: String(value.label))

                    hasCustomEdges = true
                }
            }
        }

        if !hasCustomEdges {
            db.edge(from: call.function, to: node, label: "function")

            for input in call.inputs { db.edge(from: input, to: node, label: "input") }
        }
    }
}
