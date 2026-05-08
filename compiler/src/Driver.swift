struct TopLevelDefinitions: Fact {
    var definitions: [[String: [Definition]]] = []

    func render(into context: RenderContext) {}
}

public func defaultFilter(db: DB) -> (Node) -> Bool {
    { node in node.belongs(to: db) && db.contains(Syntax.self, for: node) }
}

public func compile(db: DB, files: [any Visitable]) -> (root: Node, files: [Node]) {
    // Define/resolve names and collect constraints

    let topLevelDefinitions = db[.topLevel, TopLevelDefinitions.self, default: .init()].definitions

    let root = db.node(isHidden: true)

    let visitor = Visitor(db: db, root: root, definitions: topLevelDefinitions)
    let files = files.map { visitor.visit($0) }
    let visited = visitor.finish()

    db[.topLevel, TopLevelDefinitions.self, default: .init()].definitions
        .append(visited.definitions)

    // Solve constraints from each definition, implying all bounds

    db.forEachFact(DefinitionConstraints.self) { definitionNode, definitionConstraints in
        guard definitionNode.belongs(to: db) else { return }

        let solver = Solver(db: db)

        // If the definition is an instance, imply it inside itself
        db.forEachFact(Instances.self) { _, instances in
            for instance in instances.instances {
                if instance.node == definitionNode {
                    solver.imply(instance)
                    return true
                }
            }

            return false
        }

        // Also imply all of the definition's bounds (so they remain generic
        // while resolving the definition)
        for constraint in definitionConstraints.constraints {
            if let constraint = constraint as? BoundConstraint {
                // Only imply bounds from constraints, not from inside the
                // definition's value!
                guard db.contains(IsBound.self, for: constraint.node) else { continue }

                solver.imply(.resolved(constraint.node, from: constraint.bound))
            }
        }

        solver.constraints.insert(definitionConstraints.constraints)
        solver.run()

        setGroups(db: db, solver: solver)
    }

    // Solve constraints from top-level expressions
    let solver = Solver(db: db)
    solver.constraints.insert(visited.constraints)
    solver.run()
    setGroups(db: db, solver: solver)

    // Check for exhaustiveness
    checkExhaustiveness(db: db)

    // Check for overlapping instances

    db.forEachFact(Instances.self) { traitNode, instances in
        checkForOverlappingInstances(db: db, traitNode: traitNode, instances: instances.instances)
        return false
    }

    // Resolved `Mismatched` trait for mismatched types
    if let mismatchedTrait = visited.utilities["Mismatched"],
        let mismatchedTraitDefinition = db[mismatchedTrait, Defined.self]?.definition
            as? TraitDefinition
    {
        for solver in runMismatchedTrait(
            db: db,
            traitDefinition: mismatchedTraitDefinition,
            filter: defaultFilter(db: db),
        ) { setGroups(db: db, solver: solver) }
    }

    // Add custom connections
    createConnections(db: db, filter: defaultFilter(db: db))

    return (root, files)
}

func setGroups(db: DB, solver: Solver) {
    for group in solver.sortedGroups(by: { GroupOrder(db: db, node: $0) }) {
        var nodes: [Node] = []
        for node in group.nodes {
            let typed = db[node, Typed.self, default: .init()]

            if typed.group == nil {
                nodes.append(node)
                typed.group = group
            }
        }

        if !nodes.isEmpty { db.group(nodes: nodes, types: group.types) }
    }
}
