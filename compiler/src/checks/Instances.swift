struct OverlappingInstances: Fact {
    let instances: [Node]

    func render(into context: RenderContext) {
        context.string("has \(self.instances.count) overlapping instances")
    }
}

func checkForOverlappingInstances(db: DB, traitNode: Node, instances: some Sequence<Instance>) {
    let solver = Solver(db: db)
    let instanceGroups = groupInstances(instances)
        .map { instances in
            instances.map { instance in
                var instance = instance

                // Instantiate the *instance's own* parameters
                let replacements = Replacements()
                let substitutions = Substitutions()
                solver.constraints.insert(
                    InstantiateConstraint(
                        .init(
                            sourceNode: instance.node,
                            definition: instance.node,
                            replacements: replacements,
                            substitutions: substitutions,
                            applySubstitutions: false,
                        )
                    )
                )

                // Instantiate the substitutions for the *trait*

                let context = InstantiateContext(
                    db: db,
                    definition: instance.node,
                    sourceNode: instance.node,
                    replacements: replacements,
                    substitutions: substitutions,
                )

                instance.substitutions = context.instantiate(instance.substitutions)

                return instance
            }
        }

    solver.runPass(until: BoundConstraint.self)

    for instances in instanceGroups {
        var overlapping: [Node] = []
        for (leftIndex, leftInstance) in instances.enumerated() {
            for rightInstance in instances[(leftIndex + 1)...] {
                let copy = Solver(db: db)
                copy.groups = solver.groups

                copy.unify(leftInstance.substitutions, rightInstance.substitutions, trace: nil)

                guard !copy.error else { continue }

                if !overlapping.contains(leftInstance.node) {
                    overlapping.append(leftInstance.node)
                }

                if !overlapping.contains(rightInstance.node) {
                    overlapping.append(rightInstance.node)
                }
            }
        }

        if !overlapping.isEmpty {
            db[traitNode, OverlappingInstances.self] = .init(instances: overlapping)
        }
    }
}

func runMismatchedTrait(db: DB, traitDefinition: TraitDefinition, filter: (Node) -> Bool)
    -> [Solver]
{
    var solvers: [Solver] = []
    db.forEachFact(Typed.self) { node, typed in
        guard let group = typed.group, filter(node), group.types.count > 1 else { return }

        for leftIndex in group.types.indices {
            for rightIndex in group.types.indices where rightIndex != leftIndex {
                let left = group.types[leftIndex]
                let right = group.types[rightIndex]

                let substitutions = Substitutions()
                substitutions[traitDefinition.parameters[0]] = .node(node)
                substitutions[traitDefinition.parameters[1]] = .constructed(right)

                let solver = Solver(db: db)

                solver.constraints.insert(TypeConstraint(node, left))
                solver.constraints.insert(
                    BoundConstraint(
                        node,
                        .init(
                            sourceNode: node,
                            boundNode: node,
                            traitNode: traitDefinition.node,
                            substitutions: substitutions,
                            isOptional: true,
                        ),
                    )
                )

                solver.run()

                solvers.append(solver)
            }
        }
    }

    return solvers
}
