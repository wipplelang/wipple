struct Instantiation {
    var sourceNode: Node
    var definition: Node
    var replacements: Replacements
    var substitutions: Substitutions
    var applySubstitutions: Bool
}

class InstantiateConstraint: Constraint {
    let instantiation: Instantiation

    init(_ instantiation: Instantiation) {
        self.instantiation = instantiation
        super.init(instantiation.sourceNode)
    }

    override func instantiate(with context: InstantiateContext) -> Constraint {
        let newReplacements = Replacements()
        for (node, replacement) in self.instantiation.replacements {
            newReplacements[node] = context.instantiate(replacement)
        }

        let newSubstitutions = context.instantiate(self.instantiation.substitutions)

        var instantiation = self.instantiation
        instantiation.sourceNode = context.sourceNode
        instantiation.replacements = newReplacements
        instantiation.substitutions = newSubstitutions

        return InstantiateConstraint(instantiation)
    }

    override func run(with solver: Solver) -> RunResult {
        // NOTE: Types are *not* applied before instantiating; we have access to
        // all related nodes/constraints here, which together will form better
        // groups

        if self.instantiation.applySubstitutions {
            solver.substitutionsToApply.append(self.instantiation.substitutions)
        }

        let definitionConstraints = solver.db[
            self.instantiation.definition,
            DefinitionConstraints.self,
            default: .init(),
        ]

        let context = InstantiateContext(
            db: solver.db,
            definition: self.instantiation.definition,
            sourceNode: self.instantiation.sourceNode,
            replacements: self.instantiation.replacements,
            substitutions: self.instantiation.substitutions,
        )

        let instantiatedConstraints = Array(
            definitionConstraints.constraints.lazy.filter(\.canInstantiate)
                .map { $0.instantiate(with: context) }
        )

        return .insert(instantiatedConstraints)
    }
}
