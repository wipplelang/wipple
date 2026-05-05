class BoundConstraint: Constraint {
    var bound: Bound

    init(_ node: Node, _ bound: Bound) {
        self.bound = bound
        super.init(node)
    }

    override func instantiate(with context: InstantiateContext) -> Constraint {
        let node = context.instantiate(self.node)

        var bound = self.bound
        bound.sourceNode = context.sourceNode
        bound.substitutions = context.instantiate(self.bound.substitutions)

        return BoundConstraint(node, bound)
    }

    override func run(with solver: Solver) -> RunResult {
        // These are for the *trait's* parameters
        let boundInferred = Substitutions()
        let boundSubstitutions = Substitutions()
        for (parameter, substitution) in self.bound.substitutions {
            // NOTE: No need to instantiate the substitution here; the bound
            // has already been instantiated
            if solver.db.contains(InferredParameter.self, for: parameter) {
                boundInferred[parameter] = substitution
            } else {
                boundSubstitutions[parameter] = substitution
            }
        }

        let instances = solver.db[self.bound.traitNode, Instances.self, default: .init()].instances

        var instanceGroups = [(solver.impliedInstances, keepGeneric: true)]
        for group in groupInstances(instances) {
            instanceGroups.append((group, keepGeneric: false))
        }

        let resolvedNode = solver.db.node()

        struct Candidate {
            let solver: Solver
            let instance: Instance
            let substitutions: Substitutions
        }

        var candidates: [Candidate] = []
        for (index, (instances, keepGeneric)) in instanceGroups.enumerated() {
            let isLastInstanceGroup = index == instanceGroups.count - 1

            for instance in instances {
                guard instance.traitNode == self.bound.traitNode else { continue }

                let copy = Solver(db: solver.db)
                copy.groups = solver.groups
                copy.impliedInstances = solver.impliedInstances

                // These are for the *instance's own* parameters, not the trait
                // parameters like with the bound
                let replacements = Replacements()
                let substitutions = Substitutions()

                copy.constraints.insert(
                    InstantiateConstraint(
                        .init(
                            sourceNode: resolvedNode,
                            definition: instance.node,
                            replacements: replacements,
                            substitutions: substitutions,
                            applySubstitutions: true,
                        )
                    )
                )

                // Run the solver (excluding bounds) to populate `replacements`
                copy.runPass(until: BoundConstraint.self)

                // Propagate the target node to new bounds
                for constraint in copy.constraints {
                    guard let constraint = constraint as? BoundConstraint else { continue }

                    if constraint.bound.targetNode == nil {
                        constraint.bound.targetNode = self.bound.targetNode ?? self.bound.sourceNode
                    }
                }

                // These are for the *trait's* parameters
                let instanceSubstitutions = Substitutions()
                let instanceInferred = Substitutions()
                for (parameter, var substitution) in instance.substitutions {
                    if !keepGeneric {
                        let context = InstantiateContext(
                            db: solver.db,
                            definition: instance.node,
                            sourceNode: resolvedNode,
                            replacements: replacements,
                            substitutions: substitutions,
                        )

                        substitution = context.instantiate(substitution)
                    }

                    if solver.db.contains(InferredParameter.self, for: parameter) {
                        instanceInferred[parameter] = substitution
                    } else {
                        instanceSubstitutions[parameter] = substitution
                    }
                }

                copy.error = false
                copy.unify(instanceSubstitutions, boundSubstitutions, trace: self)
                if !copy.error {
                    copy.unify(instanceInferred, boundInferred, trace: self)
                    candidates.append(
                        .init(solver: copy, instance: instance, substitutions: substitutions)
                    )
                }
            }

            let resolvedSubstitutions = Substitutions()
            for (parameter, substitution) in boundSubstitutions {
                resolvedSubstitutions[parameter] = substitution
            }
            for (parameter, substitution) in boundInferred {
                resolvedSubstitutions[parameter] = substitution
            }

            var resolvedBound = self.bound
            resolvedBound.substitutions = resolvedSubstitutions

            // Allow multiple candidates (picking the first) if considering implied
            // instances
            let hasCandidate = keepGeneric ? !candidates.isEmpty : candidates.count == 1

            if hasCandidate {
                let candidate = candidates[0]

                solver.groups = candidate.solver.groups
                solver.substitutionsToApply.append(resolvedSubstitutions)

                // Record the resolved bound on the current source node
                // (necessary for codegen) and the original target node (set
                // above; necessary for error reporting).
                for node in [self.bound.targetNode, self.bound.sourceNode] {
                    guard let node else { continue }

                    solver.db[node, Bounds.self, default: .init()].bounds[self.bound.boundNode] = (
                        resolvedBound,
                        ResolvedBound(
                            instance: candidate.instance,
                            instanceSubstitutions: candidate.substitutions,
                            resolvedNode: resolvedNode,
                            resolvedSubstitutions: resolvedSubstitutions,
                        ),
                    )
                }

                solver.progress = true

                return .enqueue(Array(candidate.solver.constraints))
            } else if candidates.count > 1 {
                return .enqueue([self])  // ambiguous; try again
            }

            if isLastInstanceGroup && !self.bound.isOptional {
                solver.substitutionsToApply.append(resolvedSubstitutions)

                for node in [self.bound.targetNode, self.bound.sourceNode] {
                    guard let node else { continue }

                    solver.db[node, Bounds.self, default: .init()].bounds[self.bound.boundNode] = (
                        resolvedBound, nil,
                    )
                }
            }
        }

        return .none
    }
}

func groupInstances(_ instances: some Sequence<Instance>) -> [[Instance]] {
    var regularInstances: [Instance] = []
    var errorInstances: [Instance] = []
    var defaultInstances: [Instance] = []
    var defaultErrorInstances: [Instance] = []
    for instance in instances {
        switch (instance.isError, instance.isDefault) {
        case (false, false): regularInstances.append(instance)
        case (true, false): errorInstances.append(instance)
        case (false, true): defaultInstances.append(instance)
        case (true, true): defaultErrorInstances.append(instance)
        }
    }

    return [regularInstances, errorInstances, defaultInstances, defaultErrorInstances]
}
