class GroupConstraint: Constraint {
    let other: Node

    init(_ node: Node, _ other: Node) {
        self.other = other
        super.init(node)
    }

    override func instantiate(with context: InstantiateContext) -> Constraint {
        let node = context.instantiate(self.node)
        let other = context.instantiate(self.other)
        return GroupConstraint(node, other)
    }

    override func run(with solver: Solver) -> RunResult {
        solver.unify(self.node, self.other, trace: self)
        return .none
    }
}
