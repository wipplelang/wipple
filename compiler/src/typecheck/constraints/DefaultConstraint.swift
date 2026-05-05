class DefaultConstraint: Constraint {
    let `default`: Node

    init(_ node: Node, _ default: Node) {
        self.default = `default`
        super.init(node)
    }

    override func instantiate(with context: InstantiateContext) -> Constraint {
        let node = context.instantiate(self.node)
        let `default` = context.instantiate(self.default)
        return DefaultConstraint(node, `default`)
    }

    override func run(with solver: Solver) -> RunResult {
        if case .node(let node) = solver.apply(.node(self.node)) {
            solver.unify(node, self.default, trace: self)
        }

        return .none
    }
}
