class TypeConstraint: Constraint {
    let type: ConstructedType

    init(_ node: Node, _ type: ConstructedType) {
        self.type = type
        super.init(node)
    }

    override func instantiate(with context: InstantiateContext) -> Constraint {
        let node = context.instantiate(self.node)
        let type = context.instantiate(.constructed(self.type))

        switch type {
        case .node(let other): return GroupConstraint(node, other)
        case .constructed(let type): return TypeConstraint(node, type)
        }
    }

    override func run(with solver: Solver) -> RunResult {
        solver.unify(self.node, self.type, trace: self)
        return .none
    }
}
