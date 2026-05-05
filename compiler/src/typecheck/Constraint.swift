import Collections

public class Constraint {
    enum RunResult {
        case none
        case insert([Constraint])
        case enqueue([Constraint])
    }

    public let node: Node
    var isActive = true
    var canInstantiate = true

    init(_ node: Node) { self.node = node }

    func instantiate(with context: InstantiateContext) -> Constraint {
        fatalError("instantiate(with:) not implemented")
    }

    func run(with solver: Solver) -> RunResult { fatalError("run(with:) not implemented") }
}

private let constraintOrder = [
    ObjectIdentifier(InstantiateConstraint.self), ObjectIdentifier(GroupConstraint.self),
    ObjectIdentifier(TypeConstraint.self), ObjectIdentifier(BoundConstraint.self),
]

struct Constraints: Sequence {
    private var constraints: [ObjectIdentifier: Deque<Constraint>] = [:]
    private var defaultConstraints: [Constraint] = []

    mutating func insert(_ constraint: Constraint) {
        if let constraint = constraint as? DefaultConstraint {
            self.defaultConstraints.append(constraint)
        } else {
            let type = type(of: constraint)

            guard constraintOrder.contains(ObjectIdentifier(type)) else {
                fatalError("unsupported constraint type")
            }

            self.constraints[ObjectIdentifier(type), default: []].append(constraint)
        }
    }

    mutating func insert(_ constraints: some Sequence<Constraint>) {
        for constraint in constraints { self.insert(constraint) }
    }

    mutating func run(with solver: Solver, until stop: Constraint.Type?) {
        var requeuedConstraints: [Constraint] = []
        while true {
            guard let constraint = self.dequeue(stop) else { break }

            guard constraint.isActive else { continue }

            switch constraint.run(with: solver) {
            case .none: continue
            case .insert(let constraints): self.insert(constraints)
            case .enqueue(let constraints): requeuedConstraints.append(contentsOf: constraints)
            }
        }

        self.insert(requeuedConstraints)
    }

    mutating func runDefaults(with solver: Solver) {
        for constraint in self.defaultConstraints { _ = constraint.run(with: solver) }

        self.defaultConstraints = []
    }

    private mutating func dequeue(_ stop: Constraint.Type?) -> Constraint? {
        for type in constraintOrder {
            if let stop, type == ObjectIdentifier(stop) { return nil }

            if let constraint = self.constraints[type]?.popFirst() { return constraint }
        }

        return nil
    }

    func makeIterator() -> some IteratorProtocol<Constraint> {
        [AnySequence(self.constraints.values.lazy.joined()), AnySequence(self.defaultConstraints)]
            .joined().makeIterator()
    }
}
