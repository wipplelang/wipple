import Foundation

extension Trait.ID {
    static let `operator` = Self(debugLabel: "Operator")
}

public extension Trait {
    static func `operator`(_ op: Operator) -> Trait {
        Trait(id: .operator) { _ in
            op
        }
    }
}

public extension Value {
    func operatorValue(_ env: inout Environment) throws -> Operator {
        try Trait.value(.operator, in: self, &env)
    }

    func operatorValueIfPresent(_ env: inout Environment) throws -> Operator? {
        try Trait.value(.operator, ifPresentIn: self, &env)
    }
}

public struct Operator: Identifiable {
    public var id = UUID()
    public var arity: Arity
    public var associativity: Associativity

    public init(arity: Arity, associativity: Associativity) {
        self.arity = arity
        self.associativity = associativity
    }

    public enum Arity {
        case binary((_ left: Value, inout Environment) throws -> (_ right: Value, inout Environment) throws -> Value)
        case variadic((_ left: [Value], inout Environment) throws -> (_ right: [Value], inout Environment) throws -> Value)
    }

    public enum Associativity {
        case left
        case right
    }

    public enum Precedence {
        case highest
        case lowest
        case sameAs(Operator)
        case higherThan(Operator)
        case lowerThan(Operator)
    }
}

extension Operator: Hashable {
    public static func == (lhs: Self, rhs: Self) -> Bool {
        lhs.id == rhs.id
    }

    public func hash(into hasher: inout Hasher) {
        hasher.combine(self.id)
    }
}

public extension Operator.Arity {
    static func binary(_ call: @escaping (_ left: Value, _ right: Value, inout Environment) throws -> Value) -> Self {
        .binary { left, env in
            return { right, env in
                try call(left, right, &env)
            }
        }
    }

    static func binary(call: @escaping CallFunction) -> Self {
        .binary({ left, env in
            let nextCall = try call(left, &env).callValue(&env)

            return { right, env in
                return try nextCall(right, &env)
            }
        })
    }

    static func variadic(_ call: @escaping (_ left: [Value], _ right: [Value], inout Environment) throws -> Value) -> Self {
        .variadic { left, env in
            return { right, env in
                try call(left, right, &env)
            }
        }
    }

    static func variadic(call: @escaping CallFunction) -> Self {
        .variadic({ leftItems, env in
            let left = Value.new(.list(leftItems))
            let nextCall = try call(left, &env).callValue(&env)

            return { rightItems, env in
                let right = Value.new(.list(rightItems))
                return try nextCall(right, &env)
            }
        })
    }
}

public typealias OperatorsByPrecedence = [Set<Operator>]
public typealias OperatorList = [(operator: Operator, index: Int)]

public extension Environment {
    mutating func registerOperator(_ op: Operator, precedence: Operator.Precedence) throws {
        // Ensure the operators are the same arity
        switch precedence {
        case let .sameAs(other), let .higherThan(other), let .lowerThan(other):
            switch (op.arity, other.arity) {
            case (.binary, .binary), (.variadic, .variadic):
                break
            default:
                throw ProgramError("Cannot relate to the precedence of an operator with a different arity")
            }
        default:
            break
        }

        func prepend() {
            self.operatorsByPrecedence.insert([op], at: 0)
        }

        func append() {
            self.operatorsByPrecedence.append([op])
        }

        func tryInserting(_ other: Operator, at relativeIndex: Int, else prependOrAppend: () -> Void) {
            let index = self.operatorsByPrecedence.firstIndex(where: { $0.contains(other) })! + relativeIndex
            if self.operatorsByPrecedence.indices.contains(index) {
                self.operatorsByPrecedence[index].insert(op)
            } else {
                prependOrAppend()
            }
        }

        switch precedence {
        case .highest:
            prepend()
        case .lowest:
            append()
        case let .sameAs(other):
            tryInserting(other, at: 0, else: { fatalError("Unreachable") })
        case let .higherThan(other):
            tryInserting(other, at: -1, else: prepend)
        case let .lowerThan(other):
            tryInserting(other, at: 1, else: append)
        }
    }
}

// MARK: - Operator parsing

public func findOperators(in list: [Value], _ env: inout Environment) throws -> OperatorList {
    var operators: OperatorList = []

    for (index, value) in list.enumerated() {
        if let op = try getOperator(value, &env) {
            operators.append((op, index))
        }
    }

    return operators
}

func getOperator(_ value: Value, _ env: inout Environment) throws -> Operator? {
    var op: Operator?

    @discardableResult
    func getOperator(_ value: Value) throws -> Bool {
        if let operatorValue = try value.operatorValueIfPresent(&env) {
            op = operatorValue
            return true
        }

        return false
    }

    if
        !(try getOperator(value)),
        let name = try value.nameValueIfPresent(&env),
        let variable = env.variables[name]
    {
        try getOperator(variable)
    }

    return op
}

extension Operator.Arity {
    func asCallFunction() -> CallFunction {
        switch self {
        case let .binary(call):
            return { left, env in
                let call = try call(left, &env)
                return Value.new(.call(call))
            }
        case let .variadic(call):
            return { left, env in
                let call = try call(getItems(left, &env), &env)

                return Value.new(.call { right, env in
                    try call(getItems(right, &env), &env)
                })
            }
        }
    }
}


func getItems(_ list: Value, _ env: inout Environment) throws -> List {
    guard case let .valid(items) = try Trait.validation(for: .list)(list, &env) else {
        throw ProgramError("Application of variadic operator requires a list")
    }

    return items as! List
}

func parseOperators(
    evaluating list: List,
    operatorsInList: OperatorList,
    _ env: inout Environment
) throws -> Value? {
    var operatorsInList = operatorsInList

    if operatorsInList.isEmpty {
        return nil
    }

    // Special cases: list with 0/1 values isn't parsed
    switch list.count {
    case 0:
        return nil
    case 1:
        if let op = operatorsInList.first {
            return Value.new(.call(op.operator.arity.asCallFunction()))
        } else {
            return list[0]
        }
    default:
        break
    }

    // Ensure there are no operators with the same precedence (ambiguous)
    guard env.operatorsByPrecedence.allSatisfy({ operators in
        operators.intersection(operatorsInList.map(\.operator)).count <= 1
    }) else {
        throw ProgramError("Found multiple operators with the same precedence; group the expression into lists to disambiguate")
    }

    let partition = operatorsInList.partition(by: {
        switch $0.operator.arity {
        case .variadic:
            return true
        case .binary:
            return false
        }
    })

    let (binaryOperators, variadicOperators) = (Array(operatorsInList[..<partition]), Array(operatorsInList[partition...]))

    func getHighest(_ ops: OperatorList) -> OperatorList.Element {
        func index(of op: Operator) -> Int {
            guard let precedence = env.operatorsByPrecedence.firstIndex(where: { $0.contains(op) }) else {
                fatalError("Operator \(op) is not registered")
            }

            return precedence
        }

        // Select consecutive instances of the operator with the highest precedence
        let highestPrecedence = ops
            .sorted { index(of: $0.operator) < index(of: $1.operator) }
            .take { a, b in
                guard let b = b else { return true }

                return type(of: a.operator) == type(of: b.operator) && a.index == b.index - 1
            }

        // Choose the leftmost operator if it has left associativity, and vice
        // versa
        let associativity = highestPrecedence[0].operator.associativity
        let op = associativity == .left
            ? highestPrecedence.first!
            : highestPrecedence.last!

        return op
    }

    if !variadicOperators.isEmpty {
        let op = getHighest(variadicOperators)

        guard case let .variadic(call) = op.operator.arity else {
            fatalError("Unreachable")
        }

        let leftItems = Array(list[0..<op.index])
        let rightItems = Array(list[(op.index + 1)...])

        if leftItems.isEmpty {
            // Partially apply the left side
            return Value.new(.call { left, env in
                let call = try call(getItems(left, &env), &env)

                return Value.new(.call { right, env in
                    try call(getItems(right, &env), &env)
                })
            })
        }

        if rightItems.isEmpty {
            // Partially apply the right side
            return Value.new(.call { right, env in
                Value.new(.call { left, env in
                    try call(getItems(left, &env), &env)(getItems(right, &env), &env)
                })
            })
        }

        return try call(leftItems, &env)(rightItems, &env)
    } else {
        let op = getHighest(binaryOperators)

        guard case let .binary(call) = op.operator.arity else {
            fatalError("Unreachable")
        }

        // Take a single value from each side of the operator
        let left = list[try: op.index - 1]
        let right = list[try: op.index + 1]

        if left == nil {
            // Partially apply the left side
            return Value.new(.call { left, env in
                try call(left, &env)(right!, &env)
            })
        }

        if right == nil {
            // Partially apply the right side
            return Value.new(.call { right, env in
                try call(left!, &env)(right, &env)
            })
        }

        return try call(left!, &env)(right!, &env)
    }
}

private extension Array {
    subscript(try index: Int) -> Element? {
        self.indices.contains(index) ? self[index] : nil
    }

    func take(while fn: (Element, _ prev: Element?) throws -> Bool, _ _prev: Element? = nil) rethrows -> [Element] {
        if try self.count > 0 && fn(self[0], _prev) {
            if self.count == 1 {
                return [self[0]]
            }

            return try [self[0]] + Array(self[1...]).take(while: fn, self[0])
        } else {
            return []
        }
    }
}
