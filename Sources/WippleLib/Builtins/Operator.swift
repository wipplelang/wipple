import Foundation

extension Trait.ID {
    static let `operator` = Self(debugLabel: "Operator")
}

public extension Trait {
    static func `operator`(_ op: Operator<CallFunction>) -> Trait {
        Trait(id: .operator) { _ in
            op
        }
    }

    static func `operator`(arity: Operator<CallFunction>.Arity, associativity: Operator<CallFunction>.Associativity, call: @escaping CallFunction) -> Trait {
        self.operator(Operator(arity: arity, associativity: associativity, call: call))
    }
}

public extension Value {
    func operatorValue(_ env: inout Environment) throws -> Operator<CallFunction> {
        try Trait.find(.operator, in: self, &env).value(&env) as! Operator<CallFunction>
    }
}

public struct Operator<Call>: Identifiable {
    public var id = UUID()
    public var arity: Arity
    public var associativity: Associativity
    public var call: Call

    public init(arity: Arity, associativity: Associativity, call: Call) {
        self.arity = arity
        self.associativity = associativity
        self.call = call
    }

    public enum Arity {
        case binary
        case variadic
    }

    public enum Associativity {
        case left
        case right
    }

    public enum Precedence {
        case highest
        case lowest
        case sameAs(Operator<Call>)
        case higherThan(Operator<Call>)
        case lowerThan(Operator<Call>)
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

public typealias OperatorsByPrecedence<C> = [Set<Operator<C>>]

internal enum RegisterOperatorError: String, Error {
    case differentArities = "Cannot relate to the precedence of an operator with a different arity"
}

internal func registerOperator<C>(_ op: Operator<C>, precedence: Operator<C>.Precedence, in operatorsByPrecedence: inout OperatorsByPrecedence<C>) -> Result<(), RegisterOperatorError> {
    // Ensure the operators are the same arity
    switch precedence {
    case let .sameAs(other), let .higherThan(other), let .lowerThan(other):
        guard op.arity == other.arity else {
            return .failure(.differentArities)
        }
    default:
        break
    }

    func prepend() {
        operatorsByPrecedence.insert([op], at: 0)
    }

    func append() {
        operatorsByPrecedence.append([op])
    }

    func tryInserting(_ other: Operator<C>, at relativeIndex: Int, else prependOrAppend: () -> Void) {
        let index = operatorsByPrecedence.firstIndex(where: { $0.contains(other) })! + relativeIndex
        if operatorsByPrecedence.indices.contains(index) {
            operatorsByPrecedence[index].insert(op)
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

    return .success(())
}

public extension Environment {
    mutating func registerOperator(_ op: Operator<CallFunction>, precedence: Operator<CallFunction>.Precedence) throws {
        try WippleLib.registerOperator(op, precedence: precedence, in: &self.operatorsByPrecedence)
            .mapError { ProgramError($0.rawValue) }
            .get()
    }
}

// MARK: - Operator parsing

public func findOperators(in list: [Value], _ env: inout Environment) throws -> OperatorList<CallFunction> {
    try findOperators(in: list, getOperator: { value in
        var op: Operator<CallFunction>?

        @discardableResult
        func getOperator(_ value: Value) throws -> Bool {
            if let operatorTrait = try Trait.find(.operator, ifPresentIn: value, &env) {
                let operatorValue = try operatorTrait.value(&env) as! Operator<CallFunction>
                op = operatorValue
                return true
            }

            return false
        }

        if !(try getOperator(value)), try Trait.check(.name, isPresentIn: value, &env) {
            let evaluatedValue = try value.evaluate(&env)
            try getOperator(evaluatedValue)
        }

        return op
    })
}

public func parseOperators(in list: [Value], operators: OperatorList<CallFunction>, _ env: Environment) throws -> Value {
    let result = parseOperators(
        in: list,
        operatorsInList: operators,
        operatorsByPrecedence: env.operatorsByPrecedence,
        groupCall: { Value.assoc(.call($0)) },
        groupList: { Value.assoc(.list($0)) }
    )

    switch result {
    case let .success(value):
        return value
    case let .failure(error):
        throw ProgramError(error.rawValue)
    }
}

public typealias OperatorList<Call> = [(operator: Operator<Call>, index: Int)]

enum ParseOperatorsError: String, Error, Equatable {
    case multipleOperatorsWithSamePrecedence = "Found multiple operators with the same precedence; group the expression into lists to disambiguate"
    case missingBinaryLeft = "Expected value on left side of binary operator expression"
    case missingBinaryRight = "Expected value on right side of binary operator expression"
    case missingVariadicLeft = "Expected one or more values on left side of variadic operator expression"
    case missingVariadicRight = "Expected one or more values on right side of variadic operator expression"
}

public func findOperators<V, C>(in list: [V], getOperator: (V) throws -> Operator<C>?) rethrows -> OperatorList<C> {
    var operators: OperatorList<C> = []

    for (index, value) in list.enumerated() {
        if let op = try getOperator(value) {
            operators.append((op, index))
        }
    }

    return operators
}

func parseOperators<V, C>(
    in list: [V],
    operatorsInList: OperatorList<C>,
    operatorsByPrecedence: OperatorsByPrecedence<C>,
    groupCall: (C) -> V, // eg. convert a CallFunction to a Call value
    groupList: ([V]) -> V // eg. convert a list of values to a List value
) -> Result<V, ParseOperatorsError> {
    var operatorsInList = operatorsInList

    // Special case: list with single value isn't parsed
    if list.count == 1 {
        if let op = operatorsInList.first {
            return .success(groupCall(op.operator.call))
        } else {
            return .success(list[0])
        }
    }

    // Ensure there are no operators with the same precedence (ambiguous)
    guard operatorsByPrecedence.allSatisfy({ operators in
        operators.intersection(operatorsInList.map(\.operator)).count <= 1
    }) else {
        return .failure(.multipleOperatorsWithSamePrecedence)
    }

    let partition = operatorsInList.partition(by: { $0.operator.arity == .variadic })
    let (binaryOperators, variadicOperators) = (Array(operatorsInList[..<partition]), Array(operatorsInList[partition...]))

    func getHighest(_ ops: OperatorList<C>) -> OperatorList<C>.Element {
        func index(of op: Operator<C>) -> Int {
            guard let precedence = operatorsByPrecedence.firstIndex(where: { $0.contains(op) }) else {
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

        // Group the values on each side -- a single value is left as-is and
        // multiple values are grouped into a list
        func group(_ items: [V]) -> V {
            items.count == 1
                ? items[0]
                : groupList(items)
        }

        let leftItems = Array(list[0..<op.index])
        guard !leftItems.isEmpty else {
            return .failure(.missingVariadicLeft)
        }
        let left = group(leftItems)

        let rightItems = Array(list[(op.index + 1)...])
        guard !rightItems.isEmpty else {
            return .failure(.missingVariadicRight)
        }
        let right = group(rightItems)

        // Transform the expression into prefix notation, substituting the
        // operator with its defined callable value
        let transformed = [groupCall(op.operator.call), left, right]

        return .success(groupList(transformed))
    } else if !binaryOperators.isEmpty {
        let op = getHighest(binaryOperators)

        // Take a single value from each side of the operator
        guard let left = list[try: op.index - 1] else {
            return .failure(.missingBinaryLeft)
        }
        guard let right = list[try: op.index + 1] else {
            return .failure(.missingBinaryRight)
        }

        // Transform the expression into prefix notation, substituting the
        // operator with its defined callable value
        let transformed = groupList([groupCall(op.operator.call), left, right])

        // Substitute the binary operator expression with the transformed value
        let before = list[0..<(op.index - 1)]
        let after = list[(op.index + 2)...]
        let result = Array(before + [transformed] + after)

        return .success(groupList(result))
    } else {
        // The list contains no operators
        return .success(groupList(list))
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
