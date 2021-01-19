import Foundation

extension Trait.ID {
    static let `operator` = Self(debugLabel: "Operator")
}

public extension Trait {
    static func `operator`(_ op: Operator<Decimal, CallFunction>) -> Trait {
        Trait(id: .operator) { _ in
            op
        }
    }

    static func `operator`(precedence: Decimal, arity: Operator<Decimal, CallFunction>.Arity, associativity: Operator<Decimal, CallFunction>.Associativity, call: @escaping CallFunction) -> Trait {
        self.operator(Operator(precedence: precedence, arity: arity, associativity: associativity, call: call))
    }
}

public extension Value {
    func operatorValue(_ env: inout Environment) throws -> Operator<Decimal, CallFunction> {
        try Trait.find(.operator, in: self, &env).value(&env) as! Operator<Decimal, CallFunction>
    }
}

public struct Operator<Precedence: Comparable, Call> {
    public var precedence: Precedence
    public var arity: Arity
    public var associativity: Associativity
    public var call: Call

    public init(precedence: Precedence, arity: Arity, associativity: Associativity, call: Call) {
        self.precedence = precedence
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
}

// MARK: - Operator parsing

public typealias OperatorList<Precedence: Comparable, Call> = [(operator: Operator<Precedence, Call>, index: Int)]

public func findOperators(in list: [Value], _ env: inout Environment) throws -> OperatorList<Decimal, CallFunction> {
    var operators: OperatorList<Decimal, CallFunction> = []

    for (index, value) in list.enumerated() {
        var op: Operator<Decimal, CallFunction>!

        @discardableResult
        func getOperator(_ value: Value) throws -> Bool {
            if let operatorTrait = try Trait.find(.operator, ifPresentIn: value, &env) {
                let operatorValue = try operatorTrait.value(&env) as! Operator<Decimal, CallFunction>
                op = operatorValue
                return true
            }

            return false
        }

        if !(try getOperator(value)), try Trait.check(.name, isPresentIn: value, &env) {
            let evaluatedValue = try value.evaluate(&env)
            try getOperator(evaluatedValue)
        }

        if let op = op {
            operators.append((op, index))
        }
    }

    return operators
}

public func parseOperators(in list: [Value], using operators: OperatorList<Decimal, CallFunction>) throws -> Value {
    let result = parseOperators(
        in: list,
        using: operators,
        groupItems: { Value.assoc(.list($0)) },
        groupSingle: { Value.assoc(.call($0)) }
    )

    switch result {
    case let .success(value):
        return value
    case let .failure(error):
        throw ProgramError(error.rawValue)
    }
}

enum ParseOperatorsError: String, Error, Equatable {
    case missingBinaryLeft = "Expected value on left side of binary operator expression"
    case missingBinaryRight = "Expected value on right side of binary operator expression"
    case missingVariadidLeft = "Expected one or more values on left side of variadic operator expression"
    case missingVariadicRight = "Expected one or more values on right side of variadic operator expression"
}

func parseOperators<V, P: Comparable, C>(
    in list: [V],
    using operators: OperatorList<P, C>,
    groupItems: ([V]) -> V, // eg. convert a list of values to a List value
    groupSingle: (C) -> V // eg. convert a CallFunction to a Call value
) -> Result<V, ParseOperatorsError> {
    // Special case: list with single value isn't parsed
    if list.count == 1 {
        if let op = operators.first(where: { $0.index == 0 }) {
            return .success(groupSingle(op.operator.call))
        } else {
            return .success(list[0])
        }
    }

    var operators = operators
    let partition = operators.partition(by: { $0.operator.arity == .variadic })
    let (binaryOperators, variadicOperators) = (Array(operators[..<partition]), Array(operators[partition...]))

    func getHighest(_ ops: OperatorList<P, C>) -> OperatorList<P, C>.Element {
        // Select consecutive instances of the operator with the highest precedence
        let highestPrecedence = ops
            .sorted(by: { $0.operator.precedence > $1.operator.precedence })
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
                : groupItems(items)
        }

        let leftItems = Array(list[0..<op.index])
        guard !leftItems.isEmpty else {
            return .failure(.missingVariadidLeft)
        }
        let left = group(leftItems)

        let rightItems = Array(list[(op.index + 1)...])
        guard !rightItems.isEmpty else {
            return .failure(.missingVariadicRight)
        }
        let right = group(rightItems)

        // Transform the expression into prefix notation, substituting the
        // operator with its defined callable value
        let transformed = [groupSingle(op.operator.call), left, right]

        return .success(groupItems(transformed))
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
        let transformed = groupItems([groupSingle(op.operator.call), left, right])

        // Substitute the binary operator expression with the transformed value
        let before = list[0..<(op.index - 1)]
        let after = list[(op.index + 2)...]
        let result = Array(before + [transformed] + after)

        return .success(groupItems(result))
    } else {
        // The list contains no operators
        return .success(groupItems(list))
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
