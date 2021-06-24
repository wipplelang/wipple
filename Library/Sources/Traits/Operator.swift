import Wipple

// MARK: Operator

public class Operator: Primitive {
    public enum Direction: ValueConvertible {
        case left
        case right
    }

    public typealias OperationOf<Left: _Arity, Right: _Arity, Result> = (Left.Parsed, Right.Parsed, Env, Stack) throws -> Result
    public typealias Operation<Left: _Arity, Right: _Arity> = OperationOf<Left, Right, Value>

    public typealias Parse = (_ left: ArraySlice<Value>, _ right: ArraySlice<Value>, Env, Stack) throws ->
        (left: ArraySlice<Value>?, result: Value, right: ArraySlice<Value>?)

    public typealias PartialApplication = (ArraySlice<Value>, Env, Stack) throws -> (Function, remaining: ArraySlice<Value>?)

    public fileprivate(set) var precedence: Precedence!

    public let leftArity: AnyArity
    public let rightArity: AnyArity
    public let parse: Parse
    public let partiallyApplyLeft: PartialApplication
    public let partiallyApplyRight: PartialApplication
    public let function: Function

    public init<Left: _Arity, Right: _Arity>(leftArity: Left, rightArity: Right, direction: Direction = .left, operation: @escaping Operation<Left, Right>) {
        self.leftArity = leftArity.erased
        self.rightArity = rightArity.erased

        self.parse = { left, right, env, stack in
            let (left, remainingLeft) = leftArity.parse(direction: .left, items: left)
            let (right, remainingRight) = rightArity.parse(direction: .right, items: right)

            let result = try operation(left, right, env, stack)

            return (remainingLeft, result, remainingRight)
        }

        self.partiallyApplyLeft = Self.makePartialApplication(
            firstArity: leftArity,
            direction: .left,
            secondArity: rightArity,
            operation: operation
        )

        self.partiallyApplyRight = Self.makePartialApplication(
            firstArity: rightArity,
            direction: .right,
            secondArity: leftArity,
            operation: { try operation($1, $0, $2, $3) }
        )

        switch direction {
        case .left:
            self.function = Self.makeFunction(
                firstArity: leftArity,
                direction: .left,
                secondArity: rightArity,
                operation: operation
            )
        case .right:
            self.function = Self.makeFunction(
                firstArity: rightArity,
                direction: .right,
                secondArity: leftArity,
                operation: { try operation($1, $0, $2, $3) }
            )
        }
    }
}

internal func setupOperator(_ env: Env, _ stack: Stack) throws {
    try env.setVariable("Operator", to: Value(Trait(Operator.self)), stack)

    // Operator == Function
    try env.addRelation(from: Operator.self, to: Function.self, stack, \.function)

    // Operator == Text
    try env.addRelation(text: "operator", for: Operator.self, stack)
}

// MARK: Arity

public protocol _Arity {
    associatedtype Parsed

    var erased: AnyArity { get }

    func parse(value: Value?) -> Parsed?
    func parse(direction: Operator.Direction, items: ArraySlice<Value>) -> (Parsed, remaining: ArraySlice<Value>?)
}

public struct NoneArity: _Arity {
    public let erased: AnyArity = .none

    public func parse(value: Value?) -> Void? {
        value == nil ? () : nil
    }

    public func parse(direction: Operator.Direction, items: ArraySlice<Value>) -> (Void, remaining: ArraySlice<Value>?) {
        ((), items)
    }
}

public extension _Arity where Self == NoneArity {
    static var none: NoneArity {
        NoneArity()
    }
}

public struct SingleArity: _Arity {
    public let erased: AnyArity = .single

    public func parse(value: Value?) -> Value? {
        value
    }

    public func parse(direction: Operator.Direction, items: ArraySlice<Value>) -> (Value, remaining: ArraySlice<Value>?) {
        var items = items

        switch direction {
        case .left:
            return (items.removeLast(), items)
        case .right:
            return (items.removeFirst(), items)
        }
    }
}

public extension _Arity where Self == SingleArity {
    static var single: SingleArity {
        SingleArity()
    }
}

public struct ManyArity: _Arity {
    public let erased: AnyArity = .many

    public func parse(value: Value?) -> [Value]? {
        guard let value = value else { return nil }

        if let list = value.primitiveValue as? List {
            return list.rawValue
        } else {
            return nil
        }
    }

    public func parse(direction: Operator.Direction, items: ArraySlice<Value>) -> ([Value], remaining: ArraySlice<Value>?) {
        (Array(items), nil)
    }
}

public extension _Arity where Self == ManyArity {
    static var many: ManyArity {
        ManyArity()
    }
}

public enum AnyArity: ValueConvertible {
    case none
    case single
    case many
}

// TODO: Remove when Swift 5.5 is released and we can use the above protocol extensions
public enum Arity {
    static let none = NoneArity()
    static let single = SingleArity()
    static let many = ManyArity()
}

// MARK: Partial application

private extension Operator {
    static func makePartialApplication<First: _Arity, Second: _Arity>(firstArity: First, direction: Direction, secondArity: Second, operation: @escaping Operation<First, Second>) -> PartialApplication {
        { first, env, stack in
            let (first, remaining) = firstArity.parse(direction: direction, items: first)

            let function = Self.makePartialFunction(
                with: First.self,
                first: first,
                secondArity: secondArity,
                operation: operation
            )

            return (function, remaining)
        }

        // FIXME: MAKE SURE THIS WORKS WITH NONE ARITY
    }

    static func makeFunction<First: _Arity, Second: _Arity>(firstArity: First, direction: Direction, secondArity: Second, operation: @escaping Operation<First, Second>) -> Function {
        Function { value, env, stack in
            guard let first = firstArity.parse(value: value) else {
                throw Self.arityError(stack)
            }

            return Value(Self.makePartialFunction(
                with: First.self,
                first: first,
                secondArity: secondArity,
                operation: operation
            ))
        }
    }

    static func makePartialFunction<First: _Arity, Second: _Arity>(with firstType: First.Type, first: First.Parsed, secondArity: Second, operation: @escaping Operation<First, Second>) -> Function {
        Function { value, env, stack in
            guard let second = secondArity.parse(value: value) else {
                throw Self.arityError(stack)
            }

            return try operation(first, second, env, stack)
        }
    }

    private static func arityError(_ stack: Stack) -> Exit {
        .error("Input to partially-applied operator does not match the operator's arity", stack)
    }
}

// MARK: Precedence

public final class Precedence: Hashable, Primitive {
    public enum Associativity: ValueConvertible {
        case none
        case left
        case right
    }

    public let associativity: Associativity
    public var operators: [Operator]

    internal init(associativity: Associativity) {
        self.associativity = associativity
        self.operators = []
    }

    public static func == (lhs: Precedence, rhs: Precedence) -> Bool {
        lhs === rhs
    }

    public func hash(into hasher: inout Hasher) {
        hasher.combine(ObjectIdentifier(self))
    }
}

public extension Env {
    private struct PrecedencesKey: EnvKey {
        typealias Value = [Precedence]

        static let visibility = EnvKeyVisibility<Value>.private
    }

    var precedences: [Precedence] {
        get {
            precondition(self.isGlobal)

            return self[PrecedencesKey.self] ?? []
        }
        set {
            precondition(self.isGlobal)

            self[PrecedencesKey.self] = newValue
        }
    }
}

public extension Env {
    func addHighestPrecedence(associativity: Precedence.Associativity) -> Precedence {
        let prec = Precedence(associativity: associativity)
        self.precedences.insert(prec, at: 0)
        return prec
    }

    func addLowestPrecedence(associativity: Precedence.Associativity) -> Precedence {
        let prec = Precedence(associativity: associativity)
        self.precedences.append(prec)
        return prec
    }

    func addPrecedence(associativity: Precedence.Associativity, higherThan other: Precedence) -> Precedence {
        let prec = Precedence(associativity: associativity)

        let index = self.precedences.firstIndex(of: other)!
        self.precedences.insert(prec, at: index)

        return prec
    }

    func addPrecedence(associativity: Precedence.Associativity, lowerThan other: Precedence) -> Precedence {
        let prec = Precedence(associativity: associativity)

        let index = self.precedences.firstIndex(of: other)!
        self.precedences.insert(prec, at: index + 1)

        return prec
    }
}

public extension Env {
    func addOperator(_ name: String, _ oper: Operator, to prec: Precedence, _ stack: Stack) throws {
        precondition(self.isGlobal)
        precondition(oper.precedence == nil, "Operator has already been added")

        oper.precedence = prec

        let index = self.precedences.firstIndex(of: prec)!
        self.precedences[index].operators.append(oper)

        try self.setVariable(name, to: Value(oper), stack)
    }
}

// MARK: Parsing

internal extension List {
    typealias Operators = [(index: Int, oper: Operator)]

    var operators: Operators {
        var operators: Operators = []

        for (index, item) in self.rawValue.enumerated() {
            var oper: Operator? {
                if let oper = item.primitiveValue as? Operator {
                    return oper
                }

                if let name = item.primitiveValue as? Name,
                   case .value(let variable) = name.resolveVariableIfPresent(Env.global),
                   let oper = variable.primitiveValue as? Operator
                {
                    return oper
                }

                return nil
            }

            if let oper = oper {
                operators.append((index, oper))
            }
        }

        return operators
    }

    func parseOperators(_ env: Env, _ stack: Stack) throws -> Value? {
        let operators = self.operators

        // No need to parse a list containing no operators
        if operators.isEmpty {
            return nil
        }

        // Return a single operator as-is if it is by itself
        if operators.count == 1 && self.rawValue.count == 1 {
            return Value(operators[0].oper)
        }

        // Sort the operators into their precedences
        let sortedOperatorsByPrecedence: [(Precedence, Operators)] =
            Dictionary(grouping: operators, by: \.oper.precedence!).sorted { lhs, rhs in
                func index(_ prec: Precedence) -> Int {
                    Env.global.precedences.firstIndex(of: prec)!
                }

                return index(lhs.key) < index(rhs.key)
            }

        // Get the highest precedence present in the list
        var (prec, sortedOperators) = sortedOperatorsByPrecedence[0]
        sortedOperators.sort { $0.index < $1.index }

        // Select an operator based on the precedence's associativity
        let (index, oper): Operators.Element
        switch prec.associativity {
        case .none:
            guard sortedOperators.count == 1 else {
                throw Exit.error("Adjacent operators are not associative", stack)
            }

            (index, oper) = sortedOperators[0]
        case .left:
            (index, oper) = sortedOperators.first!
        case .right:
            (index, oper) = sortedOperators.last!
        }

        let left = self.rawValue[..<index]
        let right = self.rawValue[(index + 1)...]

        switch (left.isEmpty, right.isEmpty) {
        case (false, false),
             (true, false) where oper.leftArity == .none,
             (false, true) where oper.rightArity == .none:
            let parsed = try oper.parse(left, right, env, stack)

            return Value(Array(parsed.left ?? []) + CollectionOfOne(parsed.result) + Array(parsed.right ?? []))
        case (false, true):
            let (result, remaining) = try oper.partiallyApplyLeft(left, env, stack)

            return Value(CollectionOfOne(Value(result)) + Array(remaining ?? []))
        case (true, false):
            let (result, remaining) = try oper.partiallyApplyRight(right, env, stack)

            return Value(Array(remaining ?? []) + CollectionOfOne(Value(result)))
        default:
            preconditionFailure("Unreachable because the operator itself would have already been returned")
        }
    }
}
