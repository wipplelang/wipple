import Foundation

public struct SomeOperator<T> {
    public var id: UUID
    public var function:
        (T, inout Environment, ProgramStack) throws -> (T, inout Environment, ProgramStack) throws
            -> Value

    public init(
        _ function: @escaping (T, inout Environment, ProgramStack) throws -> (
            T, inout Environment, ProgramStack
        ) throws
            -> Value
    ) {
        self.id = UUID()
        self.function = function
    }
}

extension SomeOperator: Equatable {
    public static func == (lhs: Self, rhs: Self) -> Bool {
        lhs.id == rhs.id
    }
}

extension SomeOperator: Hashable {
    public func hash(into hasher: inout Hasher) {
        self.id.hash(into: &hasher)
    }
}

extension SomeOperator {
    public init(
        _ function: @escaping (T, T, inout Environment, ProgramStack) throws -> Value
    ) {
        self.init { left, _, _ in
            { right, env, stack in
                try function(left, right, &env, stack)
            }
        }
    }
}

public typealias BinaryOperator = SomeOperator<Value>

extension BinaryOperator {
    public init(
        _ function: @escaping Function
    ) {
        self.init { left, env, stack in
            try function(left, &env, stack).getTrait(.function, &env, stack)
        }
    }
}

public typealias VariadicOperator = SomeOperator<[Value]>

extension VariadicOperator {
    public init(
        _ function: @escaping Function
    ) {
        self.init {
            left,
            env,
            stack -> ([Value], inout Environment, ProgramStack) throws -> Value in
            let nextFunction = try function(Value(.list(List(left))), &env, stack)
                .getTrait(
                    .function,
                    &env,
                    stack
                )

            return { right, env, stack in
                try nextFunction(Value(.list(List(right))), &env, stack)
            }
        }
    }
}

public enum Operator {
    case binary(BinaryOperator)
    case variadic(VariadicOperator)
}

extension Operator {
    public var id: UUID {
        switch self {
        case .binary(let op):
            return op.id
        case .variadic(let op):
            return op.id
        }
    }
}

extension Operator: Equatable {
    public static func == (lhs: Self, rhs: Self) -> Bool {
        lhs.id == rhs.id
    }
}

extension Operator: Hashable {
    public func hash(into hasher: inout Hasher) {
        self.id.hash(into: &hasher)
    }
}

public enum Arity: Equatable {
    case binary
    case variadic
}

public enum Associativity: Equatable {
    case left
    case right

    // TODO: Add 'none' case that prevents having multiple operators of the same
    // precedence group (useful for assignment)
}

public typealias OperatorList = [(op: Operator, index: Int)]

extension TraitID where T == Operator {
    public static var `operator`: Self {
        .builtin("Operator")
    }
}

extension Trait where T == Operator {
    public static func `operator`(_ value: Operator) -> Self {
        Trait(id: .operator) { _, _ in value }
    }
}

public struct SomePrecedenceGroup<T> {
    public var id: UUID
    public var operators: Set<SomeOperator<T>>
    public var associativity: Associativity

    public init(
        associativity: Associativity
    ) {
        self.id = UUID()
        self.operators = []
        self.associativity = associativity
    }
}

extension SomePrecedenceGroup: Equatable {
    public static func == (lhs: Self, rhs: Self) -> Bool {
        lhs.id == rhs.id
    }
}

extension SomePrecedenceGroup: Hashable {
    public func hash(into hasher: inout Hasher) {
        self.id.hash(into: &hasher)
    }
}

public typealias BinaryPrecedenceGroup = SomePrecedenceGroup<Value>
public typealias VariadicPrecedenceGroup = SomePrecedenceGroup<[Value]>

public enum PrecedenceGroup: Equatable, Hashable {
    case binary(BinaryPrecedenceGroup)
    case variadic(VariadicPrecedenceGroup)
}

extension PrecedenceGroup {
    public var id: UUID {
        switch self {
        case .binary(let p):
            return p.id
        case .variadic(let p):
            return p.id
        }
    }

    public var operators: Set<Operator> {
        get {
            switch self {
            case .binary(let p):
                return Set(p.operators.map(Operator.binary))
            case .variadic(let p):
                return Set(p.operators.map(Operator.variadic))
            }
        }
        set {
            switch self {
            case .binary(var p):
                p.operators = Set(
                    newValue.map {
                        switch $0 {
                        case .binary(let op):
                            return op
                        default:
                            fatalError()
                        }
                    }
                )
                self = .binary(p)
            case .variadic(var p):
                p.operators = Set(
                    newValue.map {
                        switch $0 {
                        case .variadic(let op):
                            return op
                        default:
                            fatalError()
                        }
                    }
                )
                self = .variadic(p)
            }
        }
    }

    public var associativity: Associativity {
        switch self {
        case .binary(let p):
            return p.associativity
        case .variadic(let p):
            return p.associativity
        }
    }
}

public struct PrecedenceGroupComparison<P> {
    public var comparison: (P, inout Environment) -> Void
}

extension PrecedenceGroupComparison {
    fileprivate static func relative(
        _ other: P,
        _ convert: @escaping (P) -> PrecedenceGroup,
        offset: Int
    ) -> Self {
        Self { group, env in
            let index = env.operatorPrecedences.firstIndex(of: convert(other))! + offset

            env.operatorPrecedences.insert(convert(group), at: index)
        }
    }
}

public typealias BinaryPrecedenceGroupComparison = PrecedenceGroupComparison<BinaryPrecedenceGroup>
public typealias VariadicPrecedenceGroupComparison = PrecedenceGroupComparison<
    VariadicPrecedenceGroup
>

extension BinaryPrecedenceGroupComparison {
    public static var highest: Self {
        Self { group, env in
            env.operatorPrecedences.insert(.binary(group), at: 0)
        }
    }

    public static var lowest: Self {
        Self { group, env in
            env.operatorPrecedences.append(.binary(group))
        }
    }

    public static func higherThan(_ other: BinaryPrecedenceGroup) -> Self {
        Self.relative(other, PrecedenceGroup.binary, offset: 0)
    }

    public static func lowerThan(_ other: BinaryPrecedenceGroup) -> Self {
        Self.relative(other, PrecedenceGroup.binary, offset: 1)
    }
}

extension VariadicPrecedenceGroupComparison {
    public static var highest: Self {
        Self { group, env in
            env.operatorPrecedences.insert(.variadic(group), at: 0)
        }
    }

    public static var lowest: Self {
        Self { group, env in
            env.operatorPrecedences.append(.variadic(group))
        }
    }

    public static func higherThan(_ other: VariadicPrecedenceGroup) -> Self {
        Self.relative(other, PrecedenceGroup.variadic, offset: 0)
    }

    public static func lowerThan(_ other: VariadicPrecedenceGroup) -> Self {
        Self.relative(other, PrecedenceGroup.variadic, offset: 0)
    }
}

extension Environment {
    public mutating func addPrecedenceGroup<P>(
        associativity: Associativity,
        _ comparison: PrecedenceGroupComparison<SomePrecedenceGroup<P>>
    ) -> SomePrecedenceGroup<P> {
        let group = SomePrecedenceGroup<P>(associativity: associativity)

        comparison.comparison(group, &self)

        return group
    }

    public mutating func addOperator(
        _ op: BinaryOperator,
        in group: BinaryPrecedenceGroup
    ) {
        let index = self.operatorPrecedences.firstIndex(of: .binary(group))!
        self.operatorPrecedences[index].operators.insert(.binary(op))
    }

    public mutating func addOperator(
        _ op: VariadicOperator,
        in group: VariadicPrecedenceGroup
    ) {
        let index = self.operatorPrecedences.firstIndex(of: .variadic(group))!
        self.operatorPrecedences[index].operators.insert(.variadic(op))
    }
}

// MARK: Operator parsing

extension List {
    public func findOperators(
        _ env: inout Environment,
        _ stack: ProgramStack
    ) throws -> OperatorList {
        var operators: OperatorList = []

        for (index, item) in self.items.enumerated() {
            if let op = try getOperator(from: item, &env, stack) {
                operators.append((op, index))
            }
        }

        return operators
    }
}

public func getOperator(
    from value: Value,
    _ env: inout Environment,
    _ stack: ProgramStack
) throws
    -> Operator?
{
    if let op = try value.getTraitIfPresent(.operator, &env, stack) {
        return op
    }

    if let name = try value.getTraitIfPresent(.name, &env, stack),
        let variable = env.variables[name.name]
    {
        return try variable.getTraitIfPresent(.operator, &env, stack)
    }

    return nil
}

public func getVariadicItems(
    from list: Value,
    _ env: inout Environment,
    _ stack: ProgramStack
)
    throws -> [Value]
{
    try list.getTrait(
        .list,
        orError: "Application of variadic operator requires a list",
        &env,
        stack
    )
    .items
}

extension BinaryOperator {
    public func asFunction() -> Function {
        { left, env, stack in
            Value(.function(try self.function(left, &env, stack)))
        }
    }
}

extension VariadicOperator {
    public func asFunction() -> Function {
        { left, env, stack in
            let function = try self.function(getVariadicItems(from: left, &env, stack), &env, stack)

            return Value(
                .function { right, env, stack in
                    try function(getVariadicItems(from: right, &env, stack), &env, stack)
                }
            )
        }
    }
}

extension Operator {
    public func asFunction() -> Function {
        switch self {
        case .binary(let op):
            return op.asFunction()
        case .variadic(let op):
            return op.asFunction()
        }
    }
}

extension List {
    public func parseOperators(
        using operatorsInList: OperatorList,
        _ env: inout Environment,
        _ stack: ProgramStack
    ) throws -> Value? {
        // No need to parse a list containing no operators
        if operatorsInList.isEmpty {
            return nil
        }

        // List with 0/1 values isn't parsed
        switch self.items.count {
        case 0:
            return nil
        case 1:
            if let op = operatorsInList.first {
                return Value(.function(op.op.asFunction()))
            } else {
                return self.items[0]
            }
        default:
            break
        }

        // Sort the operators into their precedence groups
        let sortedOperatorsByPrecedence: [(PrecedenceGroup, OperatorList)] = {
            // TODO: Probably very inefficient

            var precedences = [Int: (PrecedenceGroup, OperatorList)]()

            for op in operatorsInList {
                let (precedence, group) = env.operatorPrecedences.enumerated()
                    .first(where: { $0.element.operators.contains(op.op) })!

                precedences[precedence, default: (group, [op])].1.append(op)
            }

            let sorted = Array(precedences).sorted(by: { $0.key < $1.key }).map(\.value)

            return sorted
        }()

        // Get the highest precedence group present in the list
        let (precedenceGroup, sortedOperators): (PrecedenceGroup, OperatorList) = {
            let (group, operators) = sortedOperatorsByPrecedence.first!
            let sortedOperators = operators.sorted(by: { $0.index < $1.index })
            return (group, sortedOperators)
        }()

        // Choose the left/rightmost operator based on the level's associativity
        // Left and right are flipped because associativity is for inner
        // grouping, and the outer grouping is evaluated first
        let (op, index): OperatorList.Element
        switch precedenceGroup.associativity {
        case .left:
            (op, index) = sortedOperators.last!
        case .right:
            (op, index) = sortedOperators.first!
        }

        // List with 2 values is partially applied
        switch op {
        case .binary(let op):
            // Take a single value from each side of the operator
            let left = self.items[safe: index - 1]
            let right = self.items[safe: index + 1]

            switch (left, right) {
            case (let left?, let right?):
                // Convert to a function call
                return try op.function(left, &env, stack)(right, &env, stack)
            case (let left?, nil):
                // Partially apply the left side
                let partial = try op.function(left, &env, stack)
                return Value(.function(partial))
            case (nil, let right?):
                // Partially apply the right side
                let partial: Function = { left, env, stack in
                    try op.function(left, &env, stack)(right, &env, stack)
                }

                return Value(.function(partial))
            case (nil, nil):
                fatalError("Unreachable")
            }
        case .variadic(let op):
            // Take a single value from each side of the operator
            let left = Array(self.items[..<index])
            let right = Array(self.items[(index + 1)...])

            switch (left.isEmpty, right.isEmpty) {
            case (false, false):
                // Convert to a function call
                return try op.function(left, &env, stack)(right, &env, stack)
            case (false, true):
                // Partially apply the left side
                let partial: Function = { right, env, stack in
                    try op.function(left, &env, stack)(
                        getVariadicItems(from: right, &env, stack),
                        &env,
                        stack
                    )
                }

                return Value(.function(partial))
            case (true, false):
                // Partially apply the right side
                let partial: Function = { left, env, stack in
                    try op.function(getVariadicItems(from: left, &env, stack), &env, stack)(
                        right,
                        &env,
                        stack
                    )
                }

                return Value(.function(partial))
            case (true, true):
                fatalError("Unreachable")
            }
        }
    }
}

internal func setupOperator(_ env: inout Environment) {
    // Operator ::= Text
    env.addConformance(
        Conformance(
            derivedTraitID: .text,
            validation: TraitID.operator.validation,
            deriveTraitValue: { _, _, _ in
                Text("<operator>")
            }
        )
    )
}

extension Array {
    fileprivate subscript(safe index: Int) -> Element? {
        guard self.indices.contains(index) else {
            return nil
        }

        return self[index]
    }
}
