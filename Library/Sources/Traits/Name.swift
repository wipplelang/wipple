import Wipple

public struct Name: RawRepresentable, Hashable, Primitive {
    public var rawValue: String

    public init(rawValue: String) {
        self.rawValue = rawValue
    }
}

public extension Name {
    static var hole = Name(rawValue: "_")

    var isHole: Bool {
        self == .hole
    }
}

public enum Variable {
    public typealias Compute = (Env, Stack) throws -> Value

    case value(Value)
    case computed(Compute)
}

public extension Variable {
    func getValue(_ env: Env, _ stack: Stack) throws -> Value {
        switch self {
        case .value(let value):
            return value
        case .computed(let compute):
            return try compute(env, stack)
        }
    }
}

public extension Env {
    private struct VariablesKey: EnvKey {
        typealias Value = [String: Variable]

        static let visibility = EnvKeyVisibility<Value>.public { old, new, _, _ in
            old.merge(new, uniquingKeysWith: { old, _ in old })
        }
    }

    var variables: [String: Variable] {
        get { self[VariablesKey.self] ?? [:] }
        set { self[VariablesKey.self] = newValue }
    }
}

public struct Assign: Primitive {
    public typealias RawValue = (Value, Env, Stack) throws -> (String, Value)

    public let rawValue: RawValue

    public init(_ rawValue: @escaping RawValue) {
        self.rawValue = rawValue
    }

    public func callAsFunction(_ value: Value, _ env: Env, _ stack: Stack) throws -> (String, Value) {
        try self.rawValue(value, env, stack)
    }
}

public typealias HandleAssignment = Operator.OperationOf<ManyArity, ManyArity, Void>

public extension Env {
    private struct HandleAssignmentKey: EnvKey {
        typealias Value = HandleAssignment

        static let visibility = EnvKeyVisibility<Value>.private
    }

    var handleAssignment: HandleAssignment {
        get {
            self[HandleAssignmentKey.self] ?? { left, right, env, stack in
                guard left.count == 1 else {
                    throw Exit.error("Expected single value on left side of assignment", stack)
                }

                let assign = try left[0].get(Assign.self, or: "Cannot assign to this value because it does not have the Assign trait", env, stack)

                let (name, value) = try assign(Value(right), env, stack)

                try env.setVariable(name, to: value, stack)
            }
        }
        set { self[HandleAssignmentKey.self] = newValue }
    }
}

public extension Env {
    private struct HandleComputedAssignmentKey: EnvKey {
        typealias Value = HandleAssignment

        static let visibility = EnvKeyVisibility<Value>.private
    }

    var handleComputedAssignment: HandleAssignment {
        get {
            self[HandleComputedAssignmentKey.self] ?? { left, right, env, stack in
                guard left.count == 1 else {
                    throw Exit.error("Expected single value on left side of assignment", stack)
                }

                let name = try left[0].get(Name.self, or: "Expected name", env, stack)

                let capturedEnv = env.child()

                try env.setComputedVariable(name.rawValue, stack) { _, stack in
                    try catchReturn {
                        try Value(right).evaluate(capturedEnv, stack)
                    }
                }
            }
        }
        set { self[HandleComputedAssignmentKey.self] = newValue }
    }
}

public extension Env {
    func setVariable(_ name: String, to value: Value, _ stack: Stack) throws {
        try self.setVariable(name, to: .value(value), stack)
    }

    func setComputedVariable(_ name: String, to compute: @escaping Variable.Compute, _ stack: Stack) throws {
        try self.setComputedVariable(name, stack, compute: compute)
    }

    func setComputedVariable(_ name: String, _ stack: Stack, compute: @escaping Variable.Compute) throws {
        try self.setVariable(name, to: .computed(compute), stack)
    }

    private func setVariable(_ name: String, to variable: Variable, _ stack: Stack) throws {
        if Name(rawValue: name).isHole {
            return // assignments to '_' are ignored
        }

        if self.variables.keys.contains(name) {
            throw Exit.error("Cannot assign to a variable more than once in the same scope", stack)
        }

        self.variables[name] = variable
    }
}

public extension Name {
    func resolve(_ env: Env, _ stack: Stack) throws -> Value {
        var stack = stack
        stack.diagnostics.add("Resolving variable '\(self.rawValue)'")

        return try self.resolveVariable(env, stack, addDiagnostics: false).getValue(env, stack)
    }

    func resolveIfPresent(_ env: Env, _ stack: Stack) throws -> Value? {
        var stack = stack
        stack.diagnostics.add("Resolving variable '\(self.rawValue)'")

        guard let variable = self.resolveVariableIfPresent(env) else {
            return nil
        }

        return try variable.getValue(env, stack)
    }

    func resolveVariable(
        _ env: Env,
        _ stack: Stack,
        addDiagnostics: Bool = true
    ) throws -> Variable {
        var stack = stack

        if addDiagnostics {
            stack.diagnostics.add("Resolving variable '\(self.rawValue)'")
        }

        if self.isHole {
            return Variable.value(.empty)
        }

        guard let variable = self.resolveVariableIfPresent(env) else {
            throw Exit.error("'\(self.rawValue)' does not refer to a variable", stack)
        }

        return variable
    }

    func resolveVariableIfPresent(_ env: Env) -> Variable? {
        if let variable = env.variables[self.rawValue] {
            return variable
        }

        if let parent = env.parent {
            return self.resolveVariableIfPresent(parent)
        }

        return nil
    }
}

internal func setupName(_ env: Env, _ stack: Stack) throws {
    // Name : trait
    try env.setVariable("Name", to: Value(Trait(Name.self)), stack)

    // Name == Evaluate
    try env.addRelation(from: Name.self, to: Evaluate.self, stack) { name in
        Evaluate(name.resolve)
    }

    // Name == Assign
    try env.addRelation(from: Name.self, to: Assign.self, stack) { name in
        Assign { value, env, stack in
            (name.rawValue, try value.evaluate(env, stack))
        }
    }

    // Name == Text
    try env.addRelation(from: Name.self, to: Text.self, stack) { name in
        Text(rawValue: name.rawValue)
    }
}
