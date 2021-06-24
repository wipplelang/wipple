import Wipple

public struct Evaluate: Primitive {
    public typealias RawValue = (Env, Stack) throws -> Value

    public let rawValue: RawValue

    public init(_ rawValue: @escaping RawValue) {
        self.rawValue = rawValue
    }

    public func callAsFunction(_ env: Env, _ stack: Stack) throws -> Value {
        try self.rawValue(env, stack)
    }
}

public extension Value {
    func evaluate(_ env: Env, _ stack: Stack) throws -> Value {
        guard let evaluate = try self.getIfPresent(Evaluate.self, env, stack) else {
            return self
        }

        return try evaluate(env, stack)
    }
}

internal func setupEvaluate(_ env: Env, _ stack: Stack) throws {
    try env.setVariable("Evaluate", to: Value(Trait(Evaluate.self)), stack)

    try env.setVariable(
        "evaluate",
        to: Value(Function { value, env, stack in
            try value.evaluate(env, stack).evaluate(env, stack)
        }),
        stack
    )
}
