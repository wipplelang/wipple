import Wipple

public struct Function: Primitive {
    public let rawValue: (Value, Env, Stack) throws -> Value

    /// If `true`, the function won't catch any `return`s inside it
    public let isTransparent: Bool

    public init(
        transparent: Bool = false,
        rawValue: @escaping (Value, Env, Stack) throws -> Value
    ) {
        self.rawValue = rawValue
        self.isTransparent = transparent
    }

    public func callAsFunction(_ input: Value, _ env: Env, _ stack: Stack) throws -> Value {
        if self.isTransparent {
            return try self.rawValue(input, env, stack)
        } else {
            return try Wipple.catchReturn {
                try self.rawValue(input, env, stack)
            }
        }
    }
}

public extension Value {
    func call(with input: Value, _ env: Env, _ stack_: Stack) throws -> Value {
        var stack = stack_
        stack.diagnostics.add("Calling '\(self.formatWithFallback(env, stack_))'")

        let function = try self.get(
            Function.self,
            or: "Cannot call this value because it does not have the Function trait",
            env,
            stack
        )

        return try function(input, env, stack)
    }
}

internal func setupFunction(_ env: Env, _ stack: Stack) throws {
    try setupClosure(env, stack)
    try setupTemplate(env, stack)

    try env.addRelation(text: "function", for: Function.self, stack)

    try env.setVariable("Function", to: Value(Trait(Function.self)), stack)
}
