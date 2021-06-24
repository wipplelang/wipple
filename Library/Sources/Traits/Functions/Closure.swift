import Wipple

public struct Closure: Primitive {
    public var capturedEnv: Env
    public var parameter: (pattern: Pattern, name: String)
    public var returnValue: Value

    public init(
        capturing capturedEnv: Env,
        parameter: (pattern: Pattern, name: String),
        returning returnValue: Value
    ) {
        self.capturedEnv = capturedEnv
        self.parameter = parameter
        self.returnValue = returnValue
    }
}

internal func setupClosure(_ env: Env, _ stack: Stack) throws {
    // Closure == Function
    try env.addRelation(from: Closure.self, to: Function.self, stack) { closure in
        Function { value, env, stack in
            let value = try value.evaluate(env, stack)

            guard let matchedValue = try closure.parameter.pattern.match(value, env, stack) else {
                throw Exit.error("Cannot use this value as input to this closure", stack)
            }

            let closureEnv = closure.capturedEnv.child()
            try closureEnv.setVariable(closure.parameter.name, to: matchedValue, stack)

            return try closure.returnValue.evaluate(closureEnv, stack)
        }
    }

    // Closure == Text
    try env.addRelation(text: "closure", for: Closure.self, stack)
}
