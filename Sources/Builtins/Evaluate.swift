public typealias EvaluateFunc = (inout Environment, ProgramStack) throws -> Value

extension TraitID where T == EvaluateFunc {
    public static let evaluate = TraitID(debugLabel: "Evaluate")
}

extension Trait where T == EvaluateFunc {
    public static func evaluate(_ value: @escaping EvaluateFunc) -> Self {
        Trait(id: .evaluate) { _, _ in value }
    }
}

extension Value {
    public func evaluate(_ env: inout Environment, _ stack: ProgramStack) throws -> Value {
        let stack = stack.add("Evaluating '\(self.format(&env, stack))'")

        guard let evaluate = try self.traitIfPresent(.evaluate, &env, stack) else {
            return self
        }

        return try evaluate(&env, stack)
    }
}

internal func setupEvaluate(_ env: inout Environment) {
    env.variables["eval!"] = Value(
        .function { value, env, stack in
            try value.evaluate(&env, stack).evaluate(&env, stack)
        }
    )
}
