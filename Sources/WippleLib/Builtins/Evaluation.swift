import Foundation

public typealias EvaluateFunction = (inout Environment) throws -> Value

extension Trait.ID {
    static let evaluate = Self(debugLabel: "Evaluate")
}

public extension Trait {
    static func evaluate(_ evaluate: @escaping EvaluateFunction) -> Trait {
        Trait(id: .evaluate) { _ in
            evaluate
        }
    }
}

public extension Value {
    func evaluateValue(_ env: inout Environment) throws -> EvaluateFunction {
        try Trait.value(.evaluate, in: self, &env)
    }

    func evaluateValueIfPresent(_ env: inout Environment) throws -> EvaluateFunction? {
        try Trait.value(.evaluate, ifPresentIn: self, &env)
    }

    func evaluateValueWithDefault(_ env: inout Environment) throws -> EvaluateFunction {
        try self.evaluateValueIfPresent(&env) ?? { _ in self }
    }

    func evaluate(_ env: inout Environment) throws -> Value {
        try self.evaluateValueWithDefault(&env)(&env)
    }
}

func initializeEvaluation(_ env: inout Environment) {
    env.variables["eval"] = Value.assoc(.call { input, env in
        try input.evaluate(&env).evaluate(&env)
    })
}
