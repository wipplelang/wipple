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
    func evaluate(_ env: inout Environment) throws -> Value {
        guard let evaluateTrait = try Trait.find(.evaluate, ifPresentIn: self, &env) else {
            return self
        }

        let evaluate = try evaluateTrait.value(&env) as! EvaluateFunction
        
        return try evaluate(&env)
    }
}

func initializeEvaluation(_ env: inout Environment) {
    env.variables["eval"] = Value.assoc(.call { input, env in
        try input.evaluate(&env).evaluate(&env)
    })
}
