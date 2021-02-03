import Foundation

public typealias EvaluateFunction = (inout Environment) throws -> Value

extension TraitID where T == EvaluateFunction {
    static let evaluate = Self(debugLabel: "Evaluate")
}

public extension Trait {
    static func evaluate(_ evaluate: @escaping EvaluateFunction) -> Trait<EvaluateFunction> {
        .init(id: .evaluate) { _ in
            evaluate
        }
    }
}

public extension Value {
    func evaluate(_ env: inout Environment) throws -> Value {
        guard let evaluate = try self.traitIfPresent(.evaluate, &env) else {
            return self
        }
        
        return try evaluate(&env)
    }
}

func initializeEvaluation(_ env: inout Environment) {
    env.variables["eval"] = Value.new(.function { input, env in
        try input.evaluate(&env).evaluate(&env)
    })
}
