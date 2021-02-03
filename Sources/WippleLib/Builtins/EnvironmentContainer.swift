import Foundation

extension TraitID where T == Environment {
    static let environmentContainer = Self(debugLabel: "Environment")
}

public extension Trait {
    static func environmentContainer(_ env: Environment) -> Trait<Environment> {
        .init(id: .environmentContainer) { _ in
            env
        }
    }
}

// MARK: - Initialize

func initializeEnvironmentContainer(_ env: inout Environment) {
    env.variables["capture-env!"] = Value
        .new(.evaluate { env in
            Value.new(.environmentContainer(env))
        })
        .add(.computed())
    
    env.variables["apply-env!"] = Value.new(.function { input, env in
        let capturedEnv = try input.trait(.environmentContainer, &env)
        
        env = capturedEnv
        
        return Value()
    })

    // Environment ::= Text
    env.addConformance(
        derivedTraitID: .text,
        validation: TraitID.environmentContainer.validation(),
        deriveTraitValue: { value, env in
            "<environment>"
        }
    )
}
