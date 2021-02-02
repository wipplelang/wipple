import Foundation

extension TraitID where T == Environment {
    static let environmentContainer = Self(debugLabel: "Environment")
}

public extension Trait {
    static func environmentContainer(_ env: inout Environment) -> Trait<Environment> {
        let capturedEnv = env

        return .init(id: .environmentContainer) { _ in
            capturedEnv
        }
    }
}

// MARK: - Initialize

func initializeEnvironmentContainer(_ env: inout Environment) {
    env.variables["env!"] = Value
        .new(.evaluate { env in
            Value.new(.environmentContainer(&env))
        })
        .add(.computed())

    // Environment ::= Text
    env.addConformance(
        derivedTraitID: .text,
        validation: TraitID.environmentContainer.validation(),
        deriveTraitValue: { value, env in
            "<environment>"
        }
    )
}
