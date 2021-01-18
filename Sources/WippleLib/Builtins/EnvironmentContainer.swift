import Foundation

extension Trait.ID {
    static let environmentContainer = Self(debugLabel: "Environment")
}

public extension Trait {
    static func environmentContainer(_ env: inout Environment) -> Trait {
        let capturedEnv = env

        return Trait(id: .environmentContainer) { _ in
            capturedEnv
        }
    }
}

public extension Value {
    func environmentContainerValue(_ env: inout Environment) throws -> Environment {
        try Trait.find(.environmentContainer, in: self, &env).value(&env) as! Environment
    }
}

// MARK: - Initialize

func initializeEnvironmentContainer(_ env: inout Environment) {
    env.variables["env!"] = Value
        .assoc(.evaluate { env in
            Value.assoc(.environmentContainer(&env))
        })
        .trait(.computed())

    // Environment ::= Display
    env.addConformance(
        derivedTraitID: .display,
        validation: Trait.validation(for: .environmentContainer),
        deriveTraitValue: { value, env in
            "<environment>"
        }
    )
}
