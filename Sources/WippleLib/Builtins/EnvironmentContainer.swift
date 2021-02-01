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
        try Trait.value(.environmentContainer, in: self, &env)
    }

    func environmentContainerValueIfPresent(_ env: inout Environment) throws -> Environment? {
        try Trait.value(.environmentContainer, ifPresentIn: self, &env)
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
        validation: Trait.validation(for: .environmentContainer),
        deriveTraitValue: { value, env in
            "<environment>"
        }
    )
}
