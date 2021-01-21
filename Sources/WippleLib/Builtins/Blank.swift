import Foundation

extension Trait.ID {
    static let blank = Self(debugLabel: "_")
}

public extension Trait {
    static func blank() -> Trait {
        Trait(id: .blank) { _ in
            Void()
        }
    }
}

public extension Value {
    func isBlank(_ env: inout Environment) throws -> Bool {
        try Trait.check(.blank, isPresentIn: self, &env)
    }
}

func initializeEmpty(_ env: inout Environment) {
    // Allow use of '_' as a catch-all validation that returns its input
    env.addConformance(
        derivedTraitID: .validationContainer,
        validation: Trait.validation(for: .blank),
        deriveTraitValue: { value, env -> Validation in
            return { value, env in
                .valid(newValue: value)
            }
        }
    )
}
