import Foundation

public struct TraitConstructor {
    public var id: Trait.ID
    public var validation: Validation
}

extension Trait.ID {
    static let traitConstructor = Self(debugLabel: "Trait")
}

extension Trait {
    static func traitConstructor(_ id: Trait.ID, validation: @escaping Validation) -> Trait {
        Trait(id: .traitConstructor) { _ in
            TraitConstructor(id: id, validation: validation)
        }
    }
}

extension Value {
    func traitConstructorValue(_ env: inout Environment) throws -> TraitConstructor {
        try Trait.value(.traitConstructor, in: self, &env)
    }

    func traitConstructorValueIfPresent(_ env: inout Environment) throws -> TraitConstructor? {
        try Trait.value(.traitConstructor, ifPresentIn: self, &env)
    }
}

// MARK: - Initialize

func initializeTraitConstructor(_ env: inout Environment) {
    // Trait ::= Text
    env.addConformance(
        derivedTraitID: .text,
        validation: Trait.validation(for: .traitConstructor),
        deriveTraitValue: { value, env in
            "<trait>"
        }
    )
}
