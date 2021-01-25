import Foundation

public struct TraitConstructor {
    public var id: Trait.ID
    public var check: (Value, inout Environment) throws -> Bool
}

extension Trait.ID {
    static let traitConstructor = Self(debugLabel: "Trait")
}

extension Trait {
    static func traitConstructor(_ id: Trait.ID, check: @escaping (Value, inout Environment) throws -> Bool) -> Trait {
        Trait(id: .traitConstructor) { _ in
            TraitConstructor(id: id, check: check)
        }
    }
}

extension Value {
    func traitConstructorValue(_ env: inout Environment) throws -> TraitConstructor {
        try Trait.find(.traitConstructor, in: self, &env).value(&env) as! TraitConstructor
    }
}

// MARK: - Initialize

func initializeTraitConstructor(_ env: inout Environment) {
    // Trait ::= Display
    env.addConformance(
        derivedTraitID: .display,
        validation: Trait.validation(for: .traitConstructor),
        deriveTraitValue: { value, env in
            "<trait>"
        }
    )
}
