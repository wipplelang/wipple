import Foundation

extension Trait.ID {
    static let number = Self(debugLabel: "Number")
}

public extension Trait {
    static func number(_ number: Decimal) -> Trait {
        Trait(id: .number) { _ in
            number
        }
    }
}

public extension Value {
    func numberValue(_ env: inout Environment) throws -> Decimal {
        try Trait.value(.number, in: self, &env)
    }

    func numberValueIfPresent(_ env: inout Environment) throws -> Decimal? {
        try Trait.value(.number, ifPresentIn: self, &env)
    }
}

// MARK: - Initialize

public func initializeNumber(_ env: inout Environment) {
    // Number ::= Text
    env.addConformance(
        derivedTraitID: .text,
        validation: Trait.validation(for: .number),
        deriveTraitValue: { number, env in
            String(describing: number as! Decimal)
        }
    )
}
