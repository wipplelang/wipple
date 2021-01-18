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
        try Trait.find(.number, in: self, &env).value(&env) as! Decimal
    }
}

// MARK: - Initialize

public func initializeNumber(_ env: inout Environment) {
    // Number ::= Display
    env.addConformance(
        derivedTraitID: .display,
        validation: Trait.validation(for: .number),
        deriveTraitValue: { number, env in
            return String(describing: number as! Decimal)
        }
    )
}
