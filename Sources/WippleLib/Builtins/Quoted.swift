import Foundation

public typealias Quoted = Value

extension Trait.ID {
    static let quoted = Self(debugLabel: "Quoted")
}

public extension Trait {
    static func quoted(_ quotedValue: Quoted) -> Trait {
        Trait(id: .quoted) { _ in
            quotedValue
        }
    }
}

public extension Value {
    func quotedValue(_ env: inout Environment) throws -> Quoted {
        try Trait.find(.quoted, in: self, &env).value(&env) as! Quoted
    }
}

// MARK: - Initialize

public func initializeQuoted(_ env: inout Environment) {
    // (Quoted and Display) ::= Display
    env.addConformance(
        derivedTraitID: .display,
        validation: Trait.validation(for: .quoted) && Trait.validation(for: .display),
        deriveTraitValue: { value, env in
            let display = value as! String

            return "'\(display)"
        }
    )

    // Quoted ::= Evaluate
    env.addConformance(
        derivedTraitID: .evaluate,
        validation: Trait.validation(for: .quoted),
        deriveTraitValue: { value, env in
            value
        }
    )
}
