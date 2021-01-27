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
        try Trait.value(.quoted, in: self, &env)
    }

    func quotedValueIfPresent(_ env: inout Environment) throws -> Quoted? {
        try Trait.value(.quoted, ifPresentIn: self, &env)
    }
}

// MARK: - Initialize

public func initializeQuoted(_ env: inout Environment) {
    // (Quoted and Text) ::= Text
    env.addConformance(
        derivedTraitID: .text,
        validation: Trait.validation(for: .quoted) && Trait.validation(for: .text),
        deriveTraitValue: { value, env in
            let text = value as! Text

            return "'\(text)"
        }
    )

    // Quoted ::= Evaluate
    env.addConformance(
        derivedTraitID: .evaluate,
        validation: Trait.validation(for: .quoted),
        deriveTraitValue: { value, env -> EvaluateFunction in
            let quotedValue = value as! Value

            return { env in
                quotedValue
            }
        }
    )
}
