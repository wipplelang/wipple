import Foundation

public typealias Quoted = Value

extension TraitID where T == Quoted {
    static let quoted = Self(debugLabel: "Quoted")
}

public extension Trait {
    static func quoted(_ quotedValue: Quoted) -> Trait<Quoted> {
        .init(id: .quoted) { _ in
            quotedValue
        }
    }
}

// MARK: - Initialize

public func initializeQuoted(_ env: inout Environment) {
    // (Quoted and Text) ::= Text
    env.addConformance(
        derivedTraitID: .text,
        validation: TraitID.quoted.validation() && TraitID.text.validation(),
        deriveTraitValue: { text, env in
            "'\(text)"
        }
    )

    // Quoted ::= Evaluate
    env.addConformance(
        derivedTraitID: .evaluate,
        validation: TraitID.quoted.validation(),
        deriveTraitValue: { quotedValue, env in
            return { env in
                quotedValue
            }
        }
    )
    
    // Quoted ::= Macro-Parameter
    env.addConformance(
        derivedTraitID: .macroParameter,
        validation: TraitID.quoted.validation(),
        deriveTraitValue: { quotedValue, env in
            return { input, env in
                let defineParameter = try quotedValue.trait(.macroParameter, &env)
                
                return try defineParameter(Value.new(.quoted(input)), &env)
            }
        }
    )
    
    // Quoted ::= Macro-Expand
    env.addConformance(
        derivedTraitID: .macroExpand,
        validation: TraitID.quoted.validation(),
        deriveTraitValue: { quotedValue, env in
            return { parameter, replacement, env in
                let newValue = try quotedValue.macroExpand(parameter: parameter, replacement: replacement, &env)
                
                return Value.new(.quoted(newValue))
            }
        }
    )
}
