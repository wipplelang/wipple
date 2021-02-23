public struct Quoted {
    public var value: Value
    public var location: SourceLocation?

    public init(
        _ value: Value,
        location: SourceLocation? = nil
    ) {
        self.value = value
        self.location = location
    }
}

extension TraitID where T == Quoted {
    public static let quoted = TraitID(debugLabel: "Quoted")
}

extension Trait where T == Quoted {
    public static func quoted(_ value: Quoted) -> Self {
        Trait(id: .quoted) { _, _ in value }
    }
}

internal func setupQuoted(_ env: inout Environment) {
    // Quoted ::= Evaluate
    env.addConformance(
        Conformance(
            derivedTraitID: .evaluate,
            validation: TraitID.quoted.validation,
            deriveTraitValue: { quoted, _, _ in
                { _, _ in
                    quoted.value
                }
            }
        )
    )

    // (Quoted and Macro-Parameter) ::= Macro-Parameter
    env.addConformance(
        Conformance(
            derivedTraitID: .macroParameter,
            validation: TraitID.quoted.validation
                && Validation { quoted, env, stack in
                    try TraitID.macroParameter.validation(quoted.value, &env, stack)
                },
            deriveTraitValue: { x, _, _ in
                let (quoted, defineParameter) = x

                return { value, env, stack in
                    var stack = stack
                    if let location = quoted.location {
                        stack.queueLocation(location)
                    }

                    return try defineParameter(Value(.quoted(Quoted(value))), &env, stack)
                }
            }
        )
    )

    // Quoted ::= Macro-Expand
    env.addConformance(
        Conformance(
            derivedTraitID: .macroExpand,
            validation: TraitID.quoted.validation,
            deriveTraitValue: { quoted, _, _ in
                { parameter, replacement, env, stack in
                    var stack = stack
                    if let location = quoted.location {
                        stack.queueLocation(location)
                    }

                    let value = try quoted.value.macroExpand(
                        parameter: parameter,
                        replacement: replacement,
                        &env,
                        stack
                    )

                    return Value(.quoted(Quoted(value)))
                }
            }
        )
    )

    // (Quoted and Text) ::= Text
    env.addConformance(
        Conformance(
            derivedTraitID: .text,
            validation: TraitID.quoted.validation & Validation { quoted, env, stack in
                try TraitID.text.validation(quoted.value, &env, stack)
            },
            deriveTraitValue: { text, _, _ in
                Text("'\(text.text)")
            }
        )
    )
}
