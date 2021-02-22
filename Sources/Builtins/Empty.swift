internal func setupEmpty(_ env: inout Environment) {
    // _ : <empty value>
    env.variables["_"] = Value.empty

    let emptyValidation: Validation<Value, Value> = { value, _, _ in
        value.traits.isEmpty ? .valid(value) : .invalid
    }

    // Allow the use of '_' as a catch-all validation that returns its input
    env.addConformance(
        Conformance(
            derivedTraitID: .validation,
            validation: emptyValidation,
            deriveTraitValue: { _, _, _ in
                { value, _, _ in
                    .valid(value)
                }
            }
        )
    )

    // empty : <validation>
    env.variables["empty"] = Value(.validation(emptyValidation))

    // empty ::= Text
    env.addConformance(
        Conformance(
            derivedTraitID: .text,
            validation: emptyValidation,
            deriveTraitValue: { _, _, _ in
                Text("<empty value>")
            }
        )
    )
}
