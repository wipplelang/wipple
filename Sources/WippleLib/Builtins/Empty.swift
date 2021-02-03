import Foundation

func initializeEmpty(_ env: inout Environment) {
    // _ : <empty value>
    env.variables["_"] = Value()

    let emptyValidation: Validation<Value, Value> = { value, env in
        value.traits.isEmpty
            ? .valid(newValue: value)
            : .invalid
    }

    // Allow use of '_' as a catch-all validation that returns its input
    env.addConformance(
        derivedTraitID: .validationContainer,
        validation: emptyValidation,
        deriveTraitValue: { value, env in
            return { value, env in
                .valid(newValue: value)
            }
        }
    )

    // <empty value> ::= Text
    env.addConformance(
        derivedTraitID: .text,
        validation: emptyValidation,
        deriveTraitValue: { value, env -> Text in
            "<empty value>"
        }
    )
}
