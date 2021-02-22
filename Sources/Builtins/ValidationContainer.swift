extension TraitID where T == Validation<Value, Value> {
    public static var validation: Self {
        .builtin("Validation")
    }
}

extension Trait where T == Validation<Value, Value> {
    public static func validation(_ value: @escaping Validation<Value, Value>) -> Self {
        Trait(id: .validation) { _, _ in value }
    }
}

internal func setupValidationContainer(_ env: inout Environment) {
    // Validation ::= Text
    env.addConformance(
        Conformance(
            derivedTraitID: .text,
            validation: TraitID.validation.validation,
            deriveTraitValue: { _, _, _ in Text("<validation>") }
        )
    )
}
