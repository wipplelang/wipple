public struct TraitConstructor {
    public var id: AnyTraitID
    public var validation: AnyValidation

    public init<T>(
        id: TraitID<T>,
        validation: @escaping AnyValidation
    ) {
        self.id = AnyTraitID(id)
        self.validation = validation
    }
}

extension TraitID where T == TraitConstructor {
    public static var traitConstructor: Self {
        .builtin("Trait")
    }
}

extension Trait where T == TraitConstructor {
    public static func traitConstructor(_ value: TraitConstructor) -> Self {
        Trait(id: .traitConstructor) { _, _ in value }
    }
}

internal func setupTraitConstructor(_ env: inout Environment) {
    // Trait ::= Text
    env.addConformance(
        Conformance(
            derivedTraitID: .text,
            validation: TraitID.traitConstructor.validation,
            deriveTraitValue: { _, _, _ in Text("<trait>") }
        )
    )
}
