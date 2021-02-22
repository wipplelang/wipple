public struct Number {
    // TODO: Represent as arbitrary-precision rational
    public var number: Double
    public var location: SourceLocation?

    public init(
        _ number: Double,
        location: SourceLocation? = nil
    ) {
        self.number = number
        self.location = location
    }
}

extension TraitID where T == Number {
    public static var number: Self {
        .builtin("Number")
    }
}

extension Trait where T == Number {
    public static func number(_ value: Number) -> Self {
        Trait(id: .number) { _, _ in value }
    }
}

internal func setupNumber(_ env: inout Environment) {
    // Number ::= Text
    env.addConformance(
        Conformance(
            derivedTraitID: .text,
            validation: TraitID.number.validation,
            deriveTraitValue: { number, _, _ in
                Text("\(number.number)")
            }
        )
    )
}
