import Foundation

public struct TraitConstructor<A, B> {
    public var id: TraitID<B>
    public var validation: Validation<A, B>
}

public struct AnyTraitConstructor {
    public var id: AnyTraitID
    public var validation: AnyValidation
    
    public init<A, B>(_ traitConstructor: TraitConstructor<A, B>) {
        self.id = .init(erasing: traitConstructor.id)
        self.validation = any(traitConstructor.validation)
    }
}

extension TraitID where T == AnyTraitConstructor {
    static let traitConstructor = Self(debugLabel: "Trait")
}

extension Trait {
    static func traitConstructor<A, B>(_ id: TraitID<B>, validation: @escaping Validation<A, B>) -> Trait<AnyTraitConstructor> {
        .init(id: .traitConstructor) { _ in
            AnyTraitConstructor(TraitConstructor(id: id, validation: validation))
        }
    }
}

// MARK: - Initialize

func initializeTraitConstructor(_ env: inout Environment) {
    // Trait ::= Text
    env.addConformance(
        derivedTraitID: .text,
        validation: TraitID.traitConstructor.validation(),
        deriveTraitValue: { value, env in
            "<trait>"
        }
    )
}
