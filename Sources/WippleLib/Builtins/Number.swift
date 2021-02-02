import Foundation

public typealias Number = Decimal

extension TraitID where T == Number {
    static let number = Self(debugLabel: "Number")
}

public extension Trait {
    static func number(_ number: Number) -> Trait<Number> {
        .init(id: .number) { _ in
            number
        }
    }
}

// MARK: - Initialize

public func initializeNumber(_ env: inout Environment) {
    // Number ::= Text
    env.addConformance(
        derivedTraitID: .text,
        validation: TraitID.number.validation(),
        deriveTraitValue: { number, env in
            String(describing: number)
        }
    )
}
