import Foundation

extension TraitID where T == AnyValidation {
    static let validationContainer = Self(debugLabel: "Validation")
}

public extension Trait {
    static func validationContainer<A, B>(_ validation: @escaping Validation<A, B>) -> Trait<AnyValidation> {
        .init(id: .validationContainer) { _ in
            any(validation)
        }
    }
}
