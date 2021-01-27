import Foundation

extension Trait.ID {
    static let validationContainer = Self(debugLabel: "Validation")
}

public extension Trait {
    static func validationContainer(_ validation: @escaping Validation) -> Trait {
        Trait(id: .validationContainer) { _ in
            validation
        }
    }
}

public extension Value {
    func validationContainerValue(_ env: inout Environment) throws -> Validation {
        try Trait.value(.validationContainer, in: self, &env)
    }

    func validationContainerValueIfPresent(_ env: inout Environment) throws -> Validation? {
        try Trait.value(.validationContainer, ifPresentIn: self, &env)
    }
}
