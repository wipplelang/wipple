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
        try Trait.find(.validationContainer, in: self, &env).value(&env) as! Validation
    }
}
