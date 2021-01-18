import Foundation

extension Trait.ID {
    static let display = Self(debugLabel: "Display")
}

public extension Trait {
    static func display(_ string: String) -> Trait {
        Trait(id: .display) { _ in
            string
        }
    }
}

public extension Value {
    func displayString(_ env: inout Environment) throws -> String {
        guard let displayTrait = try Trait.find(.display, ifPresentIn: self, &env) else {
            return "<value>"
        }

        return try displayTrait.value(&env) as! String
    }
}
