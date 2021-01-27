import Foundation

public typealias Text = String

extension Trait.ID {
    static let text = Self(debugLabel: "Text")
}

public extension Trait {
    static func text(_ text: Text) -> Trait {
        Trait(id: .text) { _ in
            text
        }
    }
}

public extension Value {
    func textValue(_ env: inout Environment) throws -> Text {
        try Trait.value(.text, in: self, &env)
    }

    func textValueIfPresent(_ env: inout Environment) throws -> Text? {
        try Trait.value(.text, ifPresentIn: self, &env)
    }

    func textValueWithDefault(_ env: inout Environment) throws -> Text {
        try self.textValueIfPresent(&env) ?? "<value>"
    }
}
