import Foundation

public typealias Text = String

public extension TraitID where T == Text {
    static let text = Self(debugLabel: "Text")
}

public extension Trait {
    static func text(_ text: Text) -> Trait<Text> {
        .init(id: .text) { _ in
            text
        }
    }
}
