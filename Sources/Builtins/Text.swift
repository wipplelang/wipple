public struct Text {
    public var text: String
    public var location: SourceLocation?

    public init(
        _ text: String,
        location: SourceLocation? = nil
    ) {
        self.text = text
        self.location = location
    }
}

extension TraitID where T == Text {
    public static var text: Self {
        .builtin("Text")
    }
}

extension Trait where T == Text {
    public static func text(_ value: Text) -> Self {
        Trait(id: .text) { _, _ in value }
    }
}

extension Value {
    public func format(_ env: inout Environment, _ stack: ProgramStack) -> String {
        var stack = stack
        stack.disableRecording()

        return (try? self.getTraitIfPresent(.text, &env, stack))?.text ?? "<error retrieving text>"
    }
}
