import Wipple

public struct Text: RawRepresentable, Primitive {
    public var rawValue: String

    public init(rawValue: String) {
        self.rawValue = rawValue
    }
}

public extension Value {
    func format(_ env: Env, _ stack: Stack) throws -> String {
        var stack = stack
        stack.diagnostics.disableRecording()

        guard let text = try self.getIfPresent(Text.self, env, stack) else {
            return "<value>"
        }

        return text.rawValue
    }

    func formatWithFallback(_ env: Env, _ stack: Stack) -> String {
        do {
            return try self.format(env, stack)
        } catch {
            return "<error retrieving text>"
        }
    }
}

public extension Env {
    func addRelation<T: Primitive>(text: String, for type: T.Type, _ stack: Stack) throws {
        try self.addRelation(from: type, to: Text.self, stack) { _ in
            Text(rawValue: "<\(text)>")
        }
    }
}

internal func setupText(_ env: Env, _ stack: Stack) throws {
    // Text : trait
    try env.setVariable("Text", to: Value(Trait(Text.self)), stack)
}
