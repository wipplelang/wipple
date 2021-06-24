import Wipple

public struct Empty: Primitive {}

public extension Value {
    static let empty = Value(Empty())

    var isEmpty: Bool {
        self.trait == Trait(Empty.self)
    }
}

internal func setupEmpty(_ env: Env, _ stack: Stack) throws {
    try env.setVariable("Empty", to: Value(Trait(Empty.self)), stack)

    // Allow the use of '_' as a catch-all pattern that returns its input
    try env.addRelation(from: Empty.self, to: Pattern.self, stack) { _ in .any }

    try env.addRelation(text: "empty", for: Empty.self, stack)
}
