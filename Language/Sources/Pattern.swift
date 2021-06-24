public struct Pattern {
    public typealias Match = (Value, Env, Stack) throws -> Value?

    public let match: Match

    public init(match: @escaping Match) {
        self.match = match
    }
}

public extension Pattern {
    static let any = Pattern { value, _, _ in value }

    init<T: Primitive>(_ type: T.Type) {
        self.init(trait: Trait(type))
    }

    init(trait: Trait) {
        self.init { value, env, stack in
            try value.getIfPresent(trait: trait, env, stack)
        }
    }

    init(is pattern: Pattern) {
        self.init { value, env, stack in
            _ = try pattern.match(value, env, stack)
            return value
        }
    }
}
