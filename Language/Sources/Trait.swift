public enum Trait {
    case primitive(Any.Type)
    case runtime(ID, Pattern)

    public init(pattern: Pattern) {
        self = .runtime(ID(), pattern)
    }

    public init(_ type: Any.Type) {
        self = .primitive(type)
    }
}

public extension Trait {
    var id: ID {
        switch self {
        case .primitive(let type):
            return ID(type)
        case .runtime(let id, _):
            return id
        }
    }

    var pattern: Pattern {
        switch self {
        case .primitive:
            return Pattern(trait: self)
        case .runtime(_, let pattern):
            return pattern
        }
    }
}

extension Trait: Equatable {
    public static func == (lhs: Self, rhs: Self) -> Bool {
        switch (lhs, rhs) {
        case (.primitive(let lhs), .primitive(let rhs)):
            return lhs == rhs
        case (.runtime(let lhs, _), .runtime(let rhs, _)):
            return lhs == rhs
        default:
            return false
        }
    }
}

public extension Value {
    func `is`(_ trait: Trait, _ env: Env, _ stack: Stack) throws -> Bool {
        try self.getIfPresent(trait: trait, env, stack) != nil
    }

    func get(
        trait: Trait,
        or message: @autoclosure () -> String = "Value does not have this trait",
        _ env: Env,
        _ stack: Stack
    ) throws -> Value {
        guard let value = try self.getIfPresent(trait: trait, env, stack) else {
            throw Exit.error(message(), stack)
        }

        return value
    }

    func getIfPresent(trait: Trait, _ env: Env, _ stack: Stack) throws -> Value? {
        try self.directValue(for: trait) ?? self.derive(trait, env, stack)
    }
}

public extension Value {
    func `is`<T: Primitive>(_ type: T.Type, _ env: Env, _ stack: Stack) throws -> Bool {
        try self.is(Trait(type), env, stack)
    }

    func get<T: Primitive>(
        _ type: T.Type,
        or message: @autoclosure () -> String = "Value does not have this trait",
        _ env: Env,
        _ stack: Stack
    ) throws -> T {
        guard let value = try self.getIfPresent(type, env, stack) else {
            throw Exit.error(message(), stack)
        }

        return value
    }

    func getIfPresent<T: Primitive>(
        _ type: T.Type,
        _ env: Env,
        _ stack: Stack
    ) throws -> T? {
        try self.getIfPresent(trait: Trait(type), env, stack)?.primitiveValue as! T?
    }
}
