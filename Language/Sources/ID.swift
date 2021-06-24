public enum ID {
    case counter(UInt)
    case type(Any.Type)
}

public extension ID {
    private static var counter: UInt = 0

    init() {
        self = .counter(Self.counter)
        Self.counter += 1
    }

    init(_ type: Any.Type) {
        self = .type(type)
    }
}

extension ID: Hashable {
    public static func == (lhs: Self, rhs: Self) -> Bool {
        lhs.hashValue == rhs.hashValue
    }

    public func hash(into hasher: inout Hasher) {
        switch self {
        case .counter(let counter):
            hasher.combine(counter)
        case .type(let type):
            hasher.combine(ObjectIdentifier(type))
        }
    }
}
