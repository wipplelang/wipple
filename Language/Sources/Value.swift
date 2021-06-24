public protocol Primitive {}

public struct Value {
    private var storage: Storage
    public internal(set) var attributes: [Attribute] = []

    public init(trait: Trait, value: Value) {
        self.storage = .runtime(trait, value)
    }

    public init(_ value: Primitive) {
        self.storage = .primitive(value)
    }

    private indirect enum Storage {
        case primitive(Any)
        case runtime(Trait, Value)
    }
}

public extension Value {
    var trait: Trait {
        switch self.storage {
        case .primitive(let value):
            return Trait(type(of: value))
        case .runtime(let trait, _):
            return trait
        }
    }

    func directValue(for trait: Trait) -> Value? {
        self.trait == trait ? self.storedValue : nil
    }

    func isPrimitive(ofType type: Any.Type) -> Bool {
        switch self.storage {
        case .primitive(let value):
            return Swift.type(of: value) == type
        case .runtime:
            return false
        }
    }

    var primitiveValue: Any? {
        switch self.storage {
        case .primitive(let value):
            return value
        case .runtime:
            return nil
        }
    }
}

internal extension Value {
    var storedValue: Value {
        switch self.storage {
        case .primitive:
            return self
        case .runtime(_, let value):
            return value
        }
    }
}
