public struct Stack {
    private var items: [ID: Any]

    public init() {
        self.items = [:]
    }
}

public extension Stack {
    subscript<Key: StackKey>(key: Key.Type) -> Key.Value? {
        get { self.items[ID(key)] as! Key.Value? }
        set { self.items[ID(key)] = newValue }
    }

    subscript(key: DynamicStackKey) -> Any? {
        get { self.items[key.id] }
        set { self.items[key.id] = newValue }
    }
}

public protocol StackKey {
    associatedtype Value
}

public struct DynamicStackKey {
    public typealias Value = Any

    fileprivate let id: ID

    public init() {
        self.id = ID()
    }
}
