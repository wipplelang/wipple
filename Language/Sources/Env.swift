public final class Env {
    private var items: [AnyEnvKey: Any]
    public var parent: Env?

    private init() {
        self.items = [:]
        self.parent = nil
    }

    public func child() -> Env {
        self.child { _ in }
    }

    public func child(prepare: (Env) throws -> Void) rethrows -> Env {
        let env = Env()
        env.parent = self
        try prepare(env)
        return env
    }
}

public extension Env {
    static let global = Env()

    var isGlobal: Bool {
        self === Env.global
    }
}

public extension Env {
    subscript<Key: EnvKey>(key: Key.Type) -> Key.Value? {
        get { self.items[AnyEnvKey(key)] as! Key.Value? }
        set { self.items[AnyEnvKey(key)] = newValue }
    }

    subscript(key: DynamicEnvKey) -> Any? {
        get { self.items[AnyEnvKey(key)] }
        set { self.items[AnyEnvKey(key)] = newValue }
    }

    func clear() {
        self.items.removeAll()
    }
}

public protocol EnvKey {
    associatedtype Value

    static var visibility: EnvKeyVisibility<Value> { get }
}

public struct DynamicEnvKey {
    public typealias Value = Any

    fileprivate let id: ID

    public let visibility: EnvKeyVisibility<Any>

    public init(visibility: EnvKeyVisibility<Any>) {
        self.id = ID()
        self.visibility = visibility
    }
}

private struct AnyEnvKey: Hashable {
    let id: ID
    let visibility: EnvKeyVisibility<Any>

    init<Key: EnvKey>(_ key: Key.Type) {
        self.id = ID(key)

        switch key.visibility {
        case .private:
            self.visibility = .private
        case .public(let merge):
            self.visibility = .public { erasedOld, erasedNew, env, stack in
                var old = erasedOld as! Key.Value
                let new = erasedNew as! Key.Value

                try merge(&old, new, env, stack)

                erasedOld = old
            }
        }
    }

    init(_ key: DynamicEnvKey) {
        self.id = key.id
        self.visibility = key.visibility
    }

    static func == (lhs: Self, rhs: Self) -> Bool {
        lhs.id == rhs.id
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(self.id)
    }
}

public enum EnvKeyVisibility<Value> {
    /// The value cannot be `use`d by another environment.
    case `private`

    /// The value can be `use`d by another environment. If the value already
    /// exists in the other environment, the provided `UseMergeFn` will be
    /// called to handle merging them. If the value doesn't exist, it will just
    /// be copied over.
    case `public`(merge: Merge)

    public typealias Merge = (inout Value, Value, Env, Stack) throws -> Void
}

public extension Env {
    func use(_ other: Env, _ stack: Stack) throws {
        try self.use(other, self, stack)
    }

    func use(_ other: Env, _ env: Env, _ stack: Stack) throws {
        for (key, value) in other.items {
            switch key.visibility {
            case .public(let merge):
                if self.items.keys.contains(key) {
                    try merge(&self.items[key]!, value, env, stack)
                } else {
                    self.items[key] = value
                }
            case .private:
                break
            }
        }
    }
}
