public struct Value {
    public var traits: Set<AnyTrait>
}

extension Value {
    public static var empty: Value {
        Value(traits: [])
    }
}
