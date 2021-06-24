public struct Attribute {
    public let trait: Trait
    public let value: Value

    public init(trait: Trait, value: Value) {
        self.trait = trait
        self.value = value
    }

    public struct Trait: RawRepresentable {
        public var rawValue: Wipple.Trait

        public init(rawValue: Wipple.Trait) {
            self.rawValue = rawValue
        }
    }
}

public extension Value {
    mutating func addAttribute(_ attribute: Attribute) {
        self.attributes.append(attribute)
    }

    mutating func addAttributes(_ attributes: [Attribute]) {
        self.attributes.append(contentsOf: attributes)
    }

    mutating func addAttribute<T: Primitive>(value: T) {
        self.addAttribute(Attribute(trait: .init(rawValue: Trait(T.self)), value: Value(value)))
    }
}

public extension Value {
    func instances(of attribute: Attribute.Trait) -> [Value] {
        self.attributes.compactMap { $0.trait == attribute ? $0.value : nil }
    }

    func instances<T: Primitive>(of type: T.Type) -> [T] {
        self.attributes.compactMap {
            $0.trait == .init(rawValue: Trait(type))
                ? ($0.value.primitiveValue as! T)
                : nil
        }
    }
}
