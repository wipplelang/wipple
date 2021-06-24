import Wipple

public extension Env {
    private struct LinkedValuesKey: EnvKey {
        typealias Value = [String: Wipple.Value]

        static let visibility = EnvKeyVisibility<Value>.private
    }

    var linkedValues: [String: Value] {
        get { self[LinkedValuesKey.self] ?? [:] }
        set { self[LinkedValuesKey.self] = newValue }
    }
}

// MARK: Booleans

public extension Variant {
    init(condition bool: Bool) {
        self = Env.global.linkedValues[bool ? "True" : "False"]!
            .primitiveValue as! Variant
    }

    var condition: Bool? {
        guard self.belongs(to: .condition) else { return nil }

        return self.name == "True"
    }
}

public extension VariantSet {
    static var condition: VariantSet {
        Env.global.linkedValues["Condition"]!
            .primitiveValue as! VariantSet
    }
}

// MARK: Maybes

public extension Variant {
    init(maybe value: Value?) {
        if let value = value {
            self = try! Env.global.linkedValues["Some"]!
                .call(with: value, .global, Stack())
                .primitiveValue as! Variant
        } else {
            self = Env.global.linkedValues["None"]!
                .primitiveValue as! Variant
        }
    }

    var maybe: Value?? {
        guard self.belongs(to: .maybe) else { return nil }

        return self.value
    }
}

public extension VariantSet {
    static var maybe: VariantSet {
        Env.global.linkedValues["Maybe"]!
            .primitiveValue as! VariantSet
    }
}
