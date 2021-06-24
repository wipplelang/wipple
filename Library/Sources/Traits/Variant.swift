import Wipple

public struct VariantSet: Primitive {
    public var id: ID
    public var variants: [String: VariantConstructor]

    public init(id: ID = ID(), variants: [String: VariantConstructor]) {
        self.id = id
        self.variants = variants
    }
}

public struct VariantConstructor {
    public var name: String
    public var valuePattern: Pattern?

    public init(name: String, valuePattern: Pattern? = nil) {
        self.name = name
        self.valuePattern = valuePattern
    }
}

public struct Variant: Primitive {
    public var id: ID
    public var name: String
    public var value: Value?

    public init(id: ID, name: String, value: Value? = nil) {
        self.id = id
        self.name = name
        self.value = value
    }

    public init(in set: VariantSet, name: String, value: Value? = nil) {
        self.init(id: set.id, name: name, value: value)
    }

    public func belongs(to set: VariantSet) -> Bool {
        self.id == set.id
    }
}

public extension Pattern {
    init(variantSet: VariantSet) {
        self.init { value, env, stack in
            guard let variant = try value.getIfPresent(Variant.self, env, stack) else {
                return nil
            }

            return variant.id == variantSet.id ? Value(variant) : nil
        }
    }

    init(variant variantToMatch: Variant) {
        self.init { value, env, stack in
            guard let variant = try value.getIfPresent(Variant.self, env, stack) else {
                return nil
            }

            return variant.id == variantToMatch.id && variant.name == variantToMatch.name
                ? variant.value ?? Value(variant)
                : nil
        }
    }
}

public extension Module {
    init(variantSet: VariantSet, _ stack: Stack) throws {
        let env = Env.global.child()

        for (name, constructor) in variantSet.variants {
            let value: Value
            if let pattern = constructor.valuePattern {
                value = Value(Function { value, env, stack in
                    let value = try value.evaluate(env, stack)

                    guard let matchedValue = try pattern.match(value, env, stack) else {
                        throw Exit.error("Invalid value", stack)
                    }

                    return Value(Variant(id: variantSet.id, name: constructor.name, value: matchedValue))
                })
            } else {
                value = Value(Variant(id: variantSet.id, name: constructor.name))
            }

            try env.setVariable(name, to: value, stack)
        }

        self.init(capturing: env)
    }
}

internal func setupVariant(_ env: Env, _ stack: Stack) throws {
    try env.setVariable(
        "variant",
        to: Value(Function { value, env, stack in
            var variants: [String: VariantConstructor] = [:]

            if let list = try value.getIfPresent(List.self, env, stack) {
                for item in list.rawValue {
                    let name = try item.get(Name.self, or: "Expected name", env, stack).rawValue
                    variants[name] = VariantConstructor(name: name)
                }
            } else if let block = try value.getIfPresent(Block.self, env, stack) {
                let variantEnv = env.child()

                variantEnv.handleAssignment = { left, right, env, stack in
                    guard left.count == 1 else {
                        throw Exit.error("Expected single value on left side of assignment", stack)
                    }

                    let name = try left[0].get(Name.self, or: "Expected name", env, stack).rawValue

                    let value = try Value(right).evaluate(env, stack)

                    let valuePattern =
                        value.isEmpty
                            ? nil
                            : try value.get(Pattern.self, or: "Expected pattern", env, stack)

                    variants[name] = VariantConstructor(name: name, valuePattern: valuePattern)
                }

                try block.reduce(location: value.location, variantEnv, stack)
            } else {
                throw Exit.error("Expected list or block of variants", stack)
            }

            return Value(VariantSet(variants: variants))
        }),
        stack
    )

    // Variant-Set == Pattern
    try env.addRelation(from: VariantSet.self, to: Pattern.self, stack) { variantSet in
        Pattern(variantSet: variantSet)
    }

    // Variant-Set == Module
    try env.addRelation(from: VariantSet.self, to: Module.self, stack) { variantSet, _, stack in
        try Module(variantSet: variantSet, stack)
    }

    // Variant-Set == Text
    try env.addRelation(text: "variant set", for: VariantSet.self, stack)

    // Variant == Pattern
    try env.addRelation(from: Variant.self, to: Pattern.self, stack) { variant in
        Pattern(variant: variant)
    }

    // Variant == Text
    try env.addRelation(from: Variant.self, to: Text.self, stack) { variant, _, stack in
        if let value = variant.value {
            return try Text(rawValue: "\(variant.name) \(value.format(env, stack))")
        } else {
            return Text(rawValue: variant.name)
        }
    }
}
