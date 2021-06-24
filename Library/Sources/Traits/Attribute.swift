import Wipple

extension Attribute.Trait: Primitive {}
extension Attribute: Primitive {}

public struct AttributeList: Primitive {
    public var items: [Value]
    public var location: SourceLocation?

    public init(_ items: [Value], location: SourceLocation? = nil) {
        self.items = items
        self.location = location
    }
}

internal func setupAttribute(_ env: Env, _ stack: Stack) throws {
    try env.setVariable("Attribute", to: Value(Trait(Attribute.Trait.self)), stack)

    try env.addRelation(text: "attribute", for: Attribute.Trait.self, stack)

    // Attribute == Function
    try env.addRelation(from: Attribute.Trait.self, to: Function.self, stack) { attribute in
        Function { value, env, stack in
            let value = try value.evaluate(env, stack)

            guard let attributeValue = try attribute.rawValue.pattern.match(value, env, stack) else {
                throw Exit.error("Cannot use this value with this attribute", stack)
            }

            return Value(Attribute(trait: attribute, value: attributeValue))
        }
    }

    try env.addRelation(text: "applied attribute", for: Attribute.self, stack)
}
