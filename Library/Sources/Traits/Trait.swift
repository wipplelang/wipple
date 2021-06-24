import Wipple

extension Trait: Primitive {}

public extension Value {
    static func checked(trait: Trait, value: Value, _ env: Env, _ stack: Stack) throws -> Self {
        guard let traitValue = try trait.pattern.match(value, env, stack) else {
            throw Exit.error("Cannot use this value to represent this trait", stack)
        }

        return Value(trait: trait, value: traitValue)
    }
}

internal func setupTrait(_ env: Env, _ stack: Stack) throws {
    try env.setVariable("Trait", to: Value(Trait(Trait.self)), stack)

    try env.addRelation(text: "trait", for: Trait.self, stack)

    // Trait == Function
    try env.addRelation(from: Trait.self, to: Function.self, stack) { trait in
        Function { value, env, stack in
            let value = try value.evaluate(env, stack)
            return try .checked(trait: trait, value: value, env, stack)
        }
    }

    // Trait == Pattern
    try env.addRelation(from: Trait.self, to: Pattern.self, stack) { trait in
        Pattern(trait: trait)
    }
}
