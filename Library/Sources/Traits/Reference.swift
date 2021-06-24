import Wipple

public class Reference: Primitive {
    public var value: Value

    public init(_ value: Value) {
        self.value = value
    }
}

internal func setupReference(_ env: Env, _ stack: Stack) throws {
    try env.setVariable("Reference", to: Value(Trait(Reference.self)), stack)

    try env.setVariable(
        "ref",
        to: Value(Function { value, env, stack in
            Value(Reference(try value.evaluate(env, stack)))
        }),
        stack
    )

    try env.setVariable(
        "get",
        to: Value(Function { value, env, stack in
            try value
                .evaluate(env, stack)
                .get(Reference.self, or: "Expected reference", env, stack)
                .value
        }),
        stack
    )

    try env.setVariable(
        "set!",
        to: Value(Function { value, env, stack in
            let valueToStore = try value.evaluate(env, stack)

            return Value(Function { value, env, stack in
                let reference = try value
                    .evaluate(env, stack)
                    .get(Reference.self, or: "Expected reference", env, stack)

                reference.value = valueToStore

                return .empty
            })
        }),
        stack
    )

    // Empty == Text
    try env.addRelation(text: "reference", for: Reference.self, stack)
}
