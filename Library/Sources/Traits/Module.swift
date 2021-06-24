import Wipple

public struct Module: Primitive {
    public var capturedEnv: Env

    public init(capturing env: Env) {
        self.capturedEnv = env
    }
}

public extension Module {
    static var evaluateBlock: EvaluateBlock {
        { block, value, env, stack in
            let env = env.child()
            env.evaluateBlock = Module.evaluateBlock

            try block.reduce(location: value.location, env, stack)

            return Value(Module(capturing: env))
        }
    }
}

internal func setupModule(_ env: Env, _ stack: Stack) throws {
    try env.setVariable("Module", to: Value(Trait(Module.self)), stack)

    // Module == Text
    try env.addRelation(text: "module", for: Module.self, stack)

    // Module == Function
    try env.addRelation(from: Module.self, to: Function.self, stack) { module in
        Function { value, env, stack in
            try value
                .get(Name.self, or: "Expected name", env, stack)
                .resolve(module.capturedEnv, stack)
        }
    }

    // Module == Pattern
    try env.addRelation(from: Module.self, to: Pattern.self, stack) { module, _, stack in
        let variables = module.capturedEnv.variables

        let fields = try variables.mapValues { variable in
            try variable
                .getValue(module.capturedEnv, stack)
                .get(Pattern.self, or: "Expected pattern", module.capturedEnv, stack)
        }

        return Pattern { value, env, stack in
            let module = try value.get(Module.self, or: "Expected module", env, stack)

            let matchedEnv = env.child()

            for (name, variable) in module.capturedEnv.variables {
                let value = try variable.getValue(env, stack)

                guard let pattern = fields[name],
                      let matchedValue = try pattern.match(value, env, stack)
                else {
                    return nil
                }

                try matchedEnv.setVariable(name, to: matchedValue, stack)
            }

            return Value(Module(capturing: matchedEnv))
        }
    }

    try env.setVariable(
        "New",
        to: Value(Function { value, env, stack in
            let trait = try value
                .evaluate(env, stack)
                .get(Trait.self, or: "Expected trait", env, stack)

            return Value(Pattern { value, env, stack in
                let module: Value = try Module.new(value, env, stack)
                return try .checked(trait: trait, value: module, env, stack)
            })
        }),
        stack
    )
}
