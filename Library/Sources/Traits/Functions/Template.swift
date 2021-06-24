import Wipple

public struct Template: Primitive {
    public var capturedEnv: Env
    public var parameters: [(pattern: Pattern, name: String)]
    public var returnValue: Value

    internal var partiallyAppliedInputs: [Value] = []

    public init(capturing capturedEnv: Env, parameters: [(pattern: Pattern, name: String)], returning returnValue: Value) {
        self.capturedEnv = capturedEnv
        self.parameters = parameters
        self.returnValue = returnValue
    }
}

private extension Template {
    var isFullyApplied: Bool {
        self.parameters.count == self.partiallyAppliedInputs.count
    }

    func expand(_ env: Env, _ stack: Stack) throws -> Value {
        precondition(self.isFullyApplied)

        let templateEnv = self.capturedEnv.child()

        for (parameter, value) in zip(self.parameters, self.partiallyAppliedInputs) {
            guard let matchedValue = try parameter.pattern.match(value, env, stack) else {
                throw Exit.error("Cannot use this value as input to this template", stack)
            }

            try templateEnv.setVariable(parameter.name, to: matchedValue, stack)
        }

        return try self.returnValue.interpolate(direct: true, templateEnv, stack)
    }
}

internal func setupTemplate(_ env: Env, _ stack: Stack) throws {
    // Template == Function
    try env.addRelation(from: Template.self, to: Function.self, stack) { template in
        Function { value, env, stack in
            var template = template
            template.partiallyAppliedInputs.append(value)

            if template.isFullyApplied {
                return try template.expand(env, stack).evaluate(env, stack)
            } else {
                return Value(template)
            }
        }
    }

    // Template == Text
    try env.addRelation(text: "template", for: Template.self, stack)

    // TODO: Use Apply trait instead
    try env.setVariable(
        "expand",
        to: Value(Function { value, env, stack in
            let items = try value
                .evaluate(env, stack)
                .get(List.self, or: "Expected list", env, stack)
                .rawValue

            return Value(Function { template, env, stack in
                var template = try template
                    .evaluate(env, stack)
                    .get(Template.self, or: "Expected template", env, stack)

                let expectedCount = template.parameters.count - template.partiallyAppliedInputs.count

                guard items.count == expectedCount else {
                    throw Exit.error("Expected \(expectedCount) parameters, found \(items.count)", stack)
                }

                template.partiallyAppliedInputs.append(contentsOf: items)

                return try template.expand(env, stack)
            })
        }),
        stack
    )
}
