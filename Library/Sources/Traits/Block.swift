import Wipple

public struct Block: RawRepresentable, Primitive {
    public var rawValue: [List]

    public init(rawValue: [List]) {
        self.rawValue = rawValue
    }
}

public typealias EvaluateBlock = (Block, Value, Env, Stack) throws -> Value

public extension Env {
    private struct EvaluateBlockKey: EnvKey {
        typealias Value = EvaluateBlock

        static let visibility = EnvKeyVisibility<Value>.private
    }

    var evaluateBlock: EvaluateBlock {
        get {
            self[EvaluateBlockKey.self] ?? { block, value, env, stack in
                try block.reduce(location: value.location, env.child(), stack)
            }
        }
        set { self[EvaluateBlockKey.self] = newValue }
    }
}

public extension Block {
    @discardableResult func reduce(location: SourceLocation?, _ env: Env, _ stack: Stack) throws -> Value {
        var stack = stack
        stack.diagnostics.queue(location)

        var result = Value.empty

        for statement in self.rawValue {
            var stack = stack
            stack.diagnostics.queue(statement.rawValue.first!.location)

            // Evaluate each statement as a list
            result = try Value(statement).evaluate(env, stack)
        }

        return result
    }
}

internal func setupBlock(_ env: Env, _ stack: Stack) throws {
    try env.setVariable("Block", to: Value(Trait(Block.self)), stack)

    // Block == Evaluate
    try env.addRelation(from: Block.self, to: Evaluate.self, stack) { block, value in
        Evaluate { env, stack in
            try env.evaluateBlock(block, value, env, stack)
        }
    }

    // Block == Interpolate
    try env.addRelation(from: Block.self, to: Interpolate.self, stack) { block in
        Interpolate { direct, env, stack in
            let statements = try block.rawValue.map { statement in
                // Interpolate within each statement as a list
                try Value(statement)
                    .interpolate(direct: direct, env, stack)
                    .primitiveValue as! List
            }

            return Value(Block(rawValue: statements))
        }
    }

    // Block == Text
    try env.addRelation(text: "block", for: Block.self, stack)
}
