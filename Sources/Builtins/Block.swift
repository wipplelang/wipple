public struct Block {
    public var statements: [List]
    public var location: SourceLocation?

    public init(
        statements: [List],
        location: SourceLocation? = nil
    ) {
        self.statements = statements
        self.location = location
    }
}

extension TraitID where T == Block {
    public static let block = TraitID(debugLabel: "Block")
}

extension Trait where T == Block {
    public static func block(_ value: Block) -> Self {
        Trait(id: .block) { _, _ in value }
    }
}

internal func setupBlock(_ env: inout Environment) {
    // Block ::= Text
    env.addConformance(
        Conformance(
            derivedTraitID: .text,
            validation: TraitID.block.validation,
            deriveTraitValue: { _, _, _ in Text("<block>") }
        )
    )

    // Block ::= Evaluate
    env.addConformance(
        Conformance(
            derivedTraitID: .evaluate,
            validation: TraitID.block.validation,
            deriveTraitValue: { block, _, _ in
                { env, stack in
                    var stack = stack
                    if let location = block.location {
                        stack.queueLocation(location)
                    }

                    var result = Value.empty

                    for statement in block.statements {
                        var stack = stack
                        if let location = statement.location {
                            stack.queueLocation(location)
                        }

                        // Evaluate each statement as a list
                        let list = Value(.list(statement))
                        result = try list.evaluate(&env, stack)
                    }

                    return result
                }
            }
        )
    )

    // Block ::= Macro-Expand
    env.addConformance(
        Conformance(
            derivedTraitID: .macroExpand,
            validation: TraitID.block.validation,
            deriveTraitValue: { block, _, _ in
                { parameter, replacement, env, stack in
                    var stack = stack
                    if let location = block.location {
                        stack.queueLocation(location)
                    }

                    let statements: [List] = try block.statements.map { statement in
                        var stack = stack
                        if let location = statement.location {
                            stack.queueLocation(location)
                        }

                        // Expand each statement as a list
                        let list = Value(.list(statement))
                        let expanded =
                            try list.macroExpand(
                                parameter: parameter,
                                replacement: replacement,
                                &env,
                                stack
                            )
                            .trait(.list, &env, stack)

                        return expanded
                    }

                    return Value(.block(Block(statements: statements)))
                }
            }
        )
    )
}
