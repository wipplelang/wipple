import Foundation

public struct Block {
    public typealias Statement = [Value]

    public var statements: [Statement]
}

extension Trait.ID {
    static let block = Self(debugLabel: "Block")
}

public extension Trait {
    static func block(_ statements: [Block.Statement]) -> Trait {
        Trait(id: .block) { _ in
            Block(statements: statements)
        }
    }
}

public extension Value {
    func blockValue(_ env: inout Environment) throws -> Block {
        try Trait.find(.block, in: self, &env).value(&env) as! Block
    }
}

// MARK: - Initialize

public func initializeBlock(_ env: inout Environment) {
    // Block ::= Display
    // TODO: Implement in Wipple code
    env.addConformance(
        derivedTraitID: .display,
        validation: Trait.validation(for: .block),
        deriveTraitValue: { value, env in
            "<block>"
        }
    )

    // Block ::= Evaluate
    env.addConformance(
        derivedTraitID: .evaluate,
        validation: Trait.validation(for: .block),
        deriveTraitValue: { value, env -> EvaluateFunction in
            let block = value as! Block

            return { env in
                var result = Value()
                for statement in block.statements {
                    // Evaluate each statement as a list

                    let list = Value(location: statement.first?.location)
                        .trait(.list(statement))

                    result = try list.evaluate(&env)
                }

                return result
            }
        }
    )
}
