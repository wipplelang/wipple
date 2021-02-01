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
        try Trait.value(.block, in: self, &env)
    }

    func blockValueIfPresent(_ env: inout Environment) throws -> Block? {
        try Trait.value(.block, ifPresentIn: self, &env)
    }
}

// MARK: - Initialize

public func initializeBlock(_ env: inout Environment) {
    // Block ::= Text
    // TODO: Implement in Wipple code
    env.addConformance(
        derivedTraitID: .text,
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
                        .add(.list(statement))

                    result = try list.evaluate(&env)

                    print(try list.textValueWithDefault(&env), "==>", try result.textValueWithDefault(&env))
                }

                return result
            }
        }
    )
}
