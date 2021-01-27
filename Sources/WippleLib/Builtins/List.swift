import Foundation

public typealias List = [Value]

extension Trait.ID {
    static let list = Self(debugLabel: "List")
}

public extension Trait {
    static func list(_ list: List) -> Trait {
        Trait(id: .list) { _ in
            list
        }
    }
}

public extension Value {
    func listValue(_ env: inout Environment) throws -> List {
        try Trait.value(.list, in: self, &env)
    }

    func listValueIfPresent(_ env: inout Environment) throws -> List? {
        try Trait.value(.list, ifPresentIn: self, &env)
    }
}

// MARK: - Initialize

public func initializeList(_ env: inout Environment) {
    // List ::= Evaluate
    env.addConformance(
        derivedTraitID: .evaluate,
        validation: Trait.validation(for: .list),
        deriveTraitValue: { value, env -> EvaluateFunction in
            let list = value as! List

            return { env in
                let operators = try findOperators(in: list, &env)
                if let parsed = try parseOperators(evaluating: list, operatorsInList: operators, &env) {
                    return parsed
                }

                guard var result = try list.first?.evaluate(&env) else {
                    // Empty list evaluates to itself
                    return Value.assoc(.list(list))
                }

                for value in list[1...] {
                    result = try result.call(with: value, &env)
                }

                return result
            }
        }
    )

    // (List and (each Text)) ::= Text
    // TODO: Write this in Wipple code
    env.addConformance(
        derivedTraitID: .text,
        validation: Trait.validation(for: .list) && { value, env in
            let list = value as! List

            var texts: [String] = []
            for value in list {
                guard let text = try value.textValueIfPresent(&env) else {
                    return .invalid
                }

                texts.append(text)
            }

            return .valid(newValue: texts)
        },
        deriveTraitValue: { value, env in
            let displays = value as! [String]

            return "(\(displays.joined(separator: " ")))"
        }
    )
}
