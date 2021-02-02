import Foundation

public typealias List = [Value]

extension TraitID where T == List {
    static let list = Self(debugLabel: "List")
}

public extension Trait {
    static func list(_ list: List) -> Trait<List> {
        .init(id: .list) { _ in
            list
        }
    }
}

// MARK: - Initialize

public func initializeList(_ env: inout Environment) {
    // List ::= Evaluate
    env.addConformance(
        derivedTraitID: .evaluate,
        validation: TraitID.list.validation(),
        deriveTraitValue: { list, env in
            return { env in
                let operators = try findOperators(in: list, &env)
                if let parsed = try parseOperators(evaluating: list, operatorsInList: operators, &env) {
                    return parsed
                }

                guard var result = try list.first?.evaluate(&env) else {
                    // Empty list evaluates to itself
                    return Value.new(.list(list))
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
        validation: TraitID.list.validation() && { list, env in
            var texts: [String] = []
            for value in list {
                guard let text = try value.traitIfPresent(.text, &env) else {
                    return .invalid
                }

                texts.append(text)
            }

            return .valid(newValue: texts)
        },
        deriveTraitValue: { (displays: [String], env) in
            "(\(displays.joined(separator: " ")))"
        }
    )
}
