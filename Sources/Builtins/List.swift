public struct List {
    public var items: [Value]
    public var location: SourceLocation?

    public init(
        _ items: [Value],
        location: SourceLocation? = nil
    ) {
        self.items = items
        self.location = location
    }
}

extension TraitID where T == List {
    public static var list: Self {
        .builtin("List")
    }
}

extension Trait where T == List {
    public static func list(_ value: List) -> Self {
        Trait(id: .list) { _, _ in value }
    }
}

internal func setupList(_ env: inout Environment) {
    // List ::= Evaluate
    env.addConformance(
        Conformance(
            derivedTraitID: .evaluate,
            validation: TraitID.list.validation,
            deriveTraitValue: { list, _, _ in
                { env, stack in
                    var stack = stack
                    if let location = list.location {
                        stack.queueLocation(location)
                    }

                    let operators = try list.findOperators(&env, stack)
                    if let parsed = try list.parseOperators(using: operators, &env, stack) {
                        return parsed
                    }

                    // Reduce the list as a series of function calls

                    guard var result = try list.items.first?.evaluate(&env, stack) else {
                        // Empty list evaluates to itself
                        return Value(.list(list))
                    }

                    for item in list.items[1...] {
                        result = try result.call(with: item, &env, stack)
                    }

                    return result
                }
            }
        )
    )

    // List ::= Macro-Expand
    env.addConformance(
        Conformance(
            derivedTraitID: .macroExpand,
            validation: TraitID.list.validation,
            deriveTraitValue: { list, _, _ in
                { parameter, replacement, env, stack in
                    let expandedItems = try list.items.map { item in
                        try item.macroExpand(
                            parameter: parameter,
                            replacement: replacement,
                            &env,
                            stack
                        )
                    }

                    return Value(.list(List(expandedItems)))
                }
            }
        )
    )

    // (List and (each Text)) ::= Text
    env.addConformance(
        Conformance(
            derivedTraitID: .text,
            validation: TraitID.list.validation & {
                list,
                env,
                stack -> ValidationResult<[String]> in
                var texts: [String] = []

                for item in list.items {
                    guard let text = try item.getTraitIfPresent(.text, &env, stack) else {
                        return .invalid
                    }

                    texts.append(text.text)
                }

                return .valid(texts)
            },
            deriveTraitValue: { texts, _, _ in
                Text("(\(texts.joined(separator: " ")))")
            }
        )
    )
}
