import Wipple

public struct List: RawRepresentable, Primitive {
    public var rawValue: [Value]

    public init(rawValue: [Value]) {
        self.rawValue = rawValue
    }
}

public extension Value {
    init<Items: Collection>(_ list: Items) where Items.Element == Value {
        if list.count == 1 {
            self = list.first!
        } else {
            self = Value(List(rawValue: Array(list)))
        }
    }
}

internal func setupList(_ env: Env, _ stack: Stack) throws {
    try env.setVariable("List", to: Value(Trait(List.self)), stack)

    // List == Evaluate
    try env.addRelation(from: List.self, to: Evaluate.self, stack) { list, value in
        Evaluate { env, stack in
            var stack = stack
            stack.diagnostics.queue(value.location)

            // Parse operators contained within the list
            if let parsedValue = try list.parseOperators(env, stack) {
                return parsedValue
            }

            // Reduce the list as a series of function calls

            guard var result = try list.rawValue.first?.evaluate(env, stack) else {
                // Empty list evaluates to itself
                return Value(list)
            }

            for item in list.rawValue.dropFirst() {
                result = try result.call(with: item, env, stack)
            }

            return result
        }
    }

    // List == Interpolate
    try env.addRelation(from: List.self, to: Interpolate.self, stack) { list in
        Interpolate { direct, env, stack in
            let items = try list.rawValue.map { item in
                try item.interpolate(direct: direct, env, stack)
            }

            return Value(List(rawValue: items))
        }
    }

    // List == Text
    try env.addRelation(from: List.self, to: Text.self, stack) { list, _, stack in
        let items = try list.rawValue
            .map { try $0.format(env, stack) }
            .joined(separator: " ")

        return Text(rawValue: "(\(items))")
    }
}
