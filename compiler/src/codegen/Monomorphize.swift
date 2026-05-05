import OrderedCollections

class MonomorphizeContext {
    let db: DB
    private var definitions: OrderedSet<IR.DefinitionKey> = []

    init(db: DB) { self.db = db }

    func getOrInsert(
        definition: Node,
        substitutions: Substitutions,
        bounds: OrderedDictionary<Node, IR.Instance>,
        isGeneric: Bool,
    ) throws -> IR.DefinitionKey {
        let key = IR.DefinitionKey(
            node: definition,
            substitutions: try convertSubstitutions(substitutions),
            bounds: bounds,
        )

        if !isGeneric { self.insert(monomorphized: key) }

        return key
    }

    func insert(monomorphized key: IR.DefinitionKey) { self.definitions.append(key) }

    private func convertSubstitutions(_ substitutions: Substitutions) throws -> OrderedDictionary<
        Node, IR.`Type`
    > {
        var converted: OrderedDictionary<Node, IR.`Type`> = [:]
        for (parameter, type) in substitutions {
            converted[parameter] = try self.db.irType(for: type)
        }

        return converted
    }

    public func monomorphizeDefinitions() throws -> some Sequence<(IR.DefinitionKey, IR.Definition)>
    {
        let cache = Cache()
        for key in self.definitions { try self.monomorphizeDefinition(key: key, cache: cache) }

        return cache.inner.lazy.map { key, slot in (key, slot.definition!) }
    }

    private class Cache {
        enum Slot {
            case empty
            case definition(IR.Definition)

            var definition: IR.Definition? {
                switch self {
                case .empty: return nil
                case .definition(let definition): return definition
                }
            }
        }

        var inner: OrderedDictionary<IR.DefinitionKey, Slot> = [:]
    }

    private func monomorphizeDefinition(key: IR.DefinitionKey, cache: Cache) throws {
        guard cache.inner[key] == nil else { return }

        cache.inner[key] = .empty

        guard let definition = self.db[key.node, Defined.self]?.definition else {
            throw CodegenError("no definition for \(key)")
        }

        let body: Node? =
            switch definition {
            case let definition as ConstantDefinition: definition.value
            case let definition as InstanceDefinition: definition.value
            default: nil
            }

        guard let body else { throw CodegenError("definition \(key) has no value") }

        let context = CodegenContext(db: self.db)
        try context.codegen(body)
        context.instruction(.return(value: body))

        var instructions = context.popInstructions()
        var types: OrderedDictionary<Node, IR.`Type`> = [:]
        for index in instructions.indices {
            instructions[index] = try instructions[index]
                .traverse { instruction in
                    try instruction.forEachNode { node in
                        types[node] = try self.db.irType(for: .node(node))
                            .substitute(from: key.substitutions)
                    }

                    var instruction = instruction
                    if case .value(let node, let value) = instruction {
                        switch value {
                        case .constant(let constantKey):
                            instruction = .value(
                                node: node,
                                value: .constant(
                                    try self.monomorphizeKey(
                                        constantKey,
                                        substitutions: key.substitutions,
                                        bounds: key.bounds,
                                        cache: cache,
                                    )
                                ),
                            )
                        case .bound(let bound):
                            guard case .definition(let resolvedKey) = key.bounds[bound] else {
                                throw CodegenError("bound \(bound) not resolved")
                            }

                            instruction = .value(
                                node: node,
                                value: .constant(
                                    try self.monomorphizeKey(
                                        resolvedKey,
                                        substitutions: key.substitutions,
                                        bounds: key.bounds,
                                        cache: cache,
                                    )
                                ),
                            )
                        default: break
                        }
                    }

                    return instruction
                }
        }

        cache.inner[key] = .definition(
            .init(type: types[body], instructions: instructions, types: types)
        )
    }

    private func monomorphizeKey(
        _ key: IR.DefinitionKey,
        substitutions: OrderedDictionary<Node, IR.`Type`>,
        bounds: OrderedDictionary<Node, IR.Instance>,
        cache: Cache,
    ) throws -> IR.DefinitionKey {
        var key = key

        for (parameter, type) in key.substitutions {
            key.substitutions[parameter] = try self.monomorphizeType(
                type,
                substitutions: substitutions,
            )
        }

        for (boundNode, instance) in key.bounds {
            key.bounds[boundNode] = try self.monomorphizeInstance(
                instance,
                substitutions: substitutions,
                bounds: bounds,
                cache: cache,
            )
        }

        try self.monomorphizeDefinition(key: key, cache: cache)

        return key
    }

    private func monomorphizeType(
        _ type: IR.`Type`,
        substitutions: OrderedDictionary<Node, IR.`Type`>,
    ) throws -> IR.`Type` {
        let root = type
        return try type.traverse { type in
            if case .parameter(let parameter) = type {
                guard let substitution = substitutions[parameter] else {
                    throw CodegenError(
                        "could not monomorphize \(root) with substitutions \(substitutions)"
                    )
                }

                return substitution
            }

            return type
        }
    }

    private func monomorphizeInstance(
        _ instance: IR.Instance,
        substitutions: OrderedDictionary<Node, IR.`Type`>,
        bounds: OrderedDictionary<Node, IR.Instance>,
        cache: Cache,
    ) throws -> IR.Instance {
        switch instance {
        case .bound(let bound):
            guard let instance = bounds[bound] else { throw CodegenError("no bound for \(bound)") }
            return instance
        case .definition(let key):
            return .definition(
                try self.monomorphizeKey(
                    key,
                    substitutions: substitutions,
                    bounds: bounds,
                    cache: cache,
                )
            )
        }
    }
}
