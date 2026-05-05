public struct Instantiated: Fact {
    public let definition: Node
    public let from: Node
    public let sourceNode: Node

    public func render(into context: RenderContext) {}
}

struct InstantiateContext {
    let db: DB
    let definition: Node
    let sourceNode: Node
    let replacements: Replacements
    let substitutions: Substitutions

    func instantiate(_ node: Node) -> Node {
        if let replacement = self.replacements[node] { return replacement }

        let replacement = self.db.node(isHidden: true)
        self.replacements[node] = replacement

        self.db[replacement, Typed.self] = Typed()

        self.db[replacement, Instantiated.self] = Instantiated(
            definition: self.definition,
            from: node,
            sourceNode: self.sourceNode,
        )

        return replacement
    }

    func instantiate(_ type: Type) -> Type {
        type.traverse { type in
            switch type {
            case .node(let node): return .node(self.instantiate(node))
            case .constructed(let type):
                guard case .parameter(let parameter) = type.tag else { return .constructed(type) }

                if let substitution = self.substitutions[parameter] { return substitution }

                let substitution = self.db.node(isHidden: true)
                self.substitutions[parameter] = .node(substitution)

                self.db[substitution, Typed.self] = Typed()

                self.db[substitution, Instantiated.self] = Instantiated(
                    definition: self.definition,
                    from: parameter,
                    sourceNode: self.sourceNode,
                )

                return .node(substitution)
            }
        }
    }

    func instantiate(_ substitutions: Substitutions) -> Substitutions {
        let newSubstitutions = Substitutions()
        for (parameter, substitution) in substitutions {
            newSubstitutions[parameter] = self.instantiate(substitution)
        }

        return newSubstitutions
    }
}
