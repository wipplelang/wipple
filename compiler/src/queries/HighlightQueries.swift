extension Queries {
    public static var highlightType: Query<> {
        { db, node, body in
            guard !node.isHidden, db.contains(IsType.self, for: node),
                let syntax = db[node, Syntax.self]?.value
            else { return }

            if syntax is TypeParameterSyntax { return }

            body()
        }
    }

    public static var highlightTrait: Query<> {
        { db, node, body in
            let definitionNode = db[node, Resolved.self]?.definitions.first ?? node

            guard let definition = db[definitionNode, Defined.self]?.definition,
                definition is TraitDefinition
            else { return }

            body()
        }
    }

    public static var highlightFunction: Query<> {
        Queries.highlightTyped { if case .function = $0 { true } else { false } }
    }

    public static var highlightTypeParamter: Query<> {
        Queries.highlightTyped { if case .parameter(_) = $0 { true } else { false } }
    }

    private static func highlightTyped(withTag matchTag: @escaping (ConstructedType.Tag) -> Bool)
        -> Query<>
    {
        { db, node, body in
            guard db.contains(Resolved.self, for: node), let group = db[node, Typed.self]?.group,
                let tag = group.types.first?.tag, matchTag(tag)
            else { return }

            body()
        }
    }
}
