import OrderedCollections
import StringSimilarity

extension Queries {
    public static var definitions: Query<OrderedSet<Node>> {
        { db, node, body in
            guard let definitions = db[node, Resolved.self]?.definitions else { return }

            body(definitions)
        }
    }

    public static var references: Query<Node> {
        { db, node, body in
            db.forEachFact(Resolved.self) { other, resolved in
                if resolved.definitions.contains(node) { body(other) }
            }
        }
    }

    public static var unresolved: Query<String, (node: Node, name: String)?> {
        { db, node, body in
            guard let resolved = db[node, Resolved.self], resolved.definitions.isEmpty else {
                return
            }

            let maxDistance = 3

            var suggestion: (node: Node, name: String, distance: Int)?
            db.forEachFact(Defined.self) { node, defined in
                guard let name = defined.definition.name else { return }

                let distance = levenshteinDistance(resolved.name, name)
                guard distance < maxDistance else { return }

                if let suggestion, distance >= suggestion.distance { return }

                suggestion = (node, name, distance)
            }

            body(resolved.name, suggestion.map { ($0.node, $0.name) })
        }
    }

    public static var ambiguous: Query<Node, String, OrderedSet<Node>> {
        { db, node, body in
            guard let resolved = db[node, Resolved.self], resolved.definitions.count > 1 else {
                return
            }

            body(node, resolved.name, resolved.definitions)
        }
    }

    public struct Documentation {
        public let name: String?
        public let kind: String
        public let declaration: String
        public let comments: Queries.Comments
    }

    public static var documentation: Query<Documentation> {
        { db, node, body in
            guard let definition = db[node, Defined.self]?.definition,
                let source = db[definition.node, Syntax.self]?.value.span.source
            else { return }

            let kind: String? =
                switch definition {
                case is VariableDefinition: "variable"
                case is ConstantDefinition: "constant"
                case is TypeDefinition: "type"
                case is TraitDefinition: "trait"
                case is InstanceDefinition: "instance"
                default: nil
                }

            guard let kind else { return }

            Queries.comments(includeLinks: false)(db, definition.node) { comments in
                guard !comments.comments.isEmpty else { return }

                var declaration = String(source)

                Queries.references(db, definition.node) { reference in
                    guard
                        let definition = db[reference, Defined.self]?.definition
                            as? InstanceDefinition,
                        let source = db[definition.node, Syntax.self]?.value.span.source
                    else { return }

                    declaration += "\n"

                    if definition.attributes.default { declaration += "[default] " }
                    if definition.attributes.error { declaration += "[error] " }

                    declaration += source
                }

                body(
                    .init(
                        name: definition.name,
                        kind: kind,
                        declaration: declaration,
                        comments: comments,
                    )
                )
            }
        }
    }
}
