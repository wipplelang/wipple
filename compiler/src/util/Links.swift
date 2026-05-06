import OrderedCollections

public struct Link {
    public let node: Node
    public let related: OrderedSet<Node>
    public let types: [ConstructedType]
}

func getLinks(db: DB, definitionNode: Node, sourceNode: Node) -> [String: Link] {
    var links: [String: Link] = [:]

    guard let definition = db[definitionNode, Defined.self]?.definition else { return links }

    var nodes: [(String, Node)] = []

    if let name = definition.name {
        links[name] = .init(node: definitionNode, related: [], types: [])
        nodes.append((name, definitionNode))
    }

    if let parameters = db[definitionNode, TypeParameters.self]?.typeParameters {
        for parameter in parameters {
            guard let name = db[parameter, Defined.self]?.definition.name else { continue }

            nodes.append((name, parameter))
        }
    }

    for (name, nameNode) in nodes {
        var instantiatedNode: Node?
        db.forEachFact(Instantiated.self) { node, instantiated in
            if instantiated.from == nameNode && instantiated.sourceNode == sourceNode {
                instantiatedNode = node
                return true
            }

            return false
        }

        guard let instantiatedNode, let group = db[instantiatedNode, Typed.self]?.group else {
            continue
        }

        // Prefer using the first node from the source that was grouped with
        // `instantiated`
        let node =
            db[instantiatedNode, GroupedWith.self]?.nodes
            .first { !db.contains(Instantiated.self, for: $0) } ?? group.nodes[0]

        links[name] = .init(node: node, related: OrderedSet(group.nodes), types: group.types)
    }

    return links
}
