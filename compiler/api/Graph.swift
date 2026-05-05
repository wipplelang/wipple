import Compiler
import JavaScriptKit
import OrderedCollections

@JS struct Graph {
    var groups: [GraphGroup]
    var nodes: [GraphNode]
    var edges: [GraphEdge]
}

@JS struct GraphGroup {
    var nodes: [String]
    var labels: [String]
    var conflict: Bool
}

@JS struct GraphNode {
    var id: String
    var span: GraphSpan
}

@JS struct GraphLocation { var index: Int }

@JS struct GraphSpan {
    var start: GraphLocation
    var end: GraphLocation
    var source: String
}

@JS struct GraphEdge {
    var from: String
    var to: String
    var label: String
}

extension CompileResult {
    @JS func graph() -> Graph {
        convertGraph(db.graph(including: OrderedSet(db.ownedNodes.lazy.filter { !$0.isHidden })))
    }

    func convertGraph(_ graph: SerializedGraph) -> Graph {
        Graph(
            groups: graph.groups.map { group in
                GraphGroup(nodes: group.nodes, labels: group.labels, conflict: group.conflict)
            },
            nodes: graph.nodes.map { node in
                GraphNode(
                    id: node.id,
                    span: .init(
                        start: .init(index: node.span.start.index),
                        end: .init(index: node.span.end.index),
                        source: String(node.span.source),
                    ),
                )
            },
            edges: graph.edges.map { edge in
                GraphEdge(from: edge.from, to: edge.to, label: edge.label)
            },
        )
    }
}
