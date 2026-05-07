import Compiler
import JavaScriptKit
import OrderedCollections

@JS struct Diagnostic {
    var locations: [DiagnosticLocation]
    var lines: [DiagnosticLine]
    var groups: Int
    var message: String
    var graph: Graph?
}

@JS struct DiagnosticLocation: Equatable {
    var start: Int
    var end: Int
    var group: Int
}

@JS struct DiagnosticLine {
    var source: String
    var locations: [DiagnosticLocation]
}

extension CompileResult {
    @JS func groups() -> [[DiagnosticLocation]] {
        let filter = defaultFilter(db: self.db)

        var groups: OrderedDictionary<[Node], (index: Int, locations: [DiagnosticLocation])> = [:]
        for node in self.db.ownedNodes {
            guard filter(node), let span = self.db[node, Syntax.self]?.value.span,
                span.path == self.path, let group = self.db[node, Typed.self]?.group
            else { continue }

            let groupIndex =
                groups.first(where: { $0.key == group.nodes })?.value.index ?? groups.count

            let location = DiagnosticLocation(
                start: span.start.index,
                end: span.end.index,
                group: groupIndex,
            )

            groups[group.nodes, default: (groupIndex, [])].locations.append(location)
        }

        return Array(groups.values.map(\.1))
    }

    @JS func diagnostics() -> [Diagnostic]? {
        let filter = defaultFilter(db: self.db)
        let items = collectFeedback(db: self.db, filter: { filter($0.location.primary) })

        guard !items.isEmpty else { return nil }

        return items.map { item in
            let (groups, locations, lines) = collectLines(location: item.location)

            var mask = OrderedSet([item.location.0] + item.location.1)
            let (message, nodes) = item.display(db) { segment in
                var link: (String, Node)?
                switch segment {
                case .node(let node): link = (segment.plainText(db: self.db), node)
                case .link(let label, let node): link = (label, node)
                default: break
                }

                guard let (label, node) = link else { return segment.markdown(db: self.db) }

                mask.append(node)

                if let group = self.db[node, Typed.self]?.group,
                    let index = groups.firstIndex(where: { $0.nodes == group.nodes })
                {
                    return "<code data-group=\"\(index)\">\(label)</code>"
                } else {
                    return "<code>\(label)</code>"
                }
            }

            mask.append(contentsOf: nodes)

            let graph = item.showGraph ? convertGraph(db.graph(including: mask)) : nil

            return Diagnostic(
                locations: locations,
                lines: lines,
                groups: groups.count,
                message: message,
                graph: graph,
            )
        }
    }

    private func collectLines(location: (Node, OrderedSet<Node>)) -> (
        groups: [Group], locations: [DiagnosticLocation], lines: [DiagnosticLine]
    ) {
        class LineInfo {
            var start: Int
            var end: Int
            var offset: Int
            var nodes: OrderedSet<Node>

            init(start: Int, end: Int, offset: Int, nodes: OrderedSet<Node>) {
                self.start = start
                self.end = end
                self.offset = offset
                self.nodes = nodes
            }
        }

        var groups: [Group] = []
        var locations: [DiagnosticLocation] = []
        var lines: [(line: DiagnosticLine, info: LineInfo)] = []
        for node in [location.0] + location.1 {
            guard let span = self.db[node, Syntax.self]?.value.span else { continue }

            // Hide non-instantiated, hidden nodes
            guard !node.isHidden || self.db.contains(Instantiated.self, for: node) else { continue }

            var groupIndex = -1
            if let group = self.db[node, Typed.self]?.group {
                if let index = groups.firstIndex(where: { $0.nodes == group.nodes }) {
                    groupIndex = index
                } else {
                    groupIndex = groups.count
                    groups.append(group)
                }
            }

            if span.path == self.path {
                let location = DiagnosticLocation(
                    start: span.start.index,
                    end: span.end.index,
                    group: groupIndex,
                )

                if !locations.contains(location) { locations.append(location) }

                continue
            }

            let convertOffset = { (offset: Int) in
                DiagnosticLocation(
                    start: span.start.column - 1 + offset,
                    end: span.end.column - 1 + offset,
                    group: groupIndex,
                )
            }

            if let index = lines.firstIndex(where: { line in
                span.start.line >= line.info.start && span.end.line <= line.info.end
            }) {
                lines[index].info.nodes.append(node)

                let location = convertOffset(lines[index].info.offset)
                if !lines[index].line.locations.contains(location) {
                    lines[index].line.locations.append(location)
                }

                continue
            }

            guard
                let source = db.ownedNodes.lazy
                    .compactMap({ fileNode -> Substring? in
                        guard let syntax = self.db[fileNode, Syntax.self],
                            syntax.value is FileSyntax
                        else { return nil }

                        let fileSpan = syntax.value.span

                        guard fileSpan.path == span.path else { return nil }

                        return fileSpan.source
                    })
                    .first
            else { continue }

            // Seek to the `span` location
            let lineRange = source.lineRange(
                for: source.index(
                    source.startIndex,
                    offsetBy: span.start.index,
                )...source.index(source.startIndex, offsetBy: span.end.index)
            )

            let offset = source.distance(from: source.startIndex, to: lineRange.lowerBound)

            lines.append(
                (
                    DiagnosticLine(source: String(source), locations: [convertOffset(offset)]),
                    LineInfo(
                        start: span.start.line,
                        end: span.end.line,
                        offset: offset,
                        nodes: [node],
                    ),
                )
            )
        }

        // Delete lines that have no groups
        lines.removeAll(where: { $0.line.locations.allSatisfy { $0.group == -1 } })

        return (groups, locations, lines.map(\.line))
    }
}
