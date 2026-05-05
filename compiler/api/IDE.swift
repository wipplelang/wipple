import Compiler
import JavaScriptKit

@JS struct IDEDiagnostic {
    let range: IDERange
    let message: String
}

@JS struct IDERange: Hashable {
    let start: IDEPosition
    let end: IDEPosition
}

@JS struct IDEPosition: Hashable {
    let line: Int
    let column: Int
}

@JS struct IDESemanticToken {
    let range: IDERange
    let type: String
}

@JS struct IDEHover {
    let contents: [IDEHoverItem]
    let range: IDERange
}

@JS struct IDEHoverItem {
    let value: String
    let isCode: Bool
}

@JS class IDE {
    let result: CompileResult

    @JS init(_ result: CompileResult) { self.result = result }

    @JS func diagnostics() -> [IDEDiagnostic] {
        let filter = defaultFilter(db: self.result.db)
        let items = collectFeedback(db: self.result.db, filter: { filter($0.location.primary) })

        return items.compactMap { item in
            guard let span = self.result.db[item.location.primary, Syntax.self]?.value.span else {
                return nil
            }

            let (message, _) = item.display(self.result.db) {
                $0.markdown(db: self.result.db, showSpan: false)
            }

            return IDEDiagnostic(
                range: IDERange(
                    start: IDEPosition(line: span.start.line, column: span.start.column),
                    end: IDEPosition(line: span.end.line, column: span.end.column),
                ),
                message: message,
            )
        }
    }

    @JS func semanticTokens() -> [IDESemanticToken] {
        var tokens: [IDERange: String] = [:]
        for node in self.result.db.ownedNodes {
            guard let span = self.result.db[node, Syntax.self]?.value.span else { continue }

            let range = IDERange(
                start: IDEPosition(line: span.start.line, column: span.start.column),
                end: IDEPosition(line: span.end.line, column: span.end.column),
            )

            Queries.highlightType(self.result.db, node) { tokens[range] = "type" }
            Queries.highlightTrait(self.result.db, node) { tokens[range] = "interface" }
            Queries.highlightTypeParamter(self.result.db, node) { tokens[range] = "typeParameter" }
            Queries.highlightFunction(self.result.db, node) { tokens[range] = "function" }
        }

        return tokens.map { IDESemanticToken(range: $0.key, type: $0.value) }
    }

    @JS func hover(line: Int, column: Int) -> IDEHover? {
        guard let node = self.node(atLine: line, column: column),
            let span = self.result.db[node, Syntax.self]?.value.span
        else { return nil }

        var contents: [IDEHoverItem] = []

        let definitionNode = self.result.db[node, Resolved.self]?.definitions.first

        if let definitionNode,
            let definition = self.result.db[definitionNode, Defined.self]?.definition,
            !(definition is VariableDefinition)
        {
            let renderContext = RenderContext(db: self.result.db)
            renderContext.node(definitionNode)

            let (string, _) = renderContext.render(with: { $0.plainText(db: self.result.db) })

            contents.append(.init(value: string, isCode: true))
        } else {
            Queries.hasType(self.result.db, node) { type in
                let renderContext = RenderContext(db: self.result.db)

                if definitionNode != nil {
                    renderContext.node(node)
                    renderContext.string(" :: ")
                }

                Type.constructed(type).render(into: renderContext)

                let (string, _) = renderContext.render(with: { $0.plainText(db: self.result.db) })

                contents.append(.init(value: string, isCode: true))
            }
        }

        Queries.resolvedBound(self.result.db, node) { bound, resolved in
            let renderContext = RenderContext(db: self.result.db)
            renderContext.string("instance (")
            bound.render(into: renderContext)
            renderContext.string(")")

            let (string, _) = renderContext.render(with: { $0.plainText(db: self.result.db) })

            contents.append(.init(value: string, isCode: true))
        }

        Queries.comments(includeLinks: false)(self.result.db, node) { comments in
            let writer = FeedbackWriter(db: self.result.db)
            writer.write(comments)
            let (string, _) = writer.finish(with: {
                $0.markdown(db: self.result.db, showSpan: false)
            })

            guard !string.isEmpty else { return }

            contents.append(.init(value: string, isCode: false))
        }

        guard !contents.isEmpty else { return nil }

        return IDEHover(
            contents: contents,
            range: IDERange(
                start: IDEPosition(line: span.start.line, column: span.start.column),
                end: IDEPosition(line: span.end.line, column: span.end.column),
            ),
        )
    }

    @JS func highlight(line: Int, column: Int) -> [IDERange] {
        guard let node = self.node(atLine: line, column: column) else { return [] }

        let filter = defaultFilter(db: self.result.db)

        var highlights: [IDERange] = []
        Queries.inGroup(self.result.db, node) { related in
            guard filter(related), let span = self.result.db[related, Syntax.self]?.value.span
            else { return }

            highlights.append(
                IDERange(
                    start: IDEPosition(line: span.start.line, column: span.start.column),
                    end: IDEPosition(line: span.end.line, column: span.end.column),
                )
            )
        }

        return highlights
    }

    @JS func definition(line: Int, column: Int) -> IDERange? {
        let filter = defaultFilter(db: self.result.db)

        guard let node = self.node(atLine: line, column: column) else { return nil }

        var definition: Node?
        Queries.definitions(self.result.db, node) { definitions in definition = definitions.first }

        guard let definition, filter(definition),
            let span = self.result.db[definition, Syntax.self]?.value.span
        else { return nil }

        return IDERange(
            start: IDEPosition(line: span.start.line, column: span.start.column),
            end: IDEPosition(line: span.end.line, column: span.end.column),
        )
    }

    @JS func references(line: Int, column: Int) -> [IDERange] {
        let filter = defaultFilter(db: self.result.db)

        guard let node = self.node(atLine: line, column: column) else { return [] }

        var references: [IDERange] = []
        Queries.references(self.result.db, node) { reference in
            guard filter(reference), let span = self.result.db[reference, Syntax.self]?.value.span
            else { return }

            references.append(
                IDERange(
                    start: IDEPosition(line: span.start.line, column: span.start.column),
                    end: IDEPosition(line: span.end.line, column: span.end.column),
                )
            )
        }

        return references
    }

    private func node(atLine line: Int, column: Int) -> Node? {
        var matches: [(node: Node, length: Int)] = []
        for node in self.result.db.ownedNodes {
            guard let span = self.result.db[node, Syntax.self]?.value.span else { continue }

            if span.start.line == line && span.start.column <= column && span.end.line == line
                && span.end.column >= column
            {
                let length = span.end.column - span.start.column
                matches.append((node, length))
            }
        }

        matches.sort(by: { $0.length < $1.length })

        return matches.first?.node
    }
}
