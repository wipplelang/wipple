import Compiler
import JavaScriptKit

@JS struct DocumentationItem {
    var declaration: String
    var kind: String
    var docs: String
}

extension CompileResult {
    @JS func documentation() -> [String: DocumentationItem] {
        var items: [String: DocumentationItem] = [:]
        self.db.forEachFact(Defined.self) { node, _ in
            Queries.documentation(includeLinks: false)(self.db, node) { documentation in
                guard let name = documentation.name else { return }

                let writer = FeedbackWriter(db: self.db)
                writer.write(documentation.comments)
                let (docs, _) = writer.finish { $0.markdown(db: self.db) }

                items[name] = .init(
                    declaration: documentation.declaration,
                    kind: documentation.kind,
                    docs: docs,
                )
            }
        }

        return items
    }
}
