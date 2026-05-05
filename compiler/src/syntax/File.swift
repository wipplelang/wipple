public struct FileSyntax: Sendable {
    public let span: Span
    public let statements: [any Visitable]
}

public struct ParseErrorSyntax: Visitable {
    public let span: Span
    public let error: ParseError

    public func visit(node: Node, with visitor: Visitor) {
        visitor.db[node, ParseError.self] = self.error
    }
}

public func parseFile(with parser: Parser) throws(ParseError) -> FileSyntax {
    let span = parser.spanned()
    let statements = try parseStatements(with: parser)
    _ = try parseComments(with: parser)
    return FileSyntax(span: span(), statements: statements)
}

extension FileSyntax: Visitable {
    public var isHidden: Bool { true }

    public func visit(node: Node, with visitor: Visitor) {
        let statements = self.statements.map { visitor.visit($0) }

        visitor.codegen(node: node, with: FileCodegen(node: node, statements: statements))
    }
}

private struct FileCodegen: Codegenable {
    let node: Node
    let statements: [Node]

    func codegen(with context: CodegenContext) throws {
        for statement in self.statements {
            context.instruction(.trace(location: statement))
            try context.codegen(statement)
        }
    }
}
