extension Queries {
    public static var unusedBlock: Query<> {
        { db, node, body in
            guard let syntax = db[node, Syntax.self]?.value, syntax is BlockExpressionSyntax,
                let statement = db[node, Parent.self]?.parent,
                let statementSyntax = db[statement, Syntax.self]?.value,
                statementSyntax is ExpressionStatementSyntax,
                let statements = db[statement, Parent.self]?.parent,
                let statementsSyntax = db[statements, Syntax.self]?.value
            else { return }

            let statementNodes = db[statements, Children.self]?.children ?? []

            let checkLast: Bool
            if statementsSyntax is BlockExpressionSyntax {
                checkLast = true
            } else if statementsSyntax is FileSyntax {
                checkLast = false
            } else {
                return
            }

            guard statementNodes.lazy.reversed().dropFirst(checkLast ? 0 : 1).contains(statement)
            else { return }

            body()
        }
    }
}
