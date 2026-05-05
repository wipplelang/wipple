enum GroupOrder: Comparable {
    case pattern
    case expression
    case type
    case other

    init(db: DB, node: Node) {
        if db.contains(IsPattern.self, for: node) {
            self = .pattern
        } else if db.contains(IsExpression.self, for: node) {
            self = .expression
        } else if db.contains(IsType.self, for: node) {
            self = .type
        } else {
            self = .other
        }
    }
}
