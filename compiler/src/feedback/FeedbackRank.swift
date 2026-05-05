public enum FeedbackRank: Comparable {
    case syntax
    case names
    case custom
    case conflicts
    case bounds
    case customDefault
    case exhaustiveness
    case unknown
    case placeholders
}

public func sortByRank(db: DB, feedback items: inout [FeedbackItem]) {
    // Reduce noise by preferring lower-rank items
    if let minRank = items.lazy.map(\.rank).min() { items.removeAll(where: { $0.rank != minRank }) }

    let span = { (item: FeedbackItem) in db[item.location.primary, Syntax.self]?.value.span }

    items.sort {
        switch (span($0), span($1)) {
        case (nil, nil): return true
        case (nil, _): return false
        case (_, nil): return true
        case (let left?, let right?): return left < right
        }
    }
}
