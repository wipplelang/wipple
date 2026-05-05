import OrderedCollections

public struct FeedbackItem {
    public let id: String
    public let rank: FeedbackRank
    public let showGraph: Bool
    public let location: (primary: Node, secondary: OrderedSet<Node>)
    public let display: (DB, @escaping (RenderSegment) -> String) -> (String, OrderedSet<Node>)
}

public func collectFeedback(db: DB, filter: (FeedbackItem) -> Bool) -> [FeedbackItem] {
    let context = FeedbackContext()

    var items: [FeedbackItem] = []
    for node in db.ownedNodes {
        for query in context.queries {
            query(db, node) { item in if filter(item) { items.append(item) } }
        }
    }

    sortByRank(db: db, feedback: &items)

    return items
}

class FeedbackContext {
    fileprivate var queries: [Query<FeedbackItem>] = []

    init() {
        registerAttributeFeedback(into: self)
        registerBoundFeedback(into: self)
        registerConstantFeedback(into: self)
        registerFormatFeedback(into: self)
        registerFunctionFeedback(into: self)
        registerInstanceFeedback(into: self)
        registerNameFeedback(into: self)
        registerPatternFeedback(into: self)
        registerPlaceholderFeedback(into: self)
        registerStructureFeedback(into: self)
        registerSyntaxFeedback(into: self)
        registerTypeDefinitionFeedback(into: self)
        registerTypeFeedback(into: self)
    }

    func register<each T>(
        id: String,
        query: @escaping Query<repeat each T>,
        rank: @escaping (repeat each T) -> FeedbackRank,
        location: @escaping (repeat each T) -> (primary: Node, secondary: OrderedSet<Node>),
        showGraph: Bool = false,
        display: @escaping (FeedbackWriter, repeat each T) -> Void,
    ) {
        self.queries.append { db, node, body in
            query(db, node) { (values: repeat each T) in
                let rank = rank(repeat each values)
                let location = location(repeat each values)

                let item = FeedbackItem(
                    id: id,
                    rank: rank,
                    showGraph: showGraph,
                    location: location,
                ) { db, render in
                    let writer = FeedbackWriter(db: db)
                    display(writer, repeat each values)
                    return writer.finish(with: render)
                }

                body(item)
            }
        }
    }
}
