import DequeModule
import OrderedCollections

public struct Syntax: Fact { public let value: any Visitable }

public struct Resolved: Fact {
    public let name: String
    public let definitions: OrderedSet<Node>

    public func render(into context: RenderContext) {
        if self.definitions.isEmpty {
            context.string("unresolved")
            return
        }

        context.string("resolved to \(self.definitions.count) definition(s): ")

        for (index, definition) in self.definitions.enumerated() {
            if index > 0 { context.string(", ") }

            context.node(definition)
        }
    }
}

struct DefinitionConstraints: Fact {
    public var constraints: [Constraint] = []

    func render(into context: RenderContext) { context.string("has definition constraints") }
}

public struct TypeParameters: Fact {
    public var typeParameters: [Node] = []

    public func render(into context: RenderContext) { context.string("has type parameters") }
}

public struct Captures: Fact {
    public var captures: OrderedSet<Node> = []

    public func render(into context: RenderContext) {
        if self.captures.isEmpty {
            context.string("no captures")
            return
        }

        context.string("captures ")

        for (index, capture) in self.captures.enumerated() {
            if index > 0 { context.string(", ") }

            context.node(capture)
        }
    }
}

public protocol Visitable: Sendable {
    var span: Span { get }
    var isHidden: Bool { get }
    func visit(node: Node, with visitor: Visitor)
}

extension Visitable { public var isHidden: Bool { false } }

public struct VisitAs: Visitable {
    public let syntax: any Visitable
    public let node: Node

    public init(_ syntax: any Visitable, as node: Node) {
        self.syntax = syntax
        self.node = node
    }

    public var span: Span { self.syntax.span }

    public func visit(node: Node, with visitor: Visitor) {
        assert(self.node == node)

        self.syntax.visit(node: node, with: visitor)
    }
}

private let utilities = ["Mismatched"]

public class Visitor {
    class Scope {
        var names: [String: [Definition]] = [:]
        var defined: OrderedSet<Node> = []
        var capturedVariables: OrderedSet<Node> = []

        func peek<T: Definition>(name: String) -> [T] {
            self.peek(name: name, match: { $0 as? T })
        }

        func peek<T>(name: String, match: @escaping (Definition) -> T?) -> [T] {
            self.names[name]?.compactMap(match) ?? []
        }
    }

    struct CurrentDefinition {
        let node: Node
        var implicitTypeParameters = false
        var isWithinConstantValue = false

        func constraints(db: DB) -> [Constraint] {
            db[self.node, DefinitionConstraints.self, default: .init()].constraints
        }
    }

    struct CurrentMatch {
        var root: Node?
        var arm: Node?  // opt into exhaustiveness checking
        var value: Node
        var allowOr: Bool
        var allowSet: Bool
        var isMutable: Bool
        var path: MatchPath
    }

    struct Result {
        var constraints: [Constraint] = []
        var definitions: [String: [Definition]] = [:]
        var utilities: [String: Node] = [:]
    }

    struct Queue {
        var typeDefinitions: Deque<() -> Void> = []
        var allDefinitions: Deque<() -> Void> = []
        var allExpressions: Deque<() -> Void> = []

        mutating func dequeue() -> (() -> Void)? {
            self.typeDefinitions.popFirst() ?? self.allDefinitions.popFirst()
                ?? self.allExpressions.popFirst()
        }
    }

    let db: DB
    var currentNode: Node?
    var currentDefinition: CurrentDefinition?
    var currentMatch: CurrentMatch?
    private var scopes: [Scope]
    private var constraints: [Constraint] = []
    private var queue = Queue()

    init(db: DB, definitions: some Sequence<[String: [Definition]]>) {
        self.db = db

        self.scopes = definitions.map { names in
            let scope = Scope()
            scope.names = names
            return scope
        }

        self.scopes.append(Scope())
    }

    func visit(_ value: some Visitable) -> Node {
        let node =
            if let value = value as? VisitAs { value.node } else {
                self.db.node(isHidden: value.isHidden)
            }

        self.visit(value, as: node)

        return node
    }

    func visit(_ value: some Visitable, as node: Node) {
        let parent = self.currentNode

        self.currentNode = node
        defer { self.currentNode = parent }

        if let parent {
            self.db[node, Parent.self] = .init(parent: parent)
            self.db[parent, Children.self, default: .init()].children.append(node)
        }

        self.db[node, Syntax.self] = .init(value: value)

        value.visit(node: node, with: self)
    }

    func constraint(_ constraint: Constraint) {
        if let definition = self.currentDefinition {
            if definition.isWithinConstantValue { constraint.canInstantiate = false }

            self.db[definition.node, DefinitionConstraints.self, default: .init()].constraints
                .append(constraint)
        } else {
            self.constraints.append(constraint)
        }
    }

    func codegen(node: Node, with value: some Codegenable) {
        self.db[node, Codegen.self] = .init(value: value)
    }

    func pushScope() { self.scopes.append(Scope()) }

    var currentScope: Scope { self.scopes.last! }

    func popScope() { self.scopes.removeLast() }

    func resolve<T: Definition>(name: String, as node: Node) -> T? {
        self.resolve(name: name, as: node) { $0 as? T }
    }

    func resolve<T: Definition>(name: String, as node: Node, match: @escaping (Definition) -> T?)
        -> T?
    {
        self.resolve(name: name, as: node) { definition in
            guard let definition = match(definition) else { return nil }
            return (definition, definition.node)
        }
    }

    func resolve<T>(name: String, as node: Node, match: @escaping (Definition) -> (T, Node)?) -> T?
    {
        let matches = self.peek(name: name, match: match)

        var result: T?
        var definitions: OrderedSet<Node> = []

        for (next, node) in matches {
            if result == nil { result = next }
            definitions.append(node)
        }

        self.db[node, Resolved.self] = .init(name: name, definitions: definitions)

        return result
    }

    func peek<T: Definition>(name: String) -> [T] { self.peek(name: name) { $0 as? T } }

    func peek<T>(name: String, match: @escaping (Definition) -> T?) -> [T] {
        self.scopes.reversed().map { $0.peek(name: name, match: match) }
            .first(where: { !$0.isEmpty }) ?? []
    }

    func define(_ definition: Definition) {
        self.currentScope.defined.append(definition.node)
        self.currentScope.names[definition.name!, default: []].append(definition)
    }

    func capture(_ node: Node) -> Bool {
        var captured = false
        for scope in self.scopes.lazy.reversed() {
            guard !scope.defined.contains(node) else { break }

            scope.capturedVariables.append(node)
            captured = true
        }

        return captured
    }

    @discardableResult func defining<T: Definition>(_ node: Node, perform action: () -> T) -> T {
        let existingDefinition = self.currentDefinition
        defer { self.currentDefinition = existingDefinition }

        self.currentDefinition = .init(node: node)

        let resultDefinition = action()

        self.db[node, Defined.self] = .init(definition: resultDefinition)

        return resultDefinition
    }

    func withImplicitTypeParameters<T>(perform action: () -> T) -> T {
        let previous = self.currentDefinition!.implicitTypeParameters
        self.currentDefinition!.implicitTypeParameters = true
        defer { self.currentDefinition!.implicitTypeParameters = previous }
        return action()
    }

    func withinConstantValue<T>(perform action: () -> T) -> T {
        let previous = self.currentDefinition!.isWithinConstantValue
        self.currentDefinition!.isWithinConstantValue = true
        defer { self.currentDefinition!.isWithinConstantValue = previous }
        return action()
    }

    func matching<T>(
        value: Node,
        allowOr: Bool = false,
        allowSet: Bool = false,
        perform action: () -> T,
    ) -> T {
        let existingMatch = self.currentMatch
        defer { self.currentMatch = existingMatch }

        self.currentMatch = .init(
            root: existingMatch?.root,
            arm: existingMatch?.arm,
            value: value,
            allowOr: allowOr,
            allowSet: allowSet,
            isMutable: true,
            path: existingMatch?.path ?? [],
        )

        return action()
    }

    func visitMatching(pattern: some Visitable, segment: MatchPathSegment? = nil) -> (
        pattern: Node, temporary: Node
    ) {
        let temporary = self.db.node()

        let pattern = self.matching(value: temporary, allowOr: true, allowSet: false) {
            if self.currentMatch!.root == nil { self.currentMatch!.root = temporary }
            if let segment { self.currentMatch!.path.append(segment) }
            return self.visit(pattern)
        }

        return (pattern, temporary)
    }

    func setMatches(terminal: MatchPathSegment?) -> Node {
        self.db[self.currentNode!, Matches.self] = .init(
            value: self.currentMatch!.root!,
            arm: self.currentMatch!.arm,
            path: terminal.map { self.currentMatch!.path + CollectionOfOne($0) },
        )

        return self.currentMatch!.value
    }

    func after(
        _ keyPath: WritableKeyPath<Visitor.Queue, Deque<() -> Void>>,
        perform action: @escaping () -> Void,
    ) {
        let scopesSnapshot = self.scopes
        let currentNodeSnapshot = self.currentNode
        let currentDefinitionSnapshot = self.currentDefinition

        self.queue[keyPath: keyPath]
            .append {
                let currentScopes = self.scopes
                self.scopes = scopesSnapshot
                defer { self.scopes = currentScopes }

                let currentNode = self.currentNode
                self.currentNode = currentNodeSnapshot
                defer { self.currentNode = currentNode }

                let currentDefinition = self.currentDefinition
                self.currentDefinition = currentDefinitionSnapshot
                defer { self.currentDefinition = currentDefinition }

                action()
            }
    }

    func finish() -> Result {
        while let action = self.queue.dequeue() { action() }

        var result = Result()
        result.constraints = self.constraints
        result.definitions = self.scopes.last!.names
        for name in utilities {
            let utilities = Array(self.peek(name: name, match: \.self))
            if utilities.count == 1 { result.utilities[name] = utilities[0].node }
        }

        return result
    }
}
